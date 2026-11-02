#' Run the HybriSpot processing pipeline
#'
#' Scans a folder (including sub-folders) for HybriSpot Excel probe files and
#' PDF reports, links them by Sample ID, builds the quality-controlled sample
#' database, writes the Excel, HTML and text outputs, and optionally runs the
#' follow-up analysis.
#'
#' @section Steps:
#' 1. link Excel and PDF files (duplicates are reported, one file per Sample ID is used);
#' 2. read the probe data; 3. classify probes; 4-6. combine and summarise each sample;
#' 7. read PDF metadata; 8. join Excel and PDF data and read visit dates;
#' 9. flag repeats, summarise genotypes, compare Excel and PDF results, assign QC status;
#' 10. attach file links; 11. export the database, the QC-passed database and a run summary;
#' 12. build the interactive HTML view and zip the output folder.
#'
#' @param data_folder Folder containing the run sub-folders. If `NULL`, a folder
#'   chooser (RStudio) or a prompt (interactive R) is used.
#' @param output_folder Folder for the results. Defaults to `HybriFlow_Output`
#'   in the working directory. Files made by the previous run are removed from
#'   it first; other files are never touched.
#' @param repeat_handling How repeated samples are handled in the QC-passed
#'   database: `"all"`, `"original"`, `"latest"` or `"specific"`. If `NULL`, a
#'   menu is shown in interactive sessions and `"latest"` is used otherwise.
#' @param specific_repeat Repeat number to keep when
#'   `repeat_handling = "specific"` (0 = original, 1 = first repeat, ...).
#' @param followup A list from [followup_settings()].
#' @param use_cache Save extracted data to `.rds` files so later runs read only
#'   new or changed files.
#' @param cache_dir Folder for the cache. Defaults to `HybriFlow_Cache` next to
#'   the output folder.
#' @param force_rebuild With `use_cache = TRUE`, ignore the cache and re-read
#'   every file.
#' @param split_by_year With `use_cache = TRUE`, also save one `.rds` database
#'   per visit year.
#' @param selected_years Character vector of years; exports and plots use only
#'   those years. `NULL` uses all years.
#' @param open_report Open the HTML report in a browser when finished.
#' @param zip_output Zip the output folder into `HybriFlow_Output.zip` next to it.
#' @param resource_report Print memory and disk use at the end.
#'
#' @return Invisibly, a list with `final_database`, `qc_passed`, `followup`,
#'   `run_summary` and `paths` (the files written).
#' @export
#' @examples
#' \dontrun{
#' res <- run_hybriflow(
#'   data_folder     = "D:/hybriflow_runs",
#'   output_folder   = "D:/hybriflow_results",
#'   repeat_handling = "latest",
#'   followup        = followup_settings(visits_required = 3)
#' )
#' head(res$final_database)
#' }
run_hybriflow <- function(data_folder = NULL,
                          output_folder = NULL,
                          repeat_handling = NULL,
                          specific_repeat = NA_integer_,
                          followup = followup_settings(),
                          use_cache = TRUE,
                          cache_dir = NULL,
                          force_rebuild = FALSE,
                          split_by_year = TRUE,
                          selected_years = NULL,
                          open_report = interactive(),
                          zip_output = TRUE,
                          resource_report = FALSE) {
  
  # ---- resolve folders -------------------------------------------------------
  if (is.null(data_folder)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      data_folder <- rstudioapi::selectDirectory(caption = "Select your data folder")
    } else if (interactive()) {
      cat("Enter the full path to your data folder: ")
      data_folder <- readline()
    }
  }
  if (is.null(data_folder) || !nzchar(data_folder)) stop("No data folder selected.")
  if (!dir.exists(data_folder)) stop("Data folder does not exist: ", data_folder)
  data_folder <- normalizePath(data_folder, mustWork = TRUE)
  excel_folder <- data_folder
  pdf_folder   <- data_folder
  
  if (is.null(output_folder)) output_folder <- file.path(getwd(), "HybriFlow_Output")
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
  output_folder <- normalizePath(output_folder, mustWork = TRUE)
  message("Output folder: ", output_folder)
  
  # the zip file and the cache sit next to (not inside) the output folder
  parent_folder <- dirname(output_folder)
  if (is.null(cache_dir)) cache_dir <- file.path(parent_folder, "HybriFlow_Cache")
  db_by_year_dir <- file.path(cache_dir, "database_by_year")
  cfg <- list(use = isTRUE(use_cache), force_rebuild = isTRUE(force_rebuild),
              version = "3", checkpoint_every = 100L,
              excel_cache_path = file.path(cache_dir, "excel_cache.rds"),
              pdf_cache_path   = file.path(cache_dir, "pdf_cache.rds"))
  if (cfg$use && !dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # Remove the files the previous run made, so the output folder only ever holds the latest results
  run_start <- Sys.time() - 5
  clear_previous_outputs(output_folder)
  
  # Follow-up settings (validated; see followup_settings())
  fu_settings <- validate_followup_settings(followup)
  
  
  # Step 1 - Link Excel to PDF
  message("STEP 1: Linking Excel and PDF files")
  excel_cache <- update_excel_cache(get_excel_files(excel_folder), cfg)
  
  # ---- Duplicate Excel files -------------------------------------------------
  # The same run is often present in several download folders, so one Sample ID
  # can have several Excel files. If all of them were kept, every copy would be
  # multiplied through the joins and the sample would appear many times.
  # Keep ONE file per Sample ID (the first in file order). Identical copies are
  # merged; IDs whose copies have DIFFERENT contents (real re-runs) are listed
  # in Duplicate_Excel_files_report.csv so the choice is visible.
  dup_index <- excel_cache$index %>%
    filter(valid, !is.na(sample_id)) %>%
    group_by(sample_id) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(dup_index) > 0) {
    dup_index$content_hash <- unname(tools::md5sum(dup_index$file))
    dup_report <- dup_index %>%
      group_by(sample_id) %>%
      summarise(
        n_files = n(),
        kept_file = first(file),
        other_files = paste(file[-1], collapse = " | "),
        identical_copies = n_distinct(content_hash) == 1,
        .groups = "drop"
      )
    write.csv(dup_report, file.path(output_folder, "Duplicate_Excel_files_report.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    message("Duplicate Excel files: ", nrow(dup_report), " Sample ID(s) had more than one file (",
            sum(dup_report$identical_copies), " identical copies, ",
            sum(!dup_report$identical_copies), " with DIFFERENT contents). ",
            "Only the first file of each is used. See Duplicate_Excel_files_report.csv")
    
    files_to_drop <- excel_cache$index %>%
      filter(valid, !is.na(sample_id)) %>%
      group_by(sample_id) %>%
      filter(row_number() > 1) %>%
      pull(file)
    excel_cache$index <- excel_cache$index %>% filter(!file %in% files_to_drop)
    if (".file" %in% colnames(excel_cache$probes)) {
      excel_cache$probes <- excel_cache$probes %>% filter(!.file %in% files_to_drop)
    }
  }
  
  result <- excel_cache$index %>%
    filter(valid) %>%
    transmute(
      excel_filename = basename(file),
      excel_path = file,
      sample_id = sample_id
    )
  pdf_files <- get_pdf_files(pdf_folder)
  pdf_df <- read_pdf_data(pdf_files)
  
  # One PDF per Sample ID (same reason as for the Excel files)
  if (nrow(pdf_df) > 0) {
    pdf_dup_rows <- !is.na(pdf_df$sample_id) & duplicated(pdf_df$sample_id)
    if (any(pdf_dup_rows)) {
      dup_ids <- unique(pdf_df$sample_id[pdf_dup_rows])
      pdf_dup <- pdf_df[pdf_df$sample_id %in% dup_ids, ]
      pdf_dup$content_hash <- unname(tools::md5sum(pdf_dup$pdf_path))
      pdf_dup_report <- pdf_dup %>%
        group_by(sample_id) %>%
        summarise(
          n_files = n(),
          kept_file = first(pdf_path),
          other_files = paste(pdf_path[-1], collapse = " | "),
          identical_copies = n_distinct(content_hash) == 1,
          .groups = "drop"
        )
      write.csv(pdf_dup_report, file.path(output_folder, "Duplicate_PDF_files_report.csv"),
                row.names = FALSE, fileEncoding = "UTF-8")
      message("Duplicate PDF files: ", nrow(pdf_dup_report), " Sample ID(s) had more than one PDF (",
              sum(pdf_dup_report$identical_copies), " identical copies, ",
              sum(!pdf_dup_report$identical_copies), " with DIFFERENT contents). ",
              "Only the first is used. See Duplicate_PDF_files_report.csv")
    }
    pdf_df <- pdf_df[is.na(pdf_df$sample_id) | !duplicated(pdf_df$sample_id), ]
  }
  
  if (nrow(result) > 0 && nrow(pdf_df) > 0) {
    result <- result %>% left_join(pdf_df, by = c("sample_id" = "sample_id"))
  } else if (nrow(result) > 0 && nrow(pdf_df) == 0) {
    result <- result %>% mutate(pdf_filename = NA_character_, pdf_path = NA_character_)
  } else {
    result <- tibble(
      excel_filename = character(),
      excel_path = character(),
      sample_id = character(),
      pdf_filename = character(),
      pdf_path = character()
    )
  }
  
  clean_result <- result %>%
    select(excel_filename, sample_id, pdf_filename, excel_path, pdf_path)
  
  # Step 2 - Extract probe data
  message("STEP 2: Extracting probe data")
  excluded_files <- excel_cache$index$file[!excel_cache$index$valid]
  if (length(excluded_files) > 0) {
    message("Excluded ", length(excluded_files), " non-sample file(s) (no 'Target' column found):")
    for (f in excluded_files) message("  - ", basename(f))
  }
  probe_data <- excel_cache$probes
  if (".file" %in% colnames(probe_data)) probe_data <- select(probe_data, -.file)
  
  # Excel files that are valid samples but gave NO genotype data -> list them with the reason
  files_with_probes <- if (".file" %in% colnames(excel_cache$probes)) excel_cache$probes$.file else character()
  no_genotype_files <- excel_cache$index %>%
    filter(valid, !file %in% files_with_probes) %>%
    transmute(
      run = basename(dirname(file)),
      excel_file = basename(file),
      sample_id = sample_id,
      reason = iconv(dplyr::coalesce(note, "no probe rows"), "UTF-8", "UTF-8", sub = "byte"),
      folder = dirname(file)
    )
  if (nrow(no_genotype_files) > 0) {
    message(nrow(no_genotype_files), " Excel file(s) have NO genotype data. ",
            "Details saved to: Excel_files_without_genotype_data.csv")
    write.csv(no_genotype_files, file.path(output_folder, "Excel_files_without_genotype_data.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
  }
  
  if (nrow(probe_data) == 0) {
    stop("No valid probe data were found. Check that each sample Excel file contains a 'Target' column and required probe columns.")
  }
  
  # Step 3 - Classify probes
  message("STEP 3: Classifying probes")
  probe_data_classified <- classify_probes(probe_data)
  
  # Step 4 - Combine with clean_result
  message("STEP 4: Combining Excel data")
  # Use ONE file row per Sample ID for this join. If a Sample ID matched several
  # Excel/PDF rows, every probe row was copied once per match, which repeated the
  # positions (e.g. (3,3), (3,3), (3,3), (3,3)) and inflated the discordant text.
  clean_result_one <- clean_result %>%
    distinct(sample_id, .keep_all = TRUE)
  
  n_multi <- sum(duplicated(clean_result$sample_id))
  if (n_multi > 0) {
    message("NOTE: ", n_multi, " extra Excel/PDF row(s) share a Sample ID with another ",
            "row; only the first is used for the probe join.")
  }
  
  combined <- probe_data_classified %>%
    full_join(clean_result_one, by = c("Sample ID" = "sample_id")) %>%
    select(excel_filename, `Sample ID`, pdf_filename, Target, Position, TH, LTH, Value, Result, Probe_Category) %>%
    distinct()   # drop exact duplicate probe rows
  
  # Step 5 - Create named sample list
  message("STEP 5: Creating sample list")
  sample_list <- create_sample_list(combined)
  message("Number of sample groups: ", length(sample_list))
  
  # Step 6 - Build database
  message("STEP 6: Building sample database")
  database <- map_dfr(sample_list, summarise_sample)
  message("Database rows: ", nrow(database))
  
  if (nrow(database) == 0) {
    stop("The sample database is empty.")
  }
  
  genotype_cols <- colnames(database)[grepl("^HPV_", colnames(database)) & colnames(database) != "HPV_Universal_Probe"]
  
  database <- database %>%
    select(
      Sample_ID,
      Positive_Control_Pass,
      NTC_Pass,
      Discordant,
      HPV_Universal_Probe,
      all_of(genotype_cols),
      Discordant_Comments
    )
  
  # Step 7 - Extract PDF metadata
  message("STEP 7: Extracting PDF metadata")
  pdf_metadata <- update_pdf_cache(pdf_folder, cfg)
  message("PDF metadata rows: ", nrow(pdf_metadata))
  
  # One metadata row per Sample ID (a second copy of a PDF would otherwise add a second row)
  pdf_paths_attr <- attr(pdf_metadata, "pdf_paths")
  n_meta_before <- nrow(pdf_metadata)
  pdf_metadata <- pdf_metadata[is.na(pdf_metadata$Sample_ID) | !duplicated(pdf_metadata$Sample_ID), ]
  attr(pdf_metadata, "pdf_paths") <- pdf_paths_attr
  message("Duplicate PDF metadata rows removed: ", n_meta_before - nrow(pdf_metadata))
  
  # Step 7.5 - Link PDFs that the FILE NAME did not match.
  # The first link (Step 1) uses the ID in the PDF file name (after "report_"/"report-").
  # The metadata, however, uses the Sample ID printed INSIDE the PDF. If the file name
  # does not follow the pattern, the metadata is found but the PDF link is missing.
  # Here the missing links are filled in using the Sample ID from inside the PDF.
  pdf_paths_by_content <- attr(pdf_metadata, "pdf_paths")
  if (!is.null(pdf_paths_by_content) && nrow(pdf_paths_by_content) > 0 && nrow(result) > 0) {
    content_lookup <- pdf_paths_by_content %>%
      filter(!is.na(Sample_ID)) %>%
      group_by(Sample_ID) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(
        sample_id = Sample_ID,
        pdf_path_content = .file,
        pdf_filename_content = basename(.file)
      )
    
    missing_before <- sum(is.na(result$pdf_path))
    
    result <- result %>%
      left_join(content_lookup, by = "sample_id") %>%
      mutate(
        pdf_path = dplyr::coalesce(pdf_path, pdf_path_content),
        pdf_filename = dplyr::coalesce(pdf_filename, pdf_filename_content)
      ) %>%
      select(-pdf_path_content, -pdf_filename_content)
    
    clean_result <- result %>%
      select(excel_filename, sample_id, pdf_filename, excel_path, pdf_path)
    
    message("PDF links recovered using the Sample ID inside the PDF: ",
            missing_before - sum(is.na(result$pdf_path)),
            " | still without a PDF: ", sum(is.na(result$pdf_path)))
  }
  
  # Step 8 - Join Excel database with PDF metadata
  message("STEP 8: Joining Excel and PDF data")
  final_database <- database %>% left_join(pdf_metadata, by = "Sample_ID")
  
  final_database <- final_database %>%
    mutate(
      Visit_Date = parse_visit_date_from_id(Sample_ID),
      # Year comes from the dedicated extractor; falls back to Visit_Date
      Visit_Year = as.character(dplyr::coalesce(
        extract_visit_year_from_id(Sample_ID),
        as.integer(format(Visit_Date, "%Y"))
      ))
    )
  
  # Some dates need help: 2-digit-year dates (070422) are read here, and dates that can be read two
  # ways (1092018, 11032025) are settled with the visit code, which increases with every visit.
  # Every decision is saved for checking.
  resolved_dates <- resolve_visit_dates(final_database$Sample_ID, final_database$Visit_Date)
  final_database$Visit_Date <- resolved_dates$visit_date
  if (nrow(resolved_dates$audit) > 0) {
    dec <- resolved_dates$audit$decision
    message(nrow(resolved_dates$audit), " date(s) were read or checked using the visit code or the data's own ",
            "date order: ", sum(grepl("^changed", dec)), " changed, ", sum(grepl("^read as", dec)),
            " read from 2-digit years, ", sum(grepl("check", dec)), " to check. ",
            "Saved for checking: Visit_dates_checked_against_visit_code.csv")
    write.csv(resolved_dates$audit, file.path(output_folder, "Visit_dates_checked_against_visit_code.csv"),
              row.names = FALSE)
  }
  
  # Audit list: IDs whose year came from a 2-digit year (25 -> 2025).
  # These are the least certain, so check a few by eye.
  two_digit_ids <- final_database %>%
    filter(!is.na(Visit_Year),
           is.na(extract_visit_year_from_id(Sample_ID, allow_two_digit = FALSE))) %>%
    distinct(Sample_ID, Visit_Year)
  if (nrow(two_digit_ids) > 0) {
    message(nrow(two_digit_ids), " Sample_ID(s) got their year from a 2-digit year. ",
            "Saved for checking: Sample_IDs_year_from_2digit.csv")
    write.csv(two_digit_ids, file.path(output_folder, "Sample_IDs_year_from_2digit.csv"),
              row.names = FALSE)
  }
  
  # Report Sample_IDs where no year could be read (nothing is guessed)
  unparsed_dates <- final_database %>%
    filter(is.na(Visit_Year)) %>%
    distinct(Sample_ID)
  if (nrow(unparsed_dates) > 0) {
    warning(nrow(unparsed_dates), " Sample_ID(s) have no readable year, e.g.: ",
            paste(head(unparsed_dates$Sample_ID, 5), collapse = ", "))
    write.csv(unparsed_dates, file.path(output_folder, "Sample_IDs_without_year.csv"),
              row.names = FALSE)
    message("List of IDs with no year saved to: ",
            file.path(output_folder, "Sample_IDs_without_year.csv"))
  }
  
  if (nrow(final_database) == 0) {
    stop("final_database is empty after joining metadata.")
  }
  
  # Step 9 - Detect repeat/rerun notes and keep separate from QC comments
  all_sample_ids <- unique(final_database$Sample_ID)
  
  final_database <- final_database %>%
    mutate(
      stripped_id = str_remove(Sample_ID, "-[0-9]+$"),
      suffix_number = as.integer(str_match(Sample_ID, "-([0-9]+)$")[,2]),
      
      is_confirmed_rerun =
        !is.na(suffix_number) &
        stripped_id %in% all_sample_ids &
        stripped_id != Sample_ID,
      
      Base_Sample_ID = ifelse(is_confirmed_rerun, stripped_id, Sample_ID),
      Rerun_Number = ifelse(is_confirmed_rerun, suffix_number, 0)
    ) %>%
    select(-stripped_id, -suffix_number, -is_confirmed_rerun) %>%
    group_by(Base_Sample_ID) %>%
    mutate(
      Repeat_Comments = case_when(
        n() == 1 ~ NA_character_,
        Rerun_Number == 0 ~ paste0("Original sample (", n(), " total entries incl. repeats; processed ", Processed_Date, ")"),
        TRUE ~ paste0("REPEAT #", Rerun_Number, " of sample ", Base_Sample_ID, " (processed ", Processed_Date, ")")
      )
    ) %>%
    ungroup() %>%
    select(-Base_Sample_ID, -Rerun_Number)
  
  # Step 9.6 - Summary variables
  hr_present_cols <- intersect(paste0("HPV_", high_risk), colnames(final_database))
  lr_present_cols <- intersect(paste0("HPV_", low_risk), colnames(final_database))
  
  final_database <- final_database %>%
    rowwise() %>%
    mutate(
      High_Risk_Count = sum(c_across(all_of(hr_present_cols)) == "POSITIVE", na.rm = TRUE),
      Low_Risk_Count = sum(c_across(all_of(lr_present_cols)) == "POSITIVE", na.rm = TRUE),
      High_Risk_HPV_Detected = ifelse(High_Risk_Count > 0, "YES", "NO"),
      Low_Risk_HPV_Detected = ifelse(Low_Risk_Count > 0, "YES", "NO"),
      Total_HPV_Genotypes_Detected = High_Risk_Count + Low_Risk_Count,
      Any_HPV_Detected = ifelse(Total_HPV_Genotypes_Detected > 0 | HPV_Universal_Probe == "POSITIVE", "YES", "NO")
    ) %>%
    ungroup()
  
  # Step 9.7 - Reorder summary variables
  genotype_cols_final <- colnames(final_database)[grepl("^HPV_", colnames(final_database)) & colnames(final_database) != "HPV_Universal_Probe"]
  
  other_cols <- setdiff(
    colnames(final_database),
    c(
      genotype_cols_final,
      "Any_HPV_Detected",
      "High_Risk_HPV_Detected",
      "High_Risk_Count",
      "Low_Risk_HPV_Detected",
      "Low_Risk_Count",
      "Total_HPV_Genotypes_Detected",
      "Discordant_Comments",
      "Repeat_Comments"
    )
  )
  
  final_database <- final_database %>%
    select(
      all_of(
        other_cols[
          other_cols %in% c(
            "Sample_ID",
            "Positive_Control_Pass",
            "NTC_Pass",
            "Discordant",
            "HPV_Universal_Probe"
          )
        ]
      ),
      all_of(genotype_cols_final),
      Any_HPV_Detected,
      High_Risk_HPV_Detected,
      High_Risk_Count,
      Low_Risk_HPV_Detected,
      Low_Risk_Count,
      Total_HPV_Genotypes_Detected,
      Discordant_Comments,
      Repeat_Comments,
      everything()
    )
  
  # Step 9.8 - Compare PDF HPV result with Excel-derived result
  final_database <- final_database %>%
    mutate(
      Result_Consistency = case_when(
        is.na(HPV_Status) ~ "CANNOT COMPARE",
        HPV_Status == "BLANK" ~ "INCONCLUSIVE - Insufficient material (result not reliable)",
        HPV_Status == "NEGATIVE" & HPV_Universal_Probe == "POSITIVE" & Total_HPV_Genotypes_Detected == 0 ~
          "MATCH - Universal probe positive only (differing genotype sensitivities; does not invalidate NEGATIVE result)",
        HPV_Status == "POSITIVE" & Any_HPV_Detected == "YES" ~ "MATCH",
        HPV_Status == "NEGATIVE" & Any_HPV_Detected == "NO" ~ "MATCH",
        TRUE ~ "MISMATCH"
      )
    )
  
  # Step 9.85 - Overall QC status and final comments
  #
  # BLANK samples and true QC failures do not pass QC.
  # Discordant samples remain QC-passed but are highlighted for review.
  
  final_database <- final_database %>%
    mutate(
      Overall_QC_Status = case_when(
        HPV_Status == "BLANK" ~
          "FAIL - Insufficient genetic material",
        
        Positive_Control_Pass == "NO" &
          NTC_Pass == "NO" ~
          "FAIL - Control and NTC failure",
        
        Positive_Control_Pass == "NO" ~
          "FAIL - Control failure",
        
        NTC_Pass == "NO" ~
          "FAIL - NTC failure",
        
        TRUE ~
          "PASS"
      ),
      
      Comments = case_when(
        HPV_Status == "BLANK" &
          Positive_Control_Pass == "NO" &
          NTC_Pass == "NO" ~
          paste(
            "Possible rerun: failed QC (Hybridisation Control, Amplification Control, NTC);",
            "Possible false negative: BLANK (insufficient genetic material to determine genotype)"
          ),
        
        HPV_Status == "BLANK" &
          Positive_Control_Pass == "NO" ~
          paste(
            "Possible rerun: failed QC (Hybridisation Control, Amplification Control);",
            "Possible false negative: BLANK (insufficient genetic material to determine genotype)"
          ),
        
        HPV_Status == "BLANK" &
          NTC_Pass == "NO" ~
          paste(
            "Possible rerun: failed QC (NTC);",
            "Possible false negative: BLANK (insufficient genetic material to determine genotype)"
          ),
        
        HPV_Status == "BLANK" ~
          "Possible false negative: BLANK (insufficient genetic material to determine genotype)",
        
        Positive_Control_Pass == "NO" &
          NTC_Pass == "NO" ~
          "Possible rerun: failed QC (Hybridisation Control, Amplification Control, NTC)",
        
        Positive_Control_Pass == "NO" ~
          "Possible rerun: failed QC (Hybridisation Control, Amplification Control)",
        
        NTC_Pass == "NO" ~
          "Possible rerun: failed QC (NTC)",
        
        Discordant == "YES" ~
          "Review: discordant genotype result; possible false negative",
        
        TRUE ~
          NA_character_
      )
    )
  
  # Step 10 - Attach exact file paths for Excel/PDF links
  path_lookup <- result %>%
    distinct(sample_id, excel_path, pdf_path) %>%
    rename(Sample_ID = sample_id)
  
  final_database <- final_database %>%
    left_join(path_lookup, by = "Sample_ID")
  
  # Safety check: every Sample_ID must appear exactly once
  n_dup_ids <- sum(duplicated(final_database$Sample_ID))
  if (n_dup_ids > 0) {
    dup_rows <- final_database[final_database$Sample_ID %in%
                                 final_database$Sample_ID[duplicated(final_database$Sample_ID)], ]
    write.csv(dup_rows, file.path(output_folder, "Duplicate_Sample_IDs_removed.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    final_database <- final_database[!duplicated(final_database$Sample_ID), ]
    warning(n_dup_ids, " duplicate row(s) were still present after the file clean-up and were removed ",
            "(first row kept). See Duplicate_Sample_IDs_removed.csv")
  } else {
    message("CHECK PASSED: every Sample_ID appears exactly once (", nrow(final_database), " samples).")
  }
  
  # Put Visit_Date immediately after Sample_ID.
  # Keep Comments and Repeat_Comments before the file-link columns.
  
  final_database <- final_database %>%
    select(
      Sample_ID,
      Visit_Date,
      everything()
    )
  
  columns_before_links <- setdiff(
    colnames(final_database),
    c(
      "Comments",
      "Repeat_Comments",
      "excel_path",
      "pdf_path"
    )
  )
  
  final_database <- final_database %>%
    select(
      all_of(columns_before_links),
      Comments,
      Repeat_Comments,
      any_of(c("excel_path", "pdf_path"))
    )
  
  # ======================================================
  # Repeat handling (argument, or a menu when interactive)
  # ======================================================
  repeat_labels <- c(all = "Keep ALL repeats", original = "Keep ORIGINAL only",
                     latest = "Keep LATEST only", specific = "Keep a SPECIFIC repeat number")
  if (is.null(repeat_handling)) {
    if (interactive()) {
      pick <- utils::select.list(
        choices = c("1. Keep all repeats", "2. Keep the original only",
                    "3. Keep the latest repeat only", "4. Keep a specific repeat number"),
        title = "How should repeated samples be handled?",
        graphics = capabilities("tcltk"))
      repeat_handling <- switch(substr(pick, 1, 1), "1" = "all", "2" = "original",
                                "3" = "latest", "4" = "specific", "latest")
      if (repeat_handling == "specific") {
        cat("Enter the repeat number to keep (0 = original, 1 = first repeat, etc.): ")
        specific_repeat <- suppressWarnings(as.integer(readline()))
      }
    } else {
      repeat_handling <- "latest"
    }
  }
  repeat_handling <- match.arg(repeat_handling, names(repeat_labels))
  specific_repeat_number <- suppressWarnings(as.integer(specific_repeat))
  if (repeat_handling == "specific" && is.na(specific_repeat_number)) {
    message("Invalid or missing repeat number - defaulting to LATEST only.")
    repeat_handling <- "latest"
  }
  repeat_choice <- unname(repeat_labels[repeat_handling])
  
  # Step 10.5 - Save the database as .rds (fast to reload, keeps all column types)
  if (cfg$use) message("STEP 10.5: Saving database to .rds")
  save_rds_safe(final_database, file.path(cache_dir, "final_database.rds"), cfg$use)
  
  if (cfg$use && isTRUE(split_by_year)) {
    if (!dir.exists(db_by_year_dir)) dir.create(db_by_year_dir, recursive = TRUE)
    unlink(list.files(db_by_year_dir, pattern = "\\.rds$", full.names = TRUE))
    year_labels <- ifelse(is.na(final_database$Visit_Year), "Unknown",
                          as.character(final_database$Visit_Year))
    for (yr in unique(year_labels)) {
      save_rds_safe(final_database[year_labels == yr, ],
                    file.path(db_by_year_dir, paste0("database_", yr, ".rds")), cfg$use)
    }
    message("Saved ", length(unique(year_labels)), " yearly database file(s) to: ", db_by_year_dir)
  }
  
  # Optional: restrict everything below (exports, plots) to chosen years
  if (!is.null(selected_years)) {
    final_database <- final_database %>% filter(as.character(Visit_Year) %in% as.character(selected_years))
    message("selected_years filter applied: ", nrow(final_database), " sample(s) kept.")
    if (nrow(final_database) == 0) stop("No samples found for selected_years.")
  }
  
  # Step 11 - Export main database
  message("STEP 11: Exporting final database")
  excel_out_path <- file.path(output_folder, "final_database.xlsx")
  t_export <- Sys.time()
  export_flagged_excel(final_database, excel_out_path)
  message("  Final database export took ", round(as.numeric(difftime(Sys.time(), t_export, units = "mins")), 1), " min")
  
  # Step 11.6 - Create QC-passed dataset
  #
  # This excludes:
  #   - BLANK samples
  #   - failed control samples
  #   - failed NTC samples
  #
  # Discordant samples remain included because discordance is a review flag,
  # not a QC failure.
  
  qc_passed <- final_database %>%
    filter(
      Overall_QC_Status == "PASS"
    ) %>%
    mutate(
      suffix_number = as.integer(
        str_match(
          as.character(Sample_ID),
          "-([0-9]+)$"
        )[, 2]
      ),
      
      suffix_number = ifelse(
        is.na(suffix_number),
        0L,
        suffix_number
      ),
      
      # A letter right after the date also marks a repeat (070422b = first repeat of 070422).
      suffix_number = suffix_number + letter_repeat_rank(Sample_ID),
      
      stripped_id = strip_repeat_marker(Sample_ID)
    ) %>%
    group_by(stripped_id) %>%
    mutate(
      Total_Entries_For_Sample = n()
    ) %>%
    ungroup()
  
  # Select which repeat to keep
  #
  # Visit_Date is used to determine the latest run.
  # Discordant samples remain included because they passed QC.
  
  if (repeat_choice == "Keep ORIGINAL only") {
    
    qc_passed <- qc_passed %>%
      filter(suffix_number == 0)
    
  } else if (repeat_choice == "Keep LATEST only") {
    
    qc_passed <- qc_passed %>%
      group_by(stripped_id) %>%
      arrange(
        desc(Visit_Date),
        desc(suffix_number),
        .by_group = TRUE
      ) %>%
      slice(1) %>%
      ungroup()
    
  } else if (repeat_choice == "Keep a SPECIFIC repeat number") {
    
    qc_passed <- qc_passed %>%
      filter(suffix_number == specific_repeat_number)
    
  }
  
  # If "Keep ALL repeats" was selected, no filtering is applied.
  
  # Add repeat-selection note and remove helper columns
  qc_passed <- qc_passed %>%
    mutate(
      Repeat_Selection_Note = case_when(
        Total_Entries_For_Sample == 1 ~ NA_character_,
        
        suffix_number == 0 ~ paste0(
          "Original entry of sample ",
          stripped_id,
          " (1 of ",
          Total_Entries_For_Sample,
          " total entries)"
        ),
        
        TRUE ~ paste0(
          "Repeat #",
          suffix_number,
          " of sample ",
          stripped_id,
          " (",
          suffix_number + 1,
          " of ",
          Total_Entries_For_Sample,
          " total entries)"
        )
      )
    ) %>%
    select(
      -suffix_number,
      -stripped_id,
      -Total_Entries_For_Sample
    )
  
  qc_out_path <- file.path(output_folder, "final_database_QC_passed.xlsx")
  t_export <- Sys.time()
  export_flagged_excel(qc_passed, qc_out_path)
  message("  QC-passed export took ", round(as.numeric(difftime(Sys.time(), t_export, units = "mins")), 1), " min")
  message("QC-passed database saved: ", qc_out_path, " (", nrow(qc_passed), " samples, repeats: ", repeat_choice, ")")
  
  
  # Step 11.5 - Run summary
  qc_pass_count <- sum(final_database$Overall_QC_Status == "PASS")
  qc_fail_count <- nrow(final_database) - qc_pass_count
  discordant_count <- sum(final_database$Discordant == "YES")
  blank_count <- sum(final_database$HPV_Status == "BLANK", na.rm = TRUE)
  rerun_count <- sum(!is.na(final_database$Repeat_Comments), na.rm = TRUE)
  
  any_hpv_count <- sum(final_database$Any_HPV_Detected == "YES")
  hr_detected_count <- sum(final_database$High_Risk_HPV_Detected == "YES")
  lr_detected_count <- sum(final_database$Low_Risk_HPV_Detected == "YES")
  
  genotype_cols_present <- colnames(final_database)[grepl("^HPV_", colnames(final_database)) & colnames(final_database) != "HPV_Universal_Probe"]
  genotype_counts <- sapply(genotype_cols_present, function(col) sum(final_database[[col]] == "POSITIVE", na.rm = TRUE))
  top_genotypes <- sort(genotype_counts, decreasing = TRUE)
  top_genotypes <- top_genotypes[top_genotypes > 0]
  
  # Try day-first first (common outside the US), then fall back to month-first,
  # so this doesn't silently return all-NA dates depending on source format.
  processed_dates_parsed <- dplyr::coalesce(
    as.Date(final_database$Processed_Date, format = "%d/%m/%Y"),
    as.Date(final_database$Processed_Date, format = "%m/%d/%Y")
  )
  
  date_range <- if (all(is.na(processed_dates_parsed))) {
    c(NA, NA)
  } else {
    range(processed_dates_parsed, na.rm = TRUE)
  }
  
  summary_lines <- c(
    "HybriFlow Pipeline Run Summary",
    paste0("Generated: ", Sys.time()),
    paste0("Data folder scanned: ", excel_folder),
    "",
    "=== SAMPLE COUNTS ===",
    paste0("Total samples in database: ", nrow(final_database)),
    paste0("QC passed: ", qc_pass_count, " (", round(100 * qc_pass_count / nrow(final_database), 1), "%)"),
    paste0("QC failed: ", qc_fail_count, " (", round(100 * qc_fail_count / nrow(final_database), 1), "%)"),
    paste0("Blank PDF results: ", blank_count, " (", round(100 * blank_count / nrow(final_database), 1), "%)"),
    paste0("Discordant results: ", discordant_count, " (", round(100 * discordant_count / nrow(final_database), 1), "%)"),
    paste0("Samples with repeat notes: ", rerun_count),
    "",
    "=== HPV RESULTS ===",
    paste0("Any HPV detected: ", any_hpv_count, " (", round(100 * any_hpv_count / nrow(final_database), 1), "%)"),
    paste0("High-risk HPV detected: ", hr_detected_count, " (", round(100 * hr_detected_count / nrow(final_database), 1), "%)"),
    paste0("Low-risk HPV detected: ", lr_detected_count, " (", round(100 * lr_detected_count / nrow(final_database), 1), "%)"),
    "",
    "=== TOP DETECTED GENOTYPES ===",
    if (length(top_genotypes) > 0) paste0("  - ", names(top_genotypes), ": ", top_genotypes, " sample(s)") else "  (none detected)",
    "",
    "=== DATE RANGE ===",
    paste0("Earliest processed date: ", date_range[1]),
    paste0("Latest processed date: ", date_range[2]),
    "",
    "=== FILE EXCLUSIONS ===",
    paste0("Excel files excluded (not per-sample format), ", length(excluded_files), " total:"),
    if (length(excluded_files) > 0) paste0("  - ", excluded_files) else "  (none excluded)"
  )
  
  # Discordant genotypes that were repeated: did the result change?
  discordant_check <- tryCatch(
    check_discordant_repeats(final_database),
    error = function(e) {
      message("Discordant repeat check failed: ", conditionMessage(e))
      list(lines = character(0))
    })
  summary_lines <- c(summary_lines, discordant_check$lines)
  
  summary_out_path <- file.path(output_folder, "run_summary.txt")
  writeLines(summary_lines, summary_out_path)
  message("Run summary saved: ", summary_out_path)
  
  # ============================================================
  # Step 11.7 - Follow-up (longitudinal) analysis
  # ============================================================
  # Builds a SEPARATE person-level database and the follow-up figures.
  # The main database is only read. If this step fails, the main outputs are unaffected.
  followup_result <- NULL
  if (isTRUE(fu_settings$run)) {
    followup_result <- tryCatch(
      run_followup_analysis(
        final_database = final_database,
        output_folder  = output_folder,
        hr_cols        = intersect(paste0("HPV_", high_risk), colnames(final_database)),
        lr_cols        = intersect(paste0("HPV_", low_risk),  colnames(final_database)),
        settings       = fu_settings[c("visits_required", "clear_last_k", "qc_only")]
      ),
      error = function(e) {
        message("FOLLOW-UP ANALYSIS FAILED (the main database is not affected): ", conditionMessage(e))
        NULL
      }
    )
  } else {
    message("Follow-up analysis skipped (database only).")
  }
  followup_html_path <- file.path(output_folder, "followup_analysis.html")
  followup_xlsx_path <- file.path(output_folder, "followup_database.xlsx")
  
  # Keep the clean results to return (Step 12 below adds HTML link columns)
  results_db <- final_database
  
  # Step 12 - Create interactive HTML view of final database
  final_database$highlight_flag <- ifelse(
    final_database$Overall_QC_Status != "PASS" |
      final_database$Discordant == "YES",
    "yes",
    "no"
  )
  
  col_names <- colnames(final_database)
  flag_col_index <- which(col_names == "highlight_flag") - 1
  
  qc_col_indices <- which(col_names %in% c(
    "Sample_ID", "Positive_Control_Pass", "NTC_Pass", "Discordant",
    "HPV_Universal_Probe", "Discordant_Comments", "Comments", "Repeat_Comments",
    "High_Risk_HPV_Detected", "High_Risk_Count",
    "Low_Risk_HPV_Detected", "Low_Risk_Count",
    "Total_HPV_Genotypes_Detected", "Any_HPV_Detected", "Result_Consistency",
    "File", "HPV_Status", "Report_Results",
    "PCR_Lot", "PCR_Expiry", "Chips_Lot", "Chips_Expiry",
    "Reagent_Lot", "Reagent_Expiry",
    "Validated_Date", "Processed_Date",
    "excel_path", "pdf_path",
    "Overall_QC_Status"
  )) - 1
  
  hr_col_indices <- which(col_names %in% paste0("HPV_", high_risk)) - 1
  lr_col_indices <- which(col_names %in% paste0("HPV_", low_risk)) - 1
  
  header_js <- JS(sprintf("
  function(thead, data, start, end, display) {
    var qcCols = [%s];
    var hrCols = [%s];
    var lrCols = [%s];
    $(thead).find('th').each(function(index) {
      if (qcCols.includes(index)) {
        $(this).css({'background-color': '#000000', 'color': '#FFFFFF', 'font-weight': 'bold'});
      } else if (hrCols.includes(index)) {
        $(this).css({'background-color': '#FF0000', 'color': '#FFFFFF', 'font-weight': 'bold'});
      } else if (lrCols.includes(index)) {
        $(this).css({'background-color': '#4472C4', 'color': '#FFFFFF', 'font-weight': 'bold'});
      }
    });
  }
", paste(qc_col_indices, collapse = ","), paste(hr_col_indices, collapse = ","), paste(lr_col_indices, collapse = ",")))
  
  final_database <- final_database %>%
    mutate(
      excel_path = ifelse(!is.na(excel_path),
                          paste0('<a href="file:///', gsub("\\\\", "/", excel_path), '" target="_blank">Open Excel</a>'),
                          NA),
      pdf_path = ifelse(!is.na(pdf_path),
                        paste0('<a href="file:///', gsub("\\\\", "/", pdf_path), '" target="_blank">Open PDF</a>'),
                        NA)
    )
  
  comment_cols <- which(
    colnames(final_database) %in% c(
      "Comments",
      "Discordant_Comments",
      "Repeat_Comments",
      "Report_Results"
    )
  ) - 1
  
  long_comment_js <- JS("
  function(data, type, row) {
    if (type === 'display' && data !== null && data.length > 120) {
      return '<span title=\"' + data + '\">' + data.substr(0, 120) + '...</span>';
    }
    return data;
  }
")
  
  final_table <- DT::datatable(
    final_database,
    filter = "top",
    extensions = c("Buttons"),
    escape = FALSE,
    options = list(
      pageLength = 25,
      lengthMenu = c(10, 25, 50, 100, -1),
      scrollX = TRUE,
      autoWidth = TRUE,
      searching = TRUE,
      ordering = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "print"),
      columnDefs = list(
        list(visible = FALSE, targets = flag_col_index),
        list(width = "500px", targets = comment_cols),
        list(className = "dt-left", targets = "_all"),
        list(targets = comment_cols, render = long_comment_js)
      ),
      headerCallback = header_js,
      rowCallback = JS(paste0(
        "function(row, data) {",
        "  if (data[", flag_col_index, "] == 'yes') {",
        "    $(row).css('background-color', '#FFC7CE');",
        "  }",
        "  $(row).find('td').css('vertical-align', 'middle');",
        "}"
      ))
    ),
    rownames = FALSE,
    caption = "HybriFlow QC and Discordance Summary Database"
  )
  
  html_out_path <- file.path(output_folder, "final_database.html")
  saveWidget(final_table, html_out_path, selfcontained = TRUE)
  
  # Create clickable buttons in the HTML page
  main_database_link <- basename(excel_out_path)
  qc_link <- basename(qc_out_path)
  summary_link <- basename(summary_out_path)
  followup_buttons <- if (file.exists(followup_html_path)) paste0(
    '<a href="', basename(followup_html_path), '" style="display:inline-block; margin:4px; padding:10px 16px; background-color:#8E24AA; color:white; border-radius:4px; text-decoration:none; font-weight:bold;">Open Follow-up Analysis</a>',
    '<a href="', basename(followup_xlsx_path), '" style="display:inline-block; margin:4px; padding:10px 16px; background-color:#00897B; color:white; border-radius:4px; text-decoration:none; font-weight:bold;">Open Follow-up Database</a>'
  ) else ""
  
  html_content <- paste(readLines(html_out_path, warn = FALSE), collapse = "\n")
  css_fix <- "<style>
table.dataTable td { text-align: center !important; }
table.dataTable th { text-align: center !important; }
</style>"
  
  summary_button <- paste0(
    '<div style="padding: 10px 0; font-family:sans-serif;">',
    '<a href="', main_database_link, '" style="display:inline-block; margin:4px; padding:10px 16px; background-color:#4472C4; color:white; border-radius:4px; text-decoration:none; font-weight:bold;">Open Main Database Excel</a>',
    '<a href="', qc_link, '" style="display:inline-block; margin:4px; padding:10px 16px; background-color:#4CAF50; color:white; border-radius:4px; text-decoration:none; font-weight:bold;">Open QC-Passed Database</a>',
    followup_buttons,
    '<a href="', summary_link, '" style="display:inline-block; margin:4px; padding:10px 16px; background-color:#f0f0f0; border:1px solid #ccc; border-radius:4px; text-decoration:none; color:#333;">Open Run Summary</a>',
    '</div>'
  )
  
  html_content <- sub("</html>", paste0(css_fix, "</html>"), html_content, ignore.case = TRUE)
  html_content <- sub("<body[^>]*>", paste0("\\0", summary_button), html_content, ignore.case = TRUE)
  writeLines(html_content, html_out_path)
  
  message("Done! Files saved to: ", output_folder)
  
  # Zip the entire output folder so it can be shared as one file
  # One fixed zip name, so a new run replaces it (no new zip file for every day)
  zip_out_path <- file.path(parent_folder, "HybriFlow_Output.zip")
  if (file.exists(zip_out_path)) file.remove(zip_out_path)
  
  # Remember which files this run produced (the next run removes exactly these)
  record_run_files(output_folder, run_start)
  
  if (isTRUE(zip_output)) {
    zip::zip(
      zipfile = zip_out_path,
      files = list.files(output_folder, full.names = FALSE),
      root = output_folder
    )
    message("Zipped output folder saved to: ", zip_out_path)
    message("Share this single zip file - all buttons/links will work correctly once the recipient extracts it.")
  } else {
    zip_out_path <- NA_character_
  }
  
  message("Total run time: ", round(as.numeric(difftime(Sys.time(), run_start + 5, units = "mins")), 1),
          " min (includes the time the repeats question waited for an answer)")
  
  if (isTRUE(open_report)) utils::browseURL(html_out_path)
  
  # ===========================================
  # Optional resource report (for system-requirement notes)
  # ===========================================
  if (isTRUE(resource_report)) {
    fmt_mb <- function(bytes) sprintf("%.1f MB", bytes / 1024^2)
    cache_files <- if (dir.exists(cache_dir)) list.files(cache_dir, recursive = TRUE, full.names = TRUE) else character(0)
    message("---- RESOURCE REPORT ----")
    message("Samples in database:        ", nrow(results_db))
    message("final_database in RAM:      ", fmt_mb(as.numeric(utils::object.size(results_db))))
    message("Excel probe data in RAM:    ", fmt_mb(as.numeric(utils::object.size(excel_cache$probes))))
    message("Cache + .rds on disk:       ", fmt_mb(sum(file.info(cache_files)$size, na.rm = TRUE)))
    print(gc())
  }
  
  invisible(list(
    final_database = results_db,
    qc_passed      = qc_passed,
    followup       = followup_result,
    run_summary    = summary_lines,
    paths = list(output_folder = output_folder, database_xlsx = excel_out_path,
                 qc_xlsx = qc_out_path, html = html_out_path, summary = summary_out_path,
                 followup_html = if (file.exists(followup_html_path)) followup_html_path else NA_character_,
                 followup_xlsx = if (file.exists(followup_xlsx_path)) followup_xlsx_path else NA_character_,
                 zip = zip_out_path)
  ))
}


# ===========================================
