# -------------------------------------------------------------------
# PDF FUNCTIONS
# -------------------------------------------------------------------

get_pdf_files <- function(folder) {
  if (!dir.exists(folder)) stop("Folder does not exist: ", folder)
  files <- list.files(folder, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  message("Found ", length(files), " PDF file(s) in ", folder)
  files
}

read_pdf_data <- function(pdf_files) {
  if (length(pdf_files) == 0) {
    return(tibble(
      pdf_path = character(),
      pdf_filename = character(),
      filename_clean = character(),
      sample_id = character()
    ))
  }
  
  tibble(pdf_path = pdf_files) %>%
    mutate(
      pdf_filename = basename(pdf_path),
      filename_clean = URLdecode(pdf_filename),
      sample_id = normalize_id(str_extract(filename_clean, "(?i)(?<=report[_-]).+(?=\\.pdf$)"))
    )
}

extract_after_label <- function(text, label, value_pattern = "[A-Z0-9/\\-_.]+", second_token = FALSE) {
  pat <- if (second_token) paste0("(?i)\\b", label, "[:\\s]*(", value_pattern, ")(?:\\s+([\\S]+))?")
  else paste0("(?i)\\b", label, "[:\\s]*(", value_pattern, ")")
  m <- str_match(text, pat)
  if (is.na(m[1,1])) return(NA_character_)
  if (second_token) {
    val1 <- m[1,2]
    val2 <- m[1,3]
    if (is.na(val2) || !nzchar(val2)) val2 <- NA_character_
    return(c(val1, val2))
  }
  return(m[1,2])
}

extract_hpv_status <- function(full_text) {
  if (str_detect(full_text, regex("HPV\\s+POSITIVE", ignore_case = TRUE))) return("POSITIVE")
  else if (str_detect(full_text, regex("HPV\\s+NEGATIVE", ignore_case = TRUE))) return("NEGATIVE")
  else if (str_detect(full_text, regex("\\bBLANK\\b", ignore_case = TRUE))) return("BLANK")
  else return(NA_character_)
}

extract_report_section <- function(full_text, debug = FALSE) {
  if (is.null(full_text) || nchar(full_text) == 0) return(NA_character_)
  block_pat <- "(?si)REPORT\\s*\\n?(.*?)(?=\\n(?:PROTOCOL|NOTES|CONCLUSION|METHOD|RESULTS)\\b|$)"
  m <- str_match(full_text, block_pat)
  blk <- m[1, 2]
  if (is.na(blk) || !nzchar(str_trim(blk))) return(NA_character_)
  blk_clean <- blk %>% str_replace_all("\\n+", " ") %>% str_squish()
  if (debug) message("REPORT block: ", blk_clean)
  return(blk_clean)
}

#' Read the metadata printed in one HybriSpot PDF report
#'
#' Extracts the sample ID, the reported HPV status (POSITIVE, NEGATIVE or
#' BLANK), the report text, the PCR, chip and reagent lot numbers with expiry
#' dates, and the validated and processed dates.
#'
#' @param pdf_path Path to a PDF report.
#' @param debug Print the extracted report block.
#' @return A one-row tibble.
#' @export
extract_pdf_metadata <- function(pdf_path, debug = FALSE) {
  txt_pages <- tryCatch(pdf_text(pdf_path), error = function(e) character(0))
  full_text <- paste(txt_pages, collapse = "\n")
  
  if (nchar(full_text) == 0) {
    return(tibble(
      File = basename(pdf_path),
      Sample_ID = NA_character_,
      HPV_Status = NA_character_,
      Report_Results = NA_character_,
      PCR_Lot = NA_character_,
      PCR_Expiry = NA_character_,
      Chips_Lot = NA_character_,
      Chips_Expiry = NA_character_,
      Reagent_Lot = NA_character_,
      Reagent_Expiry = NA_character_,
      Validated_Date = NA_character_,
      Processed_Date = NA_character_
    ))
  }
  
  hpv_status <- extract_hpv_status(full_text)
  report_results <- extract_report_section(full_text, debug = debug)
  norm <- str_replace_all(full_text, "\\s+", " ")
  id_pattern <- "[A-Za-z0-9\\-_/\\.]+"
  
  sample_id <- str_extract(norm, "(?i)(?<=ID SAMPLE:\\s{0,10})\\S+")
  if (is.na(sample_id)) sample_id <- extract_after_label(norm, "Sample ID", value_pattern = id_pattern)
  if (is.na(sample_id)) sample_id <- extract_after_label(norm, "ID SAMPLE", value_pattern = id_pattern)
  if (is.na(sample_id)) {
    fn <- basename(pdf_path)
    sample_id <- str_match(fn, "report[_-]([A-Za-z0-9\\-_/\\.]+)")[,2]
  }
  
  # Flexible extraction of lot numbers and expiry dates
  date_pattern <- "[0-9]{1,2}[./-][0-9]{1,2}[./-][0-9]{2,4}"
  
  extract_lot <- function(text, label) {
    pattern <- paste0(
      "(?i)\\b",
      label,
      "\\s*(?:LOT|NO\\.?|NUMBER)?\\s*[:#-]?\\s*",
      "([A-Za-z0-9][A-Za-z0-9._/-]*)"
    )
    
    result <- stringr::str_match(
      text,
      stringr::regex(pattern, ignore_case = TRUE)
    )
    
    if (is.na(result[1, 2])) {
      return(NA_character_)
    }
    
    result[1, 2]
  }
  
  extract_expiry_after_lot <- function(text, label) {
    pattern <- paste0(
      "(?i)\\b",
      label,
      "\\s*(?:LOT|NO\\.?|NUMBER)?\\s*[:#-]?\\s*",
      "[A-Za-z0-9][A-Za-z0-9._/-]*",
      ".{0,100}?",
      "(", date_pattern, ")"
    )
    
    result <- stringr::str_match(
      text,
      stringr::regex(pattern, ignore_case = TRUE)
    )
    
    if (is.na(result[1, 2])) {
      return(NA_character_)
    }
    
    result[1, 2]
  }
  
  # NOTE: these two must be defined BEFORE they are first used below
  # (moved up from further down in the function - they were previously
  # defined after pcr_expiry/chips_expiry/reagent_expiry called them,
  # which threw "could not find function 'normalize_extracted_date'")
  date_value_pattern <- "(?:\\d{1,2}[./-]\\d{1,2}[./-]\\d{2,4}|\\d{4}[./-]\\d{1,2}[./-]\\d{1,2})"
  normalize_extracted_date <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      gsub(".", "/", x, fixed = TRUE)
    )
  }
  
  pcr_lot <- extract_lot(norm, "PCR")
  pcr_expiry <- extract_expiry_after_lot(norm, "PCR") %>%
    normalize_extracted_date()
  
  chips_lot <- extract_lot(norm, "Chips")
  chips_expiry <- extract_expiry_after_lot(norm, "Chips") %>%
    normalize_extracted_date()
  
  reagent_lot <- extract_lot(norm, "Reagent")
  reagent_expiry <- extract_expiry_after_lot(norm, "Reagent") %>%
    normalize_extracted_date()
  
  validated <- extract_after_label(
    norm,
    "Validated",
    value_pattern = date_value_pattern
  ) %>%
    normalize_extracted_date()
  
  processed <- extract_after_label(
    norm,
    "Processed",
    value_pattern = date_value_pattern
  ) %>%
    normalize_extracted_date()
  
  tibble(
    File = basename(pdf_path),
    Sample_ID = ifelse(is.na(sample_id), NA_character_, normalize_id(sample_id)),
    HPV_Status = hpv_status,
    Report_Results = report_results,
    PCR_Lot = pcr_lot,
    PCR_Expiry = pcr_expiry,
    Chips_Lot = chips_lot,
    Chips_Expiry = chips_expiry,
    Reagent_Lot = reagent_lot,
    Reagent_Expiry = reagent_expiry,
    Validated_Date = validated,
    Processed_Date = processed
  )
}

extract_all_pdf_metadata <- function(pdf_folder, debug = FALSE) {
  files <- get_pdf_files(pdf_folder)
  if (length(files) == 0) {
    return(tibble(
      File = character(),
      Sample_ID = character(),
      HPV_Status = character(),
      Report_Results = character(),
      PCR_Lot = character(),
      PCR_Expiry = character(),
      Chips_Lot = character(),
      Chips_Expiry = character(),
      Reagent_Lot = character(),
      Reagent_Expiry = character(),
      Validated_Date = character(),
      Processed_Date = character()
    ))
  }
  purrr::map_dfr(files, function(f) {
    tryCatch(extract_pdf_metadata(f, debug = debug), error = function(e) {
      tibble(
        File = basename(f),
        Sample_ID = NA_character_,
        HPV_Status = NA_character_,
        Report_Results = NA_character_,
        PCR_Lot = NA_character_,
        PCR_Expiry = NA_character_,
        Chips_Lot = NA_character_,
        Chips_Expiry = NA_character_,
        Reagent_Lot = NA_character_,
        Reagent_Expiry = NA_character_,
        Validated_Date = NA_character_,
        Processed_Date = NA_character_
      )
    })
  })
}
