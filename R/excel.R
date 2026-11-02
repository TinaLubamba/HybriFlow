# -------------------------------------------------------------------
# EXCEL FUNCTIONS
# -------------------------------------------------------------------

get_excel_files <- function(folder) {
  if (!dir.exists(folder)) stop("Folder does not exist: ", folder)
  files <- list.files(folder, pattern = "^[^~].*\\.xlsx$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  message("Found ", length(files), " Excel file(s) in ", folder)
  files
}

is_valid_sample_file <- function(file_path) {
  df <- tryCatch(read_excel(file_path, sheet = 1, n_max = 5), error = function(e) NULL)
  if (is.null(df)) return(FALSE)
  colnames(df) <- trimws(colnames(df))
  return("Target" %in% colnames(df))
}

read_excel_data <- function(excel_files) {
  valid_files <- excel_files[sapply(excel_files, is_valid_sample_file)]
  if (length(valid_files) == 0) {
    return(tibble(
      excel_filename = character(),
      excel_path = character(),
      sample_id = character()
    ))
  }
  
  map_dfr(valid_files, function(f) {
    df <- read_excel(f)
    colnames(df) <- trimws(colnames(df))
    sample_id <- normalize_id(as.character(df$`Sample ID`[1]))
    tibble(
      excel_filename = basename(f),
      excel_path = f,
      sample_id = as.character(sample_id)
    )
  })
}

extract_probe_data <- function(file_path) {
  df <- tryCatch(read_excel(file_path, sheet = 1), error = function(e) NULL)
  
  if (is.null(df)) {
    warning("Skipping file (could not read): ", file_path)
    return(NULL)
  }
  
  colnames(df) <- trimws(colnames(df))
  
  required_cols <- c("Sample ID", "Target", "Position", "TH", "LTH", "Value", "Result")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if (length(missing_cols) > 0) {
    warning("Skipping file (missing required columns: ", paste(missing_cols, collapse = ", "), "): ", file_path)
    return(NULL)
  }
  
  df <- df %>%
    select(all_of(required_cols)) %>%
    filter(!is.na(Target))
  
  return(df)
}

extract_all_probe_data <- function(excel_folder) {
  excel_files <- get_excel_files(excel_folder)
  valid_files <- excel_files[sapply(excel_files, is_valid_sample_file)]
  excluded_files <- setdiff(excel_files, valid_files)
  
  if (length(excluded_files) > 0) {
    message("Excluded ", length(excluded_files), " non-sample file(s) (no 'Target' column found):")
    for (f in excluded_files) message("  - ", basename(f))
  }
  
  if (length(valid_files) == 0) {
    return(list(
      data = tibble(
        `Sample ID` = character(),
        Target = character(),
        Position = character(),
        TH = character(),
        LTH = character(),
        Value = character(),
        Result = character()
      ),
      excluded = excluded_files
    ))
  }
  
  probe_data <- map_dfr(valid_files, function(f) {
    x <- extract_probe_data(f)
    if (is.null(x)) return(tibble())
    x
  })
  
  list(data = probe_data, excluded = excluded_files)
}

#' Classify probe targets into categories
#'
#' Adds a `Probe_Category` column: `"Positive Control"` (hybridisation and
#' amplification controls), `"HPV Universal Probe"`, `"NTC"` (the empty
#' no-template spots) or `"HPV Genotype Target"`.
#'
#' @param data A data frame with a `Target` column, as read from a HybriSpot
#'   Excel file.
#' @return `data` with an added `Probe_Category` column.
#' @export
classify_probes <- function(data) {
  data %>%
    mutate(
      Probe_Category = case_when(
        Target == "Control" ~ "Positive Control",
        Target == "Amplification Control" ~ "Positive Control",
        Target == "HPV universal probe" ~ "HPV Universal Probe",
        Target == "Empty" ~ "NTC",
        TRUE ~ "HPV Genotype Target"
      )
    )
}

create_sample_list <- function(data) {
  sample_ids <- unique(data$`Sample ID`)
  sample_list <- lapply(sample_ids, function(id) data %>% filter(`Sample ID` == id))
  names(sample_list) <- sample_ids
  return(sample_list)
}

#' Summarise the probe results of one sample
#'
#' Collapses all probe rows of one sample into a single row: control and NTC
#' pass/fail, the universal-probe result, one `HPV_<genotype>` column per
#' genotype (POSITIVE only if all positions of that genotype are positive) and
#' any discordant positions.
#'
#' @param sample_data Probe rows of a single sample after [classify_probes()].
#' @return A one-row data frame.
#' @export
summarise_sample <- function(sample_data) {
  sample_id <- normalize_id(unique(sample_data$`Sample ID`))
  
  hyb_result <- sample_data %>% filter(Target == "Control") %>% pull(Result)
  hyb_pass <- length(hyb_result) > 0 && all(hyb_result == "POSITIVE")
  
  amp_result <- sample_data %>% filter(Target == "Amplification Control") %>% pull(Result)
  amp_pass <- length(amp_result) > 0 && all(amp_result == "POSITIVE")
  
  ntc_result <- sample_data %>% filter(Probe_Category == "NTC") %>% pull(Result)
  ntc_pass <- length(ntc_result) > 0 && all(ntc_result == "NEGATIVE")
  
  positive_control_pass <- hyb_pass & amp_pass
  
  universal_result <- sample_data %>% filter(Probe_Category == "HPV Universal Probe") %>% pull(Result)
  universal_result <- ifelse(length(universal_result) > 0, universal_result[1], NA)
  
  genotype_summary <- sample_data %>%
    filter(Probe_Category == "HPV Genotype Target") %>%
    group_by(Target) %>%
    summarise(
      Genotype_Result = case_when(
        all(Result == "POSITIVE") ~ "POSITIVE",
        TRUE ~ "NEGATIVE"
      ),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = Target, values_from = Genotype_Result, names_prefix = "HPV_")
  
  # Each genotype is printed at only 1-2 positions on the kit, so list each
  # position once with its result, e.g.  54: (3,3) POSITIVE, (5,7) NEGATIVE
  discordant_detail <- sample_data %>%
    filter(Probe_Category == "HPV Genotype Target") %>%
    distinct(Target, Position, Result) %>%
    group_by(Target) %>%
    filter(n_distinct(Result) > 1) %>%
    summarise(
      detail = paste0(Position, " ", Result, collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(entry = paste0(Target, ": ", detail))
  
  discordant_genotypes <- unique(discordant_detail$Target)
  discordant_flag <- length(discordant_genotypes) > 0
  discordant_comments <- if (discordant_flag) paste(discordant_detail$entry, collapse = "; ") else NA_character_
  
  summary_row <- data.frame(
    Sample_ID = sample_id,
    HPV_Universal_Probe = universal_result,
    Positive_Control_Pass = ifelse(positive_control_pass, "YES", "NO"),
    NTC_Pass = ifelse(ntc_pass, "YES", "NO"),
    Discordant = ifelse(discordant_flag, "YES", "NO"),
    stringsAsFactors = FALSE
  )
  
  if (nrow(genotype_summary) == 1) {
    summary_row <- bind_cols(summary_row, genotype_summary)
  }
  # If genotype_summary has 0 rows (sample had no HPV Genotype Target probes),
  # skip the bind - previously this threw "arguments imply differing number of rows"
  summary_row$Discordant_Comments <- discordant_comments
  
  return(summary_row)
}
