# ===========================================
# Helper functions - used throughout the pipeline
# ===========================================

# Standardises Sample IDs for matching (trims whitespace, uppercases)
normalize_id <- function(x) {
  # IDs are kept exactly as written (no capitalising). Only spaces at the
  # start/end are removed, so the same ID always matches itself.
  str_trim(x)
}

# Tries Processed_Date first; falls back to searching Sample_ID for a
# plausible 4-digit year (2015-2029). Never drops a sample silently -
# returns "Unknown" if no valid year can be found anywhere.
extract_year_flexible <- function(sample_id, processed_date) {
  d <- suppressWarnings(as.Date(processed_date, format = "%m/%d/%Y"))
  if (!is.na(d)) return(format(d, "%Y"))
  
  yr_from_id <- str_extract(sample_id, "20(1[5-9]|2[0-9])")
  if (!is.na(yr_from_id)) return(yr_from_id)
  
  yr_from_raw_date <- str_extract(as.character(processed_date), "20(1[5-9]|2[0-9])")
  if (!is.na(yr_from_raw_date)) return(yr_from_raw_date)
  
  return("Unknown")
}
