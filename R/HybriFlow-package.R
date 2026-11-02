#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import stringr
#' @import tibble
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
#' @importFrom DT datatable JS
#' @importFrom htmlwidgets saveWidget
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle writeFormula saveWorkbook
#' @importFrom pdftools pdf_text
#' @importFrom readxl read_excel
#' @importFrom stats ave binom.test fisher.test
#' @importFrom utils head write.csv URLdecode
## usethis namespace: end
NULL

# Column names used inside dplyr verbs (silences R CMD check notes)
utils::globalVariables(c(
  ".", ".file", ".size", ".mtime", "Target", "Result", "Position", "Probe_Category",
  "Sample_ID", "Sample ID", "excel_filename", "excel_path", "pdf_filename", "pdf_path",
  "sample_id", "valid", "note", "file", "size", "mtime", "folder", "reason",
  "Discordant", "Visit_Date", "Visit_Year", "Processed_Date", "HPV_Status",
  "HPV_Universal_Probe", "Total_HPV_Genotypes_Detected", "Any_HPV_Detected",
  "High_Risk_Count", "Low_Risk_Count", "Positive_Control_Pass", "NTC_Pass",
  "Overall_QC_Status", "Comments", "Repeat_Comments", "Discordant_Comments",
  "stripped_id", "suffix_number", "is_confirmed_rerun", "Base_Sample_ID",
  "Rerun_Number", "Total_Entries_For_Sample", "Repeat_Selection_Note",
  "High_Risk_HPV_Detected", "Low_Risk_HPV_Detected", "Result_Consistency",
  "Genotype_Result", "detail", "entry", "pdf_path_content", "pdf_filename_content",
  "pdf_path_content", "content_hash", "n_files", "kept_file", "other_files",
  "identical_copies", "highlight_flag", "run", "excel_file", "filename_clean",
  "Sample_ID_dup"
))
