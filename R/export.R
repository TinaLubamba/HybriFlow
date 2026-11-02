# -------------------------------------------------------------------
# EXPORT FUNCTIONS
# -------------------------------------------------------------------

#' Export the sample database to a formatted Excel workbook
#'
#' Header colours separate QC columns (black), high-risk genotypes (red) and
#' low-risk genotypes (blue). Samples that failed QC or are discordant are
#' highlighted, and the Excel/PDF paths become clickable links.
#'
#' @param final_database The database data frame.
#' @param out_path Path of the `.xlsx` file to write.
#' @return Invisibly `NULL`; called for its side effect of writing a file.
#' @export
export_flagged_excel <- function(final_database, out_path = "final_database.xlsx") {
  if (nrow(final_database) == 0) {
    stop("Cannot export empty final_database.")
  }
  
  wb <- createWorkbook()
  addWorksheet(wb, "Database")
  writeData(wb, "Database", final_database)
  
  col_names <- colnames(final_database)
  
  black_header <- createStyle(fgFill = "#000000", fontColour = "#FFFFFF", textDecoration = "bold", halign = "center")
  red_header   <- createStyle(fgFill = "#FF0000", fontColour = "#FFFFFF", textDecoration = "bold", halign = "center")
  blue_header  <- createStyle(fgFill = "#4472C4", fontColour = "#FFFFFF", textDecoration = "bold", halign = "center")
  
  qc_cols <- which(col_names %in% c(
    "Sample_ID", "Positive_Control_Pass", "NTC_Pass", "Discordant",
    "HPV_Universal_Probe", "Discordant_Comments", "Comments", "Repeat_Comments",
    "High_Risk_HPV_Detected", "High_Risk_Count",
    "Low_Risk_HPV_Detected", "Low_Risk_Count",
    "Total_HPV_Genotypes_Detected", "Any_HPV_Detected",
    "File", "HPV_Status", "Report_Results",
    "PCR_Lot", "PCR_Expiry", "Chips_Lot", "Chips_Expiry",
    "Reagent_Lot", "Reagent_Expiry",
    "Validated_Date", "Processed_Date",
    "excel_path", "pdf_path", "Result_Consistency",
    "Overall_QC_Status"
  ))
  
  addStyle(wb, "Database", black_header, rows = 1, cols = qc_cols, gridExpand = TRUE)
  
  hr_cols <- which(col_names %in% paste0("HPV_", high_risk))
  addStyle(wb, "Database", red_header, rows = 1, cols = hr_cols, gridExpand = TRUE)
  
  lr_cols <- which(col_names %in% paste0("HPV_", low_risk))
  addStyle(wb, "Database", blue_header, rows = 1, cols = lr_cols, gridExpand = TRUE)
  
  needs_highlight <- rep(FALSE, nrow(final_database))
  
  # Highlight true QC failures and BLANK samples
  if ("Overall_QC_Status" %in% col_names) {
    needs_highlight <- needs_highlight |
      final_database$Overall_QC_Status != "PASS"
  }
  
  # Also highlight discordant samples for review.
  # Discordance is not treated as a QC failure.
  if ("Discordant" %in% col_names) {
    needs_highlight <- needs_highlight |
      final_database$Discordant == "YES"
  }
  
  flag_style <- createStyle(fgFill = "#FFC7CE", textDecoration = "bold")
  flagged_rows <- which(needs_highlight) + 1
  
  if (length(flagged_rows) > 0) {
    # Plain highlight (fast). Stacking styles cell by cell is very slow for thousands of rows.
    addStyle(wb, "Database", style = flag_style, rows = flagged_rows,
             cols = 1:ncol(final_database), gridExpand = TRUE)
    # The highlight would turn Visit_Date into a number (e.g. 43396), so re-apply the date format
    # with the highlight on those cells only (one call)
    if ("Visit_Date" %in% col_names) {
      date_flag_style <- createStyle(fgFill = "#FFC7CE", textDecoration = "bold", numFmt = "yyyy/mm/dd")
      addStyle(wb, "Database", style = date_flag_style, rows = flagged_rows,
               cols = which(col_names == "Visit_Date"), gridExpand = TRUE)
    }
  }
  
  # Hyperlinks: written in blocks of consecutive rows (much faster than one cell at a time)
  write_link_column <- function(values, col, label) {
    ok <- !is.na(values) & nzchar(values)
    if (!any(ok)) return(invisible(NULL))
    f <- paste0('=HYPERLINK("', gsub('"', '""', values), '", "', label, '")')
    idx <- which(ok)
    runs <- split(idx, cumsum(c(1, diff(idx) != 1)))
    for (run in runs) {
      writeFormula(wb, "Database", x = f[run], startCol = col, startRow = run[1] + 1)
    }
    invisible(NULL)
  }
  if ("excel_path" %in% colnames(final_database)) {
    write_link_column(final_database$excel_path, which(colnames(final_database) == "excel_path"), "Open Excel")
  }
  if ("pdf_path" %in% colnames(final_database)) {
    write_link_column(final_database$pdf_path, which(colnames(final_database) == "pdf_path"), "Open PDF")
  }
  
  saveWorkbook(wb, out_path, overwrite = TRUE)
  message("Saved: ", out_path, " (", length(flagged_rows), " sample(s) highlighted)")
}
