# ===========================================
# HELPER FUNCTIONS: pop-ups, output clean-up, discordant repeat check
# ===========================================

# ---- Output folder clean-up --------------------------------------------------
# Each run removes ONLY the files the pipeline made (the previous run's list, plus the
# known pipeline file names and old follow-up figures). Anything else in the folder is
# never touched. Hidden files (settings, run list) are kept.
PIPELINE_OUTPUT_FILES <- c(
  "final_database.xlsx", "final_database_QC_passed.xlsx", "final_database.html",
  "run_summary.txt", "followup_database.xlsx", "followup_analysis.html", "visual_summary.html",
  "Duplicate_Excel_files_report.csv", "Duplicate_PDF_files_report.csv",
  "Duplicate_Sample_IDs_removed.csv", "Excel_files_without_genotype_data.csv",
  "Sample_IDs_year_from_2digit.csv", "Sample_IDs_without_year.csv")

clear_previous_outputs <- function(output_folder) {
  if (!dir.exists(output_folder)) return(invisible(character(0)))
  list_path <- file.path(output_folder, ".last_run_files.txt")
  listed <- if (file.exists(list_path)) readLines(list_path, warn = FALSE) else character(0)
  listed <- listed[nzchar(listed) & !grepl("\\.\\.", listed) & !grepl("^\\.", basename(listed))]
  cand <- file.path(output_folder, unique(c(listed, PIPELINE_OUTPUT_FILES)))
  fig_dir <- file.path(output_folder, "followup_figures")
  if (dir.exists(fig_dir)) {
    cand <- c(cand, list.files(fig_dir, pattern = "^fig[0-9]+_.*\\.png$", full.names = TRUE))
  }
  cand <- unique(cand[file.exists(cand) & !dir.exists(cand)])
  removed <- character(0)
  for (f in cand) {
    if (suppressWarnings(file.remove(f))) removed <- c(removed, basename(f))
    else message("Could not remove ", basename(f), " (is it open in another program?)")
  }
  if (length(removed) > 0) message("Cleared ", length(removed), " file(s) from the previous run.")
  invisible(removed)
}

# Writes the list of files this run produced (used by the next run's clean-up)
record_run_files <- function(output_folder, run_start) {
  rel <- list.files(output_folder, recursive = TRUE)     # hidden files are not listed
  if (length(rel) == 0) return(invisible(NULL))
  mt <- file.mtime(file.path(output_folder, rel))
  mine <- rel[!is.na(mt) & mt >= run_start]
  writeLines(mine, file.path(output_folder, ".last_run_files.txt"))
  invisible(mine)
}

# ---- Discordant genotypes that were repeated: did the result change? ---------------
# A genotype is called POSITIVE only if all its positions are positive, so a discordant
# genotype is reported NEGATIVE. For each original sample with a discordant genotype and a
# repeat in the data, the repeat's result for that genotype is compared with the original.
check_discordant_repeats <- function(fdb) {
  header <- c("", "=== DISCORDANT GENOTYPES AND THEIR REPEATS ===")
  rc <- fdb$Repeat_Comments
  rep_idx <- which(!is.na(rc) & grepl("^REPEAT #[0-9]+ of sample ", rc))
  if (length(rep_idx) == 0) return(list(lines = c(header, "No repeated samples found in the database.")))
  rep_n    <- as.integer(sub("^REPEAT #([0-9]+) of sample .*$", "\\1", rc[rep_idx]))
  rep_base <- sub("^REPEAT #[0-9]+ of sample (.*) \\(processed.*$", "\\1", rc[rep_idx])
  orig_idx <- setdiff(which(fdb$Discordant == "YES"), rep_idx)
  parse_g <- function(txt) {
    if (is.na(txt) || txt == "") return(character(0))
    trimws(sub(":.*$", "", strsplit(txt, "; ", fixed = TRUE)[[1]]))
  }
  n_with <- 0; n_without <- 0; same <- 0; same_disc <- 0; changed <- list()
  for (i in orig_idx) {
    gs <- parse_g(fdb$Discordant_Comments[i])
    if (length(gs) == 0) next
    id <- fdb$Sample_ID[i]
    cand <- which(rep_base == id)
    if (length(cand) == 0) { n_without <- n_without + 1; next }
    n_with <- n_with + 1
    latest <- rep_idx[cand[which.max(rep_n[cand])]]      # latest repeat of this sample
    rep_disc <- parse_g(fdb$Discordant_Comments[latest])
    for (g in gs) {
      col <- paste0("HPV_", g)
      if (!col %in% colnames(fdb)) next
      rep_call <- fdb[[col]][latest]
      if (is.na(rep_call)) next
      if (rep_call == "POSITIVE") {
        changed[[length(changed) + 1]] <- paste0("  - ", id, " | HPV ", g, " | negative in the original -> positive in the repeat (",
                                                 fdb$Sample_ID[latest], ")")
      } else {
        same <- same + 1
        if (g %in% rep_disc) same_disc <- same_disc + 1
      }
    }
  }
  lines <- c(header,
             "A discordant genotype (positions disagree) is reported negative. Each is compared with the same genotype in the latest repeat.",
             paste0("Original samples with a discordant genotype: ", n_with + n_without),
             paste0("  with a repeat in the data: ", n_with),
             paste0("  without a repeat in the data: ", n_without),
             paste0("Discordant genotypes in samples that have a repeat: ", same + length(changed)),
             paste0("  same (negative in the repeat): ", same, " (", same_disc, " still discordant in the repeat)"),
             paste0("  changed (positive in the repeat): ", length(changed)))
  if (length(changed) > 0) lines <- c(lines, "Changed cases:", unlist(changed))
  list(lines = lines)
}
