# -------------------------------------------------------------------
# CACHE FUNCTIONS (incremental loading)
# -------------------------------------------------------------------

# Write to a temp file first so a crash mid-save cannot corrupt the cache
save_rds_safe <- function(x, path, use_cache = FALSE) {
  if (!isTRUE(use_cache)) return(invisible(NULL))   # cache is off: save nothing
  tmp <- paste0(path, ".tmp")
  saveRDS(x, tmp)
  ok <- suppressWarnings(file.rename(tmp, path))
  if (!ok) {
    file.copy(tmp, path, overwrite = TRUE)
    file.remove(tmp)
  }
  invisible(path)
}

# A file is "unchanged" if its path, size and modified-time all match
file_signature <- function(files) {
  info <- file.info(files)
  tibble(file = files, size = as.numeric(info$size), mtime = as.numeric(info$mtime))
}

load_cache_file <- function(path, empty, cfg) {
  if (!isTRUE(cfg$use)) return(empty)            # cache is off: always start from scratch
  if (isTRUE(cfg$force_rebuild)) {
    message("force_rebuild = TRUE: ignoring cache ", basename(path))
    return(empty)
  }
  if (!file.exists(path)) return(empty)
  x <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(x) || !identical(x$version, cfg$version)) {
    message("Cache ", basename(path), " is unreadable or outdated - rebuilding it.")
    return(empty)
  }
  x
}

# Clean Excel column names: fixes hidden characters (non-breaking spaces,
# byte-order marks, invalid UTF-8) that make "Sample ID" look right but not match.
clean_header <- function(x) {
  x <- iconv(as.character(x), from = "UTF-8", to = "UTF-8", sub = "")  # drop invalid bytes
  x <- gsub("[\u00a0\u2007\u202f\u200b\ufeff]", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

# Reads one Excel file ONCE (the old code read each file 3 times)
process_excel_file <- function(f) {
  df <- tryCatch(read_excel(f, sheet = 1), error = function(e) NULL)
  if (is.null(df)) {
    return(list(valid = FALSE, sample_id = NA_character_, probe = NULL,
                note = "file could not be read"))
  }
  
  required_cols <- c("Sample ID", "Target", "Position", "TH", "LTH", "Value", "Result")
  
  # clean the names, then match the required columns ignoring upper/lower case
  colnames(df) <- clean_header(colnames(df))
  hit <- match(tolower(colnames(df)), tolower(required_cols))
  colnames(df)[!is.na(hit)] <- required_cols[hit[!is.na(hit)]]
  
  if (!"Target" %in% colnames(df)) {
    return(list(valid = FALSE, sample_id = NA_character_, probe = NULL,
                note = "no Target column"))
  }
  
  sample_id <- if ("Sample ID" %in% colnames(df)) {
    as.character(normalize_id(as.character(df$`Sample ID`[1])))
  } else NA_character_
  
  missing_cols <- setdiff(required_cols, colnames(df))
  probe <- NULL
  note  <- NA_character_
  if (length(missing_cols) > 0) {
    note <- paste0("missing column(s): ", paste(missing_cols, collapse = ", "),
                   " | columns found: ", paste(colnames(df), collapse = ", "))
  } else {
    probe <- df %>%
      select(all_of(required_cols)) %>%
      filter(!is.na(Target)) %>%
      # Same clean-up as the file list uses (spaces at the ends removed, no change
      # of letter case), so the data rows and the file list use the identical ID.
      mutate(`Sample ID` = ifelse(is.na(`Sample ID`), NA_character_,
                                  normalize_id(as.character(`Sample ID`))))
    if (nrow(probe) == 0) note <- "Target column is empty"
  }
  list(valid = TRUE, sample_id = sample_id, probe = probe, note = note)
}

update_excel_cache <- function(excel_files, cfg) {
  empty <- list(
    version = cfg$version,
    index = tibble(file = character(), size = numeric(), mtime = numeric(),
                   valid = logical(), sample_id = character(), note = character()),
    probes = tibble()
  )
  sig   <- file_signature(excel_files)
  cache <- load_cache_file(cfg$excel_cache_path, empty, cfg)
  
  keep_idx <- semi_join(cache$index, sig, by = c("file", "size", "mtime"))
  todo     <- anti_join(sig, keep_idx[, c("file", "size", "mtime")],
                        by = c("file", "size", "mtime"))
  probes_kept <- cache$probes
  if (nrow(probes_kept) > 0) probes_kept <- filter(probes_kept, .file %in% keep_idx$file)
  
  message(sprintf("Excel cache: %d unchanged | %d new/changed to read | %d removed from disk",
                  nrow(keep_idx), nrow(todo), sum(!cache$index$file %in% sig$file)))
  
  new_idx <- list(); new_probe <- list()
  assemble <- function() {
    list(
      version = cfg$version,
      index  = bind_rows(keep_idx, bind_rows(new_idx)),
      probes = bind_rows(probes_kept, bind_rows(new_probe))
    )
  }
  
  for (i in seq_len(nrow(todo))) {
    f <- todo$file[i]
    res <- process_excel_file(f)
    new_idx[[length(new_idx) + 1]] <- tibble(
      file = f, size = todo$size[i], mtime = todo$mtime[i],
      valid = res$valid, sample_id = res$sample_id, note = res$note
    )
    if (!is.null(res$probe) && nrow(res$probe) > 0) {
      new_probe[[length(new_probe) + 1]] <- mutate(res$probe, .file = f)
    }
    if (i %% cfg$checkpoint_every == 0) {
      save_rds_safe(assemble(), cfg$excel_cache_path, cfg$use)
      message("  Excel: ", i, " / ", nrow(todo), " read",
              if (isTRUE(cfg$use)) " (checkpoint saved)" else "")
    }
  }
  
  out <- assemble()
  if (nrow(todo) > 0 || !file.exists(cfg$excel_cache_path)) save_rds_safe(out, cfg$excel_cache_path, cfg$use)
  
  # Only the files present on disk right now, in the original file order
  out$index <- out$index %>% filter(file %in% sig$file)
  out$index <- out$index[order(match(out$index$file, sig$file)), ]
  if (nrow(out$probes) > 0) {
    out$probes <- out$probes %>% filter(.file %in% sig$file)
    out$probes <- out$probes[order(match(out$probes$.file, sig$file)), ]
  }
  out
}

update_pdf_cache <- function(pdf_folder, cfg, debug = FALSE) {
  files <- get_pdf_files(pdf_folder)
  if (length(files) == 0) return(extract_all_pdf_metadata(pdf_folder, debug = debug))
  
  sig <- file_signature(files) %>% rename(.file = file, .size = size, .mtime = mtime)
  empty <- list(version = cfg$version, meta = tibble())
  cache <- load_cache_file(cfg$pdf_cache_path, empty, cfg)
  
  meta_kept <- cache$meta
  if (nrow(meta_kept) > 0) {
    meta_kept <- semi_join(meta_kept, sig, by = c(".file", ".size", ".mtime"))
  }
  todo <- if (nrow(meta_kept) > 0) {
    anti_join(sig, meta_kept[, c(".file", ".size", ".mtime")],
              by = c(".file", ".size", ".mtime"))
  } else sig
  
  message(sprintf("PDF cache: %d unchanged | %d new/changed to read",
                  nrow(meta_kept), nrow(todo)))
  
  new_meta <- list()
  failed   <- list()   # failures are returned but NOT cached, so they are retried next run
  assemble <- function() list(version = cfg$version,
                              meta = bind_rows(meta_kept, bind_rows(new_meta)))
  
  for (i in seq_len(nrow(todo))) {
    f <- todo$.file[i]
    row <- tryCatch(extract_pdf_metadata(f, debug = debug), error = function(e) NULL)
    if (is.null(row)) {
      failed[[length(failed) + 1]] <- tibble(
        File = basename(f), Sample_ID = NA_character_, HPV_Status = NA_character_,
        Report_Results = NA_character_, PCR_Lot = NA_character_, PCR_Expiry = NA_character_,
        Chips_Lot = NA_character_, Chips_Expiry = NA_character_, Reagent_Lot = NA_character_,
        Reagent_Expiry = NA_character_, Validated_Date = NA_character_,
        Processed_Date = NA_character_
      ) %>% mutate(.file = f, .size = todo$.size[i], .mtime = todo$.mtime[i])
    } else {
      new_meta[[length(new_meta) + 1]] <- mutate(
        row, .file = f, .size = todo$.size[i], .mtime = todo$.mtime[i])
    }
    if (i %% cfg$checkpoint_every == 0) {
      save_rds_safe(assemble(), cfg$pdf_cache_path, cfg$use)
      message("  PDF: ", i, " / ", nrow(todo), " read",
              if (isTRUE(cfg$use)) " (checkpoint saved)" else "")
    }
  }
  
  cache_out <- assemble()
  if (nrow(todo) > 0 || !file.exists(cfg$pdf_cache_path)) save_rds_safe(cache_out, cfg$pdf_cache_path, cfg$use)
  
  all_meta <- bind_rows(cache_out$meta, bind_rows(failed))
  all_meta <- all_meta[order(match(all_meta$.file, sig$.file)), ]
  out <- all_meta %>% select(-.file, -.size, -.mtime)
  # Keep the PDF path for each Sample_ID read from INSIDE the PDF, so PDFs whose
  # file name does not match can still be linked to their Excel file.
  attr(out, "pdf_paths") <- all_meta %>% select(Sample_ID, .file)
  out
}
