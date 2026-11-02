# ===========================================
# DATE / YEAR HELPER FUNCTIONS (ready to move into a package)
# ===========================================
# ------------------------------------------------------------
# Extract Visit_Year (and Visit_Date where possible) from Sample_ID
#
# The YEAR is what matters for filtering, so the year is extracted
# separately and does not depend on knowing the day/month order.
# Everything AFTER the 13-digit sample number (i.e. after the first "_")
# is scanned, wherever the date sits. Formats handled:
#   20240729    YYYYMMDD           -> 2024
#   12032025    DDMMYYYY           -> 2025
#   10292018    MMDDYYYY           -> 2018
#   2272019     M/D/YYYY, no zeros -> 2019   (7 digits)
#   392020      M/D/YYYY, no zeros -> 2020   (6 digits)
#   2024-07-29, 12-03-2025, 3/9/2020, 12.03.2025 (separated)
#   120325      DDMMYY / MMDDYY    -> 2025   (2-digit year, last resort)
#   12/03/25    separated, 2-digit year -> 2025
#
# A 6-8 digit block only counts as a date if its year is plausible AND
# the remaining digits form a real day/month. Plain 4-digit parts such as
# the "1997" in 100-00-0003-0003 are never treated as a year.
# IDs with no readable year stay NA and are reported (nothing is guessed).
# ------------------------------------------------------------

# Sample_ID layout: <13-digit sample number>_<visit code / date ...>
#   e.g. 100-00-0001-0001_1-004-0_20240729
# The 13-digit sample number (100-00-0001-0001) is NEVER searched for a
# year, so parts like the "1997" in 100-00-0003-0003 cannot be mistaken
# for one. Only what comes AFTER the first underscore is scanned.
strip_sample_number <- function(s) {
  if (is.na(s)) return(NA_character_)
  if (grepl("_", s, fixed = TRUE)) return(sub("^[^_]*_", "", s))
  # no underscore: drop a leading 13-digit number (dashes/spaces allowed between digits)
  sub("^(?:[0-9][- ]?){12}[0-9]", "", s, perl = TRUE)
}

#' Read the visit year from a Sample ID
#'
#' Sample IDs have the layout `<13-digit sample number>_<visit code / date>`.
#' Only the part after the first underscore is searched, so digits in the
#' sample number can never be mistaken for a year. Handles `YYYYMMDD`,
#' `DDMMYYYY`, `MMDDYYYY`, 6-7 digit dates without leading zeros, separated
#' dates and (last resort) two-digit years. IDs with no readable year give `NA`.
#'
#' @param id Character vector of Sample IDs.
#' @param min_year,max_year Plausible range for the year.
#' @param allow_two_digit Allow two-digit years as a last resort.
#' @return An integer vector of years.
#' @export
#' @examples
#' extract_visit_year_from_id("100-00-0001-0001_1-004-0_20240729")
extract_visit_year_from_id <- function(id, min_year = 1990,
                                       max_year = as.integer(format(Sys.Date(), "%Y")),
                                       allow_two_digit = TRUE) {
  in_range <- function(y) !is.na(y) && y >= min_year && y <= max_year
  
  # Only the YEAR matters. Day/month just need to look plausible so we can tell
  # which end of the number holds the year (no exact calendar check).
  real_date <- function(y, m, d) {
    !is.na(m) && !is.na(d) && m >= 1 && m <= 12 && d >= 1 && d <= 31
  }
  
  # a and b are day and month in unknown order, so accept either order
  md_ok <- function(a, b, y) real_date(y, a, b) || real_date(y, b, a)
  
  one <- function(s) {
    if (is.na(s)) return(NA_integer_)
    s <- strip_sample_number(s)     # ignore the 13-digit sample number
    if (is.na(s) || !nzchar(s)) return(NA_integer_)
    
    # 1. standalone 6-, 7- or 8-digit blocks after the sample number
    blocks <- stringr::str_extract_all(s, "(?<![0-9])[0-9]{6,8}(?![0-9])")[[1]]
    for (blk in blocks) {
      n <- nchar(blk)
      
      # YYYYMMDD (year first)
      if (n == 8) {
        y1 <- as.integer(substr(blk, 1, 4))
        if (in_range(y1) && real_date(y1, as.integer(substr(blk, 5, 6)),
                                      as.integer(substr(blk, 7, 8)))) return(y1)
      }
      
      # day/month digits first, YEAR LAST (DDMMYYYY, MMDDYYYY, M/D/YYYY ...)
      y2  <- as.integer(substr(blk, n - 3, n))
      pre <- substr(blk, 1, n - 4)
      if (in_range(y2)) {
        splits <- switch(as.character(nchar(pre)),
                         "2" = list(c(1, 1)),
                         "3" = list(c(1, 2), c(2, 1)),
                         "4" = list(c(2, 2)))
        for (sp in splits) {
          a <- as.integer(substr(pre, 1, sp[1]))
          b <- as.integer(substr(pre, sp[1] + 1, nchar(pre)))
          if (md_ok(a, b, y2)) return(y2)
        }
      }
    }
    
    # 2. separated dates anywhere in the ID
    ymd <- stringr::str_match_all(
      s, "(?<![0-9])((?:19|20)[0-9]{2})[-/.]([0-9]{1,2})[-/.]([0-9]{1,2})(?![0-9])")[[1]]
    for (i in seq_len(nrow(ymd))) {
      y <- as.integer(ymd[i, 2])
      if (in_range(y) && md_ok(as.integer(ymd[i, 3]), as.integer(ymd[i, 4]), y)) return(y)
    }
    dmy <- stringr::str_match_all(
      s, "(?<![0-9])([0-9]{1,2})[-/.]([0-9]{1,2})[-/.]((?:19|20)[0-9]{2})(?![0-9])")[[1]]
    for (i in seq_len(nrow(dmy))) {
      y <- as.integer(dmy[i, 4])
      if (in_range(y) && md_ok(as.integer(dmy[i, 2]), as.integer(dmy[i, 3]), y)) return(y)
    }
    
    # 2b. YYYYMMDD stuck to other digits/characters (e.g. visit code + date joined)
    glued <- stringr::str_match_all(
      s, "((?:19|20)[0-9]{2})(0[1-9]|1[0-2])(0[1-9]|[12][0-9]|3[01])")[[1]]
    for (i in seq_len(nrow(glued))) {
      y <- as.integer(glued[i, 2])
      if (in_range(y)) return(y)
    }
    
    # 2c. 8-digit block that starts with a plausible year but has an odd
    #     day/month (e.g. a typo): the year is still the first 4 digits
    for (blk in blocks[nchar(blocks) == 8]) {
      y <- as.integer(substr(blk, 1, 4))
      if (in_range(y)) return(y)
    }
    
    # 3. LAST RESORT: 2-digit years (25 -> 2025). Only used when the ID has
    #    no 4-digit-year date anywhere, to avoid misreading other numbers.
    if (allow_two_digit) {
      to_year <- function(yy) {
        y <- 2000L + yy
        if (y > max_year) y <- 1900L + yy
        y
      }
      
      # 6-digit blocks: DDMMYY / MMDDYY (year last), then YYMMDD (year first)
      for (blk in blocks[nchar(blocks) == 6]) {
        y <- to_year(as.integer(substr(blk, 5, 6)))
        if (in_range(y) && md_ok(as.integer(substr(blk, 1, 2)),
                                 as.integer(substr(blk, 3, 4)), y)) return(y)
        y <- to_year(as.integer(substr(blk, 1, 2)))
        if (in_range(y) && md_ok(as.integer(substr(blk, 3, 4)),
                                 as.integer(substr(blk, 5, 6)), y)) return(y)
      }
      
      # separated with 2-digit year, e.g. 12/03/25 or 12.03.25
      # (dashes are not used here because dashes are part of the Sample IDs)
      dmy2 <- stringr::str_match_all(
        s, "(?<![0-9])([0-9]{1,2})[/.]([0-9]{1,2})[/.]([0-9]{2})(?![0-9])")[[1]]
      for (i in seq_len(nrow(dmy2))) {
        y <- to_year(as.integer(dmy2[i, 4]))
        if (in_range(y) && md_ok(as.integer(dmy2[i, 2]), as.integer(dmy2[i, 3]), y)) return(y)
      }
    }
    
    NA_integer_
  }
  
  vapply(as.character(id), one, integer(1), USE.NAMES = FALSE)
}

# Full date. Used to order repeat runs; the YEAR does not depend on this
# succeeding. 6-7 digit codes are read as M/D/YYYY (e.g. 7172020 = 17 Jul 2020).
parse_visit_date_code <- function(code, min_year = 1990,
                                  max_year = as.integer(format(Sys.Date(), "%Y"))) {
  d1 <- as.Date(code, format = "%Y%m%d")
  d2 <- as.Date(code, format = "%d%m%Y")
  d3 <- as.Date(code, format = "%m%d%Y")
  ok <- function(d) !is.na(d) &
    as.integer(format(d, "%Y")) >= min_year &
    as.integer(format(d, "%Y")) <= max_year
  dplyr::case_when(ok(d1) ~ d1, ok(d2) ~ d2, ok(d3) ~ d3, TRUE ~ as.Date(NA))
}

#' Read the full visit date from a Sample ID
#'
#' Used to order repeat runs and follow-up visits. The year does not depend on
#' this succeeding (see [extract_visit_year_from_id()]). Six- and seven-digit
#' codes are read as M/D/YYYY.
#'
#' @inheritParams extract_visit_year_from_id
#' @return A `Date` vector (`NA` where no date can be read).
#' @export
#' @examples
#' parse_visit_date_from_id("100-00-0001-0001_1-004-0_20240729")
parse_visit_date_from_id <- function(id, min_year = 1990,
                                     max_year = as.integer(format(Sys.Date(), "%Y"))) {
  try_fmt <- function(x, fmts) {
    x <- stringr::str_replace_all(x, "[-/.]", "-")
    for (fm in fmts) {
      d <- as.Date(x, format = fm)
      if (!is.na(d)) {
        yr <- as.integer(format(d, "%Y"))
        if (yr >= min_year && yr <= max_year) return(as.numeric(d))
      }
    }
    NULL
  }
  
  one <- function(s) {
    if (is.na(s)) return(NA_real_)
    s <- strip_sample_number(s)     # ignore the 13-digit sample number
    if (is.na(s) || !nzchar(s)) return(NA_real_)
    
    # 1. every standalone 8-digit block after the sample number
    compact <- stringr::str_extract_all(s, "(?<![0-9])[0-9]{8}(?![0-9])")[[1]]
    for (code in compact) {
      d <- parse_visit_date_code(code, min_year, max_year)
      if (!is.na(d)) return(as.numeric(d))
    }
    
    # 1b. 6- or 7-digit M/D/YYYY written without leading zeros:
    #     392020 -> 3/9/2020, 7172020 -> 7/17/2020, 2112020 -> 2/11/2020
    short <- stringr::str_extract_all(s, "(?<![0-9])[0-9]{6,7}(?![0-9])")[[1]]
    for (blk in short) {
      n  <- nchar(blk)
      yr <- as.integer(substr(blk, n - 3, n))
      if (is.na(yr) || yr < min_year || yr > max_year) next
      pre <- substr(blk, 1, n - 4)                       # month + day digits
      splits <- if (nchar(pre) == 2) list(c(1, 1)) else list(c(1, 2), c(2, 1))
      for (sp in splits) {
        m <- as.integer(substr(pre, 1, sp[1]))
        d <- as.integer(substr(pre, sp[1] + 1, nchar(pre)))
        dt <- as.Date(sprintf("%04d-%02d-%02d", yr, m, d), format = "%Y-%m-%d")
        if (!is.na(dt)) return(as.numeric(dt))
      }
    }
    
    # 2. separated dates anywhere: 2024-07-29, 12-03-2025, 12.03.2025, 12/03/2025
    sep_ymd <- stringr::str_extract_all(
      s, "(?<![0-9])(?:19|20)[0-9]{2}[-/.][0-9]{1,2}[-/.][0-9]{1,2}(?![0-9])")[[1]]
    sep_dmy <- stringr::str_extract_all(
      s, "(?<![0-9])[0-9]{1,2}[-/.][0-9]{1,2}[-/.](?:19|20)[0-9]{2}(?![0-9])")[[1]]
    
    for (x in sep_ymd) { r <- try_fmt(x, "%Y-%m-%d"); if (!is.null(r)) return(r) }
    for (x in sep_dmy) { r <- try_fmt(x, c("%d-%m-%Y", "%m-%d-%Y")); if (!is.null(r)) return(r) }
    NA_real_
  }
  as.Date(vapply(as.character(id), one, numeric(1), USE.NAMES = FALSE),
          origin = "1970-01-01")
}

# ------------------------------------------------------------------------------
# Resolve ambiguous 6-7 digit dates using the visit code
#
# A date written M/D/YYYY without leading zeros can be split two ways:
#   1092018 -> 1/09/2018 (9 Jan) or 10/9/2018 (9 Oct)
# The parser takes the first valid reading. The visit code in the Sample ID
# (the middle number of e.g. 100-00-0002-0002_1-019-0_1092018) increases from
# one visit to the next, so a woman's visits must have non-decreasing dates in
# visit-code order. This step uses that to choose between the two readings.
# Everything it decides is returned in an audit table; nothing is hidden.
# ------------------------------------------------------------------------------

# All valid M/D/YYYY readings of a short (6-7 digit) date block in a Sample ID.
# Returns an empty vector when the date is not a short block or has one reading only
# (the first element is the reading parse_visit_date_from_id() uses).
short_date_candidates <- function(s, min_year = 1990,
                                  max_year = as.integer(format(Sys.Date(), "%Y"))) {
  none <- as.Date(character())
  if (is.na(s)) return(none)
  s <- strip_sample_number(s)
  if (is.na(s) || !nzchar(s)) return(none)
  compact <- stringr::str_extract_all(s, "(?<![0-9])[0-9]{8}(?![0-9])")[[1]]
  for (code in compact) {
    d1 <- as.Date(code, format = "%Y%m%d")                 # YYYYMMDD with a plausible year is clear
    if (!is.na(d1) && as.integer(format(d1, "%Y")) >= min_year &&
        as.integer(format(d1, "%Y")) <= max_year) return(none)
    # year last: DDMMYYYY or MMDDYYYY. Two readings only if day and month are both <= 12 and differ.
    yr <- as.integer(substr(code, 5, 8)); a <- as.integer(substr(code, 1, 2)); b <- as.integer(substr(code, 3, 4))
    if (!is.na(yr) && yr >= min_year && yr <= max_year && a >= 1 && a <= 12 && b >= 1 && b <= 12 && a != b) {
      dmy <- as.Date(sprintf("%04d-%02d-%02d", yr, b, a), format = "%Y-%m-%d")   # the parser's default
      mdy <- as.Date(sprintf("%04d-%02d-%02d", yr, a, b), format = "%Y-%m-%d")
      return(c(dmy, mdy))
    }
    if (!is.na(parse_visit_date_code(code, min_year, max_year))) return(none)  # any other valid 8-digit date
  }
  short <- stringr::str_extract_all(s, "(?<![0-9])[0-9]{6,7}(?![0-9])")[[1]]
  for (blk in short) {
    n  <- nchar(blk)
    yr <- as.integer(substr(blk, n - 3, n))
    if (is.na(yr) || yr < min_year || yr > max_year) next
    pre <- substr(blk, 1, n - 4)
    splits <- if (nchar(pre) == 2) list(c(1, 1)) else list(c(1, 2), c(2, 1))
    out <- none
    for (sp in splits) {
      m  <- as.integer(substr(pre, 1, sp[1]))
      d  <- as.integer(substr(pre, sp[1] + 1, nchar(pre)))
      dt <- as.Date(sprintf("%04d-%02d-%02d", yr, m, d), format = "%Y-%m-%d")
      if (!is.na(dt)) out <- c(out, dt)
    }
    return(if (length(out) >= 2) out else none)
  }
  none
}

# Visit number from the Sample ID: the middle number of <sample number>_<a>-<visit>-<b>...
visit_code_number <- function(id) {
  m <- stringr::str_match(as.character(id), "^[^_]*_[0-9]+-([0-9]+)(?:-|_|$)")
  suppressWarnings(as.integer(m[, 2]))
}

#' Choose between two readings of an ambiguous date using the visit code
#'
#' @param sample_id Character vector of Sample IDs.
#' @param visit_date Dates as read by [parse_visit_date_from_id()].
#' @param min_year,max_year Plausible year range.
#' @return A list with `visit_date` (the dates after resolving) and `audit`
#'   (one row per ambiguous date: what was read, what was chosen and why).
#' @keywords internal
#' @noRd
resolve_visit_dates_by_visit_code <- function(sample_id, visit_date, min_year = 1990,
                                              max_year = as.integer(format(Sys.Date(), "%Y"))) {
  sample_id  <- as.character(sample_id)
  new_date   <- as.Date(visit_date)
  vnum       <- visit_code_number(sample_id)
  woman      <- sub("_.*$", "", sample_id)
  cands      <- lapply(sample_id, short_date_candidates, min_year = min_year, max_year = max_year)
  ambiguous  <- vapply(cands, function(x) length(x) >= 2, logical(1)) & !is.na(vnum)
  audit <- list()
  
  no_code <- vapply(cands, function(x) length(x) >= 2, logical(1)) & is.na(vnum)
  for (i in which(no_code)) {
    audit[[length(audit) + 1]] <- data.frame(
      woman = woman[i], Sample_ID = sample_id[i], visit_number = NA_integer_,
      date_read_by_default = cands[[i]][1], date_chosen = cands[[i]][1],
      other_reading = as.character(cands[[i]][2]),
      decision = "ambiguous and no visit code to decide - check", stringsAsFactors = FALSE)
  }
  for (w in unique(woman[ambiguous])) {
    idx <- which(woman == w & !is.na(vnum))
    amb <- idx[ambiguous[idx]]
    amb <- amb[order(vnum[amb])]
    for (i in amb) {
      before <- idx[vnum[idx] < vnum[i] & !is.na(new_date[idx])]
      after  <- idx[vnum[idx] > vnum[i] & !ambiguous[idx] & !is.na(new_date[idx])]
      lower  <- if (length(before)) max(new_date[before]) else NA
      upper  <- if (length(after))  min(new_date[after])  else NA
      cand   <- cands[[i]]
      default <- cand[1]
      ok <- rep(TRUE, length(cand))
      if (!is.na(lower)) ok <- ok & cand >= lower
      if (!is.na(upper)) ok <- ok & cand <= upper
      valid <- cand[ok]
      if (length(valid) == 0) {
        chosen <- default; status <- "NO READING FITS the visit order - check this ID"
      } else if (length(valid) == 1) {
        chosen <- valid
        status <- if (chosen == default) "confirmed by visit order" else "changed to fit the visit order"
      } else if (!is.na(lower)) {
        chosen <- valid[which.min(abs(as.numeric(valid - lower)))]
        status <- "both readings fit; chose the one closest to the previous visit - check"
      } else if (!is.na(upper)) {
        chosen <- valid[which.min(abs(as.numeric(valid - upper)))]
        status <- "both readings fit; chose the one closest to the next visit - check"
      } else {
        chosen <- default; status <- "both readings fit and no other visits to decide - check"
      }
      new_date[i] <- chosen
      audit[[length(audit) + 1]] <- data.frame(
        woman = w, Sample_ID = sample_id[i], visit_number = vnum[i],
        date_read_by_default = default, date_chosen = chosen,
        other_reading = paste(setdiff(as.character(cand), as.character(chosen)), collapse = ", "),
        decision = status, stringsAsFactors = FALSE)
    }
  }
  list(visit_date = new_date,
       audit = if (length(audit)) do.call(rbind, audit) else
         data.frame(woman = character(), Sample_ID = character(), visit_number = integer(),
                    date_read_by_default = as.Date(character()), date_chosen = as.Date(character()),
                    other_reading = character(), decision = character()))
}


# ------------------------------------------------------------------------------
# Dates written with a 2-digit year (e.g. 070422 = 7 Apr 2022, or 90522 = 9 May 2022)
#
# The year is always read from these IDs, but a full date used to be left unread, so
# the woman could not be placed in a visit order. Here the day/month ORDER is taken
# from the data itself: among the 6-digit codes, if some have a first pair above 12
# (so it can only be a day) and none have a second pair above 12, the order is
# day-month-year. If the data show both orders, or neither, nothing is read.
# ------------------------------------------------------------------------------
read_two_digit_year_dates <- function(sample_id, visit_date, min_year = 1990,
                                      max_year = as.integer(format(Sys.Date(), "%Y"))) {
  sample_id <- as.character(sample_id)
  new_date  <- as.Date(visit_date)
  blk <- vapply(sample_id, function(s) {
    s <- strip_sample_number(s)
    if (is.na(s)) return(NA_character_)
    b <- stringr::str_extract_all(s, "(?<![0-9])[0-9]{5,6}(?![0-9])")[[1]]
    if (length(b)) b[1] else NA_character_
  }, character(1), USE.NAMES = FALSE)
  todo <- is.na(new_date) & !is.na(blk)
  empty <- data.frame(woman = character(), Sample_ID = character(), visit_number = integer(),
                      date_read_by_default = as.Date(character()), date_chosen = as.Date(character()),
                      other_reading = character(), decision = character(), stringsAsFactors = FALSE)
  if (!any(todo)) return(list(visit_date = new_date, audit = empty, order = NA_character_))
  
  six <- blk[todo & nchar(blk) == 6]
  a <- as.integer(substr(six, 1, 2)); b <- as.integer(substr(six, 3, 4))
  n_dmy <- sum(a > 12 & b <= 12, na.rm = TRUE)     # first pair can only be a day
  n_mdy <- sum(b > 12 & a <= 12, na.rm = TRUE)     # second pair can only be a day
  order <- if (n_dmy > 0 && n_mdy == 0) "dmy" else if (n_mdy > 0 && n_dmy == 0) "mdy" else NA_character_
  if (is.na(order)) return(list(visit_date = new_date, audit = empty, order = order))
  
  to_year <- function(yy) { y <- 2000L + yy; if (y > max_year) y <- 1900L + yy; y }
  audit <- list()
  for (i in which(todo)) {
    x <- blk[i]; n <- nchar(x)
    yy <- as.integer(substr(x, n - 1, n)); mid <- substr(x, 1, n - 2)
    if (order == "dmy") {
      d <- as.integer(substr(mid, 1, nchar(mid) - 2)); m <- as.integer(substr(mid, nchar(mid) - 1, nchar(mid)))
    } else {
      m <- as.integer(substr(mid, 1, nchar(mid) - 2)); d <- as.integer(substr(mid, nchar(mid) - 1, nchar(mid)))
    }
    y  <- to_year(yy)
    dt <- if (any(is.na(c(d, m, yy))) || nchar(mid) < 3) as.Date(NA) else
      as.Date(sprintf("%04d-%02d-%02d", y, m, d), format = "%Y-%m-%d")
    if (is.na(dt) || y < min_year || y > max_year) next
    new_date[i] <- dt
    audit[[length(audit) + 1]] <- data.frame(
      woman = sub("_.*$", "", sample_id[i]), Sample_ID = sample_id[i], visit_number = NA_integer_,
      date_read_by_default = as.Date(NA), date_chosen = dt, other_reading = "",
      decision = paste0("read as ", if (order == "dmy") "DDMMYY" else "MMDDYY",
                        " (2-digit year; order taken from ", if (order == "dmy") n_dmy else n_mdy,
                        " unambiguous dates in the data)"),
      stringsAsFactors = FALSE)
  }
  list(visit_date = new_date, audit = if (length(audit)) do.call(rbind, audit) else empty, order = order)
}

# Both steps, in order: read 2-digit-year dates, then settle ambiguous dates by visit code.
resolve_visit_dates <- function(sample_id, visit_date, min_year = 1990,
                                max_year = as.integer(format(Sys.Date(), "%Y"))) {
  a <- read_two_digit_year_dates(sample_id, visit_date, min_year, max_year)
  b <- resolve_visit_dates_by_visit_code(sample_id, a$visit_date, min_year, max_year)
  list(visit_date = b$visit_date, audit = rbind(a$audit, b$audit))
}


# ------------------------------------------------------------------------------
# Repeats of a sample
#
# A repeat is marked right after the date in the Sample ID, either with a number
# (7172020-1) or with a letter (070422b, 140622B = first repeat; c = second ...),
# or both (200622-1b). The original has no marker. A higher number means a later run.
# ------------------------------------------------------------------------------

# Rank of the letter that follows the date (b = 1, c = 2, ...; none = 0).
letter_repeat_rank <- function(id) {
  m <- stringr::str_match(as.character(id),
                          "(?<![0-9])[0-9]{5,8}(?:-[0-9]+)*([A-Za-z])(?:-[0-9]+)*$")[, 2]
  r <- match(tolower(m), letters) - 1L
  r[is.na(r)] <- 0L
  as.integer(r)
}

# Sample ID without its repeat marker (so an original and its repeats share one ID).
strip_repeat_marker <- function(id) {
  x <- as.character(id)
  x <- stringr::str_remove(x, "(?<=[0-9])[A-Za-z]$")
  x <- stringr::str_remove(x, "-[0-9]+$")
  stringr::str_remove(x, "(?<=[0-9])[A-Za-z]$")
}

# How many runs after the original this sample is (0 = original), for IDs that end with the date
# and its repeat marker. IDs that end with the visit code have no repeat marker here.
repeat_number_from_id <- function(id) {
  id <- as.character(id)
  m <- stringr::str_match(id, "(?<![0-9])[0-9]{5,8}((?:-[0-9]+)*)([A-Za-z]?)((?:-[0-9]+)*)$")
  nums <- vapply(seq_along(id), function(i) {
    if (is.na(m[i, 1])) return(0L)
    sum(as.integer(stringr::str_extract_all(paste0(m[i, 2], m[i, 4]), "[0-9]+")[[1]]))
  }, integer(1))
  r <- match(tolower(m[, 3]), letters) - 1L
  r[is.na(r)] <- 0L
  as.integer(nums + r)
}
