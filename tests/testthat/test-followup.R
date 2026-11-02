# A genotype only "persists" if it is detected at baseline and at EVERY follow-up visit.
# One that is missed at any follow-up counts as "cleared", even if it is detected again later.

make_visits <- function(id, patterns) {
  do.call(rbind, lapply(seq_along(patterns), function(v) {
    g <- patterns[[v]]
    data.frame(
      Sample_ID = sprintf("%s_%d_2024%02d15", id, v, v * 3),
      Visit_Date = sprintf("2024-%02d-15", v * 3),
      Overall_QC_Status = "PASS",
      HPV_Universal_Probe = if (length(g)) "POSITIVE" else "NEGATIVE",
      HPV_16 = if ("16" %in% g) "POSITIVE" else "NEGATIVE",
      HPV_18 = if ("18" %in% g) "POSITIVE" else "NEGATIVE",
      HPV_6  = "NEGATIVE",
      stringsAsFactors = FALSE
    )
  }))
}

test_that("persistence means detected at every visit; a gap counts as cleared", {
  none <- character(0)
  women <- list(
    persist     = list("16", "16", "16"),
    gap         = list("16", none, "16"),            # positive, negative, positive again
    clearearly = list("16", none, none),
    clearlate  = list("16", "16", none),
    persistacq = list("16", "16", c("16", "18")),
    mixed       = list(c("16", "18"), "16", c("16", "18"))   # 16 persisted, 18 has a gap
  )
  db <- do.call(rbind, Map(make_visits, names(women), women))
  out <- tempfile(); dir.create(out)
  res <- suppressWarnings(suppressMessages(utils::capture.output(
    r <- run_followup_analysis(db, out, hr_cols = c("HPV_16", "HPV_18"), lr_cols = "HPV_6",
                               settings = list(visits_required = 3)))))
  cls <- r$followup_db
  grp <- stats::setNames(cls$group, cls$participant)
  expect_equal(unname(grp["persist"]),     "Persist only")
  expect_equal(unname(grp["gap"]),         "Clear only")
  expect_equal(unname(grp["clearearly"]), "Clear only")
  expect_equal(unname(grp["clearlate"]),  "Clear only")
  expect_equal(unname(grp["persistacq"]), "Persist + acquire")
  expect_equal(unname(grp["mixed"]),       "Persist + clear")
  # the gap is still recorded so these women can be found
  expect_equal(cls$relapse[cls$participant == "gap"], "YES")
  expect_match(cls$intermittent_types[cls$participant == "gap"], "HPV_16")
})

test_that("when a visit has an original and a repeat, the latest repeat is used", {
  row <- function(id, date, g16) data.frame(
    Sample_ID = id, Visit_Date = date, Overall_QC_Status = "PASS",
    HPV_Universal_Probe = if (g16) "POSITIVE" else "NEGATIVE",
    HPV_16 = if (g16) "POSITIVE" else "NEGATIVE", HPV_18 = "NEGATIVE", HPV_6 = "NEGATIVE",
    stringsAsFactors = FALSE)
  d <- function(x) as.Date(x)
  db <- rbind(
    # original baseline negative, repeat (b) positive, then negative twice: P > N > N -> cleared
    row("wlatest_140622",  d("2022-06-14"), FALSE), row("wlatest_140622b", d("2022-06-14"), TRUE),
    row("wlatest_120722",  d("2022-07-12"), FALSE), row("wlatest_111022",  d("2022-10-11"), FALSE),
    # original baseline positive, repeat (-1) negative: the repeat wins, so the baseline is negative
    row("wrepeat_7172020", d("2020-07-17"), TRUE),  row("wrepeat_7172020-1", d("2020-07-17"), FALSE),
    row("wrepeat_8172020", d("2020-08-17"), FALSE), row("wrepeat_9172020", d("2020-09-17"), FALSE),
    # no repeats: unchanged, persists at every visit
    row("wsingle_140622", d("2022-06-14"), TRUE),   row("wsingle_120722", d("2022-07-12"), TRUE),
    row("wsingle_111022", d("2022-10-11"), TRUE)
  )
  out <- tempfile(); dir.create(out)
  utils::capture.output(suppressWarnings(suppressMessages(
    r <- run_followup_analysis(db, out, hr_cols = c("HPV_16", "HPV_18"), lr_cols = "HPV_6",
                               settings = list(visits_required = 3)))))
  grp <- stats::setNames(r$followup_db$group, r$followup_db$participant)
  expect_equal(unname(grp["wlatest"]), "Clear only")
  expect_match(unname(grp["wrepeat"]), "Baseline not positive")
  expect_equal(unname(grp["wsingle"]), "Persist only")
})

test_that("a letter after the date marks a repeat and pairs it with its original", {
  ids <- c("200-0001-001-01_140622", "200-0001-001-01_140622b", "200-0002-002-00_200622-1b",
           "100-00-0004-0004_1-005-0_20240807-1")
  expect_equal(repeat_number_from_id(ids), c(0L, 1L, 2L, 1L))
  expect_equal(strip_repeat_marker(ids)[1], strip_repeat_marker(ids)[2])
})
