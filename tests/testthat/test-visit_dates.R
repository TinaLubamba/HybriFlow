test_that("year is read from common ID layouts", {
  expect_equal(extract_visit_year_from_id("100-00-0001-0001_1-004-0_20240729"), 2024L)
  expect_equal(extract_visit_year_from_id("100-00-0001-0001_1-004-0_7172020-1"), 2020L)
})

test_that("the sample number is never read as a year", {
  expect_true(is.na(extract_visit_year_from_id("100-00-0003-0003")))
})

test_that("full dates are parsed", {
  d <- parse_visit_date_from_id("100-00-0001-0001_1-004-0_20240729")
  expect_s3_class(d, "Date")
  expect_equal(d, as.Date("2024-07-29"))
  expect_equal(parse_visit_date_from_id("100-00-0001-0001_1-004-0_7172020"),
               as.Date("2020-07-17"))
})

test_that("an ambiguous short date is resolved using the visit code", {
  ids <- c("100-00-0002-0002_1-006-0_20180710",
           "100-00-0002-0002_1-019-0_1092018",   # 9 Jan 2018 or 9 Oct 2018?
           "100-00-0002-0002_1-034-5_1252019")   # 25 Jan 2019 or 5 Dec 2019?
  d <- parse_visit_date_from_id(ids)
  expect_equal(as.character(d[2]), "2018-01-09")          # the default reading puts it first
  r <- resolve_visit_dates_by_visit_code(ids, d)
  expect_equal(as.character(r$visit_date), c("2018-07-10", "2018-10-09", "2019-01-25"))
  expect_equal(r$audit$decision[r$audit$visit_number == 19], "changed to fit the visit order")
  expect_true(all(diff(r$visit_date) > 0))               # visits now in date order
})

test_that("unambiguous dates and dates with no fit are left alone and reported", {
  ids <- c("100-00-0001-0001_1-001-0_20240115", "100-00-0001-0001_1-010-0_20240415",
           "100-00-0004-0004_1-001-0_20180710", "100-00-0004-0004_1-005-0_1092017")
  d <- parse_visit_date_from_id(ids)
  r <- resolve_visit_dates_by_visit_code(ids, d)
  expect_equal(r$visit_date, d)                           # nothing invented
  expect_match(r$audit$decision, "NO READING FITS")
})

test_that("2-digit-year dates are read when the data show a single day/month order", {
  ids <- c("100-00-0005-0005_070422", "100-00-0005-0005_070422b",   # 7 Apr 2022 (+ repeat marker b)
           "200-0003-003-13_260922",                                 # 26 Sep 2022: only a day can be 26
           "200-0003-003-00_90522")                                  # 9 May 2022, no leading zero
  d <- parse_visit_date_from_id(ids)
  expect_true(all(is.na(d)))                                        # not read before
  r <- resolve_visit_dates(ids, d)
  expect_equal(as.character(r$visit_date), c("2022-04-07", "2022-04-07", "2022-09-26", "2022-05-09"))
})

test_that("2-digit-year dates are NOT read when the data do not show the order", {
  ids <- c("100-00-0001-0001_070422", "100-00-0002-0002_080422")    # both pairs <= 12: cannot tell
  r <- resolve_visit_dates(ids, parse_visit_date_from_id(ids))
  expect_true(all(is.na(r$visit_date)))
})

test_that("an ambiguous 8-digit date (DDMMYYYY or MMDDYYYY) is settled by the visit code", {
  ids <- c("100-00-0009-0009_1-006-0_20181109",    # 9 Nov 2018
           "100-00-0009-0009_1-019-0_02112019")    # DDMMYYYY = 2 Nov 2019 (before) or MMDDYYYY = 11 Feb 2019
  d <- parse_visit_date_from_id(ids)
  r <- resolve_visit_dates(ids, d)
  expect_true(all(diff(r$visit_date) > 0))
  expect_true(any(grepl("check|visit order", r$audit$decision)))
})
