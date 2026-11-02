make_sample <- function() {
  data.frame(
    `Sample ID` = "S1",
    Target   = c("Control", "Amplification Control", "Empty",
                 "HPV universal probe", "16", "16", "18", "18"),
    Position = c("(1,1)", "(1,2)", "(1,3)", "(2,1)", "(3,1)", "(3,2)", "(4,1)", "(4,2)"),
    TH = NA, LTH = NA, Value = NA,
    Result = c("POSITIVE", "POSITIVE", "NEGATIVE", "POSITIVE",
               "POSITIVE", "POSITIVE", "POSITIVE", "NEGATIVE"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

test_that("probes are classified", {
  x <- classify_probes(make_sample())
  expect_equal(x$Probe_Category[x$Target == "Control"], "Positive Control")
  expect_equal(x$Probe_Category[x$Target == "Empty"], "NTC")
  expect_equal(x$Probe_Category[x$Target == "HPV universal probe"], "HPV Universal Probe")
  expect_equal(unique(x$Probe_Category[x$Target == "16"]), "HPV Genotype Target")
})

test_that("a sample is summarised with controls, genotypes and discordance", {
  s <- summarise_sample(classify_probes(make_sample()))
  expect_equal(nrow(s), 1)
  expect_equal(s$Positive_Control_Pass, "YES")
  expect_equal(s$NTC_Pass, "YES")
  expect_equal(s$HPV_16, "POSITIVE")
  expect_equal(s$HPV_18, "NEGATIVE")          # one position negative -> not positive
  expect_equal(s$Discordant, "YES")
  expect_match(s$Discordant_Comments, "^18:")
})

test_that("follow-up settings are validated", {
  expect_silent(followup_settings())
  expect_error(followup_settings(visits_required = 1))
  expect_error(followup_settings(clear_last_k = 5))
  expect_error(followup_settings(persist_rule = "all"))   # old setting no longer exists
  expect_equal(followup_settings(clear_last_k = 2)$clear_last_k, 2L)
})

test_that("risk groups are available", {
  g <- hpv_genotypes()
  expect_true(all(c("16", "18") %in% g$high_risk))
  expect_true(all(c("6", "11") %in% g$low_risk))
})
