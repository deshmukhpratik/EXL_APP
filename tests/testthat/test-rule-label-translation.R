library(testthat)

# Load engine + globals so wi_rule_vars is available
source("../../R/03_globals_policies.R", local = .GlobalEnv)
for (f in sort(list.files("../../R/engine", pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = .GlobalEnv)
}

test_that("every wi_rule_vars id translates to its label in a rule string", {
  for (v in wi_rule_vars) {
    rule <- paste0(v$id, " < 100")
    out  <- translate_rule_var_labels(rule)
    expect_true(grepl(v$label, out, fixed = TRUE),
                info = paste0("id=", v$id, " label=", v$label, " got=", out))
  }
})

test_that("compound rules translate multiple ids in one string", {
  rule <- "fico_score < 680 & risk_model_score_3 >= 0.0017 & REVOL_BAL > 5000"
  out  <- translate_rule_var_labels(rule)
  expect_true(grepl("FICO Score", out, fixed = TRUE))
  expect_true(grepl("Risk Score 3", out, fixed = TRUE))
  expect_true(grepl("Revolving Balance", out, fixed = TRUE))
})

test_that("extract_rule_vars returns ids in order for compound rule", {
  rule <- "fico_score < 680 & risk_model_score_3 >= 0.0017 & REVOL_BAL > 5000"
  vars <- extract_rule_vars(rule)
  expect_equal(vars[1:3], c("fico_score", "risk_model_score_3", "REVOL_BAL"))
})

test_that("extract_rule_vars returns empty on empty input", {
  expect_length(extract_rule_vars(""), 0)
  expect_length(extract_rule_vars(NA_character_), 0)
})

test_that("extract_rule_vars avoids substring collisions between risk_model_score ids", {
  rule <- "risk_model_score_1 > 0.5 & risk_model_score_3 < 0.2"
  vars <- extract_rule_vars(rule)
  expect_true("risk_model_score_1" %in% vars)
  expect_true("risk_model_score_3" %in% vars)
})
