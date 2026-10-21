library(testthat)
source("../../R/engine/root_cause.R", local = .GlobalEnv)
# Stub get_scoped_data so we don't pull real loan_data
get_scoped_data <- function(scope = "issued", df = NULL) df
source("../../R/services/root_cause_service.R", local = .GlobalEnv)

make_df <- function() {
  set.seed(42); n <- 500
  data.frame(
    loan_id = 1:n,
    fico_score = sample(c(600, 680, 740, 780), n, replace = TRUE),
    debt_to_income_ratio = runif(n, 0.1, 0.6),
    performance_delinquency_flag = rbinom(n, 1, 0.1),
    performance_delinquency_amount = 0,
    issued_amount = 10000,
    issued_flag = 1,
    booking_vintage = sample(c("2025Q3", "2025Q4"), n, replace = TRUE)
  )
}

test_that("run_root_cause returns path + (optional) shift + alternatives", {
  df <- make_df()
  out <- run_root_cause(df, metric = "dq_rate", comparison = "none",
                        candidates = c("fico_score", "debt_to_income_ratio"))
  expect_type(out, "list")
  expect_true(!is.null(out$path))
  expect_null(out$shift)
  expect_true(!is.null(out$alternatives))
})

test_that("run_root_cause with vintage comparison returns shift frame", {
  df <- make_df()
  out <- run_root_cause(df, metric = "dq_rate", comparison = "prior_vintage",
                        candidates = c("fico_score"))
  expect_true(!is.null(out$shift))
  expect_s3_class(out$shift, "data.frame")
  expect_true(all(c("segment", "mix_contrib_pp", "rate_contrib_pp") %in% names(out$shift)))
})

test_that("run_root_cause rejects unknown metric", {
  df <- make_df()
  expect_error(run_root_cause(df, metric = "nonsense", comparison = "none",
                              candidates = "fico_score"),
               regexp = "metric")
})
