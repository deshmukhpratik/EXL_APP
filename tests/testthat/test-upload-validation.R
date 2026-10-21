library(testthat)
source("../../R/services/upload_service.R", local = .GlobalEnv)

test_that("valid issued file passes all checks", {
  df <- data.frame(
    loan_id = 1:5, fico_score = c(650, 680, 700, 720, 750),
    risk_grade = rep("High", 5), apr = rep(0.15, 5),
    loan_amount = rep(10000, 5), term_months = rep(36, 5),
    issued_date = rep("2024-01-01", 5), product_code = rep("PL", 5)
  )
  rep <- validate_upload(df, "issued")
  expect_true(rep$ok_to_commit)
  expect_length(rep$checks, 5)
})

test_that("missing required column fails structural check", {
  df <- data.frame(loan_id = 1:3, fico_score = c(650, 700, 720))
  rep <- validate_upload(df, "issued")
  expect_false(rep$ok_to_commit)
  cols <- Filter(function(c) c$name == "Columns present", rep$checks)[[1]]
  expect_equal(cols$status, "fail")
})

test_that("FICO out of range raises range-sanity warning but does not block", {
  df <- data.frame(
    loan_id = 1:3, fico_score = c(200, 700, 900),
    risk_grade = rep("High", 3), apr = rep(0.15, 3),
    loan_amount = rep(10000, 3), term_months = rep(36, 3),
    issued_date = rep("2024-01-01", 3), product_code = rep("PL", 3)
  )
  rep <- validate_upload(df, "issued")
  expect_true(rep$ok_to_commit)
  rng <- Filter(function(c) c$name == "Range sanity", rep$checks)[[1]]
  expect_equal(rng$status, "warn")
})

test_that("unknown data_type returns fail", {
  rep <- validate_upload(data.frame(x = 1), "unknown_type")
  expect_false(rep$ok_to_commit)
})

test_that("duplicate IDs produce warn", {
  df <- data.frame(
    loan_id = c(1, 1, 2), fico_score = c(700, 700, 700),
    risk_grade = rep("High", 3), apr = rep(0.15, 3),
    loan_amount = rep(10000, 3), term_months = rep(36, 3),
    issued_date = rep("2024-01-01", 3), product_code = rep("PL", 3)
  )
  rep <- validate_upload(df, "issued")
  dup <- Filter(function(c) c$name == "Duplicates", rep$checks)[[1]]
  expect_equal(dup$status, "warn")
})
