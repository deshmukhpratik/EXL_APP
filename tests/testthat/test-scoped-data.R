library(testthat)

# Synthetic loan_data
loan_data <- data.frame(
  loan_id     = 1:10,
  issued_flag = c(rep(TRUE, 6), rep(FALSE, 4)),
  fico_score  = seq(600, 780, length.out = 10)
)
assign("loan_data", loan_data, envir = .GlobalEnv)

source("../../R/services/upload_service.R", local = .GlobalEnv)

test_that("issued scope returns only issued_flag == TRUE rows", {
  got <- get_scoped_data("issued")
  expect_equal(nrow(got), 6)
  expect_true(all(got$issued_flag))
})

test_that("applications scope returns all rows", {
  got <- get_scoped_data("applications")
  expect_equal(nrow(got), 10)
})

test_that("unknown scope returns issued (safe default)", {
  got <- get_scoped_data("garbage")
  expect_equal(nrow(got), 6)
})

test_that("numeric 1/0 issued_flag also filters correctly", {
  df <- data.frame(loan_id = 1:4, issued_flag = c(1, 1, 0, 0))
  got <- get_scoped_data("issued", df = df)
  expect_equal(nrow(got), 2)
})
