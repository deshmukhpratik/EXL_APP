library(testthat)
source("../../R/engine/root_cause.R", local = .GlobalEnv)

test_that("rc_metric_dq returns weighted DQ rate", {
  df <- data.frame(
    performance_delinquency_flag = c(1, 0, 1, 0, 0),
    issued_flag = c(1, 1, 1, 1, 1)
  )
  expect_equal(rc_metric_dq(df), 0.4)
})

test_that("rc_metric_dq handles empty or all-NA gracefully", {
  expect_true(is.na(rc_metric_dq(data.frame(performance_delinquency_flag = integer(0)))))
  expect_true(is.na(rc_metric_dq(data.frame(performance_delinquency_flag = c(NA, NA)))))
})

test_that("rc_metric_loss_rate computes sum(loss)/sum(issued_amount)", {
  df <- data.frame(
    performance_delinquency_amount = c(100, 0, 50, 0),
    issued_amount = c(1000, 1000, 500, 500)
  )
  expect_equal(rc_metric_loss_rate(df), 150 / 3000)
})

test_that("rc_metric_chargeoff_dollars sums the loss amount", {
  df <- data.frame(performance_delinquency_amount = c(100, 0, 50, NA))
  expect_equal(rc_metric_chargeoff_dollars(df), 150)
})

test_that("rc_bin_variable bins FICO into standard bands", {
  df <- data.frame(fico_score = c(620, 700, 740, 780))
  out <- rc_bin_variable(df, "fico_score")
  expect_equal(length(out), 4)
  expect_true(is.factor(out))
  expect_equal(length(unique(out)), 4)
})

test_that("rc_bin_variable passes categorical columns through as factor", {
  df <- data.frame(partner_channel = c("direct", "partner", "direct"))
  out <- rc_bin_variable(df, "partner_channel")
  expect_true(is.factor(out))
  expect_equal(as.character(out), c("direct", "partner", "direct"))
})

test_that("rc_bin_variable returns NULL for unknown column", {
  df <- data.frame(x = 1:3)
  expect_null(rc_bin_variable(df, "nonexistent_col"))
})

test_that("rc_bin_variable bins DTI into quartile-ish buckets", {
  df <- data.frame(debt_to_income_ratio = c(0.1, 0.25, 0.35, 0.5, 0.6))
  out <- rc_bin_variable(df, "debt_to_income_ratio")
  expect_true(is.factor(out))
  expect_true(length(levels(out)) >= 2)
})

test_that("rc_score_split returns per-bucket metric and contribution in pp", {
  df <- data.frame(
    fico_score = c(rep(600, 100), rep(720, 100)),
    performance_delinquency_flag = c(rep(1, 20), rep(0, 80), rep(1, 5), rep(0, 95))
  )
  out <- rc_score_split(df, "fico_score", rc_metric_dq)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("bucket", "n", "pct", "metric_value", "contribution_pp") %in% names(out)))
  low_row  <- out[out$bucket == "<620", , drop = FALSE]
  high_row <- out[out$bucket == "700-740", , drop = FALSE]
  expect_equal(low_row$n, 100)
  expect_equal(high_row$n, 100)
  expect_equal(low_row$metric_value,  0.20, tolerance = 1e-8)
  expect_equal(high_row$metric_value, 0.05, tolerance = 1e-8)
})

test_that("rc_score_split returns empty frame for unknown variable", {
  df <- data.frame(x = 1:5, performance_delinquency_flag = c(0,1,0,1,0))
  out <- rc_score_split(df, "nonexistent", rc_metric_dq)
  expect_equal(nrow(out), 0)
})

test_that("rc_compute_pareto_path walks greedy top-|contribution| path", {
  set.seed(1)
  n <- 1000
  df <- data.frame(
    fico_score = c(rep(600, 400), rep(720, 600)),
    debt_to_income_ratio = c(
      c(rep(0.5, 200), rep(0.2, 200)),
      rep(0.2, 600)
    ),
    performance_delinquency_flag = c(
      c(rep(1, 100), rep(0, 100), rep(0, 200)),
      rep(0, 600)
    )
  )
  path <- rc_compute_pareto_path(df, rc_metric_dq,
                                 candidates = c("fico_score", "debt_to_income_ratio"),
                                 max_depth = 2, min_seg_pct = 0.05)
  expect_type(path, "list")
  expect_equal(path$depth, 0)
  expect_equal(path$metric_value, 0.1, tolerance = 1e-8)
  expect_equal(path$chosen$variable, "fico_score")
  expect_true(!is.null(path$child))
  expect_equal(path$child$chosen$variable, "debt_to_income_ratio")
})

test_that("rc_compute_pareto_path stops at max_depth", {
  df <- data.frame(
    fico_score = c(rep(600, 50), rep(720, 50)),
    performance_delinquency_flag = c(rep(1, 25), rep(0, 25), rep(0, 50))
  )
  path <- rc_compute_pareto_path(df, rc_metric_dq,
                                 candidates = "fico_score",
                                 max_depth = 1, min_seg_pct = 0.05)
  expect_equal(path$child$depth, 1)
  expect_null(path$child$child)
})

test_that("rc_compute_pareto_path respects min_seg_pct", {
  df <- data.frame(
    fico_score = c(rep(600, 2), rep(720, 98)),
    performance_delinquency_flag = c(1, 1, rep(0, 98))
  )
  path <- rc_compute_pareto_path(df, rc_metric_dq,
                                 candidates = "fico_score",
                                 max_depth = 2, min_seg_pct = 0.05)
  expect_true(is.null(path$child) || path$child$n / nrow(df) >= 0.05)
})

test_that("rc_decompose_shift returns mix_contrib_pp + rate_contrib_pp per segment", {
  baseline <- data.frame(
    seg = c(rep("A", 50), rep("B", 50)),
    performance_delinquency_flag = c(rep(c(1,0), times = c(5, 45)),
                                      rep(c(1,0), times = c(10, 40)))
  )
  current <- data.frame(
    seg = c(rep("A", 50), rep("B", 50)),
    performance_delinquency_flag = c(rep(c(1,0), times = c(5, 45)),
                                      rep(c(1,0), times = c(15, 35)))
  )
  out <- rc_decompose_shift(current, baseline, rc_metric_dq, seg_col = "seg")
  expect_s3_class(out, "data.frame")
  expect_setequal(out$segment, c("A", "B"))
  total_mix  <- sum(out$mix_contrib_pp,  na.rm = TRUE)
  total_rate <- sum(out$rate_contrib_pp, na.rm = TRUE)
  expect_equal(total_mix, 0, tolerance = 1e-6)
  expect_equal(total_rate, 5, tolerance = 1e-6)
})

test_that("rc_decompose_shift captures pure mix shift", {
  baseline <- data.frame(
    seg = c(rep("A", 50), rep("B", 50)),
    performance_delinquency_flag = c(rep(c(1,0), times = c(5, 45)),
                                      rep(c(1,0), times = c(15, 35)))
  )
  current <- data.frame(
    seg = c(rep("A", 20), rep("B", 80)),
    performance_delinquency_flag = c(rep(c(1,0), times = c(2, 18)),
                                      rep(c(1,0), times = c(24, 56)))
  )
  out <- rc_decompose_shift(current, baseline, rc_metric_dq, seg_col = "seg")
  total_rate <- sum(out$rate_contrib_pp, na.rm = TRUE)
  total_mix  <- sum(out$mix_contrib_pp,  na.rm = TRUE)
  expect_equal(total_rate, 0, tolerance = 1e-6)
  expect_true(abs(total_mix) > 1)
})
