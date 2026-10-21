# Tests for get_segment_elasticity() and get_segment_bad_rate_projection()
# waterfalls, and backward-compat of get_segment_assumption().

library(testthat)

# %||% operator (from rlang) — shim for isolated test context
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
  assign("%||%", `%||%`, envir = .GlobalEnv)
}

# Source engine files (tests run from package root)
for (f in sort(list.files("../../R/engine", pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = .GlobalEnv)
}

# ── Fixtures ──────────────────────────────────────────────────────────────
.fixture_cache <- function() {
  cells <- data.frame(
    grade                     = c("High", "Med", "Low", "High", "Med", "Low"),
    test_group                = c(rep("PL-V4-TA", 3), rep("PL-V4-TB", 3)),
    attribute                 = rep("N_HARD_INQ_6M", 6),
    bps_difference            = c(rep(100, 3), rep(150, 3)),
    control_take_rate         = c(0.30, 0.25, 0.20, 0.30, 0.25, 0.20),
    test_take_rate            = c(0.25, 0.20, 0.15, 0.21, 0.17, 0.12),
    elasticity_pct_per_100bps = c(5.0, 5.0, 5.0, 3.0, 3.3, 3.3),
    n_control                 = c(500L, 400L, 300L, 500L, 400L, 300L),
    n_test                    = c(500L, 400L, 300L, 500L, 400L, 300L),
    source                    = rep("test_control", 6),
    reliable                  = rep(TRUE, 6),
    sign_expected             = rep(TRUE, 6),
    stringsAsFactors          = FALSE
  )
  gs <- data.frame(grade = c("High","Med","Low"),
                   mean_elasticity_pct = c(4.0, 4.15, 4.15))
  attr(cells, "grade_summary") <- gs
  attr(cells, "version") <- "1.1"
  cells
}

.fixture_config <- function() {
  list(
    elasticity = c(High = 3, Med = 5, Low = 8),
    adverse_selection = c(High = 2, Med = 5, Low = 8),
    anchor_fico_floor = 760
  )
}

# ── Return contract ────────────────────────────────────────────────────────
test_that("get_segment_elasticity returns list with all required fields", {
  cache <- .fixture_cache()
  r <- get_segment_elasticity("N_HARD_INQ_6M", c(High=1, Med=0, Low=0),
                               cache, .fixture_config())
  expect_type(r, "list")
  expect_named(r, c("value","source","layer","confidence","n","detail"),
               ignore.order = TRUE)
  expect_true(is.numeric(r$value) && length(r$value) == 1)
  expect_true(is.character(r$source))
  expect_true(is.integer(r$layer))
  expect_true(is.character(r$detail))
})

test_that("get_segment_assumption returns length-1 numeric", {
  v <- get_segment_assumption(rep("High", 10), "elasticity")
  expect_true(is.numeric(v))
  expect_equal(length(v), 1)
})

# ── Layer reachability ─────────────────────────────────────────────────────
test_that("Layer 1: direct match fires with attribute + grade >= 50%", {
  cache <- .fixture_cache()
  r <- get_segment_elasticity("N_HARD_INQ_6M", c(High=1, Med=0, Low=0),
                               cache, .fixture_config())
  expect_equal(r$source, "direct_test_control")
  expect_equal(r$layer, 1L)
  expect_equal(r$confidence, "high")
  expect_equal(r$value, 4.0)  # average of 5.0 and 3.0 for High grade
  expect_equal(r$n, 1000L)    # 500 + 500 from V4-TA and V4-TB
})

test_that("Layer 1: skipped when primary_var does not match any cell", {
  cache <- .fixture_cache()
  r <- get_segment_elasticity("some_other_var", c(High=1, Med=0, Low=0),
                               cache, .fixture_config())
  expect_false(r$source == "direct_test_control")
})

test_that("Layer 1: skipped when primary_var is NA", {
  cache <- .fixture_cache()
  r <- get_segment_elasticity(NA_character_, c(High=1, Med=0, Low=0),
                               cache, .fixture_config())
  expect_false(r$source == "direct_test_control")
})

test_that("Layer 3: grade-weighted cache fallback fires when L1 misses", {
  cache <- .fixture_cache()
  r <- get_segment_elasticity(NA_character_, c(High=1, Med=0, Low=0),
                               cache, .fixture_config())
  expect_equal(r$source, "closest_segment")
  expect_equal(r$layer, 3L)
  expect_equal(r$confidence, "medium_low")
  expect_equal(round(r$value, 2), 4.0)
})

test_that("Layer 4: consultant default fires when no cache", {
  r <- get_segment_elasticity("N_HARD_INQ_6M", c(High=0, Med=0, Low=1),
                               NULL, .fixture_config())
  expect_equal(r$source, "consultant_default")
  expect_equal(r$layer, 4L)
  expect_equal(r$confidence, "low")
  expect_equal(r$value, 8)  # Low grade default
  expect_true(is.na(r$n))
})

# ── Waterfall priority ─────────────────────────────────────────────────────
test_that("Layer 1 wins when multiple layers could match", {
  cache <- .fixture_cache()
  r <- get_segment_elasticity("N_HARD_INQ_6M", c(High=1, Med=0, Low=0),
                               cache, .fixture_config())
  expect_equal(r$layer, 1L)  # Not 3, even though grade_summary is valid
})

# ── Malformed cache handling ──────────────────────────────────────────────
test_that("Cache missing attribute column: L1 skipped, falls to L3", {
  cache <- .fixture_cache()
  cache$attribute <- NULL  # remove the column
  r <- get_segment_elasticity("N_HARD_INQ_6M", c(High=1, Med=0, Low=0),
                               cache, .fixture_config())
  expect_equal(r$layer, 3L)
})

test_that("Cache missing grade_summary attr: L3 skipped, falls to L4", {
  cache <- .fixture_cache()
  cache$attribute <- NULL  # also remove so L1 can't fire
  attr(cache, "grade_summary") <- NULL
  r <- get_segment_elasticity(NA_character_, c(High=0, Med=1, Low=0),
                               cache, .fixture_config())
  expect_equal(r$layer, 4L)
})

test_that("No cache at all: L4 always reachable, returns numeric", {
  r <- get_segment_elasticity("any", c(High=0, Med=1, Low=0),
                               NULL, .fixture_config())
  expect_equal(r$layer, 4L)
  expect_true(is.numeric(r$value))
  expect_equal(r$value, 5)  # Med grade default
})

test_that("No cache, no consultant_config: L4 uses hardcoded defaults", {
  r <- get_segment_elasticity("any", c(High=1, Med=0, Low=0), NULL, NULL)
  expect_equal(r$layer, 4L)
  expect_equal(r$value, 3)  # hardcoded High default
})

# ── Dominant grade logic ──────────────────────────────────────────────────
test_that("Grade < 50% dominant falls back to Med", {
  cache <- .fixture_cache()
  # No grade has >= 50% — mix is 33/33/34
  r <- get_segment_elasticity("N_HARD_INQ_6M",
                               c(High=0.33, Med=0.34, Low=0.33),
                               cache, .fixture_config())
  # Med grade Layer 1: (5.0 + 3.3) / 2 = 4.15
  expect_equal(r$source, "direct_test_control")
  expect_equal(r$value, 4.15)
})

# ── Bad-rate projection waterfall ─────────────────────────────────────────
test_that("bad_rate L4 portfolio_mean fallback returns numeric", {
  cd <- data.frame(
    loan_id = 1:100,
    issued_flag = rep(1L, 100),
    risk_grade = sample(c("High","Med","Low"), 100, replace=TRUE),
    performance_delinquency_flag = c(rep(1L, 4), rep(0L, 96))
  )
  r <- get_segment_bad_rate_projection(
    list(primary_var = NA_character_, grade_mix = c(High=0, Med=1, Low=0)),
    cd, NULL, NULL, .fixture_config())
  expect_true(is.numeric(r$value))
  expect_true(r$value >= 0 && r$value <= 1)
})
