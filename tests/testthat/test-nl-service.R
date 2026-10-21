library(testthat)

# Source service under test
source("../../R/services/nl_service.R", local = TRUE)

test_that("NL_ALLOWED_METRICS whitelist contains all documented metrics", {
  expect_true(is.character(NL_ALLOWED_METRICS))
  expect_true(all(c("bad_rate", "delinquency_rate", "irr", "decline_rate",
                    "approval_rate", "take_rate", "issued_count", "loan_count")
                  %in% NL_ALLOWED_METRICS))
})

test_that("NL_ALLOWED_DIMENSIONS whitelist contains all documented dimensions", {
  expect_true(is.character(NL_ALLOWED_DIMENSIONS))
  expect_true(all(c("partner_channel", "risk_grade", "fico_score_band",
                    "booking_vintage", "product_type", "borrower_repeat_type",
                    "test_group", "underwriting_risk_tier", "N_HARD_INQ_6M")
                  %in% NL_ALLOWED_DIMENSIONS))
})

test_that("NL_SUPPORTED_SHAPES lists the 6 approved shapes", {
  expect_setequal(NL_SUPPORTED_SHAPES,
                  c("ranking", "distribution", "time_compare", "top_n",
                    "funnel_ranking", "multi_criteria"))
})

source("helper-nl-fixtures.R", local = TRUE)

test_that("nl_compute_ranking returns the top dim for bad_rate", {
  df <- .nl_fixture_loans_with_channel_signal()
  out <- nl_compute_ranking(df, list(metric = "bad_rate",
                                     dimension = "partner_channel"))
  expect_true(is.list(out))
  expect_true(all(c("headline", "rows", "chart_type") %in% names(out)))
  expect_equal(out$chart_type, "bar_horizontal")
  # Direct was injected to be the highest bad-rate channel
  expect_equal(out$rows$partner_channel[1], "Direct")
  expect_true(out$rows$value[1] > out$rows$value[nrow(out$rows)])
  expect_match(out$headline, "Direct", fixed = TRUE)
})

test_that("nl_compute_ranking fails closed on unknown metric", {
  df <- .nl_fixture_loans()
  expect_error(
    nl_compute_ranking(df, list(metric = "nonsense", dimension = "partner_channel")),
    "metric"
  )
})

test_that("nl_compute_ranking fails closed on unknown dimension", {
  df <- .nl_fixture_loans()
  expect_error(
    nl_compute_ranking(df, list(metric = "bad_rate", dimension = "eye_color")),
    "dimension"
  )
})

test_that("nl_compute_ranking flags ties within 0.05pp and breaks alphabetically", {
  df <- data.frame(
    loan_id = sprintf("L%03d", 1:30),
    partner_channel = rep(c("Direct", "Indirect", "Broker"), each = 10),
    performance_delinquency_flag = c(
      rep(c(1L, 0L), 5),   # Direct  : bad rate 0.5
      rep(c(1L, 0L), 5),   # Indirect: bad rate 0.5 (tie with Direct)
      rep(0L, 10)          # Broker  : bad rate 0.0
    ),
    stringsAsFactors = FALSE
  )
  out <- nl_compute_ranking(df, list(metric = "bad_rate",
                                     dimension = "partner_channel"))
  expect_true(isTRUE(out$tie_broken))
  # Alphabetical tie-break: Direct < Indirect, so Direct ranks first
  expect_equal(out$rows$partner_channel[1], "Direct")
  expect_equal(out$rows$partner_channel[2], "Indirect")
  expect_match(out$headline, "tie broken alphabetically")
})

test_that("nl_compute_distribution returns share meeting threshold", {
  df <- .nl_fixture_loans()
  out <- nl_compute_distribution(df, list(
    threshold = list(col = "N_HARD_INQ_6M", op = ">=", value = 3)
  ))
  expect_true(all(c("headline", "rows", "chart_type") %in% names(out)))
  expect_equal(out$chart_type, "donut")
  # rows has exactly 2: meeting, not meeting
  expect_equal(nrow(out$rows), 2)
  expect_setequal(out$rows$bucket, c("meets", "does_not_meet"))
  expect_equal(sum(out$rows$n), nrow(df))
  # Value is a share, between 0 and 1
  expect_true(out$rows$value[out$rows$bucket == "meets"] >= 0 &&
              out$rows$value[out$rows$bucket == "meets"] <= 1)
  expect_match(out$headline, "hard inquiries|N_HARD_INQ_6M", ignore.case = TRUE)
})

test_that("nl_compute_distribution fails closed on unknown column", {
  df <- .nl_fixture_loans()
  expect_error(
    nl_compute_distribution(df, list(threshold = list(col = "zzz", op = ">=", value = 3))),
    "col"
  )
})

test_that("nl_compute_distribution supports <, <=, ==, >, >=", {
  df <- .nl_fixture_loans()
  for (op in c("<", "<=", "==", ">", ">=")) {
    out <- nl_compute_distribution(df, list(
      threshold = list(col = "N_HARD_INQ_6M", op = op, value = 3)
    ))
    expect_equal(nrow(out$rows), 2)
  }
})

test_that("nl_compute_time_compare returns delta between two periods", {
  df <- .nl_fixture_loans()
  out <- nl_compute_time_compare(df, list(
    metric = "bad_rate",
    time_a = "2026_Q1",
    time_b = "2025_Q4",
    time_dim = "booking_vintage"
  ))
  expect_true(all(c("headline", "rows", "chart_type") %in% names(out)))
  expect_equal(out$chart_type, "bar_grouped")
  expect_equal(nrow(out$rows), 2)
  expect_setequal(out$rows$period, c("2026_Q1", "2025_Q4"))
  expect_match(out$headline, "2026_Q1|Q1", ignore.case = TRUE)
})

test_that("nl_compute_time_compare defaults time_dim to booking_vintage", {
  df <- .nl_fixture_loans()
  out <- nl_compute_time_compare(df, list(
    metric = "bad_rate",
    time_a = "2026_Q1",
    time_b = "2025_Q4"
  ))
  expect_equal(nrow(out$rows), 2)
})

test_that("nl_compute_time_compare fails on unknown period", {
  df <- .nl_fixture_loans()
  expect_error(
    nl_compute_time_compare(df, list(metric = "bad_rate",
                                     time_a = "1999_Q9",
                                     time_b = "2025_Q4")),
    "period"
  )
})

test_that("nl_compute_top_n returns top-N rows sorted by metric", {
  df <- .nl_fixture_loans_with_channel_signal()
  out <- nl_compute_top_n(df, list(
    metric = "bad_rate",
    dimension = "partner_channel",
    top_n = 3,
    direction = "max"
  ))
  expect_equal(out$chart_type, "table")
  expect_equal(nrow(out$rows), 3)
  expect_true(out$rows$value[1] >= out$rows$value[2])
})

test_that("nl_compute_top_n direction='min' returns worst (lowest) rows", {
  df <- .nl_fixture_loans_with_channel_signal()
  out <- nl_compute_top_n(df, list(
    metric = "bad_rate",
    dimension = "partner_channel",
    top_n = 2,
    direction = "min"
  ))
  expect_equal(nrow(out$rows), 2)
  expect_true(out$rows$value[1] <= out$rows$value[2])
})

test_that("nl_compute_top_n caps top_n at number of groups", {
  df <- .nl_fixture_loans()
  out <- nl_compute_top_n(df, list(
    metric = "bad_rate", dimension = "partner_channel", top_n = 99
  ))
  expect_lte(nrow(out$rows), length(unique(df$partner_channel)))
})

test_that("nl_compute_funnel_ranking ranks dim by decline_rate", {
  df <- .nl_fixture_loans()
  out <- nl_compute_funnel_ranking(df, list(
    metric = "decline_rate",
    dimension = "fico_score_band"
  ))
  expect_equal(out$chart_type, "bar_horizontal")
  expect_true(all(out$rows$value >= 0 & out$rows$value <= 1))
})

test_that("nl_compute_funnel_ranking rejects non-funnel metrics", {
  df <- .nl_fixture_loans()
  expect_error(
    nl_compute_funnel_ranking(df, list(metric = "bad_rate",
                                       dimension = "fico_score_band")),
    "funnel metric"
  )
})

test_that("nl_compute_multi_criteria ranks by combined z-score", {
  df <- .nl_fixture_loans_with_channel_signal()
  out <- nl_compute_multi_criteria(df, list(
    metrics = c("bad_rate", "loan_count"),
    directions = c("max", "min"),
    dimension = "partner_channel"
  ))
  expect_equal(out$chart_type, "scatter")
  expect_true("composite_score" %in% names(out$rows))
  expect_equal(nrow(out$rows), length(unique(df$partner_channel)))
  expect_true(out$rows$composite_score[1] >= out$rows$composite_score[nrow(out$rows)])
})

test_that("nl_compute_multi_criteria requires matching lengths", {
  df <- .nl_fixture_loans()
  expect_error(
    nl_compute_multi_criteria(df, list(
      metrics = c("bad_rate", "loan_count"),
      directions = c("max"),
      dimension = "partner_channel"
    )),
    "length"
  )
})

# Mock `call_llm` by direct env manipulation. testthat::with_mocked_bindings
# requires a loaded package context (pkgload); our tests source the service
# with local=TRUE so there is no package. We swap `call_llm` in the env
# where `nl_classify` was defined, then restore.
.nl_env <- environment(nl_classify)
.nl_env$call_llm <- function(system, user, ...) stop("no LLM in test")

.with_mock_llm <- function(mock_fn, body) {
  old <- .nl_env$call_llm
  .nl_env$call_llm <- mock_fn
  on.exit(.nl_env$call_llm <- old, add = TRUE)
  body()
}

test_that("nl_classify returns structured output on valid LLM response", {
  out <- .with_mock_llm(
    function(system, user, ...) {
      '{"confidence":"high","shape":"ranking","metric":"bad_rate","dimension":"partner_channel"}'
    },
    function() nl_classify("which channel has max bad rate")
  )
  expect_equal(out$confidence, "high")
  expect_equal(out$shape, "ranking")
  expect_equal(out$metric, "bad_rate")
  expect_equal(out$dimension, "partner_channel")
})

test_that("nl_classify handles prose-wrapped JSON (strips code fences)", {
  out <- .with_mock_llm(
    function(system, user, ...) {
      "```json\n{\"confidence\":\"high\",\"shape\":\"ranking\",\"metric\":\"bad_rate\",\"dimension\":\"partner_channel\"}\n```"
    },
    function() nl_classify("x")
  )
  expect_equal(out$shape, "ranking")
})

test_that("nl_classify returns low confidence on unparseable response", {
  out <- .with_mock_llm(
    function(system, user, ...) "I think you want to know about...",
    function() nl_classify("x")
  )
  expect_equal(out$confidence, "low")
})

test_that("nl_classify returns low confidence when LLM returns NULL", {
  out <- .with_mock_llm(
    function(system, user, ...) NULL,
    function() nl_classify("x")
  )
  expect_equal(out$confidence, "low")
})

test_that("nl_classify returns low confidence when shape is unknown", {
  out <- .with_mock_llm(
    function(system, user, ...) {
      '{"confidence":"high","shape":"quadratic_regression","metric":"bad_rate"}'
    },
    function() nl_classify("x")
  )
  expect_equal(out$confidence, "low")
})

test_that("nl_classify returns low confidence when metric is unknown", {
  out <- .with_mock_llm(
    function(system, user, ...) {
      '{"confidence":"high","shape":"ranking","metric":"tartness","dimension":"partner_channel"}'
    },
    function() nl_classify("x")
  )
  expect_equal(out$confidence, "low")
})

test_that("nl_answer wires classifier -> compute end-to-end", {
  df <- .nl_fixture_loans_with_channel_signal()
  out <- .with_mock_llm(
    function(system, user, ...) {
      '{"confidence":"high","shape":"ranking","metric":"bad_rate","dimension":"partner_channel"}'
    },
    function() nl_answer("which channel has max bad rate", df, "Current view")
  )
  expect_equal(out$shape, "ranking")
  expect_true(!is.null(out$result))
  expect_equal(out$result$chart_type, "bar_horizontal")
  expect_equal(out$scope_label, "Current view")
  expect_true(!is.null(out$n))
  expect_equal(out$confidence, "high")
})

test_that("nl_answer returns confidence=low on classifier failure", {
  df <- .nl_fixture_loans()
  out <- .with_mock_llm(
    function(system, user, ...) "not json",
    function() nl_answer("gibberish", df, "Current view")
  )
  expect_equal(out$confidence, "low")
  expect_null(out$result)
})

test_that("nl_answer fails to low when compute throws", {
  df <- .nl_fixture_loans()
  df$partner_channel <- NULL  # compute will fail on missing col
  out <- .with_mock_llm(
    function(system, user, ...) {
      '{"confidence":"high","shape":"ranking","metric":"bad_rate","dimension":"partner_channel"}'
    },
    function() nl_answer("x", df, "Current view")
  )
  expect_equal(out$confidence, "low")
  expect_null(out$result)
  expect_match(out$reason %||% "", "compute|partner_channel", ignore.case = TRUE)
})
