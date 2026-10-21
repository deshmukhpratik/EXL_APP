# ============================================================
# NL Q&A Service
# Design: docs/superpowers/specs/2026-04-21-nl-qa-bar-design.md
#
# Exposes:
#   - Whitelists: NL_ALLOWED_METRICS, NL_ALLOWED_DIMENSIONS, NL_SUPPORTED_SHAPES
#   - Compute fns: nl_compute_<shape>(loans_df, params) -> list(headline, rows, chart_type)
#   - Classifier:  nl_classify(query) -> list(confidence, shape, metric, ...)
#   - Dispatcher:  nl_answer(query, loans_df, scope_label) -> list(card = shiny::tag, ...)
#   - Telemetry:   nl_log_submission(record)
#   - Session registry: nl_register_current_view(), nl_get_current_view()
# ============================================================

# %||% operator (from rlang) — shim for isolated evaluation contexts
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# ---- Whitelists (the LLM is told about these; compute fns fail closed otherwise) ----

NL_ALLOWED_METRICS <- c(
  "bad_rate", "delinquency_rate", "irr", "decline_rate",
  "approval_rate", "take_rate", "issued_count", "loan_count"
)

NL_ALLOWED_DIMENSIONS <- c(
  "partner_channel", "risk_grade", "fico_score_band",
  "booking_vintage", "product_type", "borrower_repeat_type",
  "test_group", "underwriting_risk_tier", "N_HARD_INQ_6M"
)

NL_SUPPORTED_SHAPES <- c(
  "ranking", "distribution", "time_compare",
  "top_n", "funnel_ranking", "multi_criteria"
)

# ---- Telemetry ------------------------------------------------------------

nl_log_submission <- function(record,
                              path = "logs/demo_questions.log") {
  tryCatch({
    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
    line <- jsonlite::toJSON(record, auto_unbox = TRUE)
    cat(line, "\n", file = path, append = TRUE, sep = "")
  }, error = function(e) invisible(NULL))
}

# ---- Session registry for per-tab "current view" reactives ----------------

nl_register_current_view <- function(session, tab_id, rx) {
  session$userData$nl_tab_views <- session$userData$nl_tab_views %||% list()
  session$userData$nl_tab_views[[tab_id]] <- rx
  invisible(NULL)
}

nl_get_current_view <- function(session, tab_id) {
  views <- session$userData$nl_tab_views %||% list()
  views[[tab_id]]
}

# ---- Compute functions (one per shape) — filled in by subsequent tasks ----

# Metric evaluators: each takes a subsetted df, returns one numeric.
.nl_eval_metric <- function(df, metric) {
  if (metric == "bad_rate" || metric == "delinquency_rate") {
    mean(df$performance_delinquency_flag %||% NA, na.rm = TRUE)
  } else if (metric == "decline_rate") {
    mean(1 - (df$offered_decision_flag %||% NA), na.rm = TRUE)
  } else if (metric == "approval_rate") {
    mean(df$offered_decision_flag %||% NA, na.rm = TRUE)
  } else if (metric == "take_rate") {
    mean(df$offer_accepted_flag %||% NA, na.rm = TRUE)
  } else if (metric == "issued_count") {
    sum(df$issued_flag %||% 0L, na.rm = TRUE)
  } else if (metric == "loan_count") {
    nrow(df)
  } else if (metric == "irr") {
    # Portfolio-level IRR requires the perf tape; nl compute falls back
    # to a simple yield proxy for tests/offline use. Full IRR is wired
    # at the dispatch level when tape is available.
    stop("irr computed at dispatch level, not per-group here")
  } else {
    stop(sprintf("unknown metric: %s", metric))
  }
}

nl_compute_ranking <- function(loans_df, params) {
  metric <- params$metric
  dim    <- params$dimension
  if (!metric %in% NL_ALLOWED_METRICS) stop(sprintf("unknown metric: %s", metric))
  if (!dim    %in% NL_ALLOWED_DIMENSIONS) stop(sprintf("unknown dimension: %s", dim))
  if (!dim %in% names(loans_df)) stop(sprintf("dimension %s missing from data", dim))

  groups <- split(loans_df, loans_df[[dim]])
  vals <- vapply(groups, function(g) .nl_eval_metric(g, metric), numeric(1))
  ord <- order(vals, decreasing = TRUE, na.last = NA)
  rows <- data.frame(
    dim   = names(groups)[ord],
    value = as.numeric(vals[ord]),
    n     = vapply(groups[ord], nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  names(rows)[1] <- dim

  # Tie detection: top two within 0.05pp
  tie_broken <- nrow(rows) >= 2 &&
                is.finite(rows$value[1]) && is.finite(rows$value[2]) &&
                abs(rows$value[1] - rows$value[2]) < 0.0005

  if (tie_broken) {
    # Re-sort by value desc, then by dim label asc (alphabetical tie-break)
    ord2 <- order(-rows$value, rows[[dim]])
    rows <- rows[ord2, , drop = FALSE]
  }

  pct <- function(x) sprintf("%.1f%%", x * 100)
  top_label <- rows[[dim]][1]
  top_val <- if (metric %in% c("issued_count", "loan_count")) {
    format(rows$value[1], big.mark = ",")
  } else pct(rows$value[1])

  headline <- sprintf("%s has the highest %s at %s.",
                      top_label, gsub("_", " ", metric), top_val)
  if (tie_broken) headline <- paste0(headline, " (tie broken alphabetically)")

  list(headline = headline, rows = rows, chart_type = "bar_horizontal",
       tie_broken = tie_broken)
}

nl_compute_distribution <- function(loans_df, params) {
  thr <- params$threshold
  if (is.null(thr) || is.null(thr$col)) stop("threshold missing col")
  if (!thr$col %in% NL_ALLOWED_DIMENSIONS) stop(sprintf("unknown col: %s", thr$col))
  if (!thr$col %in% names(loans_df)) stop(sprintf("col %s missing from data", thr$col))
  if (!thr$op %in% c("<", "<=", "==", ">", ">=")) stop(sprintf("unknown op: %s", thr$op))

  vals <- loans_df[[thr$col]]
  meets <- switch(thr$op,
    "<"  = vals <  thr$value,
    "<=" = vals <= thr$value,
    "==" = vals == thr$value,
    ">"  = vals >  thr$value,
    ">=" = vals >= thr$value
  )
  meets[is.na(meets)] <- FALSE
  share <- mean(meets)
  rows <- data.frame(
    bucket = c("meets", "does_not_meet"),
    n      = c(sum(meets), sum(!meets)),
    value  = c(share, 1 - share),
    stringsAsFactors = FALSE
  )
  # Use original column name if it matches a known label, otherwise transform
  col_label <- if (thr$col == "N_HARD_INQ_6M") {
    "hard inquiries"
  } else {
    gsub("_", " ", tolower(thr$col))
  }
  headline <- sprintf("%.1f%% of loans (%s) have %s %s %s.",
                      share * 100, format(sum(meets), big.mark = ","),
                      col_label, thr$op, thr$value)
  list(headline = headline, rows = rows, chart_type = "donut")
}
nl_compute_time_compare <- function(loans_df, params) {
  metric <- params$metric
  time_a <- params$time_a
  time_b <- params$time_b
  time_dim <- params$time_dim %||% "booking_vintage"
  if (!metric %in% NL_ALLOWED_METRICS) stop(sprintf("unknown metric: %s", metric))
  if (!time_dim %in% names(loans_df)) stop(sprintf("time_dim %s missing from data", time_dim))
  present <- unique(loans_df[[time_dim]])
  if (!time_a %in% present) stop(sprintf("period %s not present in data", time_a))
  if (!time_b %in% present) stop(sprintf("period %s not present in data", time_b))

  df_a <- loans_df[loans_df[[time_dim]] == time_a, , drop = FALSE]
  df_b <- loans_df[loans_df[[time_dim]] == time_b, , drop = FALSE]
  val_a <- .nl_eval_metric(df_a, metric)
  val_b <- .nl_eval_metric(df_b, metric)

  rows <- data.frame(
    period = c(time_a, time_b),
    value  = c(val_a, val_b),
    n      = c(nrow(df_a), nrow(df_b)),
    stringsAsFactors = FALSE
  )
  delta_pp <- (val_a - val_b) * 100
  pct <- function(x) sprintf("%.1f%%", x * 100)
  headline <- sprintf("%s %s vs %s: %s (%+0.1fpp).",
                      gsub("_", " ", metric), time_a, time_b,
                      pct(val_a), delta_pp)
  list(headline = headline, rows = rows, chart_type = "bar_grouped")
}
nl_compute_top_n <- function(loans_df, params) {
  metric <- params$metric
  dim    <- params$dimension
  n      <- as.integer(params$top_n %||% 5L)
  direction <- params$direction %||% "max"
  if (!metric %in% NL_ALLOWED_METRICS)   stop(sprintf("unknown metric: %s", metric))
  if (!dim    %in% NL_ALLOWED_DIMENSIONS) stop(sprintf("unknown dimension: %s", dim))
  if (!dim %in% names(loans_df)) stop(sprintf("dimension %s missing from data", dim))
  if (!direction %in% c("max", "min")) stop(sprintf("unknown direction: %s", direction))

  groups <- split(loans_df, loans_df[[dim]])
  vals <- vapply(groups, function(g) .nl_eval_metric(g, metric), numeric(1))
  ord <- order(vals, decreasing = (direction == "max"), na.last = NA)
  n <- min(n, length(ord))
  rows <- data.frame(
    dim   = names(groups)[ord[seq_len(n)]],
    value = as.numeric(vals[ord[seq_len(n)]]),
    n     = vapply(groups[ord[seq_len(n)]], nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  names(rows)[1] <- dim

  label <- if (direction == "max") "highest" else "lowest"
  headline <- sprintf("Top %d %s by %s (%s).", n, dim, gsub("_", " ", metric), label)
  list(headline = headline, rows = rows, chart_type = "table")
}
nl_compute_funnel_ranking <- function(loans_df, params) {
  metric <- params$metric
  dim    <- params$dimension
  funnel_metrics <- c("decline_rate", "approval_rate", "take_rate")
  if (!metric %in% funnel_metrics) {
    stop(sprintf("funnel metric must be one of %s, got: %s",
                 paste(funnel_metrics, collapse = ", "), metric))
  }
  if (!dim %in% NL_ALLOWED_DIMENSIONS) stop(sprintf("unknown dimension: %s", dim))
  if (!dim %in% names(loans_df)) stop(sprintf("dimension %s missing from data", dim))

  groups <- split(loans_df, loans_df[[dim]])
  vals <- vapply(groups, function(g) .nl_eval_metric(g, metric), numeric(1))
  ord <- order(vals, decreasing = TRUE, na.last = NA)
  rows <- data.frame(
    dim   = names(groups)[ord],
    value = as.numeric(vals[ord]),
    n     = vapply(groups[ord], nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  names(rows)[1] <- dim
  top_label <- rows[[dim]][1]
  pct <- function(x) sprintf("%.1f%%", x * 100)
  headline <- sprintf("%s has the highest %s at %s.",
                      top_label, gsub("_", " ", metric), pct(rows$value[1]))
  list(headline = headline, rows = rows, chart_type = "bar_horizontal")
}
nl_compute_multi_criteria <- function(loans_df, params) {
  metrics    <- params$metrics
  directions <- params$directions
  dim        <- params$dimension
  if (length(metrics) != length(directions)) {
    stop("metrics and directions must be the same length")
  }
  for (m in metrics) if (!m %in% NL_ALLOWED_METRICS)
    stop(sprintf("unknown metric: %s", m))
  if (!dim %in% NL_ALLOWED_DIMENSIONS) stop(sprintf("unknown dimension: %s", dim))
  if (!dim %in% names(loans_df)) stop(sprintf("dimension %s missing from data", dim))
  if (!all(directions %in% c("max", "min"))) stop("directions must be max or min")

  groups <- split(loans_df, loans_df[[dim]])
  metric_vals <- lapply(metrics, function(m)
    vapply(groups, function(g) .nl_eval_metric(g, m), numeric(1)))
  names(metric_vals) <- metrics

  # Z-score each metric; flip sign for direction=min so higher = "worse"
  z <- Map(function(v, d) {
    s <- sd(v, na.rm = TRUE)
    zscore <- if (is.finite(s) && s > 0) (v - mean(v, na.rm = TRUE)) / s else rep(0, length(v))
    if (d == "min") zscore <- -zscore
    zscore
  }, metric_vals, directions)
  composite <- Reduce(`+`, z)

  rows <- data.frame(
    dim = names(groups),
    composite_score = composite,
    n = vapply(groups, nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  for (m in metrics) rows[[m]] <- metric_vals[[m]]
  rows <- rows[order(-rows$composite_score), , drop = FALSE]
  names(rows)[1] <- dim

  winner <- rows[[dim]][1]
  metric_desc <- paste0(directions, " ", metrics)
  headline <- sprintf("%s ranks worst across (%s).", winner,
                      paste(metric_desc, collapse = " + "))
  list(headline = headline, rows = rows, chart_type = "scatter")
}

# ---- Classifier and dispatcher — filled in by subsequent tasks ------------

# ---- Classifier -----------------------------------------------------------

.nl_build_system_prompt <- function() {
  paste0(
    "You are a question classifier for a credit risk tool. ",
    "Classify the user's question into ONE of six shapes: ",
    paste(NL_SUPPORTED_SHAPES, collapse = ", "), ". ",
    "Extract the pieces needed to run the computation. ",
    "Return ONLY a raw JSON object. No markdown, no prose, no code fences.\n\n",
    "Available metrics: ", paste(NL_ALLOWED_METRICS, collapse = ", "), "\n",
    "Available dimensions: ", paste(NL_ALLOWED_DIMENSIONS, collapse = ", "), "\n\n",
    "If you cannot confidently map the question to one of the shapes with ",
    "all required pieces, return {\"confidence\":\"low\"}.\n\n",
    "Required pieces per shape:\n",
    "- ranking, funnel_ranking: metric, dimension\n",
    "- distribution: threshold (object with col, op, value)\n",
    "- time_compare: metric, time_a, time_b (optional time_dim, defaults to booking_vintage)\n",
    "- top_n: metric, dimension, top_n (integer), direction (max or min)\n",
    "- multi_criteria: metrics (array), directions (array, same length), dimension\n"
  )
}

.nl_strip_fences <- function(raw) {
  x <- trimws(raw %||% "")
  x <- sub("^\\s*```[a-zA-Z0-9_-]*\\s*\\n?", "", x)
  x <- sub("\\n?\\s*```\\s*$", "", x)
  trimws(x)
}

.nl_low <- function(reason = NULL) {
  out <- list(confidence = "low")
  if (!is.null(reason)) out$reason <- reason
  out
}

nl_classify <- function(query) {
  if (is.null(query) || !nzchar(trimws(query))) return(.nl_low("empty query"))
  raw <- tryCatch(
    call_llm(system = .nl_build_system_prompt(), user = query,
             max_tokens = 300, temperature = 0.1),
    error = function(e) NULL
  )
  if (is.null(raw) || !nzchar(raw)) return(.nl_low("no llm response"))
  cleaned <- .nl_strip_fences(raw)
  parsed <- tryCatch(jsonlite::fromJSON(cleaned, simplifyVector = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed) || !is.list(parsed)) return(.nl_low("unparseable"))
  if (identical(parsed$confidence, "low")) return(.nl_low("llm reported low"))

  if (is.null(parsed$shape) || !parsed$shape %in% NL_SUPPORTED_SHAPES) {
    return(.nl_low("unknown shape"))
  }
  if (!is.null(parsed[["metric"]]) && !parsed[["metric"]] %in% NL_ALLOWED_METRICS) {
    return(.nl_low("unknown metric"))
  }
  if (!is.null(parsed$metrics)) {
    unk <- setdiff(parsed$metrics, NL_ALLOWED_METRICS)
    if (length(unk) > 0) return(.nl_low("unknown metric(s)"))
  }
  if (!is.null(parsed$dimension) && !parsed$dimension %in% NL_ALLOWED_DIMENSIONS) {
    return(.nl_low("unknown dimension"))
  }
  shape <- parsed$shape
  required <- switch(shape,
    ranking        = c("metric", "dimension"),
    funnel_ranking = c("metric", "dimension"),
    distribution   = c("threshold"),
    time_compare   = c("metric", "time_a", "time_b"),
    top_n          = c("metric", "dimension", "top_n", "direction"),
    multi_criteria = c("metrics", "directions", "dimension")
  )
  missing <- setdiff(required, names(parsed))
  if (length(missing) > 0) return(.nl_low(sprintf("missing: %s",
                                                  paste(missing, collapse = ","))))

  parsed$confidence <- "high"
  parsed
}

# ---- Dispatcher -----------------------------------------------------------

nl_answer <- function(query, loans_df, scope_label = "Current view") {
  t0 <- Sys.time()
  classified <- nl_classify(query)
  if (identical(classified$confidence, "low")) {
    return(list(
      confidence  = "low",
      shape       = NA_character_,
      result      = NULL,
      scope_label = scope_label,
      n           = nrow(loans_df),
      reason      = classified$reason %||% "low confidence",
      latency_ms  = as.integer(1000 * as.numeric(Sys.time() - t0, units = "secs"))
    ))
  }

  compute_fn <- switch(classified$shape,
    ranking        = nl_compute_ranking,
    distribution   = nl_compute_distribution,
    time_compare   = nl_compute_time_compare,
    top_n          = nl_compute_top_n,
    funnel_ranking = nl_compute_funnel_ranking,
    multi_criteria = nl_compute_multi_criteria
  )
  result <- tryCatch(compute_fn(loans_df, classified),
                     error = function(e) e)
  if (inherits(result, "error")) {
    return(list(
      confidence  = "low",
      shape       = classified$shape,
      result      = NULL,
      scope_label = scope_label,
      n           = nrow(loans_df),
      reason      = sprintf("compute error: %s", conditionMessage(result)),
      latency_ms  = as.integer(1000 * as.numeric(Sys.time() - t0, units = "secs"))
    ))
  }

  list(
    confidence  = "high",
    shape       = classified$shape,
    params      = classified,
    result      = result,
    scope_label = scope_label,
    n           = nrow(loans_df),
    latency_ms  = as.integer(1000 * as.numeric(Sys.time() - t0, units = "secs"))
  )
}
