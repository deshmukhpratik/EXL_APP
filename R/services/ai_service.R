# ============================================================
# R/services/ai_service.R
#
# Unified LLM service layer. Implements the offline-first
# contract from the spec: AI features are STRICTLY ADDITIVE.
# Every exported high-level function (explain_segment,
# narrate_scenario, summarize_report) has a deterministic
# template fallback that NEVER returns NULL.
#
# The low-level call_llm() returns NULL on any error and tab
# code must be prepared to handle that — but the high-level
# helpers above take care of the fallback, so tab code just
# calls explain_segment(...) and always gets text back.
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---- Internal state (package-local) ----------------------------------------
.cpt_ai_state <- new.env(parent = emptyenv())
.cpt_ai_state$provider     <- NULL    # memoized provider
.cpt_ai_state$detected     <- FALSE   # has detection run yet?
.cpt_ai_state$tokens_used  <- 0L
.cpt_ai_state$budget       <- 100000L # 100K tokens per session
.cpt_ai_state$errors       <- 0L
.cpt_ai_state$last_error   <- NULL
.cpt_ai_state$calls        <- 0L
.cpt_ai_state$cache        <- list()   # in-memory cache
.cpt_ai_state$cache_loaded <- FALSE

.cpt_ai_cache_path <- function() {
  if (dir.exists("cache")) "cache/ai_cache.rds"
  else "ai_cache.rds"
}

.cpt_ai_load_cache <- function() {
  if (isTRUE(.cpt_ai_state$cache_loaded)) return(invisible())
  p <- .cpt_ai_cache_path()
  if (file.exists(p)) {
    .cpt_ai_state$cache <- tryCatch(readRDS(p), error = function(e) list())
  }
  .cpt_ai_state$cache_loaded <- TRUE
  invisible()
}

.cpt_ai_save_cache <- function() {
  p <- .cpt_ai_cache_path()
  tryCatch(saveRDS(.cpt_ai_state$cache, p),
           error = function(e) invisible(NULL))
}

# ---- detect_llm_provider ----------------------------------------------------
#' Detect which provider to use based on env vars.
#' Priority: LLM_PROVIDER > Azure > OpenAI > Anthropic > NULL.
#' Memoized in .cpt_ai_state$provider for the session.
detect_llm_provider <- function(force = FALSE) {
  if (isTRUE(.cpt_ai_state$detected) && !force) return(.cpt_ai_state$provider)

  explicit <- Sys.getenv("LLM_PROVIDER", "")
  provider <- NULL

  if (nzchar(explicit)) {
    provider <- switch(tolower(explicit),
      "openai"    = if (exists("make_openai_provider")) make_openai_provider(),
      "azure"     = if (exists("make_azure_provider")) make_azure_provider(),
      "anthropic" = if (exists("make_anthropic_provider")) make_anthropic_provider(),
      NULL
    )
  } else {
    # Auto-detect
    if (exists("make_azure_provider") &&
        nzchar(Sys.getenv("AZURE_OPENAI_API_KEY", "")) &&
        nzchar(Sys.getenv("AZURE_OPENAI_ENDPOINT", "")) &&
        nzchar(Sys.getenv("AZURE_OPENAI_DEPLOYMENT", ""))) {
      provider <- make_azure_provider()
    } else if (exists("make_openai_provider") &&
               nzchar(Sys.getenv("OPENAI_API_KEY", ""))) {
      provider <- make_openai_provider()
    } else if (exists("make_anthropic_provider") &&
               nzchar(Sys.getenv("ANTHROPIC_API_KEY", ""))) {
      provider <- make_anthropic_provider()
    }
  }

  .cpt_ai_state$provider <- provider
  .cpt_ai_state$detected <- TRUE

  if (!is.null(provider)) {
    .cpt_ai_load_cache()
    message(sprintf("cpt: AI provider detected - %s (model: %s)",
                    provider$name, provider$model))
  }
  provider
}

#' Is there a usable LLM provider?
#' Requires a detected provider AND the httr2 package (used by every provider's
#' call function). Without httr2 every call fails after an 8-second timeout,
#' which made the Opportunity Explorer "Why this action?" strip sit in a
#' recalculating state for ~20s before the template fallback rendered. Gating
#' up-front keeps the template path synchronous.
llm_available <- function() {
  if (is.null(detect_llm_provider())) return(FALSE)
  requireNamespace("httr2", quietly = TRUE)
}

#' Lightweight snapshot of AI state for the topbar indicator.
ai_status <- function() {
  p <- detect_llm_provider()
  list(
    provider    = if (is.null(p)) NULL else p$name,
    model       = if (is.null(p)) NULL else p$model,
    tokens_used = .cpt_ai_state$tokens_used,
    budget      = .cpt_ai_state$budget,
    errors      = .cpt_ai_state$errors,
    last_error  = .cpt_ai_state$last_error,
    calls       = .cpt_ai_state$calls,
    cache_size  = length(.cpt_ai_state$cache)
  )
}

#' Low-level LLM call.
#'
#' Returns character(1) on success, or NULL on any failure
#' (no provider, budget exhausted, network error, auth error,
#' timeout, etc). Caller falls back to template string.
call_llm <- function(system, user, max_tokens = 200,
                     temperature = 0.2, timeout_sec = 8) {
  provider <- detect_llm_provider()
  if (is.null(provider)) return(NULL)

  # Budget check
  if (.cpt_ai_state$tokens_used >= .cpt_ai_state$budget) {
    .cpt_ai_state$last_error <- "budget exhausted"
    return(NULL)
  }

  # Cache check
  key <- digest::digest(list(system, user, provider$name, provider$model))
  cached <- .cpt_ai_state$cache[[key]]
  if (!is.null(cached)) return(cached)

  # Make the call
  .cpt_ai_state$calls <- .cpt_ai_state$calls + 1L
  result <- tryCatch(
    provider$call(system, user,
                  max_tokens = max_tokens,
                  temperature = temperature,
                  timeout_sec = timeout_sec),
    error = function(e) {
      .cpt_ai_state$errors <- .cpt_ai_state$errors + 1L
      .cpt_ai_state$last_error <- conditionMessage(e)
      message("cpt ai: call failed - ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(result)) return(NULL)

  text <- trimws(result$text %||% "")
  if (!nzchar(text)) return(NULL)

  # Update state + cache
  .cpt_ai_state$tokens_used <- .cpt_ai_state$tokens_used +
                                as.integer(result$tokens_used %||% 0L)
  .cpt_ai_state$cache[[key]] <- text
  .cpt_ai_save_cache()

  text
}

# ---- High-level helpers: always return character(1) -----------------------

#' Build a segment-explanation prompt + deterministic template fallback.
#'
#' @param segment A list with keys: rule (or display_rule), n, bad_rate (or loss),
#'                mean_irr (or irr), lift, source_filter
#' @param portfolio_context list(portfolio_bad, portfolio_irr, product)
#' @return character(1) - NEVER NULL
explain_segment <- function(segment, portfolio_context = NULL) {
  if (is.null(segment)) return("")

  # Normalize inputs
  rule_txt  <- segment$display_rule %||% segment$rule %||% "(no rule)"
  n         <- segment$n %||% 0L
  bad_rate  <- segment$bad_rate %||% segment$loss %||% NA_real_
  mean_irr  <- segment$mean_irr %||% segment$irr %||% NA_real_
  lift      <- segment$lift %||% NA_real_

  port_bad  <- portfolio_context$portfolio_bad %||% NA_real_
  port_irr  <- portfolio_context$portfolio_irr %||% NA_real_
  product   <- portfolio_context$product %||% "this portfolio"

  # Template fallback — always computed first as a safety net
  .fmt_pct <- function(x) if (is.na(x)) "n/a" else sprintf("%.1f%%", x * 100)
  template <- sprintf(
    "%s loans matching %s. Loss rate %s%s, avg IRR %s%s.",
    format(n, big.mark = ","),
    rule_txt,
    .fmt_pct(bad_rate),
    if (!is.na(lift)) sprintf(" (%.1fx portfolio)", lift) else "",
    .fmt_pct(mean_irr),
    if (!is.na(port_irr)) sprintf(" vs portfolio %s", .fmt_pct(port_irr)) else ""
  )

  if (!llm_available()) return(template)

  system <- paste(
    "You are a concise credit-risk analyst explaining flagged loan segments",
    "to a credit-committee audience. Write in plain English, no jargon.",
    "Always 2 sentences maximum. Do NOT restate the rule verbatim -",
    "interpret what kind of borrower it describes, and explain the risk."
  )
  user <- sprintf(
    "Segment: %s\nLoans in segment: %s\nLoss rate: %s\nPortfolio avg loss: %s\nLift: %s\nAvg IRR: %s\nProduct: %s\n\nExplain this segment in 2 sentences.",
    rule_txt, format(n, big.mark = ","),
    .fmt_pct(bad_rate), .fmt_pct(port_bad),
    if (!is.na(lift)) sprintf("%.2fx", lift) else "n/a",
    .fmt_pct(mean_irr), product
  )

  result <- call_llm(system = system, user = user,
                     max_tokens = 140, temperature = 0.3)
  if (is.null(result) || !nzchar(result)) return(template)
  result
}

#' Build a scenario-narrator prompt + deterministic template fallback.
#' @return character(1) - NEVER NULL
narrate_scenario <- function(baseline_kpis, scenario_kpis, changed_params = list()) {
  if (is.null(baseline_kpis) || is.null(scenario_kpis)) {
    return("Run a scenario to see a plain-English summary of the impact.")
  }

  .fmt_pct <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%.2f%%", x * 100)
  .fmt_bps <- function(b, s) {
    if (is.null(b) || is.null(s) || is.na(b) || is.na(s)) return("no change")
    delta <- (s - b) * 10000
    sprintf("%+d bps", as.integer(round(delta)))
  }

  # Deterministic template
  changed_summary <- if (length(changed_params) == 0) "no parameters changed" else {
    parts <- vapply(names(changed_params), function(nm) {
      sprintf("%s -> %s", nm, as.character(changed_params[[nm]]))
    }, character(1))
    paste(parts, collapse = ", ")
  }
  template <- sprintf(
    "Scenario changes: %s. Approval rate goes from %s to %s. Portfolio IRR %s. Loss rate %s.",
    changed_summary,
    .fmt_pct(baseline_kpis$approval_rate),
    .fmt_pct(scenario_kpis$approval_rate),
    .fmt_bps(baseline_kpis$portfolio_irr, scenario_kpis$portfolio_irr),
    .fmt_bps(baseline_kpis$avg_loss_rate, scenario_kpis$avg_loss_rate)
  )

  if (!llm_available()) return(template)

  system <- paste(
    "You are a concise credit-risk analyst summarising the impact of a",
    "policy change to a credit committee. 2 sentences maximum. Plain",
    "English, no jargon. Focus on what the changes mean, not the mechanics."
  )
  user <- sprintf(
    "Changed parameters:\n%s\n\nBaseline KPIs:\n  approval_rate = %s\n  portfolio_irr = %s\n  avg_loss_rate = %s\n\nScenario KPIs:\n  approval_rate = %s\n  portfolio_irr = %s\n  avg_loss_rate = %s\n\nWrite a 2-sentence summary of the impact.",
    changed_summary,
    .fmt_pct(baseline_kpis$approval_rate),
    .fmt_pct(baseline_kpis$portfolio_irr),
    .fmt_pct(baseline_kpis$avg_loss_rate),
    .fmt_pct(scenario_kpis$approval_rate),
    .fmt_pct(scenario_kpis$portfolio_irr),
    .fmt_pct(scenario_kpis$avg_loss_rate)
  )

  result <- call_llm(system = system, user = user,
                     max_tokens = 180, temperature = 0.3)
  if (is.null(result) || !nzchar(result)) return(template)
  result
}

#' Generate executive bullet-point insights from KPI data.
#'
#' @param context_type character(1): "scan", "simulation", or "comparison"
#' @param kpi_summary  named list of KPIs
#' @param details      optional named list of additional context
#' @return character(1) - NEVER NULL
generate_insights <- function(context_type, kpi_summary, details = list()) {
  if (is.null(kpi_summary)) return("")

  .fmt_pct <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%.1f%%", x * 100)
  .fmt_num <- function(x) if (is.null(x) || is.na(x)) "n/a" else format(as.integer(x), big.mark = ",")

  # ---- Deterministic template fallback (always computed first) ----
  template <- if (context_type == "scan") {
    paste0(
      "- Portfolio bad rate is ", .fmt_pct(kpi_summary$bad_rate),
      " across ", .fmt_num(kpi_summary$n_issued), " issued loans.\n",
      "- ", .fmt_num(kpi_summary$segment_count %||% 0), " actionable segments identified",
      if (!is.null(kpi_summary$top_segment_rule)) paste0("; top segment: ", kpi_summary$top_segment_rule) else "",
      ".\n",
      "- Portfolio IRR is ", .fmt_pct(kpi_summary$irr), "."
    )
  } else if (context_type == "simulation") {
    paste0(
      "- Approval rate moves from ", .fmt_pct(kpi_summary$baseline_appr_rate),
      " to ", .fmt_pct(kpi_summary$scenario_appr_rate), ".\n",
      "- Bad rate changes from ", .fmt_pct(kpi_summary$baseline_bad_rate),
      " to ", .fmt_pct(kpi_summary$scenario_bad_rate), ".\n",
      "- Avg APR shifts from ", .fmt_pct(kpi_summary$baseline_avg_apr),
      " to ", .fmt_pct(kpi_summary$scenario_avg_apr), "."
    )
  } else if (context_type == "comparison") {
    paste0(
      "- Policy A approval rate ", .fmt_pct(kpi_summary$a_appr_rate),
      " vs Policy B ", .fmt_pct(kpi_summary$b_appr_rate), ".\n",
      "- Policy A bad rate ", .fmt_pct(kpi_summary$a_bad_rate),
      " vs Policy B ", .fmt_pct(kpi_summary$b_bad_rate), ".\n",
      "- Policy A IRR ", .fmt_pct(kpi_summary$a_irr),
      " vs Policy B ", .fmt_pct(kpi_summary$b_irr), "."
    )
  } else {
    "- No insights available for this context."
  }

  if (!llm_available()) return(template)

  # ---- Build LLM prompt ----
  kpi_lines <- vapply(names(kpi_summary), function(nm) {
    v <- kpi_summary[[nm]]
    if (is.numeric(v)) sprintf("  %s = %s", nm, if (abs(v) < 1) .fmt_pct(v) else .fmt_num(v))
    else sprintf("  %s = %s", nm, as.character(v))
  }, character(1))
  kpi_table <- paste(kpi_lines, collapse = "\n")

  detail_text <- if (length(details) > 0) {
    paste(vapply(names(details), function(nm) {
      sprintf("  %s: %s", nm, as.character(details[[nm]]))
    }, character(1)), collapse = "\n")
  } else ""

  system <- paste(
    "You are a credit-risk analyst generating executive bullet-point insights.",
    "Write exactly 3-5 bullets. Each bullet is one sentence.",
    "Highlight actionable findings."
  )
  user <- sprintf(
    "Context: %s\n\nKPIs:\n%s%s\n\nGenerate insights.",
    context_type, kpi_table,
    if (nzchar(detail_text)) paste0("\n\nAdditional details:\n", detail_text) else ""
  )

  result <- call_llm(system = system, user = user,
                     max_tokens = 250, temperature = 0.3)
  if (is.null(result) || !nzchar(result)) return(template)
  result
}


#' Generate concise talking points for a CRO meeting.
#'
#' @param finding_type character(1): "segment", "scenario", or "comparison"
#' @param finding_data named list with KPIs and context
#' @return character(1) - NEVER NULL
generate_talking_points <- function(finding_type, finding_data) {
  if (is.null(finding_data)) return("")

  .fmt_pct <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%.1f%%", x * 100)
  .fmt_num <- function(x) if (is.null(x) || is.na(x)) "n/a" else format(as.integer(x), big.mark = ",")

  # ---- Deterministic template fallback (always computed first) ----
  template <- if (finding_type == "segment") {
    # Build action-specific recommendation
    rec <- finding_data$recommended_action %||% "review"
    action_line <- if (identical(rec, "decline")) {
      paste0("3. Recommended action: decline this segment — at ",
             if (!is.null(finding_data$lift)) sprintf("%.1fx", finding_data$lift) else "n/a",
             " portfolio average, losses far outweigh revenue",
             if (!is.null(finding_data$mean_irr) && !is.na(finding_data$mean_irr))
               paste0(" (segment IRR: ", .fmt_pct(finding_data$mean_irr), ")") else "",
             ".")
    } else if (identical(rec, "reprice")) {
      paste0("3. Recommended action: reprice this segment +100-200 bps — ",
             "elevated risk at ", if (!is.null(finding_data$lift)) sprintf("%.1fx", finding_data$lift) else "n/a",
             " portfolio average may be offset by higher yield",
             if (!is.null(finding_data$avg_apr) && !is.na(finding_data$avg_apr))
               paste0(" (current APR: ", .fmt_pct(finding_data$avg_apr), ")") else "",
             ".")
    } else {
      paste0("3. Monitor closely — lift of ",
             if (!is.null(finding_data$lift)) sprintf("%.1fx", finding_data$lift) else "n/a",
             " is moderate; test tighter cutoffs before acting",
             if (!is.null(finding_data$mean_irr) && !is.na(finding_data$mean_irr))
               paste0(" (segment IRR: ", .fmt_pct(finding_data$mean_irr), ")") else "",
             ".")
    }
    paste0(
      "1. Segment matching [", finding_data$rule %||% "n/a", "] has ",
      .fmt_num(finding_data$n), " loans with a bad rate of ",
      .fmt_pct(finding_data$bad_rate), " (",
      if (!is.null(finding_data$lift)) sprintf("%.1fx", finding_data$lift) else "n/a",
      " portfolio average).\n",
      "2. This represents ", .fmt_pct(finding_data$pct_of_book), " of the book",
      if (!is.null(finding_data$mean_irr) && !is.na(finding_data$mean_irr))
        paste0("; segment IRR is ", .fmt_pct(finding_data$mean_irr),
               " vs portfolio average") else "",
      ".\n",
      action_line
    )
  } else if (finding_type == "scenario") {
    paste0(
      "1. The proposed policy change moves approval rate from ",
      .fmt_pct(finding_data$baseline_appr_rate), " to ",
      .fmt_pct(finding_data$scenario_appr_rate), ".\n",
      "2. Bad rate impact: ", .fmt_pct(finding_data$baseline_bad_rate),
      " to ", .fmt_pct(finding_data$scenario_bad_rate), ".\n",
      "3. Net effect on portfolio APR: ",
      .fmt_pct(finding_data$baseline_avg_apr), " to ",
      .fmt_pct(finding_data$scenario_avg_apr), "."
    )
  } else if (finding_type == "comparison") {
    paste0(
      "1. Policy A approves ", .fmt_pct(finding_data$a_appr_rate),
      " vs Policy B at ", .fmt_pct(finding_data$b_appr_rate), ".\n",
      "2. Bad rate difference: ", .fmt_pct(finding_data$a_bad_rate),
      " (A) vs ", .fmt_pct(finding_data$b_bad_rate), " (B).\n",
      "3. IRR comparison: ", .fmt_pct(finding_data$a_irr),
      " (A) vs ", .fmt_pct(finding_data$b_irr), " (B)."
    )
  } else {
    "1. No data available.\n2. Re-run the analysis.\n3. Check input parameters."
  }

  if (!llm_available()) return(template)

  # ---- Build LLM prompt ----
  data_lines <- vapply(names(finding_data), function(nm) {
    v <- finding_data[[nm]]
    if (is.numeric(v)) sprintf("  %s = %s", nm, if (abs(v) < 1) .fmt_pct(v) else .fmt_num(v))
    else sprintf("  %s = %s", nm, as.character(v))
  }, character(1))
  data_text <- paste(data_lines, collapse = "\n")

  system <- paste(
    "You are writing 3 concise talking points for a Chief Risk Officer.",
    "Each point is one sentence. Business language, not jargon.",
    "Numbered list. Focus on impact and action."
  )
  user <- sprintf(
    "Finding type: %s\n\nData:\n%s\n\nWrite 3 talking points.",
    finding_type, data_text
  )

  result <- call_llm(system = system, user = user,
                     max_tokens = 200, temperature = 0.2)
  if (is.null(result) || !nzchar(result)) return(template)
  result
}


#' Narrate the differences between two credit policies.
#'
#' @param policy_a_name character(1) identifier for policy A
#' @param policy_b_name character(1) identifier for policy B
#' @param param_diff data.frame with columns param, value_a, value_b, changed
#' @param kpi_delta  data.frame with columns metric, a, b, delta (or named list)
#' @return character(1) - NEVER NULL
narrate_comparison <- function(policy_a_name, policy_b_name, param_diff, kpi_delta) {
  if (is.null(param_diff) && is.null(kpi_delta)) {
    return("Select two policies to see a comparison narrative.")
  }

  .fmt_pct <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%.2f%%", x * 100)
  .fmt_bps <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%+d bps", as.integer(round(x * 10000)))

  # Identify changed parameters
  changed_params <- if (!is.null(param_diff) && nrow(param_diff) > 0) {
    param_diff[param_diff$changed == TRUE, , drop = FALSE]
  } else {
    data.frame(param = character(0), value_a = character(0),
               value_b = character(0), changed = logical(0),
               stringsAsFactors = FALSE)
  }
  n_changed <- nrow(changed_params)
  param_list <- if (n_changed > 0) paste(changed_params$param, collapse = ", ") else "none"

  # Extract KPI deltas
  .get_kpi <- function(metric_name) {
    if (is.null(kpi_delta) || !is.data.frame(kpi_delta) || nrow(kpi_delta) == 0) return(NA_real_)
    row <- kpi_delta[kpi_delta$metric == metric_name, , drop = FALSE]
    if (nrow(row) == 0) return(NA_real_)
    row$delta[1]
  }
  appr_delta <- .get_kpi("approval_rate")
  bad_delta  <- .get_kpi("avg_loss_rate")
  irr_delta  <- .get_kpi("portfolio_irr")

  # Deterministic template fallback
  template <- sprintf(
    "Policy %s differs from %s in %d parameter%s: %s. Key impacts: approval rate %s, bad rate %s, IRR %s.",
    policy_b_name, policy_a_name,
    n_changed, if (n_changed == 1) "" else "s",
    param_list,
    .fmt_bps(appr_delta), .fmt_bps(bad_delta), .fmt_bps(irr_delta)
  )

  if (!llm_available()) return(template)

  # Build LLM prompts
  system <- paste(
    "You are a credit-risk analyst narrating differences between two credit",
    "policies for a credit committee. Explain what changed, why it matters,",
    "and the likely business impact. 3-4 sentences. Plain English."
  )

  param_table <- if (n_changed > 0) {
    paste(capture.output(print(changed_params[, c("param", "value_a", "value_b")],
                               row.names = FALSE)), collapse = "\n")
  } else {
    "(no parameter differences)"
  }

  kpi_table <- if (!is.null(kpi_delta) && is.data.frame(kpi_delta) && nrow(kpi_delta) > 0) {
    paste(capture.output(print(kpi_delta, row.names = FALSE)), collapse = "\n")
  } else {
    "(no KPI data available)"
  }

  user <- sprintf(
    "Policy A: %s\nPolicy B: %s\n\nChanged parameters:\n%s\n\nKPI deltas (B minus A):\n%s\n\nNarrate the comparison in 3-4 sentences.",
    policy_a_name, policy_b_name, param_table, kpi_table
  )

  result <- call_llm(system = system, user = user,
                     max_tokens = 250, temperature = 0.3)
  if (is.null(result) || !nzchar(result)) return(template)
  result
}

#' Root cause analysis for a risky segment.
#'
#' @param segment list with display_rule, bad_rate, mean_irr, n, lift
#' @param variable_distributions named list of top 5 variables with
#'        segment_mean, portfolio_mean, correlation
#' @param portfolio_context optional list with portfolio_bad, portfolio_irr
#' @return character(1) - NEVER NULL
root_cause_analysis <- function(segment, variable_distributions, portfolio_context = NULL) {
  if (is.null(segment)) return("")

  rule_txt  <- segment$display_rule %||% segment$rule %||% "(no rule)"
  bad_rate  <- segment$bad_rate %||% segment$loss %||% NA_real_
  mean_irr  <- segment$mean_irr %||% segment$irr %||% NA_real_
  n         <- segment$n %||% 0L
  lift      <- segment$lift %||% NA_real_

  .fmt_pct <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%.1f%%", x * 100)
  .fmt_num <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%.2f", x)

  # Identify top 2 variables by absolute difference between segment and portfolio
  var_names <- names(variable_distributions)
  if (length(var_names) == 0) {
    return(sprintf(
      "This segment (%s loans) has a loss rate of %s with lift of %s. Insufficient variable data for root cause analysis.",
      format(n, big.mark = ","), .fmt_pct(bad_rate),
      if (!is.na(lift)) sprintf("%.1fx", lift) else "n/a"
    ))
  }

  diffs <- vapply(var_names, function(v) {
    d <- variable_distributions[[v]]
    abs((d$segment_mean %||% 0) - (d$portfolio_mean %||% 0))
  }, numeric(1))
  top_idx <- order(diffs, decreasing = TRUE)
  top1 <- var_names[top_idx[1]]
  d1 <- variable_distributions[[top1]]
  top2_txt <- ""
  if (length(top_idx) >= 2) {
    top2 <- var_names[top_idx[2]]
    d2 <- variable_distributions[[top2]]
    top2_txt <- sprintf(" and %s (segment: %s vs portfolio: %s)",
                        gsub("_", " ", top2),
                        .fmt_num(d2$segment_mean), .fmt_num(d2$portfolio_mean))
  }

  template <- sprintf(
    "This segment's elevated risk is driven by %s (segment avg: %s vs portfolio avg: %s)%s.",
    gsub("_", " ", top1),
    .fmt_num(d1$segment_mean), .fmt_num(d1$portfolio_mean),
    top2_txt
  )

  if (!llm_available()) return(template)

  # Build variable distribution table for prompt
  var_table_lines <- vapply(var_names, function(v) {
    d <- variable_distributions[[v]]
    sprintf("  %s: segment_mean=%.4f, portfolio_mean=%.4f, correlation=%.3f",
            v, d$segment_mean %||% 0, d$portfolio_mean %||% 0, d$correlation %||% 0)
  }, character(1))

  system <- paste(
    "You are a credit-risk analyst explaining WHY a loan segment has elevated risk.",
    "Identify the 2-3 borrower attributes that most explain the problem.",
    "Cite specific numbers. 3-5 sentences. Plain English for a credit committee."
  )
  user <- sprintf(
    "Segment rule: %s\nLoans: %s\nLoss rate: %s\nLift: %s\nAvg IRR: %s\n\nVariable distributions (segment vs portfolio):\n%s\n\nExplain why this segment has elevated risk.",
    rule_txt, format(n, big.mark = ","),
    .fmt_pct(bad_rate),
    if (!is.na(lift)) sprintf("%.2fx", lift) else "n/a",
    .fmt_pct(mean_irr),
    paste(var_table_lines, collapse = "\n")
  )

  result <- call_llm(system = system, user = user,
                     max_tokens = 300, temperature = 0.3)
  if (is.null(result) || !nzchar(result)) return(template)
  result
}

#' Explain why a decline or reprice action is recommended.
#'
#' @param action_type "decline" or "reprice"
#' @param segment list with bad_rate, mean_irr, n, lift, display_rule, avg_apr
#' @param marginal_data optional boundary band data
#' @param portfolio_context optional list with portfolio_bad, portfolio_irr
#' @return character(1) - NEVER NULL
explain_action <- function(action_type, segment, marginal_data = NULL, portfolio_context = NULL) {
  if (is.null(segment)) return("")

  action_type <- tolower(action_type %||% "decline")
  rule_txt  <- segment$display_rule %||% segment$rule %||% "(no rule)"
  bad_rate  <- segment$bad_rate %||% segment$loss %||% NA_real_
  mean_irr  <- segment$mean_irr %||% segment$irr %||% NA_real_
  n         <- segment$n %||% 0L
  lift      <- segment$lift %||% NA_real_
  avg_apr   <- segment$avg_apr %||% NA_real_

  .fmt_pct <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%.1f%%", x * 100)
  .fmt_bps <- function(x) if (is.null(x) || is.na(x)) "n/a" else sprintf("%d bps", as.integer(round(x * 10000)))

  port_bad <- portfolio_context$portfolio_bad %||% NA_real_
  port_irr <- portfolio_context$portfolio_irr %||% NA_real_

  # Template fallback. Only mention IRR when it's known -- otherwise the
  # phrase "Segment IRR of n/a is below hurdle" shows up on small segments
  # where loan_irr is unavailable, which is confusing and unprofessional.
  if (action_type == "decline") {
    excess_bps <- if (!is.na(bad_rate) && !is.na(port_bad))
      sprintf(" (%s above portfolio)", .fmt_bps(bad_rate - port_bad)) else ""
    irr_clause <- if (!is.na(mean_irr))
      sprintf(" Segment IRR of %s is below hurdle.", .fmt_pct(mean_irr)) else ""
    template <- sprintf(
      "Decline recommended: loss rate %s%s on %s loans.%s",
      .fmt_pct(bad_rate), excess_bps,
      format(n, big.mark = ","), irr_clause
    )
  } else {
    if (!is.na(avg_apr)) {
      new_apr <- sprintf("%.1f%%", (avg_apr + 0.02) * 100)
      template <- sprintf(
        "Repricing recommended: segment IRR can be improved by adjusting APR from %s to %s.",
        .fmt_pct(avg_apr), new_apr
      )
    } else {
      template <- sprintf(
        "Repricing recommended on %s loans to improve risk-adjusted return.",
        format(n, big.mark = ","))
    }
  }

  if (!llm_available()) return(template)

  # Build marginal context if available
  marginal_txt <- ""
  if (!is.null(marginal_data) && !is.null(marginal_data$bands)) {
    bands <- marginal_data$bands
    if (is.data.frame(bands) && nrow(bands) > 0) {
      band_lines <- apply(bands, 1, function(row) {
        paste(names(row), "=", row, collapse = ", ")
      })
      marginal_txt <- paste0("\n\nBoundary/marginal band data:\n",
                             paste(head(band_lines, 5), collapse = "\n"))
    }
  }

  system <- paste(
    "You are a credit-risk analyst justifying a recommended action (decline or reprice)",
    "to a credit committee. Explain WHY this action is appropriate using the metrics.",
    "Reference boundary/marginal data if available. 2-3 sentences."
  )
  user <- sprintf(
    "Action: %s\nSegment rule: %s\nLoans: %s\nLoss rate: %s\nPortfolio loss rate: %s\nLift: %s\nAvg IRR: %s\nPortfolio IRR: %s\nAvg APR: %s%s\n\nExplain why this %s action is appropriate.",
    action_type, rule_txt, format(n, big.mark = ","),
    .fmt_pct(bad_rate), .fmt_pct(port_bad),
    if (!is.na(lift)) sprintf("%.2fx", lift) else "n/a",
    .fmt_pct(mean_irr), .fmt_pct(port_irr),
    .fmt_pct(avg_apr),
    marginal_txt,
    action_type
  )

  result <- call_llm(system = system, user = user,
                     max_tokens = 200, temperature = 0.3)
  if (is.null(result) || !nzchar(result)) return(template)
  result
}

#' Week 5 stub
summarize_report <- function(demo_state) {
  "Summary generation lands in Week 5."
}

# Deterministic keyword-based fallback parser for NL policy intent.
# Matches common phrasings to known parameter IDs so the feature degrades
# gracefully when the LLM is unavailable. Returns named list of overrides.
.nl_regex_parse_policy <- function(user_text, manifest_schema) {
  if (is.null(user_text) || !nzchar(user_text)) return(list())
  txt <- tolower(user_text)

  # Known param synonyms. Keys match manifest param IDs.
  synonyms <- list(
    fico_min           = c("fico", "credit score"),
    thin_file_months   = c("thin file", "thinfile", "credit history", "file age"),
    n_hard_inq_6m_max  = c("hard inquir", "inquir", "hardinq", "hard inq"),
    delinquency_max    = c("delinquen", "past due"),
    dti_cutoff         = c("dti", "debt to income", "debt-to-income"),
    revol_utl_max      = c("revol", "utilization", "utilisation", "utl"),
    apr_high           = c("apr high", "apr-h", "high apr"),
    apr_med            = c("apr med", "apr-m", "med apr"),
    apr_low            = c("apr low", "apr-l", "low apr")
  )

  # Extract first number in the text (percent suffix stripped later as needed)
  num_match <- regmatches(txt, regexpr("-?\\d+(?:\\.\\d+)?", txt))
  if (length(num_match) == 0) return(list())
  num <- suppressWarnings(as.numeric(num_match[[1]]))
  if (!is.finite(num)) return(list())

  # Find which param the text mentions
  target <- NULL
  for (pid in names(synonyms)) {
    if (any(vapply(synonyms[[pid]], function(kw) grepl(kw, txt, fixed = TRUE),
                   logical(1)))) {
      target <- pid
      break
    }
  }
  if (is.null(target)) return(list())

  # APR/DTI/utilization expressed as percent in text → convert to decimal
  pct_params <- c("dti_cutoff", "revol_utl_max",
                  "apr_high", "apr_med", "apr_low")
  if (target %in% pct_params && num > 1) num <- num / 100

  # Clamp to schema bounds when available
  for (s in manifest_schema) {
    if (identical(s$id, target)) {
      if (!is.null(s$min) && is.finite(suppressWarnings(as.numeric(s$min))))
        num <- max(as.numeric(s$min), num)
      if (!is.null(s$max) && is.finite(suppressWarnings(as.numeric(s$max))))
        num <- min(as.numeric(s$max), num)
      break
    }
  }
  out <- list()
  out[[target]] <- num
  out
}

# ---- UC2: Natural Language -> Policy Parameter Parsing ----------------------

#' Parse a natural language policy instruction into parameter overrides.
#'
#' @param user_text character(1) — free-form instruction (e.g. "tighten FICO to 660")
#' @param current_params named list of current slider values
#' @param manifest_schema simplified list of param metadata (id, label, min, max, current)
#' @return named list of parameter overrides, or list() on failure (with attr "message")
parse_policy_intent <- function(user_text, current_params, manifest_schema) {
  # Deterministic fallback: empty list with specific, user-friendly message
  fallback <- list()
  attr(fallback, "message") <- paste0(
    "I couldn't parse that. Try phrasing like 'decline FICO below 640', ",
    "'tighten DTI to 40', or 'set hard inquiries max to 3' — or use the sliders."
  )

  if (is.null(user_text) || !nzchar(trimws(user_text))) return(fallback)

  # Deterministic keyword → param regex fallback.
  # Runs BEFORE the LLM so it works offline and when the AI endpoint is down.
  # Covers the phrasings suggested in the hint so users never see a bare
  # parse error for the canned examples.
  regex_result <- .nl_regex_parse_policy(user_text, manifest_schema)
  if (length(regex_result) > 0) return(regex_result)

  if (!llm_available()) {
    attr(fallback, "message") <- paste0(
      "AI assistant isn't connected, and that phrasing didn't match any known ",
      "patterns. Try 'FICO min 640', 'DTI max 40', 'hard inquiries max 3', ",
      "or use the sliders."
    )
    return(fallback)
  }

  schema_json <- tryCatch(
    jsonlite::toJSON(manifest_schema, auto_unbox = TRUE, pretty = FALSE),
    error = function(e) "[]"
  )
  current_json <- tryCatch(
    jsonlite::toJSON(current_params, auto_unbox = TRUE, pretty = FALSE),
    error = function(e) "{}"
  )

  system <- paste0(
    "You are a credit policy parameter parser. Given a natural language instruction, ",
    "map it to parameter overrides. ",
    "Available parameters: ", schema_json, ". ",
    "Current values: ", current_json, ". ",
    "Return ONLY a raw JSON object mapping parameter IDs to new numeric values. ",
    "No markdown, no code fences, no prose, no explanation — just the JSON object."
  )
  user <- user_text

  raw <- call_llm(system = system, user = user,
                  max_tokens = 300, temperature = 0.1)
  if (is.null(raw) || !nzchar(raw)) return(fallback)

  # Strip common markdown code fences (```json ... ``` or ``` ... ```) and trim
  cleaned <- tryCatch({
    x <- trimws(raw)
    # Remove leading fence like ```json or ```
    x <- sub("^\\s*```[a-zA-Z0-9_-]*\\s*\\n?", "", x)
    # Remove trailing fence
    x <- sub("\\n?\\s*```\\s*$", "", x)
    trimws(x)
  }, error = function(e) raw)

  # Parse JSON response
  parsed <- tryCatch({
    jsonlite::fromJSON(cleaned, simplifyVector = TRUE)
  }, error = function(e) NULL)

  if (is.null(parsed) || !is.list(parsed) || length(parsed) == 0) return(fallback)

  # Build lookup of valid param IDs -> min/max from schema
  valid_ids <- list()
  for (s in manifest_schema) {
    valid_ids[[s$id]] <- list(min = s$min, max = s$max)
  }

  # Validate each returned param (coerce safely; skip anything weird)
  result <- list()
  for (nm in names(parsed)) {
    if (!(nm %in% names(valid_ids))) next
    val <- tryCatch(suppressWarnings(as.numeric(parsed[[nm]])[1]),
                    error = function(e) NA_real_)
    if (length(val) != 1 || is.na(val) || !is.finite(val)) next
    bounds <- valid_ids[[nm]]
    b_min <- suppressWarnings(as.numeric(bounds$min))
    b_max <- suppressWarnings(as.numeric(bounds$max))
    if (length(b_min) == 1 && is.finite(b_min)) val <- max(b_min, val)
    if (length(b_max) == 1 && is.finite(b_max)) val <- min(b_max, val)
    result[[nm]] <- val
  }

  if (length(result) == 0) return(fallback)
  result
}

# ---- UC5: Natural Language Filter Query Parsing -----------------------------

#' Parse a natural language filter query into filter settings.
#'
#' @param user_text character(1) — free-form query (e.g. "Show me high-loss segments in Direct channel")
#' @param available_filters list describing available filter dimensions and their allowed values
#' @return named list of filter settings, or list() on failure (with attr "message")
parse_filter_query <- function(user_text, available_filters) {
  # Deterministic fallback: empty list with specific, user-friendly message
  fallback <- list()
  attr(fallback, "message") <- paste0(
    "I couldn't parse that. Try phrasing like 'show high FICO repeat borrowers' ",
    "or 'Direct channel, low grade' — or use the filter dropdowns."
  )

  if (is.null(user_text) || !nzchar(trimws(user_text))) return(fallback)

  if (!llm_available()) return(fallback)

  schema_json <- tryCatch(
    jsonlite::toJSON(available_filters, auto_unbox = TRUE, pretty = FALSE),
    error = function(e) "[]"
  )

  system <- paste0(
    "You are a filter parser for a credit risk tool. Given a natural language query, ",
    "extract filter settings. Available filters: ", schema_json, ". ",
    "Return ONLY a raw JSON object — no markdown, no code fences, no prose. ",
    "Keys are filter IDs, values are arrays of selected options. ",
    "Only include filters that the user mentions. Do not include filters set to 'All' or 'all'."
  )
  user <- user_text

  raw <- call_llm(system = system, user = user,
                  max_tokens = 300, temperature = 0.1)
  if (is.null(raw) || !nzchar(raw)) return(fallback)

  # Strip common markdown code fences (```json ... ``` or ``` ... ```) and trim
  cleaned <- tryCatch({
    x <- trimws(raw)
    x <- sub("^\\s*```[a-zA-Z0-9_-]*\\s*\\n?", "", x)
    x <- sub("\\n?\\s*```\\s*$", "", x)
    trimws(x)
  }, error = function(e) raw)

  # Parse JSON response
  parsed <- tryCatch({
    jsonlite::fromJSON(cleaned, simplifyVector = FALSE)
  }, error = function(e) NULL)

  if (is.null(parsed) || !is.list(parsed) || length(parsed) == 0) return(fallback)

  # Build lookup of valid filter IDs -> allowed values
  valid_filters <- list()
  for (f in available_filters) {
    valid_filters[[f$id]] <- f$values
  }

  # Validate each returned filter (coerce safely; skip anything weird)
  result <- list()
  for (nm in names(parsed)) {
    if (!(nm %in% names(valid_filters))) next
    vals <- tryCatch(unlist(parsed[[nm]], use.names = FALSE),
                     error = function(e) NULL)
    if (is.null(vals) || length(vals) == 0) next
    vals <- as.character(vals)
    # Keep only values that are actually available
    vals <- vals[vals %in% as.character(valid_filters[[nm]])]
    if (length(vals) > 0) {
      result[[nm]] <- vals
    }
  }

  if (length(result) == 0) return(fallback)
  result
}

# ── UC6: Policy Comparison Narrative ──────────────────────────
narrate_comparison <- function(policy_a_name, policy_b_name,
                                param_diff = NULL, kpi_delta = NULL) {
  # ---- Deterministic template fallback ----
  n_changed <- if (!is.null(param_diff) && is.data.frame(param_diff))
    sum(param_diff$changed %||% rep(TRUE, nrow(param_diff)), na.rm = TRUE) else 0
  changed_list <- if (n_changed > 0 && is.data.frame(param_diff)) {
    rows <- param_diff[param_diff$changed %||% TRUE, , drop = FALSE]
    paste(rows$param, collapse = ", ")
  } else "none"

  kpi_lines <- ""
  if (!is.null(kpi_delta) && is.list(kpi_delta)) {
    parts <- vapply(names(kpi_delta), function(k) {
      v <- kpi_delta[[k]]
      if (is.numeric(v)) sprintf("%s %+.2f", k, v) else paste(k, v)
    }, character(1))
    kpi_lines <- paste(parts, collapse = "; ")
  }

  template <- sprintf(
    "Policy %s differs from %s in %d parameters: %s. Key impacts: %s.",
    policy_b_name, policy_a_name, n_changed, changed_list, kpi_lines
  )

  # ---- LLM enhancement ----
  if (!llm_available()) return(template)

  sys <- paste(
    "You are a credit-risk analyst narrating the differences between two",
    "credit policies for a credit committee. Explain what changed, why it",
    "matters, and the likely business impact. 3-4 sentences. Plain English."
  )
  usr <- sprintf(
    "Policy A: %s\nPolicy B: %s\n\nChanged parameters (%d):\n%s\n\nKPI deltas:\n%s",
    policy_a_name, policy_b_name, n_changed, changed_list, kpi_lines
  )

  llm_out <- call_llm(system = sys, user = usr,
                       max_tokens = 250, temperature = 0.3)
  if (is.null(llm_out)) return(template)
  llm_out
}
