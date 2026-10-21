# ============================================================
# R/services/govern_service.R
#
# Policy governance service — feeds Catalog and History tabs.
# Pure functions, no Shiny imports.
#
# Week 2: list_policies, policy_performance, verdict_for, activate_policy.
# Week 5: policy_timeline, policy_lineage_tree, compare_policies,
#         experiment_results, save_as_variant, generate_sql, export_report.
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---- list_policies ----------------------------------------------------------
#' Display-ready data frame of all policies, filtered by product.
#'
#' @param strategy_overrides The loaded manifest list
#' @param product Filter ("Personal Loan", "Top-Up", or "all")
#' @param status_filter Optional: "all", "active", "simulated", "test", "retired"
#' @return data.frame ordered by effective_from desc
list_policies <- function(strategy_overrides,
                          product = "all",
                          status_filter = "all") {
  if (length(strategy_overrides) == 0) {
    return(data.frame(
      policy_id = character(0), name = character(0),
      product = character(0), status = character(0),
      effective_from = as.Date(character(0)),
      effective_to = as.Date(character(0)),
      parent_policy = character(0),
      key_changes = character(0),
      is_test = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(names(strategy_overrides), function(pid) {
    s <- strategy_overrides[[pid]]
    is_test <- !is.null(s$test_control)
    # Derive "display status" — test variants show as "test" even if underlying status differs
    disp_status <- if (is_test && (s$status %||% "") != "simulated") "test" else (s$status %||% "unknown")

    data.frame(
      policy_id      = pid,
      name           = s$name %||% pid,
      product        = s$product %||% "Unknown",
      status         = disp_status,
      effective_from = suppressWarnings(as.Date(s$effective_from %||% NA)),
      effective_to   = suppressWarnings(as.Date(s$effective_to %||% NA)),
      parent_policy  = s$parent_policy %||% NA_character_,
      key_changes    = .summarize_params(s$params %||% list()),
      is_test        = is_test,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)

  if (!identical(product, "all")) df <- df[df$product == product, , drop = FALSE]
  if (!identical(status_filter, "all")) df <- df[df$status == status_filter, , drop = FALSE]

  df <- df[order(df$effective_from, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(df) <- NULL
  df
}

.summarize_params <- function(params) {
  if (length(params) == 0) return("Baseline")
  parts <- character(0)
  fmt <- list(
    fico_min           = function(v) paste0("FICO >= ", v),
    thin_file_months   = function(v) paste0("ThinFile ", v, "mo"),
    n_hard_inq_6m_max  = function(v) paste0("HardInq max ", v),
    delinquency_max    = function(v) paste0("Delinq max ", v),
    apr_high           = function(v) paste0("APR-H ", round(v * 100, 1), "%"),
    apr_med            = function(v) paste0("APR-M ", round(v * 100, 1), "%"),
    apr_low            = function(v) paste0("APR-L ", round(v * 100, 1), "%"),
    pti_low            = function(v) paste0("PTI-L ", round(v * 100, 0), "%"),
    exposure_cap_high  = function(v) paste0("Cap $", formatC(v, format = "d", big.mark = ",")),
    exposure_cap_other = function(v) paste0("Cap $", formatC(v, format = "d", big.mark = ",")),
    revol_utl_max      = function(v) paste0("Revol UTL max ", round(v * 100, 0), "%")
  )
  for (nm in names(params)) {
    fn <- fmt[[nm]]
    if (!is.null(fn)) parts <- c(parts, fn(params[[nm]]))
  }
  if (length(parts) == 0) return("No param changes")
  paste(parts, collapse = ", ")
}

# ---- policy_performance -----------------------------------------------------
#' KPIs scoped to a single policy_id. Wraps portfolio_kpis.
#'
#' @param policy_id   The policy to scope to
#' @param loan_data   Full loan data frame
#' @param perf_tape   Performance tape
#' @param strategy_overrides Optional: for simulated-policy verdict/result lookup
#' @return named list with same fields as portfolio_kpis + policy_id + is_simulated
policy_performance <- function(policy_id, loan_data, perf_tape = NULL,
                               strategy_overrides = NULL) {
  if (is.null(policy_id) || !nzchar(policy_id)) return(NULL)

  # If the policy is simulated (data_backed = FALSE), we can't compute real
  # data-backed metrics. Fall back to the simulation_result block the manifest
  # carries, if present.
  if (!is.null(strategy_overrides) && policy_id %in% names(strategy_overrides)) {
    s <- strategy_overrides[[policy_id]]
    if (!is.null(s$data_backed) && isFALSE(s$data_backed) &&
        !is.null(s$simulation_result)) {
      sr <- s$simulation_result
      return(list(
        policy_id      = policy_id,
        n_loans        = NA_integer_,
        approval_rate  = NA_real_,
        portfolio_irr  = NA_real_,
        avg_loss_rate  = NA_real_,
        at_risk_usd    = NA_real_,
        policies_count = 1L,
        is_simulated   = TRUE,
        simulation     = sr
      ))
    }
  }

  product <- if (!is.null(strategy_overrides) && policy_id %in% names(strategy_overrides)) {
    strategy_overrides[[policy_id]]$product %||% "Personal Loan"
  } else "Personal Loan"

  k <- portfolio_kpis(loan_data, perf_tape, product = product, policy_id = policy_id)
  k$policy_id    <- policy_id
  k$is_simulated <- FALSE
  k$simulation   <- NULL
  k
}

# ---- verdict_for (Discovery 2, first half) ----------------------------------
#' Return the editorial verdict string for a simulated policy, or NULL.
#'
#' @param policy_id The policy to look up
#' @param strategy_overrides The manifest list
#' @return character(1) or NULL
verdict_for <- function(policy_id, strategy_overrides) {
  if (is.null(policy_id) || length(strategy_overrides) == 0) return(NULL)
  s <- strategy_overrides[[policy_id]]
  if (is.null(s)) return(NULL)
  sr <- s$simulation_result
  if (is.null(sr) || is.null(sr$verdict)) return(NULL)
  as.character(sr$verdict)
}

# ---- activate_policy --------------------------------------------------------
#' No-op in Week 2 — the tab server updates the active_policy reactive
#' directly. Here just for API symmetry and future persistence.
#'
#' @return logical(1) — always TRUE in Week 2
activate_policy <- function(policy_id) {
  invisible(TRUE)
}

# ============================================================================
# Week 5 additions — timeline, lineage, comparison, SQL, experiments
# ============================================================================

# ---- policy_timeline --------------------------------------------------------
#' Flat timeline view — sortable data.frame, one row per policy.
#'
#' @param strategy_overrides The loaded manifest list
#' @param product Optional product filter ("Personal Loan", "Top-Up", NULL=all)
#' @return data.frame sorted by effective_from ascending
policy_timeline <- function(strategy_overrides, product = NULL) {
  if (length(strategy_overrides) == 0) {
    return(data.frame(
      policy_id = character(0), name = character(0), product = character(0),
      effective_from = as.Date(character(0)), effective_to = as.Date(character(0)),
      status = character(0), parent_policy = character(0),
      data_backed = logical(0), test_control_group = character(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(names(strategy_overrides), function(pid) {
    s <- strategy_overrides[[pid]]
    tc_group <- if (!is.null(s$test_control) && !is.null(s$test_control$group)) {
      s$test_control$group
    } else NA_character_

    data.frame(
      policy_id = pid,
      name = s$name %||% pid,
      product = s$product %||% "Unknown",
      effective_from = suppressWarnings(as.Date(s$effective_from %||% NA)),
      effective_to = suppressWarnings(as.Date(s$effective_to %||% NA)),
      status = s$status %||% "unknown",
      parent_policy = s$parent_policy %||% NA_character_,
      data_backed = isTRUE(s$data_backed),
      test_control_group = tc_group,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)

  if (!is.null(product)) df <- df[df$product == product, , drop = FALSE]
  df <- df[order(df$effective_from, na.last = TRUE), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# ---- policy_lineage_tree ----------------------------------------------------
#' Lineage tree as nodes + edges for visNetwork or DiagrammeR.
#'
#' Discovery 2: the rich policy lineage in strategy_manifest.R that was
#' previously only shown as a flat table.
#'
#' @param strategy_overrides The loaded manifest list
#' @param product Optional product filter
#' @return list(nodes, edges) — both data.frames
policy_lineage_tree <- function(strategy_overrides, product = NULL) {
  if (length(strategy_overrides) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }

  filtered <- if (!is.null(product)) {
    strategy_overrides[vapply(strategy_overrides,
                              function(s) identical(s$product %||% "", product),
                              logical(1))]
  } else strategy_overrides

  if (length(filtered) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }

  # Derive levels via BFS from roots (parent_policy = NULL or not in set)
  pids <- names(filtered)
  has_parent <- vapply(filtered, function(s) !is.null(s$parent_policy), logical(1))
  roots <- pids[!has_parent | !(vapply(filtered, function(s) s$parent_policy %||% "", character(1)) %in% pids)]

  levels <- setNames(rep(NA_integer_, length(pids)), pids)
  for (r in roots) levels[[r]] <- 0L
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (pid in pids) {
      if (!is.na(levels[[pid]])) next
      parent <- filtered[[pid]]$parent_policy %||% ""
      if (nzchar(parent) && parent %in% pids && !is.na(levels[[parent]])) {
        levels[[pid]] <- levels[[parent]] + 1L
        changed <- TRUE
      }
    }
  }
  # Orphans (cycle or dangling parent): assign level 0
  levels[is.na(levels)] <- 0L

  .status_group <- function(s) {
    if (!is.null(s$test_control) && !is.null(s$test_control$group)) {
      return(s$test_control$group)
    }
    s$status %||% "unknown"
  }

  .tooltip <- function(pid, s) {
    params_str <- if (length(s$params %||% list()) > 0) {
      paste(names(s$params), unlist(s$params), sep = "=", collapse = ", ")
    } else "(baseline)"
    paste0(
      "<b>", s$name %||% pid, "</b><br>",
      "<em>", s$status %||% "", "</em><br>",
      s$effective_from %||% "?", " \u2192 ", s$effective_to %||% "?", "<br>",
      "<small>", params_str, "</small>"
    )
  }

  nodes <- do.call(rbind, lapply(pids, function(pid) {
    s <- filtered[[pid]]
    data.frame(
      id = pid,
      label = pid,
      name = s$name %||% pid,
      group = .status_group(s),
      title = .tooltip(pid, s),
      level = levels[[pid]],
      data_backed = isTRUE(s$data_backed),
      stringsAsFactors = FALSE
    )
  }))

  edge_rows <- list()
  for (pid in pids) {
    parent <- filtered[[pid]]$parent_policy %||% ""
    if (!nzchar(parent) || !(parent %in% pids)) next
    is_test <- !is.null(filtered[[pid]]$test_control)
    edge_rows[[length(edge_rows) + 1]] <- data.frame(
      from = parent, to = pid,
      dashes = is_test,
      stringsAsFactors = FALSE
    )
  }
  edges <- if (length(edge_rows) > 0) do.call(rbind, edge_rows) else
    data.frame(from = character(0), to = character(0), dashes = logical(0))

  list(nodes = nodes, edges = edges)
}

# ---- compare_policies -------------------------------------------------------
#' Side-by-side diff of two policies.
#'
#' @param id_a,id_b Policy IDs
#' @param strategy_overrides Manifest list
#' @param loan_data Optional — if provided, KPI delta is computed
#' @param perf_tape Optional
#' @return list(param_diff, kpis_a, kpis_b, kpi_delta, a_name, b_name)
compare_policies <- function(id_a, id_b, strategy_overrides,
                             loan_data = NULL, perf_tape = NULL) {
  if (is.null(id_a) || is.null(id_b)) return(NULL)
  sa <- strategy_overrides[[id_a]]
  sb <- strategy_overrides[[id_b]]
  if (is.null(sa) || is.null(sb)) return(NULL)

  params_a <- sa$params %||% list()
  params_b <- sb$params %||% list()
  all_keys <- unique(c(names(params_a), names(params_b)))

  param_diff <- if (length(all_keys) == 0) {
    data.frame(param = character(0), value_a = character(0),
               value_b = character(0), changed = logical(0),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, lapply(all_keys, function(k) {
      va <- params_a[[k]]
      vb <- params_b[[k]]
      data.frame(
        param = k,
        value_a = if (is.null(va)) "(default)" else as.character(va),
        value_b = if (is.null(vb)) "(default)" else as.character(vb),
        changed = !identical(va, vb),
        stringsAsFactors = FALSE
      )
    }))
  }

  kpis_a <- if (!is.null(loan_data)) {
    policy_performance(id_a, loan_data, perf_tape, strategy_overrides)
  } else NULL
  kpis_b <- if (!is.null(loan_data)) {
    policy_performance(id_b, loan_data, perf_tape, strategy_overrides)
  } else NULL

  kpi_delta <- if (!is.null(kpis_a) && !is.null(kpis_b)) {
    metrics <- c("portfolio_irr", "approval_rate", "avg_loss_rate", "at_risk_usd", "n_loans")
    do.call(rbind, lapply(metrics, function(m) {
      va <- kpis_a[[m]]; vb <- kpis_b[[m]]
      delta <- if (is.null(va) || is.null(vb) || is.na(va) || is.na(vb)) NA_real_
               else vb - va
      data.frame(
        metric = m,
        a = if (is.null(va)) NA_real_ else va,
        b = if (is.null(vb)) NA_real_ else vb,
        delta = delta,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(metric = character(0), a = numeric(0), b = numeric(0),
               delta = numeric(0), stringsAsFactors = FALSE)
  }

  list(
    a_id = id_a, b_id = id_b,
    a_name = sa$name %||% id_a, b_name = sb$name %||% id_b,
    param_diff = param_diff,
    kpis_a = kpis_a, kpis_b = kpis_b,
    kpi_delta = kpi_delta
  )
}

# ---- generate_sql -----------------------------------------------------------
#' Generate the L10-L90 CTE waterfall SQL for a given policy.
#'
#' Discovery 1: surfaces the 725-line sql_policy_generator.R engine that
#' was previously dead code in the repo.
#'
#' @param policy_id The policy to translate
#' @param strategy_overrides The manifest list
#' @return character(1) — the full SQL string
generate_sql <- function(policy_id, strategy_overrides) {
  if (is.null(policy_id)) return("")
  s <- strategy_overrides[[policy_id]]
  if (is.null(s)) return(paste("-- Policy not found:", policy_id))

  # Merge manifest params on top of defaults
  if (!exists("build_default_params") || !exists("policy_manifest")) {
    return("-- build_default_params / policy_manifest not loaded")
  }
  params <- build_default_params(policy_manifest)
  if (length(s$params %||% list()) > 0) {
    params <- modifyList(params, s$params)
  }

  label <- paste0(policy_id, " \u2014 ", s$name %||% "")
  generate_policy_sql(params, label = label)
}

# ---- experiment_results -----------------------------------------------------
#' Test/control cells from the manifest — just structured metadata, no
#' performance math (that requires per-cell loan tagging which is out of
#' scope for the demo).
#'
#' @param strategy_overrides Manifest list
#' @param product Optional product filter
#' @return data.frame — one row per policy with non-NULL test_control
experiment_results <- function(strategy_overrides, product = NULL) {
  if (length(strategy_overrides) == 0) return(.empty_experiments())

  rows <- list()
  for (pid in names(strategy_overrides)) {
    s <- strategy_overrides[[pid]]
    if (is.null(s$test_control)) next
    if (!is.null(product) && !identical(s$product %||% "", product)) next
    tc <- s$test_control
    rows[[length(rows) + 1]] <- data.frame(
      policy_id = pid,
      name = s$name %||% pid,
      product = s$product %||% "Unknown",
      parent_policy = s$parent_policy %||% NA_character_,
      cell = tc$group %||% "unknown",
      assignment = tc$assignment %||% NA_character_,
      target_segment = tc$target_segment %||% NA_character_,
      pricing_adjustment_bps = tc$pricing_adjustment_bps %||% 0L,
      status = s$status %||% "unknown",
      effective_from = as.character(s$effective_from %||% ""),
      effective_to = as.character(s$effective_to %||% ""),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return(.empty_experiments())
  df <- do.call(rbind, rows)
  rownames(df) <- NULL
  df
}

.empty_experiments <- function() {
  data.frame(
    policy_id = character(0), name = character(0), product = character(0),
    parent_policy = character(0), cell = character(0),
    assignment = character(0), target_segment = character(0),
    pricing_adjustment_bps = integer(0), status = character(0),
    effective_from = character(0), effective_to = character(0),
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# Functions displaced from 03_globals_policies.R
# ============================================================================

# ---- Product name -> short code mapping ------------------------------------
.product_short_code <- c(
  "Personal Loan"   = "PL",
  "Auto Loan"       = "AL",
  "Credit Card"     = "CC",
  "Top-Up"          = "TU",
  "Home Equity"     = "HE",
  "Project Finance"  = "PF",
  "Mortgage"        = "MTG"
)

# ---- Policy description helpers (private) ----------------------------------
.describe_grading <- function(params) {
  .bd <- tryCatch(build_default_params(policy_manifest), error = function(e) list())
  p <- modifyList(.bd, params)
  thin <- p$thin_file_months %||% 30
  inq  <- p$n_hard_inq_6m_max
  if (is.null(inq)) {
    paste0("Standard model waterfall. ThinFile<=", thin, "mo")
  } else {
    paste0("Standard model waterfall. ThinFile<=", thin, "mo; HardInq6M max=", inq)
  }
}

.describe_pricing <- function(params) {
  .bd <- tryCatch(build_default_params(policy_manifest), error = function(e) list())
  p <- modifyList(.bd, params)
  apr_h <- round((p$apr_high %||% 0.07) * 100, 1)
  apr_m <- round((p$apr_med  %||% 0.12) * 100, 1)
  apr_l <- round((p$apr_low  %||% 0.16) * 100, 1)
  paste0("High: ", apr_h, "%, Med: ", apr_m, "%, Low: ", apr_l, "%")
}

.describe_decision <- function(params) {
  .bd <- tryCatch(build_default_params(policy_manifest), error = function(e) list())
  p <- modifyList(.bd, params)
  thin   <- p$thin_file_months %||% 30
  delinq <- p$delinquency_max  %||% 1
  dti    <- round((p$dti_max %||% 0.50) * 100, 0)
  fico   <- p$fico_min %||% 300
  parts  <- paste0("FICO>=", fico, "; ThinFile<=", thin, "mo; Delinq>", delinq, "; DTI>", dti, "%")
  paste0("Decline: ", parts)
}

.describe_term <- function(params) {
  "High-grade: up to 72mo. Med/Low: up to 60mo"
}

.describe_affordability <- function(params) {
  .bd <- tryCatch(build_default_params(policy_manifest), error = function(e) list())
  p <- modifyList(.bd, params)
  pti_l <- round((p$pti_low %||% 0.15) * 100, 0)
  paste0("PTI cap: ", pti_l, "% (Low grade). Standard for others")
}

.describe_max_amount <- function(params) {
  .bd <- tryCatch(build_default_params(policy_manifest), error = function(e) list())
  p <- modifyList(.bd, params)
  cap_h <- p$exposure_cap_high  %||% 50000
  cap_o <- p$exposure_cap_other %||% 40000
  paste0("Exposure cap: $", formatC(cap_h, format="d", big.mark=","),
         "/$", formatC(cap_o, format="d", big.mark=","))
}

# ---- build_mock_policies ---------------------------------------------------
#' Build the mockPolicies data.frame from strategy_overrides.
#'
#' @param strategy_overrides The loaded manifest list
#' @return data.frame with one row per policy
build_mock_policies <- function(strategy_overrides) {
  if (length(strategy_overrides) == 0) {
    return(data.frame(
      id = integer(0), policyId = character(0), policyName = character(0),
      status = character(0), product = character(0), version = character(0),
      updatedAt = as.Date(character(0)), endDate = as.Date(character(0)),
      grading_logic = character(0), pricing_logic = character(0),
      decision_logic = character(0), term_logic = character(0),
      affordability_logic = character(0), max_amount_logic = character(0),
      r_logic = character(0), effective_from = as.Date(character(0)),
      effective_to = as.Date(character(0)), parent_policy = character(0),
      data_backed = logical(0), test_group = character(0),
      stringsAsFactors = FALSE
    ))
  }

  .policy_ids <- names(strategy_overrides)
  .n_policies <- length(.policy_ids)

  df <- data.frame(
    id         = seq_len(.n_policies),
    policyId   = .policy_ids,
    policyName = vapply(.policy_ids, function(pid) {
      strategy_overrides[[pid]]$name %||% pid
    }, character(1), USE.NAMES = FALSE),
    status = vapply(.policy_ids, function(pid) {
      strategy_overrides[[pid]]$status %||% "simulated"
    }, character(1), USE.NAMES = FALSE),
    product = vapply(.policy_ids, function(pid) {
      full <- strategy_overrides[[pid]]$product %||% "Personal Loan"
      unname(.product_short_code[full] %||% substr(full, 1, 2))
    }, character(1), USE.NAMES = FALSE),
    version = vapply(.policy_ids, function(pid) {
      gsub("^(PL|TU|AL|CC)-", "", pid)
    }, character(1), USE.NAMES = FALSE),
    updatedAt = as.Date(vapply(.policy_ids, function(pid) {
      ef <- strategy_overrides[[pid]]$effective_from
      if (is.null(ef) || is.na(ef)) as.character(Sys.Date()) else as.character(ef)
    }, character(1), USE.NAMES = FALSE)),
    endDate = as.Date(vapply(.policy_ids, function(pid) {
      et <- strategy_overrides[[pid]]$effective_to
      if (is.null(et) || is.na(et)) NA_character_ else as.character(et)
    }, character(1), USE.NAMES = FALSE)),
    grading_logic = vapply(.policy_ids, function(pid) {
      .describe_grading(strategy_overrides[[pid]]$params)
    }, character(1), USE.NAMES = FALSE),
    pricing_logic = vapply(.policy_ids, function(pid) {
      .describe_pricing(strategy_overrides[[pid]]$params)
    }, character(1), USE.NAMES = FALSE),
    decision_logic = vapply(.policy_ids, function(pid) {
      .describe_decision(strategy_overrides[[pid]]$params)
    }, character(1), USE.NAMES = FALSE),
    term_logic = vapply(.policy_ids, function(pid) {
      .describe_term(strategy_overrides[[pid]]$params)
    }, character(1), USE.NAMES = FALSE),
    affordability_logic = vapply(.policy_ids, function(pid) {
      .describe_affordability(strategy_overrides[[pid]]$params)
    }, character(1), USE.NAMES = FALSE),
    max_amount_logic = vapply(.policy_ids, function(pid) {
      .describe_max_amount(strategy_overrides[[pid]]$params)
    }, character(1), USE.NAMES = FALSE),
    r_logic = NA_character_,
    effective_from = as.Date(vapply(.policy_ids, function(pid) {
      ef <- strategy_overrides[[pid]]$effective_from
      if (is.null(ef) || is.na(ef)) NA_character_ else as.character(ef)
    }, character(1), USE.NAMES = FALSE)),
    effective_to = as.Date(vapply(.policy_ids, function(pid) {
      et <- strategy_overrides[[pid]]$effective_to
      if (is.null(et) || is.na(et)) NA_character_ else as.character(et)
    }, character(1), USE.NAMES = FALSE)),
    parent_policy = vapply(.policy_ids, function(pid) {
      pp <- strategy_overrides[[pid]]$parent_policy
      if (is.null(pp)) NA_character_ else as.character(pp)
    }, character(1), USE.NAMES = FALSE),
    data_backed = vapply(.policy_ids, function(pid) {
      db <- strategy_overrides[[pid]]$data_backed
      if (is.null(db)) FALSE else as.logical(db)
    }, logical(1), USE.NAMES = FALSE),
    test_group = vapply(.policy_ids, function(pid) {
      tc <- strategy_overrides[[pid]]$test_control
      if (is.null(tc)) NA_character_ else as.character(tc$group %||% NA_character_)
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  df$product <- as.character(df$product)
  df
}

# ---- build_base_loans ------------------------------------------------------
#' Build the base_loans data.frame from credit_data using legacy aliases.
#'
#' @param credit_data The canonical credit_data data.frame
#' @return data.frame with legacy column names + vintage_date
build_base_loans <- function(credit_data) {
  bl <- rename_to_old(credit_data)
  bl$vintage_date <- bl$date
  bl
}

# ---- build_vintage_data ----------------------------------------------------
#' Build synthetic vintage performance data for cumulative charts.
#'
#' @param base_loans The base_loans data.frame (from build_base_loans)
#' @param mockPolicies The mockPolicies data.frame (unused but kept for API symmetry)
#' @return data.frame(loan_id, month_on_books, vintage_date, delinquency_flag)
build_vintage_data <- function(base_loans, mockPolicies = NULL) {
  set.seed(42)
  .vd <- base_loans[!is.na(base_loans$date) & !is.na(base_loans$loan_id), ]
  if (nrow(.vd) > 2500) .vd <- .vd[sample(nrow(.vd), 2500), ]
  .vd$mob_max  <- pmin(12L, pmax(1L, as.integer(.vd$month_on_books)))
  .vd$mob_max[is.na(.vd$mob_max)] <- 6L
  .vd$vdate    <- format(.vd$date, "%d/%m/%Y")
  .vd$dflag    <- as.integer(.vd$delinquency_flag == 1)
  .vd$dflag[is.na(.vd$dflag)] <- 0L
  result <- data.frame(
    loan_id        = rep(as.character(.vd$loan_id), .vd$mob_max + 1L),
    month_on_books = unlist(lapply(.vd$mob_max, function(m) 0L:m)),
    vintage_date   = rep(.vd$vdate, .vd$mob_max + 1L),
    delinquency_flag = unlist(lapply(
      seq_len(nrow(.vd)),
      function(i) c(rep(0L, .vd$mob_max[i]), .vd$dflag[i])
    )),
    stringsAsFactors = FALSE
  )
  set.seed(NULL)
  result
}

# ---- load_persisted_policies -----------------------------------------------
#' Load saved policies from disk and merge persisted closures into policy_fns.
#'
#' @param strategy_overrides The loaded manifest list
#' @param policy_fns Named list of policy closures
#' @return list(persisted_policies, persisted_fns, policy_fns)
load_persisted_policies <- function(strategy_overrides, policy_fns) {
  saved_policies_path <- "inst/data/saved_policies.rds"
  persisted_policies  <- NULL
  persisted_fns       <- list()

  if (file.exists(saved_policies_path)) {
    message("Loading persisted policies from ", saved_policies_path, "...")
    tryCatch({
      saved_store <- readRDS(saved_policies_path)
      persisted_policies <- saved_store$policies
      persisted_fns      <- saved_store$fns %||% list()
      # Re-parent each closure's environment to the current globalenv()
      for (pid in names(persisted_fns)) {
        if (!is.null(persisted_fns[[pid]])) {
          fn <- persisted_fns[[pid]]
          fn_env <- environment(fn)
          if (!is.null(fn_env) && !identical(parent.env(fn_env), globalenv()))
            parent.env(fn_env) <- globalenv()
          policy_fns[[pid]] <- fn
        }
      }
      message(sprintf("  Restored %d saved policies.", nrow(persisted_policies)))
    }, error = function(e) {
      message("  Could not load saved policies: ", e$message)
    })
  }

  list(
    persisted_policies = persisted_policies,
    persisted_fns      = persisted_fns,
    policy_fns         = policy_fns
  )
}
