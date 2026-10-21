# ============================================================
# Tab: Policy History
# Shows policy evolution timeline, side-by-side comparison of
# any two policies (data-backed or simulated), and test/control
# experiment results.
# ============================================================

# ---- Helpers ----

# Status badge HTML for DT table rendering
.ph_status_badge <- function(status) {
  colors <- list(
    active    = list(bg = "#d1fae5", fg = "#065f46", border = "none"),
    retired   = list(bg = "#f3f4f6", fg = "#6b7280", border = "none"),
    simulated = list(bg = "#dbeafe", fg = "#1e40af", border = "2px dashed #93c5fd"),
    test      = list(bg = "#ffedd5", fg = "#9a3412", border = "none")
  )
  s <- tolower(status)
  c <- colors[[s]] %||% colors$retired
  border_css <- if (c$border == "none") "" else paste0("border:", c$border, ";")
  sprintf(
    '<span style="background:%s;color:%s;padding:2px 10px;border-radius:12px;font-size:11px;font-weight:600;%s">%s</span>',
    c$bg, c$fg, border_css, tools::toTitleCase(s)
  )
}

# Format date range for period column
.ph_format_period <- function(from, to) {
  if (is.null(from) || is.na(from)) return("\u2014")
  from_str <- format(as.Date(from), "%b %Y")
  to_str   <- if (is.null(to) || is.na(to)) "Present" else format(as.Date(to), "%b %Y")
  paste(from_str, "\u2013", to_str)
}

# Derive key change text from params vs baseline
.ph_key_change <- function(params) {
  if (length(params) == 0) return("Baseline")
  parts <- character(0)
  nms <- list(
    fico_min           = function(v) paste0("FICO >= ", v),
    thin_file_months   = function(v) paste0("ThinFile ", v, "mo"),
    n_hard_inq_6m_max  = function(v) paste0("HardInq max ", v),
    delinquency_max    = function(v) paste0("Delinq max ", v),
    apr_high           = function(v) paste0("APR-H ", round(v * 100, 1), "%"),
    apr_med            = function(v) paste0("APR-M ", round(v * 100, 1), "%"),
    apr_low            = function(v) paste0("APR-L ", round(v * 100, 1), "%"),
    pti_low            = function(v) paste0("PTI-L ", round(v * 100, 0), "%"),
    exposure_cap_high  = function(v) paste0("Cap-H $", formatC(v, format = "d", big.mark = ",")),
    exposure_cap_other = function(v) paste0("Cap-O $", formatC(v, format = "d", big.mark = ",")),
    revol_utl_max      = function(v) paste0("Revol UTL max ", round(v * 100, 0), "%"),
    thin_file_reentry_delinq_gate = function(v) paste0("Re-entry gate ", v)
  )
  for (nm in names(params)) {
    fn <- nms[[nm]]
    if (!is.null(fn)) parts <- c(parts, fn(params[[nm]]))
  }
  parts <- unique(parts)
  if (length(parts) == 0) return("No param changes")
  paste(parts, collapse = ", ")
}

# Build the display dataframe from strategy_overrides
.ph_build_table_data <- function() {
  ids <- names(strategy_overrides)
  rows <- lapply(ids, function(pid) {
    so <- strategy_overrides[[pid]]
    is_test <- !is.null(so$test_control)
    # Determine effective status for test variants
    disp_status <- if (is_test && (so$status %||% "") != "simulated") "test" else so$status %||% "simulated"

    # Indent test variants
    disp_name <- if (is_test) {
      bps <- so$test_control$pricing_adjustment_bps %||% 0
      paste0("\u251c ", so$name)
    } else {
      so$name
    }

    data.frame(
      policy_id   = pid,
      name        = disp_name,
      product     = so$product %||% "Personal Loan",
      status      = disp_status,
      period      = .ph_format_period(so$effective_from, so$effective_to),
      key_change  = .ph_key_change(so$params),
      data_backed = isTRUE(so$data_backed),
      eff_from    = as.Date(ifelse(is.null(so$effective_from) || is.na(so$effective_from),
                                    NA_character_, as.character(so$effective_from))),
      parent      = so$parent_policy %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Format a KPI value for the comparison panel
.ph_fmt_kpi <- function(value, type = "number") {
  if (is.null(value) || is.na(value)) return("\u2014")
  switch(type,
    pct    = paste0(round(value * 100, 1), "%"),
    dollar = fmt_dollar_k(value),
    number = fmt_k(value),
    bps    = paste0(round(value * 10000), " bps"),
    as.character(value)
  )
}

# KPI card for comparison panel (dark theme)
.ph_kpi_card <- function(value, label, type = "number") {
  div(style = "background:#1f2937; border-radius:8px; padding:10px 6px; text-align:center; margin-bottom:8px;",
    div(style = "font-size:16px; font-weight:bold; color:#fb6e36;", .ph_fmt_kpi(value, type)),
    div(style = "font-size:10px; color:#9ca3af; margin-top:2px;", label)
  )
}

# Delta badge: shows difference between two values
.ph_delta_text <- function(a, b, type = "number") {
  if (is.null(a) || is.null(b) || is.na(a) || is.na(b)) return("\u2014")
  d <- b - a
  sign_str <- if (d > 0) "+" else ""
  switch(type,
    pct    = paste0(sign_str, round(d * 100, 1), "pp"),
    dollar = paste0(sign_str, fmt_dollar_k(d)),
    bps    = paste0(sign_str, round(d * 10000), "bps"),
    number = paste0(sign_str, fmt_k(d)),
    paste0(sign_str, round(d, 2))
  )
}

# ============================================================
# UI
# ============================================================

policy_history_ui <- function() {
  tabItem(tabName = "policy_history",

    tags$head(tags$style(HTML("
      .ph-table .dataTables_wrapper tbody tr:hover:not(.selected) {
        background-color: #fff7ed !important;
      }
      .ph-table .dataTables_wrapper tbody tr.selected {
        background-color: #fed7aa !important;
      }
      .ph-no-data-msg {
        text-align: center; padding: 40px 20px;
        color: #9ca3af; font-size: 14px;
      }
      .ph-no-data-msg .ph-icon { font-size: 32px; margin-bottom: 12px; color: #d1d5db; }
      .ph-delta-row {
        background: #111827; border-radius: 8px; padding: 12px 16px;
        color: #d1d5db; font-size: 13px; margin-top: 10px;
      }
      .ph-delta-row .delta-val { color: #fb6e36; font-weight: 600; }
      .ph-test-table { width: 100%; border-collapse: collapse; font-size: 13px; }
      .ph-test-table th {
        background: #1f2937; color: #9ca3af; padding: 10px 14px;
        font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px;
        border-bottom: 2px solid #374151;
      }
      .ph-test-table td {
        padding: 8px 14px; border-bottom: 1px solid #f3f4f6;
        color: #374151;
      }
      .ph-test-table tr:hover td { background: #fafafa; }
      .ph-section-label {
        font-size: 11px; text-transform: uppercase; letter-spacing: 1px;
        color: #9ca3af; font-weight: 600; margin-bottom: 10px;
      }
    "))),

    header_panel(
      "Policy History",
      "Track policy evolution, compare deployed vs simulated policies, and review test/control results.",
      icon_name = "clock-rotate-left"
    ),

    scope_toggle_ui("ph_data_scope"),

    # ---- Zone 1: Policy Tables ----
    fluidRow(
      column(12,
        div(class = "card ph-table",
          div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:10px;",
            h4("Deployed Policies", style = "font-weight:bold; margin:0; color:#1e293b;"),
            tags$span(style = "font-size:11px; color:#9ca3af;", "Click up to 2 rows to compare")
          ),
          DT::dataTableOutput("ph_deployed_table")
        ),
        div(class = "card ph-table", style = "margin-top:6px;",
          h4("Simulated Scenarios", style = "font-weight:bold; margin:0 0 10px 0; color:#1e293b;"),
          DT::dataTableOutput("ph_simulated_table")
        )
      )
    ),

    # ---- Zone 2: Comparison Panel ----
    fluidRow(
      column(12, uiOutput("ph_comparison_panel"))
    ),

    # AI Insights for comparison (UC3)
    fluidRow(
      column(12, uiOutput("ph_ai_insights"))
    ),

    # Talking Points button for comparison (UC7)
    fluidRow(
      column(12, div(style = "text-align:right; margin:8px 16px;",
        actionButton("ph_talking_pts", "Talking Points",
                     icon = icon("clipboard"),
                     class = "btn-sm",
                     style = "background:#374151; color:white; border:none;")
      ))
    ),

    # ---- Zone 3: Test Results Panel ----
    fluidRow(
      column(12, uiOutput("ph_test_panel"))
    )
  )
}

# ============================================================
# SERVER
# ============================================================

policy_history_server <- function(input, output, session,
                                  credit_data, policy_input_data,
                                  perf_tape, policy_fns_rv) {

  # ---- Build display table data ----
  tbl_data <- .ph_build_table_data()
  deployed_data  <- tbl_data[tbl_data$data_backed, , drop = FALSE]
  simulated_data <- tbl_data[!tbl_data$data_backed, , drop = FALSE]

  # Sort deployed: active first, then by effective_from descending
  deployed_data <- deployed_data[order(
    deployed_data$status != "active",
    -as.numeric(deployed_data$eff_from), na.last = TRUE
  ), , drop = FALSE]

  # ---- Track selections across both tables (max 2) ----
  selected_policies <- reactiveVal(character(0))

  # ---- Render deployed table ----
  output$ph_deployed_table <- DT::renderDataTable({
    df <- deployed_data[, c("policy_id", "name", "product", "status", "period", "key_change"), drop = FALSE]
    names(df) <- c("ID", "Policy Name", "Product", "Status", "Period", "Key Change")
    # Render status as badge
    df$Status <- vapply(df$Status, .ph_status_badge, character(1))
    # Product short codes
    df$Product <- vapply(df$Product, function(p) {
      code <- unname(.product_short_code[p] %||% substr(p, 1, 2))
      sprintf('<span style="background:#e0e7ff;color:#3730a3;padding:1px 8px;border-radius:10px;font-size:11px;font-weight:600;">%s</span>', code)
    }, character(1))

    DT::datatable(df,
      escape    = FALSE,
      selection = list(mode = "multiple", target = "row"),
      options   = list(
        dom         = "t",
        paging      = FALSE,
        ordering    = FALSE,
        scrollY     = "280px",
        scrollCollapse = TRUE,
        columnDefs  = list(
          list(visible = FALSE, targets = 0),  # hide ID column
          list(width = "220px", targets = 1),
          list(width = "50px", targets = 2),
          list(width = "80px", targets = 3),
          list(width = "140px", targets = 4),
          list(className = "dt-center", targets = c(2, 3))
        )
      ),
      rownames = FALSE
    )
  })

  # ---- Render simulated table ----
  output$ph_simulated_table <- DT::renderDataTable({
    df <- simulated_data[, c("policy_id", "name", "product", "status", "period", "key_change"), drop = FALSE]
    names(df) <- c("ID", "Policy Name", "Product", "Status", "Period", "Key Change")
    df$Status <- vapply(df$Status, .ph_status_badge, character(1))
    df$Product <- vapply(df$Product, function(p) {
      code <- unname(.product_short_code[p] %||% substr(p, 1, 2))
      sprintf('<span style="background:#e0e7ff;color:#3730a3;padding:1px 8px;border-radius:10px;font-size:11px;font-weight:600;">%s</span>', code)
    }, character(1))

    DT::datatable(df,
      escape    = FALSE,
      selection = list(mode = "multiple", target = "row"),
      options   = list(
        dom         = "t",
        paging      = FALSE,
        ordering    = FALSE,
        scrollY     = "200px",
        scrollCollapse = TRUE,
        columnDefs  = list(
          list(visible = FALSE, targets = 0),
          list(width = "220px", targets = 1),
          list(width = "50px", targets = 2),
          list(width = "80px", targets = 3),
          list(width = "140px", targets = 4),
          list(className = "dt-center", targets = c(2, 3))
        )
      ),
      rownames = FALSE
    )
  })

  # ---- Sync selections from both tables ----
  observeEvent(input$ph_deployed_table_rows_selected, {
    dep_sel <- deployed_data$policy_id[input$ph_deployed_table_rows_selected]
    sim_sel <- simulated_data$policy_id[input$ph_simulated_table_rows_selected %||% integer(0)]
    all_sel <- c(dep_sel, sim_sel)
    # Enforce max 2
    if (length(all_sel) > 2) {
      all_sel <- tail(all_sel, 2)
      # Deselect excess from deployed table via proxy
      keep_dep <- which(deployed_data$policy_id %in% all_sel)
      DT::dataTableProxy("ph_deployed_table") |> DT::selectRows(keep_dep)
    }
    selected_policies(all_sel)
  }, ignoreNULL = FALSE)

  observeEvent(input$ph_simulated_table_rows_selected, {
    dep_sel <- deployed_data$policy_id[input$ph_deployed_table_rows_selected %||% integer(0)]
    sim_sel <- simulated_data$policy_id[input$ph_simulated_table_rows_selected]
    all_sel <- c(dep_sel, sim_sel)
    if (length(all_sel) > 2) {
      all_sel <- tail(all_sel, 2)
      keep_sim <- which(simulated_data$policy_id %in% all_sel)
      DT::dataTableProxy("ph_simulated_table") |> DT::selectRows(keep_sim)
    }
    selected_policies(all_sel)
  }, ignoreNULL = FALSE)

  # ---- Compute metrics for a single policy ----
  .compute_policy_metrics <- function(pid) {
    so <- strategy_overrides[[pid]]
    if (is.null(so)) return(NULL)
    has_policy_col <- "policy_id" %in% names(credit_data)

    if (isTRUE(so$data_backed) && has_policy_col) {
      # Data-backed: filter credit_data by policy_id
      df <- credit_data[credit_data$policy_id == pid, , drop = FALSE]
      if (nrow(df) == 0) return(list(source = "no_data", n = 0))
      fun <- compute_funnel(df)
      # Compute IRR from perf tape if available
      tape <- perf_tape()
      irr_val <- NA_real_
      if (!is.null(tape)) {
        ids <- as.character(df$loan_id[df$issued_flag == 1])
        irr_val <- tryCatch(compute_portfolio_xirr_v2(tape, loan_ids = ids), error = function(e) NA_real_)
      }
      list(source = "data", pid = pid, name = so$name,
           apps = fun$apps, approved = fun$approved, issued = fun$issued,
           appr_rate = fun$appr_rate, conv_rate = fun$conv_rate,
           avg_apr = fun$avg_apr, avg_amt = fun$avg_amt,
           bad_rate = fun$bad_rate, irr = irr_val)

    } else if (isTRUE(so$data_backed) && !has_policy_col) {
      # Data-backed but no policy_id column -- graceful degradation
      list(source = "needs_enrichment", pid = pid, name = so$name)

    } else {
      # Simulated: run policy function on policy_input_data
      fns <- policy_fns_rv()
      fn <- fns[[pid]]
      if (is.null(fn)) return(list(source = "no_fn", pid = pid, name = so$name))
      sim_result <- tryCatch({
        df_out <- fn(as.data.frame(policy_input_data))
        compute_funnel(df_out)
      }, error = function(e) NULL)
      if (is.null(sim_result)) return(list(source = "sim_error", pid = pid, name = so$name))

      # Pull pre-baked simulation_result for IRR/bad_rate deltas
      sr <- so$simulation_result
      list(source = "simulated", pid = pid, name = so$name,
           apps = sim_result$apps, approved = sim_result$approved,
           issued = sim_result$issued, appr_rate = sim_result$appr_rate,
           conv_rate = sim_result$conv_rate, avg_apr = sim_result$avg_apr,
           avg_amt = sim_result$avg_amt, bad_rate = sim_result$bad_rate,
           irr = NA_real_, sim_result = sr)
    }
  }

  # ---- Build one side of the comparison panel ----
  .render_policy_side <- function(metrics, label) {
    if (is.null(metrics)) {
      return(div(class = "ph-no-data-msg",
        div(class = "ph-icon", icon("circle-question")),
        p("Policy not found")
      ))
    }

    if (metrics$source == "needs_enrichment") {
      return(div(class = "ph-no-data-msg",
        div(class = "ph-icon", icon("database")),
        p(tags$b(metrics$name)),
        p("Enriched data required"),
        p(style = "font-size:12px; color:#6b7280;",
          "Generate with ", tags$code("/generate-demo-data"))
      ))
    }

    if (metrics$source == "no_data") {
      return(div(class = "ph-no-data-msg",
        div(class = "ph-icon", icon("triangle-exclamation")),
        p("No loans found for this policy")
      ))
    }

    if (metrics$source %in% c("no_fn", "sim_error")) {
      return(div(class = "ph-no-data-msg",
        div(class = "ph-icon", icon("gear")),
        p("Could not run simulation for ", tags$b(metrics$name))
      ))
    }

    sim_badge <- if (metrics$source == "simulated") {
      tags$span(style = "background:#dbeafe;color:#1e40af;padding:2px 8px;border-radius:10px;font-size:10px;font-weight:600;margin-left:6px;", "SIMULATED")
    }

    tagList(
      div(style = "margin-bottom:12px;",
        tags$span(style = "font-size:11px; color:#9ca3af; text-transform:uppercase; letter-spacing:1px;", label),
        h5(style = "font-weight:bold; margin:4px 0 0 0; color:#1e293b;", metrics$name, sim_badge)
      ),
      fluidRow(
        column(3, .ph_kpi_card(metrics$apps, "Applications")),
        column(3, .ph_kpi_card(metrics$approved, "Approved")),
        column(3, .ph_kpi_card(metrics$issued, "Funded")),
        column(3, .ph_kpi_card(metrics$appr_rate, "Approval Rate", "pct"))
      ),
      fluidRow(
        column(3, .ph_kpi_card(metrics$avg_apr, "Avg APR", "pct")),
        column(3, .ph_kpi_card(metrics$avg_amt, "Avg Amount", "dollar")),
        column(3, .ph_kpi_card(metrics$bad_rate, "Bad Rate", "pct")),
        column(3, .ph_kpi_card(metrics$irr, "Portfolio IRR", "pct"))
      )
    )
  }

  # ---- Comparison Panel ----
  output$ph_comparison_panel <- renderUI({
   tryCatch({
    sel <- selected_policies()
    if (length(sel) < 2) {
      if (length(sel) == 1) {
        # Single selection: show policy detail
        m <- .compute_policy_metrics(sel[1])
        return(div(class = "card", style = "margin-top:6px;",
          .render_policy_side(m, "Selected Policy")
        ))
      }
      return(NULL)
    }

    # Two selections: side-by-side comparison
    m_a <- .compute_policy_metrics(sel[1])
    m_b <- .compute_policy_metrics(sel[2])

    # Delta row (only when both have numeric metrics)
    delta_ui <- NULL
    if (!is.null(m_a) && !is.null(m_b) &&
        m_a$source %in% c("data", "simulated") &&
        m_b$source %in% c("data", "simulated")) {
      delta_ui <- div(class = "ph-delta-row",
        tags$span(style = "font-weight:600; color:#fb6e36; margin-right:12px;", "Delta:"),
        tags$span("Approval ",
          tags$span(class = "delta-val", .ph_delta_text(m_a$appr_rate, m_b$appr_rate, "pct"))),
        tags$span(style = "margin:0 8px;", "|"),
        tags$span("Volume ",
          tags$span(class = "delta-val", .ph_delta_text(m_a$issued, m_b$issued, "number"))),
        tags$span(style = "margin:0 8px;", "|"),
        tags$span("Bad Rate ",
          tags$span(class = "delta-val", .ph_delta_text(m_a$bad_rate, m_b$bad_rate, "pct"))),
        tags$span(style = "margin:0 8px;", "|"),
        tags$span("IRR ",
          tags$span(class = "delta-val", .ph_delta_text(m_a$irr, m_b$irr, "pct"))),
        tags$span(style = "margin:0 8px;", "|"),
        tags$span("Avg APR ",
          tags$span(class = "delta-val", .ph_delta_text(m_a$avg_apr, m_b$avg_apr, "pct")))
      )
    }

    # Vintage curves (only for data-backed with policy_id column)
    vintage_ui <- NULL
    has_policy_col <- "policy_id" %in% names(credit_data)
    tape <- perf_tape()
    if (has_policy_col && !is.null(tape)) {
      vintage_ui <- div(style = "margin-top:16px;",
        div(class = "ph-section-label", "Vintage Curves"),
        plotly::plotlyOutput("ph_vintage_chart", height = "280px")
      )
    }

    div(class = "card", style = "margin-top:6px;",
      div(class = "ph-section-label", "Policy Comparison"),
      fluidRow(
        column(6, .render_policy_side(m_a, "Policy A")),
        column(6, .render_policy_side(m_b, "Policy B"))
      ),
      delta_ui,
      uiOutput("ph_comparison_narrative"),
      vintage_ui
    )
   }, error = function(e) {
     message("[PolicyHistory] ph_comparison_panel: ", conditionMessage(e))
     div(class = "card", style = "margin-top:6px; padding:14px; background:#fff1f2; border-left:4px solid #dc2626;",
       tags$b("Comparison panel error: ", style = "color:#7f1d1d;"),
       tags$span(conditionMessage(e), style = "color:#7f1d1d;")
     )
   })
  })

  # ---- AI Comparison Narrative (UC6) ----
  output$ph_comparison_narrative <- renderUI({
   tryCatch({
    sel <- selected_policies()
    req(length(sel) == 2)

    # Build param_diff from strategy_overrides
    sa <- strategy_overrides[[sel[1]]]
    sb <- strategy_overrides[[sel[2]]]
    req(sa, sb)

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

    # Build kpi_delta from computed metrics
    m_a <- .compute_policy_metrics(sel[1])
    m_b <- .compute_policy_metrics(sel[2])
    kpi_delta <- NULL
    if (!is.null(m_a) && !is.null(m_b) &&
        m_a$source %in% c("data", "simulated") &&
        m_b$source %in% c("data", "simulated")) {
      metrics <- list(
        list(metric = "approval_rate",  a = m_a$appr_rate, b = m_b$appr_rate),
        list(metric = "avg_loss_rate",  a = m_a$bad_rate,  b = m_b$bad_rate),
        list(metric = "portfolio_irr",  a = m_a$irr,       b = m_b$irr)
      )
      kpi_delta <- do.call(rbind, lapply(metrics, function(x) {
        va <- x$a; vb <- x$b
        delta <- if (is.null(va) || is.null(vb) || is.na(va) || is.na(vb)) NA_real_ else vb - va
        data.frame(metric = x$metric,
                   a = if (is.null(va)) NA_real_ else va,
                   b = if (is.null(vb)) NA_real_ else vb,
                   delta = delta, stringsAsFactors = FALSE)
      }))
    }

    name_a <- sa$name %||% sel[1]
    name_b <- sb$name %||% sel[2]

    narrative_text <- narrate_comparison(name_a, name_b, param_diff, kpi_delta)

    tags$div(
      style = "background:#1a1f2e; border-left:3px solid #fb6e36; border-radius:6px; padding:12px 16px; margin-top:12px;",
      tags$span(style = "color:#fb6e36; font-weight:600;", icon("sparkles"), " Policy Comparison Summary"),
      tags$p(style = "color:#d1d5db; font-size:13px; margin-top:8px; white-space:pre-wrap;", narrative_text)
    )
   }, error = function(e) {
     message("[PolicyHistory] ph_comparison_narrative: ", conditionMessage(e))
     NULL
   })
  })

  # ---- AI Insights for comparison (UC3) ----
  output$ph_ai_insights <- renderUI({
    sel <- selected_policies()
    req(length(sel) == 2)
    m_a <- tryCatch(.compute_policy_metrics(sel[1]), error = function(e) NULL)
    m_b <- tryCatch(.compute_policy_metrics(sel[2]), error = function(e) NULL)
    req(!is.null(m_a), !is.null(m_b))
    req(m_a$source %in% c("data", "simulated"), m_b$source %in% c("data", "simulated"))

    kpis <- list(
      a_appr_rate = m_a$appr_rate,
      b_appr_rate = m_b$appr_rate,
      a_bad_rate  = m_a$bad_rate,
      b_bad_rate  = m_b$bad_rate,
      a_irr       = m_a$irr,
      b_irr       = m_b$irr,
      a_avg_apr   = m_a$avg_apr,
      b_avg_apr   = m_b$avg_apr,
      a_name      = m_a$name,
      b_name      = m_b$name
    )
    insights_text <- tryCatch(
      generate_insights("comparison", kpis),
      error = function(e) paste0("- Could not generate insights: ", conditionMessage(e))
    )
    tags$details(
      style = "margin-top:12px; background:#1a1f2e; border-left:3px solid #fb6e36; border-radius:6px; padding:12px 16px;",
      open = "open",
      tags$summary(style = "color:#fb6e36; font-weight:600; cursor:pointer;",
        icon("sparkles"), " AI Insights"),
      tags$div(style = "color:#d1d5db; font-size:13px; margin-top:8px; white-space:pre-wrap;",
        HTML(insights_text))
    )
  })

  # ---- Talking Points for comparison (UC7) ----
  observeEvent(input$ph_talking_pts, {
    sel <- selected_policies()
    req(length(sel) == 2)
    m_a <- tryCatch(.compute_policy_metrics(sel[1]), error = function(e) NULL)
    m_b <- tryCatch(.compute_policy_metrics(sel[2]), error = function(e) NULL)
    req(!is.null(m_a), !is.null(m_b))

    tp_data <- list(
      a_appr_rate = m_a$appr_rate,
      b_appr_rate = m_b$appr_rate,
      a_bad_rate  = m_a$bad_rate,
      b_bad_rate  = m_b$bad_rate,
      a_irr       = m_a$irr,
      b_irr       = m_b$irr,
      a_name      = m_a$name,
      b_name      = m_b$name
    )
    tp_text <- tryCatch(
      generate_talking_points("comparison", tp_data),
      error = function(e) paste0("1. Error generating talking points: ", conditionMessage(e))
    )
    showModal(modalDialog(
      title = "Talking Points",
      tags$pre(id = "ph_tp_text",
        style = "background:#111827; color:#d1d5db; padding:16px; border-radius:6px; white-space:pre-wrap; font-size:13px;",
        tp_text),
      footer = tagList(
        actionButton("ph_tp_copy", "Copy to Clipboard", icon = icon("clipboard")),
        modalButton("Close")
      )
    ))
  })

  observeEvent(input$ph_tp_copy, {
    shinyjs::runjs("navigator.clipboard.writeText(document.getElementById('ph_tp_text').innerText);")
    showNotification("Copied!", type = "message", duration = 2)
  })

  # ---- Vintage chart (plotly) ----
  output$ph_vintage_chart <- plotly::renderPlotly({
   tryCatch({
    sel <- selected_policies()
    req(length(sel) == 2)
    tape <- perf_tape()
    req(!is.null(tape))
    req("policy_id" %in% names(credit_data))

    # Build vintage data for each selected policy
    traces <- list()
    colors <- c("#fb6e36", "#3b82f6")
    for (i in seq_along(sel)) {
      pid <- sel[i]
      so <- strategy_overrides[[pid]]
      loan_ids <- as.character(credit_data$loan_id[
        credit_data$policy_id == pid & credit_data$issued_flag == 1])
      if (length(loan_ids) == 0) next
      rows <- tape$rows[tape$rows$loan_id %in% loan_ids, , drop = FALSE]
      if (nrow(rows) == 0 || !"month_on_books" %in% names(rows)) next
      # Compute cumulative delinquency by MOB
      agg <- rows %>%
        dplyr::group_by(month_on_books) %>%
        dplyr::summarise(
          n = dplyr::n(),
          del = sum(ever_del == 1, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(del_rate = del / n)
      traces[[i]] <- list(data = agg, name = so$name %||% pid, color = colors[i])
    }

    p <- plotly::plot_ly()
    for (tr in traces) {
      if (is.null(tr)) next
      p <- p %>% plotly::add_trace(
        data = tr$data, x = ~month_on_books, y = ~del_rate,
        type = "scatter", mode = "lines+markers",
        name = tr$name,
        line = list(color = tr$color, width = 2),
        marker = list(color = tr$color, size = 4),
        hovertemplate = paste0(tr$name, "<br>MOB %{x}<br>Del Rate: %{y:.1%}<extra></extra>")
      )
    }
    p %>% plotly::layout(
      xaxis = list(title = "Months on Books", gridcolor = "#f3f4f6"),
      yaxis = list(title = "Delinquency Rate", tickformat = ".1%", gridcolor = "#f3f4f6"),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin = list(t = 10, b = 40, l = 50, r = 10),
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")
    )
   }, error = function(e) {
     message("[PolicyHistory] ph_vintage_chart: ", conditionMessage(e))
     plotly::plot_ly() %>%
       plotly::layout(
         title = list(text = paste("Vintage chart error:", conditionMessage(e)),
                      font = list(size = 12)),
         paper_bgcolor = "white", plot_bgcolor = "white"
       )
   })
  })

  # ---- Zone 3: Test Results Panel ----
  # Dynamically groups loans by `test_group` column (control, test_a, test_b, ...).
  # No hard-coded policy_id lists — any new test arm appears automatically
  # once rows exist with a non-empty test_group value.
  output$ph_test_panel <- renderUI({
   tryCatch({
    sel <- selected_policies()
    if (length(sel) == 0) return(NULL)

    has_test_col <- "test_group" %in% names(credit_data)
    if (!has_test_col) {
      return(div(class = "card", style = "margin-top:6px;",
        div(class = "ph-section-label", "Pricing Test Results"),
        div(class = "ph-no-data-msg",
          div(class = "ph-icon", icon("flask")),
          p("Test/control data required"),
          p(style = "font-size:12px; color:#6b7280;",
            "Generate enriched data with ",
            tags$code("/generate-demo-data"),
            " to view experiment results.")
        )
      ))
    }

    # Identify all rows that belong to any test arm (non-empty + non-NA test_group)
    tg <- credit_data$test_group
    in_test <- !is.na(tg) & nzchar(trimws(as.character(tg)))
    test_loans <- credit_data[in_test, , drop = FALSE]

    if (nrow(test_loans) == 0) {
      # No test data at all — keep the panel hidden (avoid noise on unrelated policies)
      return(NULL)
    }

    # Only show the test panel if the user actually selected a policy that
    # has test arms associated with it.  We detect this by checking whether
    # any test row shares a policy_id with the selected policies, OR whether
    # the selected policies include the "umbrella" policy that spawned the test.
    if ("policy_id" %in% names(credit_data)) {
      sel_has_tests <- any(test_loans$policy_id %in% sel)
      # Also keep the panel visible if the user selected an obvious umbrella
      # policy whose name starts with the same prefix as any test row's policy_id.
      if (!sel_has_tests) {
        pid_roots <- unique(sub("-T[A-Z0-9]+$", "", as.character(test_loans$policy_id)))
        if (!any(sel %in% pid_roots)) return(NULL)
      }
    }

    tape <- perf_tape()

    # ---- Dynamic arm discovery ----
    # Group by test_group. Preserve natural order: control first, then the rest
    # alphabetically.  Label is the capitalised test_group value; bps delta is
    # inferred from mean APR vs control so elasticity can be computed.
    group_vals <- sort(unique(as.character(test_loans$test_group)))
    group_vals <- c(
      grep("^control$", group_vals, ignore.case = TRUE, value = TRUE),
      setdiff(group_vals, grep("^control$", group_vals, ignore.case = TRUE, value = TRUE))
    )
    if (length(group_vals) == 0) return(NULL)

    # Pretty-label helper
    pretty_label <- function(g) {
      if (grepl("^control$", g, ignore.case = TRUE)) return("Control")
      # test_a  -> "Test A"
      paste("Test", toupper(sub("^test[_ ]*", "", g, ignore.case = TRUE)))
    }

    # First pass: compute base metrics per arm
    arm_metrics <- lapply(group_vals, function(g) {
      df <- test_loans[test_loans$test_group == g, , drop = FALSE]
      n_appr   <- sum(df$offered_decision_flag == "APPR", na.rm = TRUE)
      n_issued <- sum(df$issued_flag == 1, na.rm = TRUE)
      take_rate <- if (n_appr > 0) n_issued / n_appr else NA_real_
      issued_mask <- df$issued_flag == 1
      bad_rate  <- if (sum(issued_mask, na.rm = TRUE) > 0)
        mean(df$performance_delinquency_flag[issued_mask], na.rm = TRUE) else NA_real_
      avg_apr   <- if (sum(issued_mask, na.rm = TRUE) > 0)
        mean(df$interest_rate_apr[issued_mask], na.rm = TRUE) else NA_real_
      irr_val   <- NA_real_
      if (!is.null(tape)) {
        ids <- as.character(df$loan_id[issued_mask])
        irr_val <- tryCatch(compute_portfolio_xirr_v2(tape, loan_ids = ids),
                            error = function(e) NA_real_)
      }
      list(group = g, label = pretty_label(g),
           n_loans = n_issued, take_rate = take_rate,
           bad_rate = bad_rate, avg_apr = avg_apr, irr = irr_val)
    })

    # Derive bps delta vs control (first entry)
    ctrl <- arm_metrics[[1]]
    ctrl_apr  <- ctrl$avg_apr %||% NA_real_
    ctrl_take <- ctrl$take_rate

    for (i in seq_along(arm_metrics)) {
      a <- arm_metrics[[i]]
      bps <- if (i == 1 || is.na(a$avg_apr) || is.na(ctrl_apr)) 0
             else round((a$avg_apr - ctrl_apr) * 10000)
      arm_metrics[[i]]$bps <- bps
      if (i > 1 && bps != 0) {
        arm_metrics[[i]]$label <- sprintf("%s (%+dbps)", a$label, as.integer(bps))
      }
    }

    # Empirical elasticity vs control
    elast_vals <- vapply(arm_metrics, function(m) {
      if (m$bps == 0 || is.na(ctrl_take) || is.na(m$take_rate)) return(NA_real_)
      (ctrl_take - m$take_rate) / (m$bps / 100)
    }, numeric(1))

    # Build HTML table helpers
    .cell <- function(val, type = "number") {
      tags$td(style = "text-align:center;", .ph_fmt_kpi(val, type))
    }

    # Winner determination: prefer IRR when available, else lowest bad rate
    winner_idx <- which.max(vapply(arm_metrics, function(m) {
      if (is.na(m$irr)) -(m$bad_rate %||% 1) else m$irr
    }, numeric(1)))
    winner <- arm_metrics[[winner_idx]]

    # Header cells (dynamic — one per arm)
    header_cells <- lapply(arm_metrics, function(m)
      tags$th(style = "text-align:center;", m$label))

    row_cells <- function(metric_key, type) {
      lapply(arm_metrics, function(m) .cell(m[[metric_key]], type))
    }

    # Elasticity row cells: control cell is em-dash, others show value
    elast_cells <- lapply(seq_along(arm_metrics), function(i) {
      if (arm_metrics[[i]]$bps == 0)
        tags$td(style = "text-align:center; color:#9ca3af;", "\u2014")
      else
        tags$td(style = "text-align:center;",
          if (!is.na(elast_vals[i])) paste0(round(elast_vals[i] * 100, 1), "% / 100bps")
          else "\u2014")
    })

    div(class = "card", style = "margin-top:6px;",
      div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;",
        div(
          div(class = "ph-section-label", "Pricing Test Results"),
          tags$span(style = "font-size:13px; color:#374151; font-weight:600;",
                    sprintf("%d arms detected from test_group column", length(arm_metrics)))
        )
      ),
      tags$table(class = "ph-test-table",
        tags$thead(tags$tr(tags$th("Metric"), header_cells)),
        tags$tbody(
          tags$tr(tags$td(tags$b("N Loans")),        row_cells("n_loans",   "number")),
          tags$tr(tags$td(tags$b("Take Rate")),      row_cells("take_rate", "pct")),
          tags$tr(tags$td(tags$b("Bad Rate")),       row_cells("bad_rate",  "pct")),
          tags$tr(tags$td(tags$b("Avg APR")),        row_cells("avg_apr",   "pct")),
          tags$tr(tags$td(tags$b("Portfolio IRR")),  row_cells("irr",       "pct")),
          tags$tr(style = "border-top:2px solid #e5e7eb;",
            tags$td(tags$b("Empirical Elasticity")),
            elast_cells
          )
        )
      ),
      div(style = "margin-top:14px; padding:10px 14px; background:#f0fdf4; border-radius:8px; border-left:4px solid #22c55e;",
        tags$span(style = "font-weight:600; color:#065f46;", icon("trophy"), " Recommendation: "),
        tags$span(style = "color:#374151;",
          paste0(winner$label, " selected as winner."))
      )
    )
   }, error = function(e) {
     message("[PolicyHistory] ph_test_panel: ", conditionMessage(e))
     div(class = "card", style = "margin-top:6px; padding:14px; background:#fff1f2; border-left:4px solid #dc2626;",
       tags$b("Test panel error: ", style = "color:#7f1d1d;"),
       tags$span(conditionMessage(e), style = "color:#7f1d1d;")
     )
   })
  })
}
