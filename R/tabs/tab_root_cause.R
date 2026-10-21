# ============================================================
# Tab: Root Cause Explorer
# Top-down Pareto-path decomposition of a portfolio metric,
# with Oaxaca-style shift decomposition vs baseline or prior vintage.
# ============================================================

root_cause_ui <- function() {
  tabItem(tabName = "root_cause",

    header_panel(
      "Root Cause Explorer",
      "Walk top-down from a portfolio metric to the single segment path that drives it.",
      icon_name = "magnifying-glass-chart"
    ),

    tags$div(style = "color:#6b7280; font-size:12px; margin-top:-4px; margin-bottom:12px;",
             tags$i(class = "fa fa-info-circle", style = "margin-right:6px;"),
             "Root-cause walks top-down on the data scope you select below.",
             " Default is ", tags$b("Issued"),
             " \u2014 switch to Applications when investigating decline-funnel drivers."),

    fluidRow(column(12,
      product_selector_ui("rc"),
      div(class = "card",
          style = "background:#222840; border-left:3px solid #fb6e36; padding:8px 14px; margin-top:10px; border-radius:6px;",
          div(style = "font-size:11px; color:#999; text-transform:uppercase;", "Current Baseline Policy"),
          div(style = "font-size:14px; color:#fb6e36; font-weight:bold;", "PL-CURRENT"))
    )),

    fluidRow(column(12, div(class = "card",
      h4("Inputs", style = "font-weight:bold; margin-bottom:10px;"),
      fluidRow(
        column(4, radioButtons("rc_metric", "Metric",
          choices  = c("DQ Rate"       = "dq_rate",
                       "Loss Rate"     = "loss_rate",
                       "Charge-off $"  = "chargeoff_dollars",
                       "IRR"           = "irr"),
          selected = "dq_rate", inline = FALSE)),
        column(4, radioButtons("rc_scope", "Scope",
          choices = c("Issued" = "issued", "Applications" = "applications"),
          selected = "issued", inline = FALSE)),
        column(4, sliderInput("rc_max_depth", "Max Drill Depth",
          min = 1, max = 5, value = 4, step = 1))
      ),
      fluidRow(column(12,
        pickerInput("rc_candidates", "Candidate Variables",
          choices  = NULL, selected = NULL,
          multiple = TRUE,
          options  = list(`actions-box` = TRUE, `live-search` = TRUE,
                          `selected-text-format` = "count > 3"))
      )),
      div(style = "text-align:center; margin-top:12px;",
        actionButton("rc_run", "Analyze",
          icon = icon("magnifying-glass-chart"), class = "btn-lg",
          style = "background-color:#fb6e36; color:white; border:none; font-weight:bold; padding:12px 36px; border-radius:6px;"))
    ))),

    # Panel A — Pareto waterfall
    conditionalPanel(
      condition = "output.rc_ready",
      fluidRow(column(12, div(class = "card",
        h4("Headline drill path", style = "font-weight:bold;"),
        uiOutput("rc_pareto_waterfall"),
        div(id = "rc_pareto_callout", style = "margin-top:12px;",
            uiOutput("rc_callout"))
      )))
    ),

    # Panel B — Shift decomposition
    conditionalPanel(
      condition = "output.rc_ready && output.rc_has_shift",
      fluidRow(column(12, div(class = "card",
        h4("Period-over-period shift decomposition", style = "font-weight:bold;"),
        plotly::plotlyOutput("rc_shift_waterfall", height = "280px"),
        h5("Top rate-shift segments", style = "margin-top:14px;"),
        DT::DTOutput("rc_shift_rate_table"),
        h5("Top mix-shift segments", style = "margin-top:14px;"),
        DT::DTOutput("rc_shift_mix_table")
      )))
    ),

    # Panel C — Alternatives
    conditionalPanel(
      condition = "output.rc_ready",
      fluidRow(column(12, div(class = "card",
        h4("Alternative branches (runner-ups)", style = "font-weight:bold;"),
        p(style = "color:#9ca3af; font-size:12px;",
          "Click any row to re-root the waterfall on that branch."),
        DT::DTOutput("rc_alternatives_table")
      )))
    )
  )
}

root_cause_server <- function(input, output, session,
                              credit_data, perf_tape, loan_irr_cache) {

  observe({
    reg <- rc_candidate_registry()
    present <- names(reg)[names(reg) %in% names(credit_data)]
    choices <- setNames(present, unname(reg[present]))
    selected <- intersect(rc_default_candidates(), present)
    updatePickerInput(session, "rc_candidates",
                      choices = choices, selected = selected)
  })

  rc_result <- eventReactive(input$rc_run, {
    req(input$rc_metric, input$rc_scope)
    df <- get_scoped_data(input$rc_scope, credit_data)
    if (nrow(df) == 0) {
      showNotification("No rows in selected scope.", type = "warning")
      return(NULL)
    }
    cache <- tryCatch(loan_irr_cache(), error = function(e) NULL)
    # Compute can take 20-40s on 100k rows × 9 candidate vars; show progress
    # so the user knows the click registered. Capture result OUTSIDE
    # withProgress so the eventReactive returns it reliably regardless of
    # how withProgress unwinds its value.
    result <- NULL
    withProgress(message = "Running root-cause analysis...",
                 detail = "Computing Pareto path through segments",
                 value = 0.3, {
      result <- tryCatch(
        run_root_cause(
          df,
          metric     = input$rc_metric,
          comparison = "none",
          candidates = input$rc_candidates,
          irr_cache  = cache,
          max_depth  = input$rc_max_depth
        ),
        error = function(e) {
          showNotification(paste("Root cause failed:", e$message), type = "error",
                           duration = 8)
          NULL
        }
      )
      incProgress(0.7)
    })
    result
  }, ignoreInit = TRUE)

  output$rc_ready <- reactive(!is.null(rc_result()))
  outputOptions(output, "rc_ready", suspendWhenHidden = FALSE)

  output$rc_has_shift <- reactive({
    r <- rc_result(); !is.null(r) && !is.null(r$shift) && nrow(r$shift) > 0
  })
  outputOptions(output, "rc_has_shift", suspendWhenHidden = FALSE)

  # --- Panel A: waterfall as stacked cards ---
  output$rc_pareto_waterfall <- renderUI({
    r <- rc_result(); req(r)
    node <- r$path
    cards <- list()
    while (!is.null(node)) {
      label <- if (node$depth == 0) "Portfolio total" else node$rule
      val   <- rc_fmt_metric(node$metric_value, r$metric$fmt)
      contrib <- if (node$depth == 0) "" else
        sprintf(" (%+.1f pp contribution)", node$chosen$contribution_pp %||% NA_real_)
      cards[[length(cards) + 1]] <- div(
        style = "background:#1c2340; border-left:4px solid #fb6e36; padding:10px 14px; margin-bottom:6px; border-radius:4px;",
        div(style = "font-size:11px; text-transform:uppercase; color:#9ca3af;",
            sprintf("Level %d — %d loans (%.1f%% of book)",
                    node$depth, node$n, 100 * node$pct)),
        div(style = "font-size:14px; color:#e5e7eb;", label),
        div(style = "font-size:18px; color:#fb6e36; font-weight:bold;",
            paste0(val, contrib))
      )
      node <- node$child
    }
    tagList(cards)
  })

  output$rc_callout <- renderUI({
    r <- rc_result(); req(r)
    leaf <- r$path
    while (!is.null(leaf$child)) leaf <- leaf$child
    if (leaf$depth == 0) return(NULL)
    total_metric <- r$path$metric_value
    leaf_metric  <- leaf$metric_value
    share_of_total <- if (is.na(total_metric) || total_metric == 0) NA_real_ else
      (leaf$pct * leaf_metric) / total_metric
    div(style = "background:#0f172a; padding:12px; border-radius:6px; color:#e5e7eb;",
        sprintf("This %.1f%% of the book drives %.0f%% of total %s.",
                100 * leaf$pct,
                100 * (share_of_total %||% 0),
                r$metric$label))
  })

  # --- Panel B: shift decomposition ---
  output$rc_shift_waterfall <- plotly::renderPlotly({
    r <- rc_result(); req(r); req(r$shift)
    tot_mix  <- sum(r$shift$mix_contrib_pp,  na.rm = TRUE)
    tot_rate <- sum(r$shift$rate_contrib_pp, na.rm = TRUE)
    plotly::plot_ly(
      type = "waterfall", orientation = "v",
      measure = c("absolute", "relative", "relative", "total"),
      x = c("Baseline", "Mix shift", "Rate shift", "Current"),
      y = c(0, tot_mix, tot_rate, 0),
      text = sprintf("%+.1fpp", c(0, tot_mix, tot_rate, tot_mix + tot_rate))
    ) |> plotly::layout(plot_bgcolor = "#0f172a", paper_bgcolor = "#0f172a",
                        font = list(color = "#e5e7eb"))
  })

  output$rc_shift_rate_table <- DT::renderDT({
    r <- rc_result(); req(r$shift)
    d <- r$shift[order(-abs(r$shift$rate_contrib_pp)), , drop = FALSE]
    DT::datatable(head(d[, c("segment","n_current","n_baseline","rate_contrib_pp")], 3),
                  options = list(dom = "t"), rownames = FALSE) |>
      DT::formatRound("rate_contrib_pp", 2)
  })

  output$rc_shift_mix_table <- DT::renderDT({
    r <- rc_result(); req(r$shift)
    d <- r$shift[order(-abs(r$shift$mix_contrib_pp)), , drop = FALSE]
    DT::datatable(head(d[, c("segment","n_current","n_baseline","mix_contrib_pp")], 3),
                  options = list(dom = "t"), rownames = FALSE) |>
      DT::formatRound("mix_contrib_pp", 2)
  })

  # --- Panel C: alternatives ---
  output$rc_alternatives_table <- DT::renderDT({
    r <- rc_result(); req(r$alternatives); req(nrow(r$alternatives) > 0)
    cols <- intersect(c("at_depth","variable","bucket","n","metric_value","contribution_pp"),
                      names(r$alternatives))
    DT::datatable(
      r$alternatives[, cols, drop = FALSE],
      options = list(dom = "tp", pageLength = 6),
      rownames = FALSE
    ) |> DT::formatRound(intersect(c("metric_value","contribution_pp"), cols), 3)
  })

  # Force panel render outputs to evaluate even when hidden on first render,
  # so the conditionalPanel's output.rc_ready signal flips before the user
  # scrolls to the panel. Must be called AFTER the outputs are defined.
  outputOptions(output, "rc_pareto_waterfall",   suspendWhenHidden = FALSE)
  outputOptions(output, "rc_callout",            suspendWhenHidden = FALSE)
  outputOptions(output, "rc_alternatives_table", suspendWhenHidden = FALSE)
}

# ---- Formatting helpers ------------------------------------------------
rc_fmt_metric <- function(x, fmt) {
  if (is.na(x)) return("n/a")
  switch(fmt,
    percent = sprintf("%.1f%%", 100 * x),
    dollars = sprintf("$%s", format(round(x), big.mark = ",")),
    sprintf("%.3f", x)
  )
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
