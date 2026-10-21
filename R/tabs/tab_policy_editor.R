# ============================================================
# Tab: Policy Editor
# Extracted from app.R — UI and server logic
# ============================================================

policy_editor_ui <- function() {
      tabItem(tabName = "policy_editor",

        header_panel(
          "Policy Rules Editor",
          paste("Edit policy parameters across all layers, validate",
                "consistency, and simulate the cascading impact."),
          icon_name = "pen-ruler"
        ),

        scope_toggle_ui("pe_data_scope"),

        # Product selector
        fluidRow(column(12, product_selector_ui("pe"))),

        fluidRow(
          # ---- Left column: controls ----
          column(5,
            # Group selector + action buttons
            div(class = "card",
              fluidRow(
                column(6,
                  selectInput("pe_group", "Policy Group",
                    choices = c("Grading", "Decline Rules", "Pricing",
                                "Loan Limits", "Affordability", "Tenure"),
                    selected = "Decline Rules")
                ),
                column(6, style = "padding-top: 25px;",
                  div(style = "display: flex; gap: 6px; flex-wrap: wrap;",
                    actionButton("pe_validate", "Validate",
                      icon = icon("check"), class = "custom-button",
                      style = "font-size:12px; padding:6px 12px;"),
                    actionButton("pe_simulate", "Simulate",
                      icon = icon("play"), class = "custom-button",
                      style = "font-size:12px; padding:6px 12px;"),
                    actionButton("pe_reset", "Reset",
                      icon = icon("undo"),
                      class = "btn btn-default",
                      style = "font-size:12px; padding:6px 12px;")
                  )
                )
              )
            ),

            # Validation messages
            uiOutput("pe_validation_box"),

            # Cascade impact display
            div(class = "card",
              h5("Cascade Impact", style = "font-weight:bold; margin-bottom:8px; font-size:13px;"),
              uiOutput("pe_cascade_display")
            ),

            # Parameter table
            div(class = "card",
              uiOutput("pe_param_table")
            )
          ),

          # ---- Right column: results ----
          column(7,
            # KPI tiles
            uiOutput("pe_kpi_row"),

            # Charts row 1
            fluidRow(
              column(6, div(class = "card",
                h4("Decline Reasons", style = "font-weight:bold; font-size:13px;"),
                plotlyOutput("pe_chart_decline", height = "300px")
              )),
              column(6, div(class = "card",
                h4("APR Distribution (Approved)", style = "font-weight:bold; font-size:13px;"),
                plotlyOutput("pe_chart_apr", height = "300px")
              ))
            ),

            # Charts row 2
            fluidRow(
              column(6, div(class = "card",
                h4("Approval Rate by FICO Band", style = "font-weight:bold; font-size:13px;"),
                plotlyOutput("pe_chart_fico", height = "300px")
              )),
              column(6, div(class = "card",
                h4("Grade Distribution", style = "font-weight:bold; font-size:13px;"),
                plotlyOutput("pe_chart_grade", height = "300px")
              ))
            ),

            # Detail table
            div(class = "card",
              h4("Affected Records", style = "font-weight:bold; font-size:13px;"),
              DTOutput("pe_detail_table")
            )
          )
        )
      )
}

policy_editor_server <- function(input, output, session, credit_data, perf_tape, policy_input_data, loan_irr_cache = NULL) {
  # Baseline params = manifest defaults + PL-CURRENT strategy overrides.
  # Mirrors the pattern used by other tabs so the Policy Editor's "Baseline"
  # reflects the active production policy rather than naked manifest defaults.
  .pe_pl_current <- function() {
    p  <- build_default_params(policy_manifest)
    so <- get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE)
    if (!is.null(so) && !is.null(so[["PL-CURRENT"]]) &&
        !is.null(so[["PL-CURRENT"]]$params)) {
      p <- modifyList(p, so[["PL-CURRENT"]]$params)
    }
    p
  }
  pe_default_params <- .pe_pl_current()
  pe_scenario_rv    <- reactiveVal(NULL)
  pe_baseline_rv    <- reactiveVal(NULL)

  # Run baseline once on first visit
  observe({
    if (is.null(pe_baseline_rv())) {
      bl <- apply_policy_with_params(policy_input_data, pe_default_params)
      bl <- bl %>% mutate(fico_band = case_when(
        fico_score <  600 ~ "<600",
        fico_score <  650 ~ "600-649",
        fico_score <  700 ~ "650-699",
        fico_score <  750 ~ "700-749",
        fico_score >= 750 ~ "750+",
        TRUE              ~ "Unknown"
      ))
      pe_baseline_rv(bl)
    }
  })

  pe_baseline_kpis <- reactive({
    bl <- pe_baseline_rv()
    req(bl)
    compute_kpis(bl)
  })

  # ---------- IRR reactives (when perf_tape available) ----------
  pe_baseline_irr <- reactive({
    tape <- perf_tape()
    bl <- pe_baseline_rv()
    if (is.null(tape) || is.null(bl)) return(NA_real_)
    ids <- as.character(bl$loan_id[bl$issued_flag == 1])
    tryCatch(compute_portfolio_xirr_v2(tape, loan_ids = ids), error = function(e) NA_real_)
  })

  pe_scenario_irr <- reactive({
    tape <- perf_tape()
    scen <- pe_scenario_rv()
    if (is.null(tape) || is.null(scen)) return(NA_real_)
    ids <- as.character(scen$loan_id[scen$issued_flag == 1])
    tryCatch(compute_portfolio_xirr_v2(tape, loan_ids = ids), error = function(e) NA_real_)
  })

  # ---------- Build parameter table per group ----------
  pe_build_group_table <- function(grp) {
    grp_params <- Filter(function(p) p$group == grp, policy_manifest)
    subtab_names <- unique(sapply(grp_params, function(p) p$subtab))

    make_rows <- function(params_list) {
      lapply(params_list, function(p) {
        if (identical(p$type, "display")) {
          return(tags$tr(class = "pe-row-fixed",
            tags$td(class = "pe-col-variable", p$label),
            tags$td(class = "pe-col-baseline", as.character(p$default)),
            tags$td(class = "pe-col-scenario",
              span(style = "color:#9ca3af; font-style:italic; font-size:12px;", "Fixed"))
          ))
        }
        input_id <- paste0("pe_", p$id)
        baseline_display <- if (p$unit == "%") {
          paste0(round(p$default * 100, 1), "%")
        } else if (p$unit == "$") {
          paste0("$", formatC(p$default, format = "d", big.mark = ","))
        } else if (p$unit == "mo") {
          paste0(p$default, " mo")
        } else {
          as.character(p$default)
        }
        input_el <- if (p$type == "slider") {
          sliderInput(input_id, label = NULL, min = p$min, max = p$max,
                      value = p$default, step = p$step, width = "100%")
        } else {
          selectInput(input_id, label = NULL, choices = p$choices,
                      selected = as.character(p$default), width = "100%")
        }
        tags$tr(
          tags$td(class = "pe-col-variable",
            span(p$label),
            if (!is.null(p$tip) && nchar(p$tip) > 0)
              tags$span(style = "cursor:help; color:#9ca3af; margin-left:4px;",
                        title = p$tip, icon("info-circle"))
          ),
          tags$td(class = "pe-col-baseline", baseline_display),
          tags$td(class = "pe-col-scenario", input_el)
        )
      })
    }

    if (length(subtab_names) == 1) {
      rows <- make_rows(grp_params)
      return(
        tags$div(class = "pe-table-wrap", style = "overflow-x:auto;",
          tags$table(class = "pe-edit-table",
            tags$thead(tags$tr(
              tags$th("Rule"), tags$th("Baseline"), tags$th("Scenario Value")
            )),
            tags$tbody(rows)
          )
        )
      )
    }

    sub_tabs <- lapply(subtab_names, function(st) {
      st_params <- Filter(function(p) p$subtab == st, grp_params)
      rows <- make_rows(st_params)
      tabPanel(st,
        div(style = "margin-top: 8px;",
          tags$table(class = "pe-edit-table",
            tags$thead(tags$tr(
              tags$th("Rule"), tags$th("Baseline"), tags$th("Scenario Value")
            )),
            tags$tbody(rows)
          )
        )
      )
    })

    tab_id <- paste0("pe_tabs_", gsub(" ", "_", grp))
    do.call(tabsetPanel, c(list(id = tab_id, type = "pills"), sub_tabs))
  }

  # ---------- Render parameter table ----------
  output$pe_param_table <- renderUI({
    grp <- input$pe_group
    req(grp)
    pe_build_group_table(grp)
  })

  # ---------- Collect pe_* params ----------
  get_pe_params <- function() {
    params <- list()
    for (p in policy_manifest) {
      if (identical(p$type, "display")) next
      val <- input[[paste0("pe_", p$id)]]
      if (is.null(val)) val <- p$default
      if (p$type == "select") val <- as.integer(val)
      params[[p$id]] <- val
    }
    params
  }

  # ---------- Cascade display (reactive — no button) ----------
  output$pe_cascade_display <- renderUI({
    params <- get_pe_params()
    impact <- compute_cascade_impact(params, pe_default_params, policy_manifest)

    all_layer_ids <- c(20, 30, 40, 50, 60, 70, 80, 90)

    # Impact badge
    badge_class <- paste0("pe-impact-", impact$impact)
    badge_label <- switch(impact$impact,
      high   = "HIGH",
      medium = "MEDIUM",
      low    = "LOW",
      "NO CHANGE"
    )

    chips <- lapply(all_layer_ids, function(lid) {
      lname <- layer_names[as.character(lid)]
      if (lid %in% impact$direct_layers) {
        chip_class <- paste0("active-", impact$impact)
      } else if (lid %in% impact$all_layers) {
        chip_class <- paste0("active-", impact$impact)
      } else {
        chip_class <- "inactive"
      }
      span(class = paste("pe-cascade-chip", chip_class), lname)
    })

    n_changed <- length(impact$changed_ids)
    changed_summary <- if (n_changed == 0) {
      "No parameters changed from baseline."
    } else {
      paste0(n_changed, " parameter", if (n_changed > 1) "s" else "",
             " changed across ", length(impact$direct_layers), " layer",
             if (length(impact$direct_layers) > 1) "s" else "", ".")
    }

    tagList(
      div(style = "margin-bottom:8px;",
        span(class = paste("pe-impact-badge", badge_class), badge_label),
        span(style = "margin-left:8px; font-size:12px; color:#6b7280;", changed_summary)
      ),
      div(style = "display:flex; flex-wrap:wrap; gap:4px;", chips)
    )
  })

  # ---------- Validate ----------
  observeEvent(input$pe_validate, {
    params <- get_pe_params()
    result <- validate_params(params, policy_manifest)

    output$pe_validation_box <- renderUI({
      if (length(result$errors) == 0 && length(result$warnings) == 0) {
        div(class = "pe-validation-box pe-ok",
          icon("check-circle", style = "color:#22c55e; margin-right:6px;"),
          "All validation checks passed."
        )
      } else {
        tagList(
          if (length(result$errors) > 0)
            div(class = "pe-validation-box pe-error",
              icon("times-circle", style = "color:#dc2626; margin-right:6px;"),
              tags$strong("Errors:"),
              tags$ul(lapply(result$errors, tags$li))
            ),
          if (length(result$warnings) > 0)
            div(class = "pe-validation-box pe-warning",
              icon("exclamation-triangle", style = "color:#f59e0b; margin-right:6px;"),
              tags$strong("Warnings:"),
              tags$ul(lapply(result$warnings, tags$li))
            )
        )
      }
    })
  })

  # ---------- Simulate ----------
  observeEvent(input$pe_simulate, {
    params <- get_pe_params()

    # Validate first
    vresult <- validate_params(params, policy_manifest)
    if (!vresult$valid) {
      output$pe_validation_box <- renderUI({
        div(class = "pe-validation-box pe-error",
          icon("times-circle", style = "color:#dc2626; margin-right:6px;"),
          tags$strong("Cannot simulate — fix errors first:"),
          tags$ul(lapply(vresult$errors, tags$li))
        )
      })
      return()
    }

    withProgress(message = "Running policy simulation...", value = 0, {
      incProgress(0.2, detail = "Validating parameters...")

      # Show warnings if any
      if (length(vresult$warnings) > 0) {
        output$pe_validation_box <- renderUI({
          div(class = "pe-validation-box pe-warning",
            icon("exclamation-triangle", style = "color:#f59e0b; margin-right:6px;"),
            tags$strong("Simulation ran with warnings:"),
            tags$ul(lapply(vresult$warnings, tags$li))
          )
        })
      } else {
        output$pe_validation_box <- renderUI({
          div(class = "pe-validation-box pe-ok",
            icon("check-circle", style = "color:#22c55e; margin-right:6px;"),
            "Simulation complete — all checks passed."
          )
        })
      }

      incProgress(0.4, detail = "Applying policy rules...")
      result <- apply_policy_with_params(policy_input_data, params)
      result <- result %>% mutate(fico_band = case_when(
        fico_score <  600 ~ "<600",
        fico_score <  650 ~ "600-649",
        fico_score <  700 ~ "650-699",
        fico_score <  750 ~ "700-749",
        fico_score >= 750 ~ "750+",
        TRUE              ~ "Unknown"
      ))

      incProgress(0.4, detail = "Computing results...")
      pe_scenario_rv(result)
    })
  })

  # ---------- Reset ----------
  observeEvent(input$pe_reset, {
    for (p in policy_manifest) {
      input_id <- paste0("pe_", p$id)
      if (p$type == "slider") {
        updateSliderInput(session, input_id, value = p$default)
      } else if (p$type == "select") {
        updateSelectInput(session, input_id, selected = as.character(p$default))
      }
    }
    pe_scenario_rv(NULL)
    output$pe_validation_box <- renderUI({ NULL })
  })

  # ---------- KPI Row ----------
  pe_fmt_int <- function(x) {
    if (is.na(x) || is.nan(x)) return("N/A")
    fmt_k(x)
  }
  pe_fmt_pct <- function(x) {
    if (is.na(x) || is.nan(x)) return("N/A")
    paste0(formatC(x * 100, format = "f", digits = 1), "%")
  }
  pe_fmt_dollar <- function(x) {
    if (is.na(x) || is.nan(x)) return("N/A")
    fmt_dollar_k(x)
  }

  pe_kpi_box_dual <- function(label, baseline_val, scenario_val = NULL, fmt_fn = pe_fmt_int) {
    b_str <- fmt_fn(baseline_val)
    if (is.null(scenario_val)) {
      div(
        style = paste0(
          "background:#1f2937; border-radius:8px; padding:12px; color:white;",
          " text-align:center; margin-bottom:10px; min-height:100px;"
        ),
        div(style = "font-size:11px; color:#9ca3af; margin-bottom:6px; text-transform:uppercase; letter-spacing:0.5px;", label),
        div(style = "font-size:22px; font-weight:bold; color:#9ca3af;", b_str),
        div(style = "font-size:10px; color:#6b7280; margin-top:4px;", "Baseline")
      )
    } else {
      s_str <- fmt_fn(scenario_val)
      delta <- scenario_val - baseline_val
      delta_str <- fmt_fn(abs(delta))
      if (!is.na(delta) && !is.nan(delta)) {
        if (delta > 0) delta_str <- paste0("+", delta_str)
        else if (delta < 0) delta_str <- paste0("-", delta_str)
        else delta_str <- "0"
      }
      delta_col <- if (is.na(delta) || is.nan(delta)) "#9ca3af"
                   else if (abs(delta) < 1e-9) "#9ca3af"
                   else if (delta > 0) "#22c55e"
                   else "#ef4444"
      div(
        style = paste0(
          "background:#1f2937; border-radius:8px; padding:12px; color:white;",
          " text-align:center; margin-bottom:10px; min-height:100px;"
        ),
        div(style = "font-size:11px; color:#9ca3af; margin-bottom:6px; text-transform:uppercase; letter-spacing:0.5px;", label),
        div(style = "font-size:22px; font-weight:bold; color:#fb6e36;", s_str),
        div(style = paste0("font-size:12px; color:", delta_col, "; margin-top:4px; font-weight:600;"), delta_str),
        div(style = "font-size:10px; color:#6b7280; margin-top:2px;", paste("Baseline:", b_str))
      )
    }
  }

  output$pe_kpi_row <- renderUI({
    bl_kpis <- pe_baseline_kpis()
    scen <- pe_scenario_rv()
    scen_kpis <- if (!is.null(scen)) compute_kpis(scen) else NULL

    # 6th tile: IRR when tape available, else Decline Volume
    tape <- perf_tape()
    if (!is.null(tape)) {
      bl_irr <- pe_baseline_irr()
      sc_irr <- pe_scenario_irr()
      tile6 <- column(2, pe_kpi_box_dual("Portfolio IRR",
        bl_irr,
        if (!is.null(scen)) sc_irr, pe_fmt_pct))
    } else {
      tile6 <- column(2, pe_kpi_box_dual("Decline Volume",
        bl_kpis$decline_vol,
        if (!is.null(scen_kpis)) scen_kpis$decline_vol, pe_fmt_int))
    }

    fluidRow(
      column(2, pe_kpi_box_dual("Total Apps",
        bl_kpis$total_apps,
        if (!is.null(scen_kpis)) scen_kpis$total_apps, pe_fmt_int)),
      column(2, pe_kpi_box_dual("Approval Rate",
        bl_kpis$approval_rate,
        if (!is.null(scen_kpis)) scen_kpis$approval_rate, pe_fmt_pct)),
      column(2, pe_kpi_box_dual("Avg Offered APR",
        bl_kpis$avg_apr,
        if (!is.null(scen_kpis)) scen_kpis$avg_apr, pe_fmt_pct)),
      column(2, pe_kpi_box_dual("Avg Offered Amt",
        bl_kpis$avg_amount,
        if (!is.null(scen_kpis)) scen_kpis$avg_amount, pe_fmt_dollar)),
      column(2, pe_kpi_box_dual("Bad Rate (Appr)",
        bl_kpis$bad_rate,
        if (!is.null(scen_kpis)) scen_kpis$bad_rate, pe_fmt_pct)),
      tile6
    )
  })

  # ---------- Chart: Decline Reasons ----------
  output$pe_chart_decline <- renderPlotly({
    bl <- pe_baseline_rv()
    req(bl)
    scen <- pe_scenario_rv()

    all_codes <- c("01. Fico", "02. Thin file", "03. Delinquency decline",
                   "04. HighDTI", "05. max_amount_limit")

    b_flags  <- bl$offered_decision_flag
    b_counts <- as.integer(table(factor(b_flags[b_flags != "APPR"], levels = all_codes)))

    if (!is.null(scen)) {
      s_flags  <- scen$offered_decision_flag
      s_counts <- as.integer(table(factor(s_flags[s_flags != "APPR"], levels = all_codes)))
      plot_ly() %>%
        add_trace(x = all_codes, y = b_counts, name = "Baseline", type = "bar",
                  marker = list(color = "#6c757d")) %>%
        add_trace(x = all_codes, y = s_counts, name = "Scenario", type = "bar",
                  marker = list(color = "#fb6e36")) %>%
        layout(barmode = "group",
               xaxis = list(title = "", tickangle = -30),
               yaxis = list(title = "Count"),
               legend = list(orientation = "h", x = 0.3, y = 1.12),
               plot_bgcolor = "white", paper_bgcolor = "white",
               margin = list(b = 80))
    } else {
      plot_ly(x = all_codes, y = b_counts, type = "bar",
              marker = list(color = "#6c757d")) %>%
        layout(xaxis = list(title = "", tickangle = -30),
               yaxis = list(title = "Count"),
               plot_bgcolor = "white", paper_bgcolor = "white",
               margin = list(b = 80))
    }
  })

  # ---------- Chart: APR Distribution ----------
  output$pe_chart_apr <- renderPlotly({
    bl <- pe_baseline_rv()
    req(bl)
    scen <- pe_scenario_rv()
    b_apr <- bl$offered_final_apr[bl$offered_decision_flag == "APPR"]

    if (!is.null(scen)) {
      s_apr <- scen$offered_final_apr[scen$offered_decision_flag == "APPR"]
      plot_ly(alpha = 0.5) %>%
        add_histogram(x = b_apr, name = "Baseline",
                      marker = list(color = "rgba(108,117,125,0.6)")) %>%
        add_histogram(x = s_apr, name = "Scenario",
                      marker = list(color = "rgba(251,110,54,0.6)")) %>%
        layout(barmode = "overlay",
               xaxis = list(title = "APR", tickformat = ".1%"),
               yaxis = list(title = "Count"),
               legend = list(orientation = "h", x = 0.3, y = 1.12),
               plot_bgcolor = "white", paper_bgcolor = "white")
    } else {
      plot_ly(x = b_apr, type = "histogram",
              marker = list(color = "#6c757d")) %>%
        layout(xaxis = list(title = "APR", tickformat = ".1%"),
               yaxis = list(title = "Count"),
               plot_bgcolor = "white", paper_bgcolor = "white")
    }
  })

  # ---------- Chart: Approval Rate by FICO Band ----------
  output$pe_chart_fico <- renderPlotly({
    bl <- pe_baseline_rv()
    req(bl)
    scen <- pe_scenario_rv()
    band_order <- c("<600", "600-649", "650-699", "700-749", "750+")

    b_rates <- bl %>%
      filter(fico_band %in% band_order) %>%
      group_by(fico_band) %>%
      summarise(rate = mean(offered_decision_flag == "APPR", na.rm = TRUE), .groups = "drop") %>%
      mutate(fico_band = factor(fico_band, levels = band_order))

    if (!is.null(scen)) {
      s_rates <- scen %>%
        filter(fico_band %in% band_order) %>%
        group_by(fico_band) %>%
        summarise(rate = mean(offered_decision_flag == "APPR", na.rm = TRUE), .groups = "drop") %>%
        mutate(fico_band = factor(fico_band, levels = band_order))

      plot_ly() %>%
        add_trace(data = b_rates, x = ~fico_band, y = ~rate, name = "Baseline",
                  type = "bar", marker = list(color = "#6c757d")) %>%
        add_trace(data = s_rates, x = ~fico_band, y = ~rate, name = "Scenario",
                  type = "bar", marker = list(color = "#fb6e36")) %>%
        layout(barmode = "group",
               xaxis = list(title = "FICO Band"),
               yaxis = list(title = "Approval Rate", tickformat = ".0%"),
               legend = list(orientation = "h", x = 0.3, y = 1.12),
               plot_bgcolor = "white", paper_bgcolor = "white")
    } else {
      plot_ly(b_rates, x = ~fico_band, y = ~rate, type = "bar",
              marker = list(color = "#6c757d")) %>%
        layout(xaxis = list(title = "FICO Band"),
               yaxis = list(title = "Approval Rate", tickformat = ".0%"),
               plot_bgcolor = "white", paper_bgcolor = "white")
    }
  })

  # ---------- Chart: Grade Distribution ----------
  output$pe_chart_grade <- renderPlotly({
    bl <- pe_baseline_rv()
    req(bl)
    scen <- pe_scenario_rv()
    grade_order  <- c("High", "Med", "Low")
    grade_labels <- c("High (Best)", "Med", "Low (Weakest)")

    b_dist <- bl %>%
      filter(risk_grade %in% grade_order) %>%
      count(risk_grade) %>%
      mutate(risk_grade = factor(risk_grade, levels = grade_order, labels = grade_labels))

    if (!is.null(scen)) {
      s_dist <- scen %>%
        filter(risk_grade %in% grade_order) %>%
        count(risk_grade) %>%
        mutate(risk_grade = factor(risk_grade, levels = grade_order, labels = grade_labels))

      plot_ly() %>%
        add_trace(data = b_dist, x = ~risk_grade, y = ~n, name = "Baseline",
                  type = "bar", marker = list(color = "#6c757d")) %>%
        add_trace(data = s_dist, x = ~risk_grade, y = ~n, name = "Scenario",
                  type = "bar", marker = list(color = "#fb6e36")) %>%
        layout(barmode = "group",
               xaxis = list(title = "Credit Quality Grade"),
               yaxis = list(title = "Count"),
               legend = list(orientation = "h", x = 0.3, y = 1.12),
               plot_bgcolor = "white", paper_bgcolor = "white")
    } else {
      plot_ly(b_dist, x = ~risk_grade, y = ~n, type = "bar",
              marker = list(color = "#6c757d")) %>%
        layout(xaxis = list(title = "Credit Quality Grade"),
               yaxis = list(title = "Count"),
               plot_bgcolor = "white", paper_bgcolor = "white")
    }
  })

  # ---------- Detail Table ----------
  output$pe_detail_table <- renderDT({
    bl <- pe_baseline_rv()
    req(bl)
    scen <- pe_scenario_rv()

    if (is.null(scen)) {
      show_df <- bl %>%
        select(any_of(c("loan_id", "fico_score", "fico_band", "risk_grade",
               "debt_to_income_ratio", "credit_file_age_months",
               "offered_decision_flag", "offered_final_apr",
               "maximum_offered_amount"))) %>%
        head(200)
      datatable(show_df,
                options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
                rownames = FALSE)
    } else {
      .pe_lid <- if ("loan_id" %in% names(bl)) bl$loan_id else seq_len(nrow(bl))
      show_df <- data.frame(
        loan_id       = .pe_lid,
        fico          = bl$fico_score,
        grade_bl      = bl$risk_grade,
        grade_sc      = scen$risk_grade,
        dti           = round(bl$debt_to_income_ratio, 2),
        base_decision = bl$offered_decision_flag,
        scen_decision = scen$offered_decision_flag,
        base_apr      = round(bl$offered_final_apr, 4),
        scen_apr      = round(scen$offered_final_apr, 4),
        base_amount   = round(bl$maximum_offered_amount, 0),
        scen_amount   = round(scen$maximum_offered_amount, 0),
        changed       = bl$offered_decision_flag != scen$offered_decision_flag |
                        bl$risk_grade != scen$risk_grade
      )
      # Show changed records first
      show_df <- show_df %>%
        arrange(desc(changed)) %>%
        head(200)

      datatable(show_df,
                options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
                rownames = FALSE) %>%
        formatStyle("changed",
                    backgroundColor = styleEqual(c(TRUE, FALSE), c("#fff3cd", "white")))
    }
  })
}
