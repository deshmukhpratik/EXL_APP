# ============================================================
# Tab: Fair Lending Auditor
# Evaluates policy fairness across borrower dimensions using
# the 80% disparate impact rule (FFIEC / ECOA guidance).
# ============================================================

fair_lending_ui <- function() {
  tabItem(tabName = "fair_lending",

    header_panel(
      "Fair Lending Auditor",
      "Evaluate policy fairness across borrower dimensions using the 80% disparate impact rule.",
      icon_name = "scale-balanced"
    ),

    # Product selector
    fluidRow(column(12, product_selector_ui("fl"))),

    # Controls card: policy choice + run button
    fluidRow(column(12, div(class = "card",
      fluidRow(
        column(4,
          selectInput("fl_policy", "Policy to Audit",
            choices  = c("Current Baseline" = "baseline"),
            selected = "baseline",
            width = "100%")
        ),
        column(3,
          div(style = "padding-top:25px;",
            actionButton("fl_run", "Run Audit",
              icon  = icon("scale-balanced"),
              class = "custom-button",
              style = "width:100%;")
          )
        ),
        column(5,
          div(style = "padding-top:6px;",
            div(style = paste0(
              "padding:10px 14px; background:#fff8f5;",
              " border-left:3px solid #fb6e36; border-radius:4px;",
              " font-size:12px; color:#555; line-height:1.6;"
            ),
              icon("circle-info", style = "color:#fb6e36; margin-right:4px;"),
              tags$b("80% Rule:"),
              " Any protected-class group whose approval rate falls below 80% of the",
              " highest-rated group's rate is flagged as a potential disparate impact concern."
            )
          )
        )
      )
    ))),

    # Scorecard KPI row
    uiOutput("fl_scorecard"),

    # Dimension summary table
    fluidRow(column(12, div(class = "card",
      h4("Dimension Summary", style = "font-weight:bold; margin-bottom:4px;"),
      p("Click any row to see the full group-level breakdown for that dimension.",
        style = "color:#9ca3af; font-size:12px; margin-bottom:10px;"),
      DT::dataTableOutput("fl_dim_table")
    ))),

    # Drilldown panel (rendered dynamically on row selection)
    uiOutput("fl_drilldown")
  )
}


# ============================================================
# Server
# ============================================================

fair_lending_server <- function(input, output, session,
                                credit_data, policy_input_data,
                                added_policy, policy_fns_rv) {

  # ---- Product selector wiring ----
  fl_selected_product <- reactiveVal("Personal Loan")

  observeEvent(input$fl_product, {
    fl_selected_product(input$fl_product)
  }, ignoreInit = TRUE)

  # ---- Populate policy choices from saved policies ----
  observeEvent(added_policy(), {
    policies <- added_policy()
    if (is.null(policies) || length(policies) == 0) {
      choices <- c("Current Baseline" = "baseline")
    } else {
      extra <- setNames(seq_along(policies),
                        sapply(policies, function(p) p$name %||% paste0("Policy ", seq_along(policies))))
      choices <- c(c("Current Baseline" = "baseline"), extra)
    }
    updateSelectInput(session, "fl_policy", choices = choices,
                      selected = "baseline")
  }, ignoreInit = TRUE)

  # ---- Core computation function ----
  compute_fair_lending <- function(df) {
    # Require the decision column
    if (!"offered_decision_flag" %in% names(df)) return(NULL)
    if (nrow(df) == 0) return(NULL)

    # Derive banding variables
    df$income_band <- cut(
      df$annual_income,
      breaks = c(-Inf, 40000, 60000, 80000, Inf),
      labels = c("<$40K", "$40-60K", "$60-80K", "$80K+"),
      right  = TRUE
    )

    df$file_age_band <- cut(
      df$credit_file_age_months,
      breaks = c(-Inf, 24, 60, 120, Inf),
      labels = c("<24 mo", "24-60 mo", "60-120 mo", "120+ mo"),
      right  = TRUE
    )

    # Dimensions: display_name -> column name in df
    dimensions <- list(
      "Income Band"      = "income_band",
      "Partner Channel"  = "partner_channel",
      "Credit File Age"  = "file_age_band",
      "FICO Band"        = "fico_band",
      "Repeat / New"     = "borrower_repeat_type"
    )

    results <- lapply(names(dimensions), function(dim_name) {
      col <- dimensions[[dim_name]]
      if (!col %in% names(df)) return(NULL)

      grp <- df %>%
        dplyr::group_by(group = as.character(.data[[col]])) %>%
        dplyr::summarise(
          applied  = dplyr::n(),
          approved = sum(offered_decision_flag == "APPR", na.rm = TRUE),
          .groups  = "drop"
        ) %>%
        dplyr::mutate(
          declined      = applied - approved,
          approval_rate = ifelse(applied > 0, approved / applied, NA_real_)
        ) %>%
        dplyr::filter(applied >= 20, !is.na(group), group != "NA")

      if (nrow(grp) == 0) return(NULL)

      best_rate <- max(grp$approval_rate, na.rm = TRUE)

      grp <- grp %>%
        dplyr::mutate(
          ratio_to_best = ifelse(best_rate > 0, approval_rate / best_rate, NA_real_),
          status = dplyr::case_when(
            is.na(ratio_to_best)     ~ "N/A",
            ratio_to_best >= 0.80    ~ "PASS",
            ratio_to_best >= 0.75    ~ "WARN",
            TRUE                     ~ "FAIL"
          )
        )

      list(
        dimension    = dim_name,
        column       = col,
        groups       = grp,
        min_rate     = min(grp$approval_rate, na.rm = TRUE),
        max_rate     = max(grp$approval_rate, na.rm = TRUE),
        ratio        = min(grp$ratio_to_best, na.rm = TRUE),
        worst_status = dplyr::case_when(
          any(grp$status == "FAIL") ~ "FAIL",
          any(grp$status == "WARN") ~ "WARN",
          TRUE                      ~ "PASS"
        )
      )
    })

    Filter(Negate(is.null), results)
  }

  # ---- Reactive: filtered data ----
  fl_data <- reactive({
    df <- credit_data
    df <- df %>% dplyr::filter(product_type == fl_selected_product())
    df
  })

  # ---- Reactive: audit results (set on button click) ----
  fl_results <- reactiveVal(NULL)

  observeEvent(input$fl_run, {
    withProgress(message = "Running Fair Lending Audit...", value = 0, {
      df <- fl_data()
      if (nrow(df) == 0) {
        showNotification("No data available for the selected product.", type = "warning")
        return()
      }
      incProgress(0.5, detail = "Computing approval rates by group...")
      res <- compute_fair_lending(df)
      incProgress(1.0, detail = "Done.")
      if (is.null(res) || length(res) == 0) {
        showNotification("Could not compute fair lending metrics. Check data columns.",
                         type = "error")
        fl_results(NULL)
      } else {
        fl_results(res)
        showNotification(
          paste0("Audit complete: ", length(res), " dimensions analysed."),
          type = "message"
        )
      }
    })
  })

  # ---- Scorecard ----
  output$fl_scorecard <- renderUI({
    res <- fl_results()
    if (is.null(res)) {
      return(fluidRow(column(12,
        div(style = "text-align:center; padding:20px; color:#9ca3af;",
          icon("scale-balanced", style = "font-size:32px; margin-bottom:8px;"),
          p("Click 'Run Audit' to evaluate policy fairness across borrower dimensions.",
            style = "font-size:14px;")
        )
      )))
    }

    statuses <- sapply(res, `[[`, "worst_status")
    n_pass   <- sum(statuses == "PASS")
    n_warn   <- sum(statuses == "WARN")
    n_fail   <- sum(statuses == "FAIL")

    overall_text <- if (n_fail > 0) {
      paste0(n_fail, " FAIL")
    } else if (n_warn > 0) {
      paste0(n_warn, " WARN")
    } else {
      "ALL PASS"
    }
    overall_color <- if (n_fail > 0) "#ef4444" else if (n_warn > 0) "#f59e0b" else "#10b981"

    fluidRow(
      column(3, kpi_box(as.character(n_pass),   "Dimensions: PASS",
                        paste0(n_pass, " of ", length(res), " clean"))),
      column(3, kpi_box(as.character(n_warn),   "Dimensions: WARN",
                        "Ratio 75-79%")),
      column(3, kpi_box(as.character(n_fail),   "Dimensions: FAIL",
                        "Ratio < 75%")),
      column(3,
        div(
          style = paste0(
            "background:#1f2937; border-radius:8px; padding:12px 8px; color:white;",
            " text-align:center; margin-bottom:10px; min-height:90px;"
          ),
          div(style = paste0("font-size:18px; font-weight:bold; color:", overall_color, ";"),
              overall_text),
          div(style = "font-size:10px; color:#9ca3af; margin-top:3px;", "Overall Status"),
          div(style = "font-size:9px; color:#6b7280; margin-top:2px;",
              paste0(length(res), " dimensions reviewed"))
        )
      )
    )
  })

  # ---- Dimension summary table ----
  output$fl_dim_table <- DT::renderDataTable({
    res <- fl_results()
    req(!is.null(res), length(res) > 0)

    summary_df <- do.call(rbind, lapply(res, function(r) {
      data.frame(
        Dimension  = r$dimension,
        `Min Rate` = paste0(round(r$min_rate * 100, 1), "%"),
        `Max Rate` = paste0(round(r$max_rate * 100, 1), "%"),
        `Min Ratio`= paste0(round(r$ratio    * 100, 1), "%"),
        Status     = r$worst_status,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))

    status_colors <- c(
      "PASS" = "#10b981",
      "WARN" = "#f59e0b",
      "FAIL" = "#ef4444",
      "N/A"  = "#9ca3af"
    )

    DT::datatable(
      summary_df,
      selection = "single",
      rownames  = FALSE,
      options   = list(
        pageLength = 10,
        dom        = "t",
        ordering   = FALSE
      )
    ) %>%
      DT::formatStyle(
        "Status",
        backgroundColor = DT::styleEqual(
          names(status_colors),
          unname(status_colors)
        ),
        color      = "white",
        fontWeight = "bold",
        textAlign  = "center",
        borderRadius = "4px"
      )
  })

  # ---- Drilldown panel (dynamic UI based on selected row) ----
  output$fl_drilldown <- renderUI({
    res <- fl_results()
    if (is.null(res)) return(NULL)

    sel <- input$fl_dim_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)

    dim_res <- res[[sel]]
    if (is.null(dim_res)) return(NULL)

    chart_id <- paste0("fl_drill_chart_", sel)
    table_id <- paste0("fl_drill_table_", sel)

    fluidRow(column(12, div(class = "card",
      h4(paste0("Drilldown: ", dim_res$dimension),
         style = "font-weight:bold; margin-bottom:4px;"),
      p("Approval rate by group. Dashed red line = 80% of the best group's rate (disparate impact threshold).",
        style = "color:#9ca3af; font-size:12px; margin-bottom:10px;"),
      fluidRow(
        column(7, plotly::plotlyOutput(chart_id, height = "320px")),
        column(5, DT::dataTableOutput(table_id))
      )
    )))
  })

  # ---- Render drilldown chart + table when selection changes ----
  observe({
    res <- fl_results()
    if (is.null(res)) return()

    sel <- input$fl_dim_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return()
    if (sel > length(res)) return()

    dim_res <- res[[sel]]
    grp     <- dim_res$groups

    best_rate  <- max(grp$approval_rate, na.rm = TRUE)
    threshold  <- best_rate * 0.80

    # Colour bars by status
    bar_colors <- dplyr::case_when(
      grp$status == "FAIL" ~ "#ef4444",
      grp$status == "WARN" ~ "#f59e0b",
      TRUE                 ~ "#10b981"
    )

    chart_id <- paste0("fl_drill_chart_", sel)
    table_id <- paste0("fl_drill_table_", sel)

    output[[chart_id]] <- plotly::renderPlotly({
      p <- plotly::plot_ly(
        data   = grp,
        x      = ~group,
        y      = ~round(approval_rate * 100, 1),
        type   = "bar",
        marker = list(color = bar_colors),
        text   = ~paste0(round(approval_rate * 100, 1), "%"),
        textposition = "outside",
        hovertemplate = paste0(
          "<b>%{x}</b><br>",
          "Approval Rate: %{y:.1f}%<br>",
          "Applied: ", grp$applied, "<br>",
          "Approved: ", grp$approved,
          "<extra></extra>"
        )
      ) %>%
        plotly::add_segments(
          x    = ~min(as.numeric(factor(group))) - 0.5,
          xend = ~max(as.numeric(factor(group))) + 0.5,
          y    = round(threshold * 100, 1),
          yend = round(threshold * 100, 1),
          line = list(color = "#ef4444", dash = "dash", width = 2),
          showlegend = TRUE,
          name = "80% Threshold"
        ) %>%
        plotly::layout(
          xaxis      = list(title = dim_res$dimension, tickangle = -30),
          yaxis      = list(title = "Approval Rate (%)", range = c(0, 110)),
          paper_bgcolor = "#ffffff",
          plot_bgcolor  = "#ffffff",
          font          = list(family = "Source Sans Pro, sans-serif", color = "#334155"),
          margin        = list(l = 50, r = 20, t = 30, b = 60),
          legend        = list(orientation = "h", x = 0, y = -0.25),
          showlegend    = TRUE,
          bargap        = 0.3
        )
      p
    })

    output[[table_id]] <- DT::renderDataTable({
      detail_df <- data.frame(
        Group          = grp$group,
        Applied        = grp$applied,
        Approved       = grp$approved,
        Declined       = grp$declined,
        `Appr. Rate`   = paste0(round(grp$approval_rate * 100, 1), "%"),
        `Ratio to Best`= paste0(round(grp$ratio_to_best * 100, 1), "%"),
        Status         = grp$status,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      status_colors <- c(
        "PASS" = "#10b981",
        "WARN" = "#f59e0b",
        "FAIL" = "#ef4444",
        "N/A"  = "#9ca3af"
      )

      DT::datatable(
        detail_df,
        selection = "none",
        rownames  = FALSE,
        options   = list(
          pageLength = 10,
          dom        = "t",
          ordering   = FALSE,
          scrollX    = TRUE
        )
      ) %>%
        DT::formatStyle(
          "Status",
          backgroundColor = DT::styleEqual(
            names(status_colors),
            unname(status_colors)
          ),
          color      = "white",
          fontWeight = "bold",
          textAlign  = "center"
        )
    })
  })
}
