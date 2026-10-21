# ============================================================
# Tab: Risk Driver Explorer
# Ranks untapped risk factors by predictive power (correlation
# or SHAP importance) and independence from existing policy.
# SHAP button always available if xgboost + shapviz are installed.
# ============================================================

# Check if SHAP packages are available (no env var needed)
.rd_shap_available <- requireNamespace("xgboost", quietly = TRUE) &&
                      requireNamespace("shapviz", quietly = TRUE)

risk_driver_ui <- function() {
      tabItem(tabName = "risk_driver",

        # Scoped CSS fix: picker/select dropdowns on this tab were being
        # clipped by the filter card's overflow:hidden and rendered beneath
        # the KPI tiles / plotly + DT containers below. Lift dropdown panels
        # above sibling chart containers and allow the filter card to overflow.
        tags$head(tags$style(HTML("
          .rd-filter-row, .rd-filter-row .card,
          .rd-filter-row .bootstrap-select,
          .rd-filter-row .shiny-input-container {
            overflow: visible !important;
          }
          .rd-filter-row .dropdown-menu,
          .rd-filter-row .bootstrap-select .dropdown-menu,
          .rd-filter-row .selectize-dropdown {
            z-index: 1060 !important;
          }
          .rd-filter-row .bootstrap-select.open > .dropdown-menu {
            z-index: 1060 !important;
          }
        "))),

        header_panel(
          "Driver Agent",
          "Identify which borrower attributes drive 2+ delinquency risk. Click any factor to see how it separates performing from delinquent loans.",
          icon_name = "chart-column",
          data_scope = "issued_perf"
        ),

        # Product selector + active policy banner (full width)
        fluidRow(column(12,
          product_selector_ui("rd")
        )),
        active_policy_card_ui("rd"),

        scope_toggle_ui("rd_data_scope"),

        # SHAP / Correlation buttons
        fluidRow(column(12,
          div(style = "display:flex; gap:12px; justify-content:center; margin-top:10px;",
            if (.rd_shap_available)
              div(style = "flex:1; text-align:center;",
                actionButton("rd_run_shap", "SHAP",
                  icon = icon("bolt"), class = "btn-lg",
                  style = "background-color:#fb6e36; color:white; border:none; font-weight:bold; font-size:16px; padding:14px 0; width:100%; border-radius:6px;"),
                tags$p(style = "color:#9ca3af; font-size:11px; margin-top:4px;",
                  "Uses XGBoost + SHAP values to rank variables by predictive importance")
              )
            else NULL,
            div(style = "flex:1; text-align:center;",
              actionButton("rd_reset_correlation", "Correlation",
                icon = icon("chart-line"), class = "btn-lg",
                style = "background-color:#4C849B; color:white; border:none; font-weight:bold; font-size:16px; padding:14px 0; width:100%; border-radius:6px;"),
              tags$p(style = "color:#9ca3af; font-size:11px; margin-top:4px;",
                "Uses Pearson correlation to rank variables by linear association with delinquency")
            )
          )
        )),

        fluidRow(column(12, div(class = "card",
          h4("Filters", style = "font-weight:bold; margin-bottom:10px;"),
          fluidRow(
            column(3, dateRangeInput("rd_date_range", "Vintage Date Range",
              start = vs_date_min, end = vs_date_max,
              min = vs_date_min, max = vs_date_max,
              format = "dd/mm/yyyy")),
            column(2, pickerInput("rd_fico_range", "FICO Band",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("rd_grade", "Grade",
              choices  = c("All" = "all", "High", "Med", "Low"),
              selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("rd_partner", "Partner",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(3, pickerInput("rd_repeat_type", "Repeat Type",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, div(style = "padding-top:4px; color:#9ca3af; font-size:12px; font-style:italic;",
              icon("info-circle"),
              " Variable importance and correlation analyses run on the current baseline policy (PL-CURRENT) only."
            ))
          )
        ))),

        # KPI tiles
        fluidRow(
          column(3, uiOutput("rd_kpi_volume")),
          column(2, uiOutput("rd_kpi_conv")),
          column(2, uiOutput("rd_kpi_apr")),
          column(3, uiOutput("rd_kpi_bad_lifetime")),
          column(2, uiOutput("rd_kpi_bad"))
        ),

        # Risk Factor Discovery panel
        fluidRow(column(12, div(class = "card",
          div(style = "margin-bottom: 12px; display:flex; justify-content:space-between; align-items:flex-start;",
            div(
              h4(icon("magnifying-glass-chart"), " Risk Factor Discovery",
                 style = "font-weight:bold; margin-bottom:4px;"),
              uiOutput("rd_discovery_subtitle"),
              div(style = "margin-top:8px; padding:8px 12px; background:#f8fafc; border-radius:6px; font-size:12px; color:#555; line-height:1.6;",
                tags$span(tags$b("Predicts 2+ Delinquency"), " = how strongly this factor separates good loans from bad (higher = stronger signal)", tags$br()),
                tags$span(tags$b("Closest Policy Variable"), " = the existing policy variable that most overlaps with this factor and by how much", tags$br()),
                tags$span(tags$b("Action"), ": ",
                  tags$span("Investigate", style = "color:#2e7d32; font-weight:bold;"), " = adds new information beyond current policy; ",
                  tags$span("Already Captured", style = "color:#9e9e9e; font-weight:bold;"), " = existing variables cover this signal"
                )
              )
            ),
            # Right side: MOB selector + action buttons
            div(style = "display:flex; flex-direction:column; align-items:flex-end; gap:8px; min-width:220px;",
              # MOB selector
              selectInput("rd_mob_target", "2+ Rate Definition",
                choices = c(
                  "2+ Rate @3 MOB"  = "bad_3mob",
                  "2+ Rate @6 MOB"  = "bad_6mob",
                  "2+ Rate @9 MOB"  = "bad_9mob",
                  "2+ Rate @12 MOB" = "bad_12mob",
                  "Lifetime"        = "bad_lifetime"
                ),
                selected = "bad_6mob",
                width = "200px"),
              # Bad rate definition
              div(style = "padding:6px 10px; background:#fff8f5; border-left:3px solid #fb6e36; border-radius:4px; font-size:11px; color:#555; line-height:1.5; width:200px;",
                icon("circle-info", style = "color:#fb6e36; margin-right:3px;"),
                uiOutput("rd_bad_rate_definition", inline = TRUE)
              ),
              # SHAP / Correlation buttons moved to top bar
            )
          ),
          DT::dataTableOutput("rd_discovery_table")
        )))
      )
}

risk_driver_server <- function(input, output, session, credit_data, perf_tape,
                                loan_irr_cache = NULL) {

  # ---- Active policy banner under product selector ----
  wire_active_policy_card(input, output, session, "rd")

  # SHAP state (populated when user clicks "Run SHAP Analysis")
  shap_rv <- reactiveValues(
    importance     = NULL,  # data.frame: variable, mean_abs_shap
    shap_values    = NULL,  # full SHAP matrix (N x p)
    feature_matrix = NULL   # feature values (N x p)
  )

  # Reactive: selected MOB target from dropdown
  rd_target <- reactive({
    input$rd_mob_target %||% "bad_6mob"
  })

  # Dynamic subtitle — changes after SHAP runs
  output$rd_discovery_subtitle <- renderUI({
    msg <- if (!is.null(shap_rv$importance))
      "Untapped risk factors not in current policy, ranked by SHAP importance"
    else
      "Untapped risk factors not in current policy, ranked by predictive power and independence"
    p(msg, style = "color:#777; font-size:13px; margin:0;")
  })

  # Dynamic bad rate definition tooltip
  output$rd_bad_rate_definition <- renderUI({
    target <- rd_target()
    mob_map <- c(bad_3mob = 3L, bad_6mob = 6L, bad_9mob = 9L, bad_12mob = 12L)
    if (target %in% names(mob_map)) {
      mob <- mob_map[[target]]
      tags$span(sprintf("2+ = first 2+ delinquency within %d months of origination.", mob))
    } else {
      tags$span("2+ = ever reached 2+ delinquency during the loan lifetime.")
    }
  })

  # ---- Reset to Correlation button ----
  observeEvent(input$rd_reset_correlation, {
    shap_rv$importance     <- NULL
    shap_rv$shap_values    <- NULL
    shap_rv$feature_matrix <- NULL
    showNotification("Reset to Pearson correlation ranking.", type = "message")
  })

  # ---- SHAP Analysis button handler ----
  observeEvent(input$rd_run_shap, {
    if (!requireNamespace("xgboost", quietly = TRUE) ||
        !requireNamespace("shapviz", quietly = TRUE)) {
      showNotification("xgboost/shapviz packages not available.", type = "error")
      return()
    }

    withProgress(message = "Running SHAP Analysis...", value = 0, {
      df <- rd_data() %>% filter(issued_flag == 1)
      target <- rd_target()

      # Derive target vector
      mob_map <- c(bad_3mob = 3L, bad_6mob = 6L, bad_9mob = 9L, bad_12mob = 12L)
      if (!is.null(target) && target %in% names(mob_map)) {
        mob <- mob_map[[target]]
        df <- df %>% filter(months_on_books >= mob)
        target_vec <- as.numeric(!is.na(df$first_delinquency_mob) &
                                 df$first_delinquency_mob <= mob)
      } else {
        target_vec <- as.numeric(df$performance_delinquency_flag)
      }
      df$tgt <- target_vec
      df <- df %>% filter(!is.na(tgt))

      if (nrow(df) < 50) {
        showNotification("Not enough data (need >= 50 issued loans).", type = "error")
        return()
      }

      incProgress(0.2, detail = "Building feature matrix...")

      valid_vars <- as.character(analysis_vars)[as.character(analysis_vars) %in% names(df)]
      valid_vars <- valid_vars[sapply(valid_vars, function(v) is.numeric(df[[v]]))]
      if (length(valid_vars) < 2) {
        showNotification("Need at least 2 numeric variables.", type = "error")
        return()
      }

      feat_df <- df[, valid_vars, drop = FALSE]
      # Replace NAs with column medians
      for (col in names(feat_df)) {
        na_idx <- is.na(feat_df[[col]])
        if (any(na_idx)) {
          med_val <- median(feat_df[[col]], na.rm = TRUE)
          if (is.na(med_val)) med_val <- 0
          feat_df[[col]][na_idx] <- med_val
        }
      }
      feature_matrix <- as.matrix(feat_df)

      incProgress(0.4, detail = "Training XGBoost model...")

      tryCatch({
        dtrain <- xgboost::xgb.DMatrix(data = feature_matrix, label = df$tgt)
        model <- xgboost::xgb.train(
          params  = list(objective = "binary:logistic", eval_metric = "auc",
                         max_depth = 6, eta = 0.1),
          data    = dtrain,
          nrounds = 100,
          verbose = 0
        )

        incProgress(0.7, detail = "Computing SHAP values...")

        shp <- shapviz::shapviz(model, X_pred = feature_matrix)
        shap_vals <- shapviz::get_shap_values(shp)

        # Mean |SHAP| per feature
        mean_abs <- colMeans(abs(shap_vals))
        imp_df <- data.frame(
          variable      = names(mean_abs),
          mean_abs_shap = as.numeric(mean_abs),
          stringsAsFactors = FALSE
        )

        shap_rv$importance     <- imp_df
        shap_rv$shap_values    <- shap_vals
        shap_rv$feature_matrix <- feature_matrix

        incProgress(1.0, detail = "Done!")
        showNotification("SHAP analysis complete!",
                         type = "message")

      }, error = function(e) {
        showNotification(paste("SHAP analysis failed:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })
  })

  # Product selector (top bar) — uses shared product_selector_ui("rd")
  rd_selected_product <- reactiveVal("Personal Loan")

  observeEvent(input$rd_product, {
    rd_selected_product(input$rd_product)
  }, ignoreInit = TRUE)

  rd_data <- reactive({
    df <- credit_data
    # Product filter
    df <- df %>% filter(product_type == rd_selected_product())
    # Policy regime filter — hardcoded to PL-CURRENT only (no user choice)
    if ("policy_id" %in% names(df)) {
      df <- df %>% filter(policy_id == "PL-CURRENT")
    }
    # Date range filter (keep non-issued loans that have NA dates)
    if (!is.null(input$rd_date_range)) {
      df <- df %>%
        filter(is.na(date) |
               (date >= input$rd_date_range[1] & date <= input$rd_date_range[2]))
    }
    # FICO filter
    if (!("all" %in% input$rd_fico_range) &&
        length(input$rd_fico_range) > 0)
      df <- df %>% filter(fico_band %in% input$rd_fico_range)
    # Grade filter
    if (!("all" %in% input$rd_grade) &&
        length(input$rd_grade) > 0)
      df <- df %>% filter(risk_grade %in% input$rd_grade)
    # Partner filter
    if (!("all" %in% input$rd_partner) &&
        length(input$rd_partner) > 0)
      df <- df %>% filter(partner_channel %in% input$rd_partner)
    # Repeat type filter
    if (!("all" %in% input$rd_repeat_type) &&
        length(input$rd_repeat_type) > 0)
      df <- df %>% filter(borrower_repeat_type %in% input$rd_repeat_type)
    df
  })

  # KPI tiles (always reactive)
  # 1. Issued Amount
  output$rd_kpi_volume <- renderUI({
    vol <- sum(rd_data()$issued_amount, na.rm = TRUE)
    iss <- sum(rd_data()$issued_flag == 1, na.rm = TRUE)
    kpi_box(sprintf("$%.1fM", vol / 1e6), "Issued Amount",
            sprintf("%s loans funded", format(iss, big.mark = ",")))
  })

  # 2. Take Rate
  output$rd_kpi_conv <- renderUI({
    df   <- rd_data()
    appr <- sum(df$offered_decision_flag == "APPR", na.rm = TRUE)
    iss  <- sum(df$issued_flag == 1, na.rm = TRUE)
    kpi_box(sprintf("%.1f%%", 100 * iss / max(appr, 1)), "Take Rate",
            sprintf("%s / %s approved",
                    format(iss, big.mark = ","),
                    format(appr, big.mark = ",")))
  })

  # 3. Average APR
  output$rd_kpi_apr <- renderUI({
    df  <- rd_data() %>% filter(issued_flag == 1)
    avg <- mean(df$interest_rate_apr, na.rm = TRUE)
    kpi_box(sprintf("%.1f%%", avg * 100), "Average APR")
  })

  # 4. Overall Portfolio Bad Rate (lifetime, always shown)
  output$rd_kpi_bad_lifetime <- renderUI({
    df   <- rd_data() %>% filter(issued_flag == 1)
    rate <- mean(df$performance_delinquency_flag, na.rm = TRUE)
    n_bad <- sum(df$performance_delinquency_flag == 1, na.rm = TRUE)
    kpi_box(sprintf("%.1f%%", rate * 100), "Portfolio 2+ Rate",
            sprintf("%s ever 2+", format(n_bad, big.mark = ",")))
  })

  # 5. Bad Rate @MOB (reactive to dropdown)
  output$rd_kpi_bad <- renderUI({
    df     <- rd_data() %>% filter(issued_flag == 1)
    target <- rd_target()
    mob_map <- c(bad_3mob = 3L, bad_6mob = 6L, bad_9mob = 9L, bad_12mob = 12L)
    if (target %in% names(mob_map)) {
      mob <- mob_map[[target]]
      eligible <- df %>% filter(months_on_books >= mob)
      is_bad <- !is.na(eligible$first_delinquency_mob) &
                eligible$first_delinquency_mob <= mob
      rate <- mean(is_bad, na.rm = TRUE)
      n_bad <- sum(is_bad, na.rm = TRUE)
      kpi_box(sprintf("%.1f%%", rate * 100),
              paste0("2+ Rate @", mob, " MOB"),
              sprintf("%s 2+ delinquent", format(n_bad, big.mark = ",")))
    } else {
      rate <- mean(df$performance_delinquency_flag, na.rm = TRUE)
      n_bad <- sum(df$performance_delinquency_flag == 1, na.rm = TRUE)
      kpi_box(sprintf("%.1f%%", rate * 100), "2+ Rate (Lifetime)",
              sprintf("%s ever 2+", format(n_bad, big.mark = ",")))
    }
  })

  # ---- Risk Factor Discovery reactive ----
  rd_discovery <- reactive({
    df <- rd_data() %>% filter(issued_flag == 1)
    target <- rd_target()
    req(nrow(df) > 0)

    # Derive target column using selected MOB
    mob_map <- c(bad_3mob = 3L, bad_6mob = 6L, bad_9mob = 9L, bad_12mob = 12L)
    if (target %in% names(mob_map)) {
      mob <- mob_map[[target]]
      df <- df %>% filter(months_on_books >= mob)
      df$tgt <- as.numeric(!is.na(df$first_delinquency_mob) &
                           df$first_delinquency_mob <= mob)
    } else {
      df$tgt <- as.numeric(df$performance_delinquency_flag)
    }
    df <- df %>% filter(!is.na(tgt))
    if (nrow(df) < 30) return(NULL)

    # Check if SHAP importance is available
    shap_imp <- shap_rv$importance

    all_vars <- as.character(analysis_vars)
    results <- lapply(all_vars, function(v) {
      if (!(v %in% names(df)) || !is.numeric(df[[v]])) return(NULL)
      vals <- df[[v]]
      complete <- !is.na(vals) & !is.na(df$tgt)
      if (sum(complete) < 30) return(NULL)

      # Primary ranking metric: SHAP importance if available, else correlation
      if (!is.null(shap_imp) && v %in% shap_imp$variable) {
        pred_power <- shap_imp$mean_abs_shap[shap_imp$variable == v]
      } else {
        pred_power <- abs(cor(vals[complete], df$tgt[complete], use = "complete.obs"))
      }
      if (is.na(pred_power)) return(NULL)

      # Max correlation with any policy variable (excluding self) — track which one
      max_pol_cor <- 0
      max_pol_var <- ""
      for (pv in policy_vars) {
        if (pv == v) next
        if (pv %in% names(df) && is.numeric(df[[pv]])) {
          pc <- abs(cor(vals, df[[pv]], use = "complete.obs"))
          if (!is.na(pc) && pc > max_pol_cor) {
            max_pol_cor <- pc
            max_pol_var <- pv
          }
        }
      }

      incr_value <- pred_power * (1 - max_pol_cor)

      data.frame(
        variable       = v,
        pretty_name    = pretty_var(v),
        in_policy      = v %in% policy_vars,
        pred_power     = round(pred_power, 4),
        max_policy_cor = round(max_pol_cor, 4),
        max_policy_var = pretty_var(max_pol_var),
        incr_value     = round(incr_value, 4),
        stringsAsFactors = FALSE
      )
    })

    result_df <- do.call(rbind, Filter(Negate(is.null), results))
    if (is.null(result_df) || nrow(result_df) == 0) return(NULL)
    # Only show untapped factors (not already in policy)
    result_df <- result_df[!result_df$in_policy, ]
    if (nrow(result_df) == 0) return(NULL)
    result_df <- result_df[order(-result_df$incr_value), ]
    result_df
  })

  output$rd_discovery_table <- DT::renderDataTable({
    disc <- rd_discovery()
    req(!is.null(disc), nrow(disc) > 0)

    # Build readable "Overlap With" column: "38% with Risk Score 3"
    overlap_desc <- sprintf("%.0f%% with %s",
                            disc$max_policy_cor * 100,
                            disc$max_policy_var)

    # Recommendation based on whether policy already captures the signal.
    # Thresholds chosen so a top tier (~3-5 vars with the highest incremental
    # predictive power) lands in "Investigate" and a sizable middle tier lands
    # in "Marginal" — this gives the user visible signal that the model is
    # actively scoring drivers rather than rounding everything to one bucket.
    rec <- ifelse(
      disc$max_policy_cor >= 0.90, "Already Captured",
      ifelse(disc$incr_value >= 0.040, "Investigate",
        ifelse(disc$incr_value >= 0.005, "Marginal", "Already Captured"))
    )

    display_df <- data.frame(
      `Untapped Variable`      = disc$pretty_name,
      `Predicts 2+ Delinquency` = sprintf("%.1f%%", disc$pred_power * 100),
      `Closest Policy Variable` = overlap_desc,
      `Action`                  = rec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      display_df,
      selection = "single",
      rownames  = FALSE,
      escape    = FALSE,
      options   = list(
        pageLength = 12,
        dom        = "t",
        ordering   = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = 3),
          list(width = "200px", targets = 2)
        )
      )
    ) %>%
      DT::formatStyle("Action",
        color = DT::styleEqual(
          c("Investigate", "Marginal", "Already Captured"),
          c("#2e7d32", "#f57f17", "#9e9e9e")
        ),
        fontWeight = "bold"
      )
  })

  # ---- NL Q&A current-view registration ----
  # rd_data() already applies all user slicers (product, policy regime, date
  # range, FICO band, risk grade, partner channel, repeat type) so we use it
  # directly as the current-view for the NL bar.
  rd_nl_current_view <- reactive({
    base <- rd_data()
    if (is.null(base) || !is.data.frame(base) || nrow(base) == 0) return(loan_data)
    base
  })

  nl_register_current_view(session, "risk_driver", rd_nl_current_view)
}
