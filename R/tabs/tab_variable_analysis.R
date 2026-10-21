# ============================================================
# Tab: Variable Analysis
# Extracted from app.R — UI and server logic
# (includes vs_heatmap output, relocated from Policy Repo block)
#
# When .cpt_shap_available is TRUE, this tab gains:
#   - "Run SHAP Analysis" button in Discovery panel
#   - SHAP-based variable ranking (replaces heuristic correlation)
#   - SHAP dependence scatter alongside bivariate bar chart
#   - Auto-selection of top 3 untapped variables after SHAP runs
# ============================================================

# Check if AI mode is available (xgboost + shapviz installed)
.vs_ai_mode <- exists(".cpt_shap_available") && isTRUE(.cpt_shap_available)

var_analysis_ui <- function() {
      tabItem(tabName = "var_analysis",

        header_panel(
          "Variable Selector",
          "Discover high-value variables not yet in your policy. Click a variable to explore its performance.",
          icon_name = "magnifying-glass-chart"
        ),

        scope_toggle_ui("vs_data_scope"),

        # Product selector (below header)
        fluidRow(column(12, product_selector_ui("vs"))),

        fluidRow(column(12, div(class = "card",
          h4("Filters", style = "font-weight:bold; margin-bottom:10px;"),
          fluidRow(
            column(3, dateRangeInput("vs_date_range", "Vintage Date Range",
              start = vs_date_min, end = vs_date_max,
              min = vs_date_min, max = vs_date_max,
              format = "dd/mm/yyyy")),
            column(2, pickerInput("vs_fico_range", "FICO Band",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("vs_grade", "Grade",
              choices  = c("All" = "all", "High", "Med", "Low"),
              selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("vs_partner", "Partner",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("vs_repeat_type", "Repeat Type",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE)))
          )
        ))),

        # KPI tiles
        fluidRow(
          column(3, uiOutput("vs_kpi_volume")),
          column(2, uiOutput("vs_kpi_conv")),
          column(2, uiOutput("vs_kpi_apr")),
          column(3, uiOutput("vs_kpi_bad_lifetime")),
          column(2, uiOutput("vs_kpi_bad"))
        ),

        # Variable Discovery panel
        fluidRow(column(12, div(class = "card",
          div(style = "margin-bottom: 12px; display:flex; justify-content:space-between; align-items:flex-start;",
            div(
              h4(icon("magnifying-glass-chart"), " Variable Discovery",
                 style = "font-weight:bold; margin-bottom:4px;"),
              p(if (.vs_ai_mode)
                  "Untapped variables not in current policy, ranked by SHAP importance"
                else
                  "Untapped variables not in current policy, ranked by predictive power and independence",
                style = "color:#777; font-size:13px; margin:0;"),
              div(style = "margin-top:8px; padding:8px 12px; background:#f8fafc; border-radius:6px; font-size:12px; color:#555; line-height:1.6;",
                tags$span(tags$b("Bad Rate Correlation"), " = how strongly this variable predicts loan defaults (higher is better)", tags$br()),
                tags$span(tags$b("Max Policy Correlation"), " = overlap with variables already in your policy \u2014 high values mean the variable is redundant", tags$br()),
                tags$span(tags$b("Incremental Value"), " = predictive power discounted by redundancy \u2014 the net benefit of adding this variable")
              )
            ),
            if (.vs_ai_mode)
              actionButton("vs_run_shap", "Run SHAP Analysis",
                class = "custom-button", icon = icon("bolt"))
            else NULL
          ),
          DT::dataTableOutput("vs_discovery_table")
        ))),

        # Analysis panel — variable selectors
        fluidRow(column(12, div(class = "card",
          h4("Variable Analysis Settings", style = "font-weight:bold;"),
          fluidRow(
            column(4, pickerInput("vs_vars",
              "Select Variables:",
              choices  = analysis_vars,
              selected = as.character(candidate_vars),
              multiple = TRUE,
              options  = list(`actions-box` = TRUE))),
            column(3, selectInput("vs_target", "Bad Rate Definition",
              choices = c(
                "Bad Rate"            = "bad_lifetime",
                "Bad Rate @3 MOB"     = "bad_3mob",
                "Bad Rate @6 MOB"     = "bad_6mob",
                "Bad Rate @9 MOB"     = "bad_9mob",
                "Bad Rate @12 MOB"    = "bad_12mob"
              ),
              selected = "bad_6mob"))
          )
        ))),

        # Analysis type cards
        tags$style(HTML("
          .analysis-card {
            border: 2px solid #ddd; border-radius: 8px; padding: 18px 20px;
            text-align: center; cursor: pointer; transition: all 0.2s;
            background: #fff;
          }
          .analysis-card:hover { border-color: #fb6e36; background: #fff8f5; }
          .analysis-card.active-card { border-color: #fb6e36; background: #fff3ec; }
          .analysis-card h5 { margin: 0 0 4px 0; font-weight: bold; }
          .analysis-card p  { margin: 0; color: #777; font-size: 13px; }
        ")),
        fluidRow(
          column(6,
            div(class = "analysis-card active-card", id = "vs_biv_card",
              style = "cursor:pointer;",
              onclick = "Shiny.setInputValue('vs_bivariate_btn', Math.random());",
              h5(icon("chart-line"), " Bivariate Analysis"),
              p("One variable vs target — distribution & bad rate")
            )
          ),
          column(6,
            div(class = "analysis-card", id = "vs_multi_card",
              style = "cursor:pointer;",
              onclick = "Shiny.setInputValue('vs_multivariate_btn', Math.random());",
              h5(icon("project-diagram"), " Multivariate Analysis"),
              p("Correlation matrix & multi-variable risk heatmap")
            )
          )
        ),
        br(),

        # Results
        hidden(div(id = "vs_results",
          div(id = "vs_bivariate_panel",
            fluidRow(column(12, uiOutput("vs_bivariate_insight"))),
            fluidRow(
              column(6, div(class = "card",
                plotlyOutput("vs_bivariate_chart", height = "420px")
              )),
              column(6, div(class = "card",
                plotlyOutput("vs_shap_scatter", height = "420px")
              ))
            )
          ),

          # Multivariate extras
          hidden(div(id = "vs_multivariate_panel",
            fluidRow(
              column(12, div(class = "card",
                h4("Correlation Matrix", style = "font-weight:bold;"),
                uiOutput("vs_correlation_insight"),
                plotlyOutput("vs_correlation_matrix", height = "300px")))),
            fluidRow(
              column(12, div(class = "card",
                h4("Variable Pair Risk Heatmap",
                   style = "font-weight:bold;"),
                uiOutput("vs_heatmap_insight"),
                fluidRow(
                  column(3, selectInput("vs_x_var", "Variable 1",
                    choices = analysis_vars, selected = "fico_score")),
                  column(3, selectInput("vs_y_var", "Variable 2",
                    choices = analysis_vars,
                    selected = "debt_to_income_ratio"))
                ),
                plotlyOutput("vs_heatmap", height = "300px")
              )))
          ))
        ))
      )
}

var_analysis_server <- function(input, output, session, credit_data, perf_tape,
                                loan_irr_cache = NULL) {
  vs_analysis_type <- reactiveVal("Bivariate")
  vs_discovery_var <- reactiveVal(NULL)  # track variable clicked from discovery table

  # SHAP state (populated when user clicks "Run SHAP Analysis" in AI mode)
  shap_rv <- reactiveValues(
    importance     = NULL,  # data.frame: variable, mean_abs_shap
    shap_values    = NULL,  # full SHAP matrix (N x p)
    feature_matrix = NULL   # feature values (N x p)
  )

  # ---- SHAP Analysis button handler (AI mode only) ----
  observeEvent(input$vs_run_shap, {
    if (!requireNamespace("xgboost", quietly = TRUE) ||
        !requireNamespace("shapviz", quietly = TRUE)) {
      showNotification("xgboost/shapviz packages not available.", type = "error")
      return()
    }

    withProgress(message = "Running SHAP Analysis...", value = 0, {
      df <- vs_data() %>% filter(issued_flag == 1)
      target <- input$vs_target

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

        # Auto-select up to 3 top untapped variables
        untapped <- imp_df[!(imp_df$variable %in% policy_vars), ]
        untapped <- untapped[order(-untapped$mean_abs_shap), ]
        top_untapped <- head(untapped$variable, 3)
        if (length(top_untapped) > 0) {
          updatePickerInput(session, "vs_vars", selected = top_untapped)
        }

        incProgress(1.0, detail = "Done!")
        showNotification("SHAP analysis complete! Top untapped variables auto-selected.",
                         type = "message")

      }, error = function(e) {
        showNotification(paste("SHAP analysis failed:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })
  })

  observeEvent(input$vs_bivariate_btn, {
    vs_analysis_type("Bivariate")
    runjs("$('#vs_biv_card').addClass('active-card'); $('#vs_multi_card').removeClass('active-card');")
  })
  observeEvent(input$vs_multivariate_btn, {
    vs_analysis_type("Multivariate")
    runjs("$('#vs_multi_card').addClass('active-card'); $('#vs_biv_card').removeClass('active-card');")
  })

  # Product selector (top bar) — uses shared product_selector_ui("vs")
  vs_selected_product <- reactiveVal("Personal Loan")

  observeEvent(input$vs_product, {
    vs_selected_product(input$vs_product)
  }, ignoreInit = TRUE)

  vs_data <- reactive({
    df <- credit_data
    # Product filter
    df <- df %>% filter(product_type == vs_selected_product())
    # Date range filter (keep non-issued loans that have NA dates)
    if (!is.null(input$vs_date_range)) {
      df <- df %>%
        filter(is.na(date) |
               (date >= input$vs_date_range[1] & date <= input$vs_date_range[2]))
    }
    # FICO filter
    if (!("all" %in% input$vs_fico_range) &&
        length(input$vs_fico_range) > 0)
      df <- df %>% filter(fico_band %in% input$vs_fico_range)
    # Grade filter
    if (!("all" %in% input$vs_grade) &&
        length(input$vs_grade) > 0)
      df <- df %>% filter(risk_grade %in% input$vs_grade)
    # Partner filter
    if (!("all" %in% input$vs_partner) &&
        length(input$vs_partner) > 0)
      df <- df %>% filter(partner_channel %in% input$vs_partner)
    # Repeat type filter
    if (!("all" %in% input$vs_repeat_type) &&
        length(input$vs_repeat_type) > 0)
      df <- df %>% filter(borrower_repeat_type %in% input$vs_repeat_type)
    df
  })

  # KPI tiles (always reactive)
  # 1. Issued Amount
  output$vs_kpi_volume <- renderUI({
    vol <- sum(vs_data()$issued_amount, na.rm = TRUE)
    iss <- sum(vs_data()$issued_flag == 1, na.rm = TRUE)
    kpi_box(sprintf("$%.1fM", vol / 1e6), "Issued Amount",
            sprintf("%s loans funded", format(iss, big.mark = ",")))
  })

  # 2. Take Rate
  output$vs_kpi_conv <- renderUI({
    df   <- vs_data()
    appr <- sum(df$offered_decision_flag == "APPR", na.rm = TRUE)
    iss  <- sum(df$issued_flag == 1, na.rm = TRUE)
    kpi_box(sprintf("%.1f%%", 100 * iss / max(appr, 1)), "Take Rate",
            sprintf("%s / %s approved",
                    format(iss, big.mark = ","),
                    format(appr, big.mark = ",")))
  })

  # 3. Average APR
  output$vs_kpi_apr <- renderUI({
    df  <- vs_data() %>% filter(issued_flag == 1)
    avg <- mean(df$interest_rate_apr, na.rm = TRUE)
    kpi_box(sprintf("%.1f%%", avg * 100), "Average APR")
  })

  # 4. Overall Portfolio Bad Rate (lifetime, always shown)
  output$vs_kpi_bad_lifetime <- renderUI({
    df   <- vs_data() %>% filter(issued_flag == 1)
    rate <- mean(df$performance_delinquency_flag, na.rm = TRUE)
    n_bad <- sum(df$performance_delinquency_flag == 1, na.rm = TRUE)
    kpi_box(sprintf("%.1f%%", rate * 100), "Portfolio Bad Rate",
            sprintf("%s ever-delinquent", format(n_bad, big.mark = ",")))
  })

  # 5. Bad Rate @MOB (dynamic based on dropdown selection)
  output$vs_kpi_bad <- renderUI({
    df     <- vs_data() %>% filter(issued_flag == 1)
    target <- input$vs_target
    mob_map <- c(bad_3mob = 3L, bad_6mob = 6L, bad_9mob = 9L, bad_12mob = 12L)
    if (!is.null(target) && target %in% names(mob_map)) {
      mob <- mob_map[[target]]
      eligible <- df %>% filter(months_on_books >= mob)
      is_bad <- !is.na(eligible$first_delinquency_mob) &
                eligible$first_delinquency_mob <= mob
      rate <- mean(is_bad, na.rm = TRUE)
      n_bad <- sum(is_bad, na.rm = TRUE)
      kpi_box(sprintf("%.1f%%", rate * 100),
              paste0("Bad Rate @", mob, " MOB"),
              sprintf("%s delinquent", format(n_bad, big.mark = ",")))
    } else {
      rate <- mean(df$performance_delinquency_flag, na.rm = TRUE)
      kpi_box(sprintf("%.1f%%", rate * 100), "Bad Rate (Lifetime)")
    }
  })

  # ---- Variable Discovery reactive ----
  vs_discovery <- reactive({
    df <- vs_data() %>% filter(issued_flag == 1)
    target <- input$vs_target
    req(nrow(df) > 0, target)

    # Derive target column using same MOB logic
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

      # Max correlation with any policy variable (excluding self)
      max_pol_cor <- 0
      for (pv in policy_vars) {
        if (pv == v) next
        if (pv %in% names(df) && is.numeric(df[[pv]])) {
          pc <- abs(cor(vals, df[[pv]], use = "complete.obs"))
          if (!is.na(pc)) max_pol_cor <- max(max_pol_cor, pc)
        }
      }

      incr_value <- pred_power * (1 - max_pol_cor)

      data.frame(
        variable       = v,
        pretty_name    = pretty_var(v),
        in_policy      = v %in% policy_vars,
        pred_power     = round(pred_power, 4),
        max_policy_cor = round(max_pol_cor, 4),
        incr_value     = round(incr_value, 4),
        stringsAsFactors = FALSE
      )
    })

    result_df <- do.call(rbind, Filter(Negate(is.null), results))
    if (is.null(result_df) || nrow(result_df) == 0) return(NULL)
    # Only show untapped variables (not already in policy)
    result_df <- result_df[!result_df$in_policy, ]
    if (nrow(result_df) == 0) return(NULL)
    result_df <- result_df[order(-result_df$incr_value), ]
    result_df
  })

  output$vs_discovery_table <- DT::renderDataTable({
    disc <- vs_discovery()
    req(!is.null(disc), nrow(disc) > 0)

    # Column label adapts to whether SHAP was used
    power_label <- if (!is.null(shap_rv$importance)) "SHAP Importance" else "Bad Rate Correlation"

    display_df <- data.frame(
      Variable              = disc$pretty_name,
      power_col             = disc$pred_power,
      `Max Policy Correlation` = disc$max_policy_cor,
      `Incremental Value`   = disc$incr_value,
      Recommendation        = ifelse(
        disc$max_policy_cor >= 0.10, "Redundant",
        ifelse(disc$incr_value >= 0.02, "High Value",
          ifelse(disc$incr_value >= 0.005, "Moderate", "Redundant"))
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(display_df)[2] <- power_label

    DT::datatable(
      display_df,
      selection = "single",
      rownames  = FALSE,
      options   = list(
        pageLength = 12,
        dom        = "t",
        ordering   = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = 4)
        )
      )
    ) %>%
      DT::formatStyle(power_label,
        background = DT::styleColorBar(range(display_df[[power_label]], na.rm = TRUE),
                                        "#fb6e36"),
        backgroundSize  = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "left"
      ) %>%
      DT::formatStyle("Max Policy Correlation",
        background = DT::styleColorBar(range(display_df$`Max Policy Correlation`, na.rm = TRUE),
                                        "#4C849B"),
        backgroundSize  = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "left"
      ) %>%
      DT::formatStyle("Incremental Value",
        background = DT::styleColorBar(range(display_df$`Incremental Value`, na.rm = TRUE),
                                        "#2e7d32"),
        backgroundSize  = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "left"
      ) %>%
      DT::formatStyle("Recommendation",
        color = DT::styleEqual(
          c("High Value", "Moderate", "Redundant"),
          c("#2e7d32", "#f57f17", "#9e9e9e")
        ),
        fontWeight = "bold"
      ) %>%
      DT::formatPercentage(c(power_label, "Max Policy Correlation",
                              "Incremental Value"), digits = 1)
  })

  # Click-to-select: clicking a discovery row selects that variable
  observeEvent(input$vs_discovery_table_rows_selected, {
    row_idx <- input$vs_discovery_table_rows_selected
    disc <- vs_discovery()
    req(!is.null(disc), length(row_idx) == 1, row_idx <= nrow(disc))

    selected_var <- disc$variable[row_idx]
    current_sel  <- input$vs_vars
    if (!(selected_var %in% current_sel)) {
      current_sel <- c(current_sel, selected_var)
    }

    updatePickerInput(session, "vs_vars", selected = current_sel)

    # Store clicked variable so bivariate chart shows it first
    vs_discovery_var(selected_var)

    # Switch to bivariate view
    vs_analysis_type("Bivariate")
    showElement("vs_bivariate_panel")
    hideElement("vs_multivariate_panel")
    runjs("$('#vs_biv_card').addClass('active-card'); $('#vs_multi_card').removeClass('active-card');")
  })

  observe({
    vars    <- input$vs_vars
    target  <- input$vs_target
    atype   <- vs_analysis_type()

    req(length(vars) > 0, target)

    showElement("vs_results")
    df      <- vs_data()

    # Derive bad rate column based on MOB selection
    mob_map <- c(bad_3mob = 3L, bad_6mob = 6L, bad_9mob = 9L, bad_12mob = 12L)
    if (target %in% names(mob_map)) {
      mob_cutoff <- mob_map[[target]]
      df$.vs_target <- ifelse(
        df$issued_flag == 1 & df$months_on_books >= mob_cutoff,
        as.integer(!is.na(df$first_delinquency_mob) &
                   df$first_delinquency_mob <= mob_cutoff),
        NA_integer_
      )
      tgt_col   <- ".vs_target"
      tgt_label <- paste0("Bad Rate @", mob_cutoff, " MOB")
    } else {
      df$.vs_target <- df$performance_delinquency_flag
      df$.vs_target[df$issued_flag != 1] <- NA_integer_
      tgt_col   <- ".vs_target"
      tgt_label <- "Bad Rate"
    }

    # --- BIVARIATE ---
    if (atype == "Bivariate" || length(vars) == 1) {
      hideElement("vs_multivariate_panel")
      showElement("vs_bivariate_panel")
      # Use discovery-selected variable if set and still in selection
      disc_var <- isolate(vs_discovery_var())
      if (!is.null(disc_var) && disc_var %in% vars) {
        var <- disc_var
        isolate(vs_discovery_var(NULL))  # consume once
      } else {
        var <- vars[1]
      }

      df_clean <- df %>%
        filter(!is.na(.vs_target), !is.na(.data[[var]]))

      shiny::validate(need(nrow(df_clean) >= 20, "Not enough data for analysis."))

      # Bin numeric variable with human-readable labels
      fmt_num <- function(v) {
        if (abs(v) >= 1e6) sprintf("%.1fM", v / 1e6)
        else if (abs(v) >= 1e3) sprintf("%.0fK", v / 1e3)
        else if (abs(v) < 1 && v != 0) sprintf("%.2f", v)
        else sprintf("%.0f", v)
      }
      if (is.numeric(df_clean[[var]])) {
        brks <- unique(quantile(df_clean[[var]],
                                probs = seq(0, 1, 0.1), na.rm = TRUE))
        if (length(brks) < 3) brks <- NULL
        if (!is.null(brks)) {
          n_bins <- length(brks) - 1
          bin_labels <- sapply(seq_len(n_bins), function(i) {
            paste0(fmt_num(brks[i]), "-", fmt_num(brks[i + 1]))
          })
          df_clean$bin <- cut(df_clean[[var]], breaks = brks,
                              include.lowest = TRUE, labels = bin_labels)
        } else {
          df_clean$bin <- as.factor(df_clean[[var]])
        }
      } else {
        df_clean$bin <- as.factor(df_clean[[var]])
      }

      df_clean$tgt_num <- as.numeric(df_clean$.vs_target)

      summary_df <- df_clean %>%
        group_by(bin) %>%
        summarise(n = n(), rate = mean(tgt_num, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(bin) %>%
        filter(!is.na(bin))

      # Dynamic insight for bivariate chart
      output$vs_bivariate_insight <- renderUI({
        req(nrow(summary_df) >= 2)
        min_row <- summary_df[which.min(summary_df$rate), ]
        max_row <- summary_df[which.max(summary_df$rate), ]
        spread  <- abs(max_row$rate - min_row$rate)
        first_rate <- summary_df$rate[1]
        last_rate  <- summary_df$rate[nrow(summary_df)]
        strength <- if (spread >= 0.05) "strong" else if (spread >= 0.02) "moderate" else "weak"

        # Describe the actual pattern rather than assuming monotonic
        if (abs(first_rate - last_rate) < 0.005) {
          pattern <- "is relatively flat across deciles with no clear trend"
        } else if (first_rate < last_rate) {
          pattern <- sprintf("rises from %.1f%% (lowest decile) to %.1f%% (highest decile)",
                             first_rate * 100, last_rate * 100)
        } else {
          pattern <- sprintf("falls from %.1f%% (lowest decile) to %.1f%% (highest decile)",
                             first_rate * 100, last_rate * 100)
        }

        msg <- sprintf(
          "%s shows %s predictive separation \u2014 %s %s, with a peak of %.1f%% and a low of %.1f%% (%.1fpp spread).",
          pretty_var(var), strength, tgt_label, pattern,
          max_row$rate * 100, min_row$rate * 100, spread * 100
        )
        div(style = "padding:6px 10px; margin-bottom:8px; background:#fff8f5; border-left:3px solid #fb6e36; border-radius:4px; font-size:13px; color:#444;",
          icon("lightbulb", style = "color:#fb6e36; margin-right:4px;"), msg)
      })

      output$vs_bivariate_chart <- renderPlotly({
        bin_order <- levels(summary_df$bin)
        plot_ly(summary_df, x = ~bin) %>%
          add_bars(y = ~rate, name = tgt_label,
                   marker = list(color = "#fb6e36")) %>%
          add_lines(y = ~rate, name = "Trend",
                    line = list(color = "#1f2937", width = 2)) %>%
          layout(
            title  = list(text = paste(tgt_label, "by", pretty_var(var)),
                          font = list(size = 14)),
            xaxis  = list(title = pretty_var(var), tickangle = -30,
                          categoryorder = "array", categoryarray = bin_order),
            yaxis  = list(title = tgt_label, tickformat = ".1%"),
            legend = list(orientation = "h", x = 0.5, xanchor = "center",
                          y = -0.25, font = list(size = 11)),
            paper_bgcolor = "white", plot_bgcolor = "white"
          )
      })

      # ---- Scatter / SHAP Dependence (right panel) ----
      output$vs_shap_scatter <- renderPlotly({
        shap_vals <- shap_rv$shap_values
        feat_mat  <- shap_rv$feature_matrix

        # If SHAP not yet computed, show loan distribution scatter
        if (is.null(shap_vals) || is.null(feat_mat) ||
            !(var %in% colnames(shap_vals))) {
          vol_df <- summary_df
          vol_df$bad  <- round(vol_df$n * vol_df$rate)
          vol_df$good <- vol_df$n - vol_df$bad
          bin_order <- levels(summary_df$bin)
          plot_ly(vol_df, x = ~bin, y = ~good, type = "bar",
                  name = "Good", marker = list(color = "#4C849B"),
                  hovertemplate = paste0(
                    pretty_var(var), ": %{x}<br>",
                    "Good Loans: %{y:,}<extra></extra>")) %>%
            add_trace(y = ~bad, name = "Bad",
                      marker = list(color = "#e53935"),
                      hovertemplate = paste0(
                        pretty_var(var), ": %{x}<br>",
                        "Bad Loans: %{y:,}<extra></extra>")) %>%
            layout(
              barmode = "stack",
              title  = list(text = paste("Loan Volume:", pretty_var(var)),
                            font = list(size = 14)),
              xaxis  = list(title = pretty_var(var), tickangle = -20,
                            categoryorder = "array", categoryarray = bin_order),
              yaxis  = list(title = "Number of Loans"),
              legend = list(orientation = "h", x = 0.5, xanchor = "center",
                            y = -0.25, font = list(size = 11)),
              paper_bgcolor = "white", plot_bgcolor = "white"
            )
        } else {
          # Sample for performance
          n <- nrow(feat_mat)
          if (n > 5000) {
            set.seed(123)
            idx <- sample(n, 5000)
          } else {
            idx <- seq_len(n)
          }

          dep_df <- data.frame(
            feature_val = feat_mat[idx, var],
            shap_val    = shap_vals[idx, var],
            stringsAsFactors = FALSE
          )

          p <- ggplot(dep_df, aes(x = feature_val, y = shap_val)) +
            geom_point(alpha = 0.25, size = 1.2, color = "#fb6e36") +
            geom_smooth(method = "loess", se = FALSE, color = "#1f2937",
                        linewidth = 1, span = 0.5) +
            labs(x = pretty_var(var),
                 y = paste0("SHAP Value"),
                 title = paste0("SHAP Dependence: ", pretty_var(var))) +
            theme_custom +
            theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

          plotly::ggplotly(p, tooltip = c("x", "y")) %>%
            plotly::layout(
              paper_bgcolor = "#FFFFFF",
              plot_bgcolor  = "#FFFFFF",
              font = list(family = "Source Sans Pro, sans-serif",
                          color = "#334155"),
              margin = list(l = 60, r = 20, t = 40, b = 60)
            )
        }
      })

    } else {
      # --- MULTIVARIATE ---
      hideElement("vs_bivariate_panel")
      showElement("vs_multivariate_panel")

      numeric_vars <- vars[sapply(vars,
        function(v) v %in% names(df) && is.numeric(df[[v]]))]

      shiny::validate(need(length(numeric_vars) >= 2,
        "Select at least 2 numeric variables for multivariate analysis."))

      df_mat <- df[, numeric_vars, drop = FALSE]
      df_mat <- df_mat[complete.cases(df_mat), ]

      cor_mat   <- round(cor(df_mat), 3)
      var_names <- sapply(colnames(cor_mat), pretty_var)

      output$vs_bivariate_chart <- renderPlotly({
        plot_ly() %>%
          layout(paper_bgcolor = "white",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE))
      })

      output$vs_shap_scatter <- renderPlotly({
        plot_ly() %>%
          layout(paper_bgcolor = "white",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE))
      })

      # Dynamic insight for correlation matrix
      output$vs_correlation_insight <- renderUI({
        n <- ncol(cor_mat)
        if (n < 2) return(NULL)
        tmp_cor <- cor_mat
        diag(tmp_cor) <- NA
        abs_cor <- abs(tmp_cor)
        max_idx <- which(abs_cor == max(abs_cor, na.rm = TRUE), arr.ind = TRUE)[1, ]
        v1 <- var_names[max_idx[1]]; v2 <- var_names[max_idx[2]]
        val <- cor_mat[max_idx[1], max_idx[2]]
        direction <- if (val > 0) "positively" else "negatively"
        msg <- sprintf(
          "%s and %s are the most %s correlated pair (r = %.2f) \u2014 %s.",
          v1, v2, direction, val,
          if (abs(val) >= 0.7) "adding both to policy offers limited incremental value"
          else "they capture different risk dimensions and complement each other well"
        )
        div(style = "padding:6px 10px; margin-bottom:8px; background:#f0f7fa; border-left:3px solid #4C849B; border-radius:4px; font-size:13px; color:#444;",
          icon("lightbulb", style = "color:#4C849B; margin-right:4px;"), msg)
      })

      output$vs_correlation_matrix <- renderPlotly({
        plot_ly(
          z         = cor_mat,
          x         = var_names,
          y         = var_names,
          type      = "heatmap",
          colorscale = list(
            c(0.0, "#4C849B"),
            c(0.5, "#ffffff"),
            c(1.0, "#fb6e36")
          ),
          zmin = -1, zmax = 1,
          colorbar = list(title = list(text = "Correlation", font = list(size = 12))),
          text = cor_mat,
          texttemplate = "%{text:.2f}",
          textfont = list(size = 12),
          hovertemplate = paste0(
            "Row: %{y}<br>",
            "Column: %{x}<br>",
            "Correlation: %{z:.2f}",
            "<extra></extra>")
        ) %>%
          layout(
            title  = list(text = "Variable Correlation Matrix",
                          font = list(size = 14)),
            xaxis  = list(title = list(text = "Variable", font = list(size = 12))),
            yaxis  = list(title = list(text = "Variable", font = list(size = 12))),
            paper_bgcolor = "white"
          )
      })

      updateSelectInput(session, "vs_x_var",
        choices  = setNames(numeric_vars, sapply(numeric_vars, pretty_var)),
        selected = numeric_vars[1])
      updateSelectInput(session, "vs_y_var",
        choices  = setNames(numeric_vars, sapply(numeric_vars, pretty_var)),
        selected = numeric_vars[2])
    }
  })

  # ---- Variable pair heatmap (moved from Policy Repository block) ----

  # Variable pair heatmap (reactive to x/y selection)
  observe({
    req(input$vs_x_var, input$vs_y_var)
    req(vs_analysis_type() == "Multivariate")

    df     <- vs_data()
    x_var  <- input$vs_x_var
    y_var  <- input$vs_y_var
    target <- input$vs_target

    req(all(c(x_var, y_var) %in% names(df)))

    # Compute MOB-based bad rate target
    mob_map <- c(bad_3mob = 3L, bad_6mob = 6L, bad_9mob = 9L, bad_12mob = 12L)
    if (!is.null(target) && target %in% names(mob_map)) {
      mob_cutoff <- mob_map[[target]]
      df$.vs_target <- ifelse(
        df$issued_flag == 1 & df$months_on_books >= mob_cutoff,
        as.integer(!is.na(df$first_delinquency_mob) &
                   df$first_delinquency_mob <= mob_cutoff),
        NA_integer_
      )
    } else {
      df$.vs_target <- df$performance_delinquency_flag
      df$.vs_target[df$issued_flag != 1] <- NA_integer_
    }

    df_clean <- df %>%
      filter(!is.na(.vs_target),
             !is.na(.data[[x_var]]), !is.na(.data[[y_var]]))

    req(nrow(df_clean) >= 20)

    brks_x <- unique(quantile(df_clean[[x_var]],
                               probs = seq(0, 1, 0.25), na.rm = TRUE))
    brks_y <- unique(quantile(df_clean[[y_var]],
                               probs = seq(0, 1, 0.25), na.rm = TRUE))

    req(length(brks_x) >= 3, length(brks_y) >= 3)

    # Helper to create readable quartile labels from breaks
    quartile_labels <- function(brks, var_name) {
      n <- length(brks) - 1
      sapply(seq_len(n), function(i) {
        lo <- brks[i]; hi <- brks[i + 1]
        fmt <- function(v) {
          if (abs(v) >= 1e6) sprintf("%.1fM", v / 1e6)
          else if (abs(v) >= 1e3) sprintf("%.0fK", v / 1e3)
          else if (abs(v) < 1) sprintf("%.2f", v)
          else sprintf("%.0f", v)
        }
        tag <- if (n == 4) c("Low", "Mid-Low", "Mid-High", "High")[i]
               else if (n == 3) c("Low", "Mid", "High")[i]
               else paste0("Q", i)
        paste0(tag, " (", fmt(lo), "-", fmt(hi), ")")
      })
    }

    x_labels <- quartile_labels(brks_x, x_var)
    y_labels <- quartile_labels(brks_y, y_var)

    df_clean$x_bin <- cut(df_clean[[x_var]], breaks = brks_x,
                          include.lowest = TRUE, labels = x_labels)
    df_clean$y_bin <- cut(df_clean[[y_var]], breaks = brks_y,
                          include.lowest = TRUE, labels = y_labels)

    df_clean$tgt_num <- as.numeric(df_clean$.vs_target)

    heat_df <- df_clean %>%
      filter(!is.na(x_bin), !is.na(y_bin)) %>%
      group_by(x_bin, y_bin) %>%
      summarise(rate = mean(tgt_num, na.rm = TRUE), .groups = "drop")

    heat_wide <- heat_df %>%
      pivot_wider(names_from = x_bin, values_from = rate,
                  values_fill = NA)
    z_mat     <- as.matrix(heat_wide[, -1])
    rownames(z_mat) <- as.character(heat_wide$y_bin)

    # Dynamic insight for heatmap
    output$vs_heatmap_insight <- renderUI({
      req(nrow(heat_df) >= 2)
      worst  <- heat_df[which.max(heat_df$rate), ]
      best   <- heat_df[which.min(heat_df$rate), ]
      spread <- worst$rate - best$rate

      # Quartile position labels: bottom 25% / lower-mid / upper-mid / top 25%
      x_levels <- levels(heat_df$x_bin)
      y_levels <- levels(heat_df$y_bin)
      quartile_label <- function(bin_val, levels) {
        idx <- match(as.character(bin_val), levels)
        n <- length(levels)
        if (is.na(idx)) return("mid")
        labels <- if (n == 4) c("bottom-quartile", "lower-mid", "upper-mid", "top-quartile")
                  else if (n == 3) c("low", "mid", "high")
                  else if (n == 2) c("low", "high")
                  else paste0("bin ", idx, " of ", n)
        labels[min(idx, length(labels))]
      }

      worst_x <- quartile_label(worst$x_bin, x_levels)
      worst_y <- quartile_label(worst$y_bin, y_levels)
      best_x  <- quartile_label(best$x_bin, x_levels)
      best_y  <- quartile_label(best$y_bin, y_levels)

      strength <- if (spread >= 0.05) "strong" else if (spread >= 0.02) "meaningful" else "modest"

      msg <- sprintf(
        "Borrowers with %s %s and %s %s carry the highest risk (%.1f%% bad rate), while %s %s and %s %s are safest (%.1f%%) \u2014 a %.1fpp gap indicating %s interaction between these variables.",
        worst_x, pretty_var(x_var), worst_y, pretty_var(y_var), worst$rate * 100,
        best_x, pretty_var(x_var), best_y, pretty_var(y_var), best$rate * 100,
        spread * 100, strength
      )
      div(style = "padding:6px 10px; margin-bottom:8px; background:#fff8f5; border-left:3px solid #fb6e36; border-radius:4px; font-size:13px; color:#444;",
        icon("lightbulb", style = "color:#fb6e36; margin-right:4px;"), msg)
    })

    output$vs_heatmap <- renderPlotly({
      plot_ly(
        z          = z_mat * 100,
        x          = colnames(z_mat),
        y          = rownames(z_mat),
        type       = "heatmap",
        colorscale = list(
          c(0.0, "#ffffff"),
          c(0.5, "#fb6e36"),
          c(1.0, "#8b0000")
        ),
        colorbar   = list(title = list(text = "Bad Rate", font = list(size = 12)),
                          ticksuffix = "%"),
        text       = round(z_mat * 100, 1),
        texttemplate = "%{text:.1f}%",
        textfont   = list(size = 11),
        hovertemplate = paste0(
          pretty_var(x_var), ": %{x}<br>",
          pretty_var(y_var), ": %{y}<br>",
          "Bad Rate: %{z:.1f}%<extra></extra>")
      ) %>%
        layout(
          title  = list(text = paste("Risk Heatmap:",
                                     pretty_var(x_var), "vs",
                                     pretty_var(y_var)),
                        font = list(size = 14)),
          xaxis  = list(title = pretty_var(x_var), tickangle = -20),
          yaxis  = list(title = pretty_var(y_var)),
          paper_bgcolor = "white"
        )
    })
  })
}
