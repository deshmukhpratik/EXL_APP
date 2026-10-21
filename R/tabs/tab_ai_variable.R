# ============================================================
# Tab: AI Variable Importance
# XGBoost-based variable importance analysis comparing
# in-policy variables vs untapped candidates.
# ============================================================

# ---- UI Function ----
ai_variable_ui <- function() {
  tabItem(tabName = "ai_variable",

    header_panel(
      "AI Variable Importance",
      paste("Train an XGBoost model to rank all 12 analysis variables by predictive power.",
            "Identify untapped candidate variables not yet used in the credit policy.")
    ),

    # Product selector
    fluidRow(column(12, product_selector_ui("aiv"))),

    # ---- Top control bar ----
    fluidRow(column(12, div(class = "card", style = "padding: 20px;",
      fluidRow(
        column(4,
          radioGroupButtons("ai_var_target", "Prediction Target",
            choices  = c("Delinquency" = "delinq", "IRR" = "irr"),
            selected = "delinq",
            status   = "default",
            justified = TRUE,
            individual = TRUE)
        ),
        column(4, offset = 4,
          div(style = "margin-top: 25px;",
            actionButton("ai_var_run", "Run Analysis",
              class = "custom-button", icon = icon("bolt"),
              width = "100%")
          )
        )
      )
    ))),

    # ---- Row 1: Importance bar chart (full width) ----
    fluidRow(column(12, div(class = "card", style = "padding: 20px;",
      h4("Variable Importance (XGBoost Gain)",
         style = "font-weight: bold; margin-bottom: 12px;"),
      plotlyOutput("ai_var_importance", height = "420px")
    ))),

    # ---- Row 2: Policy vs Candidate lists ----
    fluidRow(
      column(6, div(class = "card", style = "padding: 20px;",
        h4("Variables In Policy",
           style = "font-weight: bold; margin-bottom: 12px;"),
        uiOutput("ai_var_policy_list")
      )),
      column(6, div(class = "card", style = "padding: 20px;",
        h4("Untapped Candidates",
           style = "font-weight: bold; margin-bottom: 12px;"),
        uiOutput("ai_var_candidate_list")
      ))
    ),

    # ---- Row 3: Deep dive (detail variable) ----
    fluidRow(column(12, div(class = "card", style = "padding: 20px;",
      fluidRow(
        column(4,
          selectInput("ai_var_detail_select", "Deep Dive Variable",
            choices = NULL, selected = NULL)
        )
      )
    ))),

    fluidRow(
      column(6, div(class = "card", style = "padding: 20px;",
        h4("Partial Dependence Plot",
           style = "font-weight: bold; margin-bottom: 12px;"),
        plotlyOutput("ai_var_pdp", height = "380px")
      )),
      column(6, div(class = "card", style = "padding: 20px;",
        h4("Bivariate Bad Rate",
           style = "font-weight: bold; margin-bottom: 12px;"),
        plotlyOutput("ai_var_bivariate", height = "380px")
      ))
    )
  )
}


# ---- Server Function ----
ai_variable_server <- function(input, output, session, credit_data, loan_irr_cache) {

  # Variable classification
  ai_var_policy_vars <- c(
    "fico_score", "credit_file_age_months", "delinquent_accounts_count",
    "debt_to_income_ratio", "risk_model_score_1", "risk_model_score_2",
    "risk_model_score_3", "months_since_last_delinquency", "internal_file_age_months"
  )

  ai_var_candidate_vars <- c("annual_income", "monthly_debt_payment", "term_months",
    "TR01", "TR03", "TR20", "RV01", "BC_UTL", "REVOL_UTL",
    "N_30DPD_12M", "N_HARD_INQ_6M", "pd_estimate", "behav_score",
    "PMT_RATIO")  # risk_bucket excluded: no delinquency correlation in synthetic data

  ai_var_all_vars <- c(ai_var_policy_vars, ai_var_candidate_vars)

  ai_var_labels <- c(
    "fico_score"                    = "FICO Score",
    "annual_income"                 = "Annual Income",
    "monthly_debt_payment"          = "Monthly Debt Payment",
    "debt_to_income_ratio"          = "Debt-to-Income Ratio",
    "credit_file_age_months"        = "Credit File Age (Mo)",
    "delinquent_accounts_count"     = "Delinquent Accounts",
    "months_since_last_delinquency" = "Mo Since Last Delinq",
    "internal_file_age_months"      = "Internal File Age (Mo)",
    "term_months"                   = "Loan Term (Mo)",
    "risk_model_score_1"            = "Risk Score 1",
    "risk_model_score_2"            = "Risk Score 2",
    "risk_model_score_3"            = "Risk Score 3",
    "TR01"                          = "Total Trades (TR01)",
    "TR03"                          = "Satisfactory Trades (TR03)",
    "TR20"                          = "Oldest Acct Age (TR20)",
    "RV01"                          = "Revolving Accts (RV01)",
    "BC_UTL"                        = "Bankcard Utilization",
    "REVOL_UTL"                     = "Revolving Utilization",
    "N_30DPD_12M"                   = "30 DPD Last 12M",
    "N_HARD_INQ_6M"                 = "Hard Inquiries 6M",
    "pd_estimate"                   = "Bureau Risk Index",
    "behav_score"                   = "Behavior Score",
    "PMT_RATIO"                     = "Payment Ratio"
  )

  # Reactive storage for model results
  rv <- reactiveValues(
    importance_df  = NULL,   # Feature importance dataframe
    model          = NULL,   # Trained xgboost model
    analysis_df    = NULL,   # Filtered data used for analysis
    feature_matrix = NULL,   # Feature matrix used for training
    target_vector  = NULL,   # Target values
    target_type    = NULL    # "delinq" or "irr"
  )

  # ---- Main computation: train XGBoost and extract importance ----
  observeEvent(input$ai_var_run, {

    target <- input$ai_var_target

    withProgress(message = "Running AI Variable Analysis...", value = 0, {

      # -- Step 1: Filter to issued loans --
      incProgress(0.1, detail = "Preparing data...")

      df <- credit_data[credit_data$issued_flag == 1, , drop = FALSE]

      if (nrow(df) == 0) {
        showNotification("No issued loans found in credit_data.", type = "error")
        return()
      }

      # -- Step 2: Set up target vector --
      if (target == "irr") {
        irr_data <- loan_irr_cache()
        if (is.null(irr_data) || nrow(irr_data) == 0) {
          showNotification(
            "IRR data not available. Upload a performance tape first.",
            type = "error")
          return()
        }
        df <- merge(df, irr_data[, c("loan_id", "loan_irr")],
                    by = "loan_id", all.x = FALSE)
        df <- df[!is.na(df$loan_irr), , drop = FALSE]
        if (nrow(df) < 50) {
          showNotification("Too few loans with IRR data (need >= 50).", type = "error")
          return()
        }
        target_vector <- df$loan_irr
      } else {
        target_vector <- as.numeric(df$performance_delinquency_flag)
        df <- df[!is.na(target_vector), , drop = FALSE]
        target_vector <- target_vector[!is.na(target_vector)]
      }

      # -- Step 3: Build feature matrix from all 12 variables --
      incProgress(0.3, detail = "Building feature matrix...")

      valid_vars <- ai_var_all_vars[ai_var_all_vars %in% names(df)]
      if (length(valid_vars) < 2) {
        showNotification("Fewer than 2 analysis variables found in data.", type = "error")
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

      # -- Step 4: Train XGBoost --
      incProgress(0.5, detail = "Training XGBoost model...")

      tryCatch({
        if (target == "delinq") {
          params <- list(
            objective   = "binary:logistic",
            eval_metric = "auc",
            max_depth   = 6,
            eta         = 0.1
          )
        } else {
          params <- list(
            objective   = "reg:squarederror",
            eval_metric = "rmse",
            max_depth   = 6,
            eta         = 0.1
          )
        }

        dtrain <- xgboost::xgb.DMatrix(
          data  = feature_matrix,
          label = target_vector
        )

        model <- xgboost::xgb.train(
          params  = params,
          data    = dtrain,
          nrounds = 100,
          verbose = 0
        )

        # -- Step 5: Extract importance --
        incProgress(0.8, detail = "Extracting importance...")

        imp <- xgboost::xgb.importance(model = model)

        # imp has columns: Feature, Gain, Cover, Frequency
        # Ensure all 12 variables appear (some may have zero gain)
        imp_df <- data.frame(
          Feature = valid_vars,
          stringsAsFactors = FALSE
        )
        imp_df <- merge(imp_df, imp[, c("Feature", "Gain")],
                        by = "Feature", all.x = TRUE)
        imp_df$Gain[is.na(imp_df$Gain)] <- 0

        # Add category label
        imp_df$category <- ifelse(
          imp_df$Feature %in% ai_var_policy_vars, "In Policy", "Candidate"
        )

        # Add pretty labels
        imp_df$label <- ifelse(
          imp_df$Feature %in% names(ai_var_labels),
          ai_var_labels[imp_df$Feature],
          gsub("_", " ", imp_df$Feature)
        )

        # Sort by Gain descending
        imp_df <- imp_df[order(-imp_df$Gain), , drop = FALSE]

        # Store results
        rv$importance_df  <- imp_df
        rv$model          <- model
        rv$analysis_df    <- df
        rv$feature_matrix <- feature_matrix
        rv$target_vector  <- target_vector
        rv$target_type    <- target

        # Update deep dive variable selector
        detail_choices <- setNames(imp_df$Feature, imp_df$label)
        updateSelectInput(session, "ai_var_detail_select",
          choices  = detail_choices,
          selected = detail_choices[1])

        incProgress(1.0, detail = "Done!")

      }, error = function(e) {
        showNotification(
          paste("Model training failed:", conditionMessage(e)),
          type = "error", duration = 10)
      })
    })
  })

  # ---- Importance bar chart ----
  output$ai_var_importance <- renderPlotly({
    req(rv$importance_df)

    imp_df <- rv$importance_df
    # Sort ascending for coord_flip (bottom = lowest)
    imp_df <- imp_df[order(imp_df$Gain), , drop = FALSE]
    imp_df$label <- factor(imp_df$label, levels = imp_df$label)

    p <- ggplot(imp_df, aes(x = label, y = Gain, fill = category)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(
        values = c("In Policy" = "#4E79A7", "Candidate" = "#fb6e36"),
        name   = ""
      ) +
      labs(x = NULL, y = "Gain (Relative Importance)",
           title = "All Variables Ranked by XGBoost Gain") +
      theme_custom +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

    ggplotly(p, tooltip = c("y", "fill")) %>%
      plotly::layout(
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font   = list(family = "Source Sans Pro, sans-serif", color = "#334155"),
        margin = list(l = 160, r = 20, t = 40, b = 40),
        legend = list(orientation = "h", x = 0.3, y = -0.1)
      )
  })

  # ---- Helper: build list rows for policy/candidate panels ----
  build_var_list <- function(imp_df, var_set, show_recommend = FALSE) {
    sub <- imp_df[imp_df$Feature %in% var_set, , drop = FALSE]
    sub <- sub[order(-sub$Gain), , drop = FALSE]

    if (nrow(sub) == 0) {
      return(div(style = "color:#9ca3af; font-size:13px;", "No variables to display."))
    }

    total_gain <- sum(imp_df$Gain)
    max_gain   <- max(imp_df$Gain)
    median_gain <- median(imp_df$Gain)

    rows <- lapply(seq_len(nrow(sub)), function(i) {
      feat <- sub$Feature[i]
      gain <- sub$Gain[i]
      lbl  <- sub$label[i]
      pct  <- if (max_gain > 0) round(gain / max_gain * 100, 0) else 0
      gain_pct <- if (total_gain > 0) round(gain / total_gain * 100, 1) else 0

      badge <- NULL
      if (show_recommend && gain > median_gain) {
        badge <- span(
          style = "background:#fb6e36; color:white; font-size:10px; padding:2px 6px; border-radius:4px; margin-left:8px;",
          "RECOMMEND"
        )
      }

      div(style = "display:flex; align-items:center; padding:8px 0; border-bottom:1px solid #e5e7eb;",
        div(style = "flex:1; font-weight:600; color:#334155;", lbl, badge),
        div(style = "flex:1;",
          div(style = paste0(
            "background:#fb6e36; height:8px; border-radius:4px; width:", pct, "%;"
          ))
        ),
        div(style = "width:60px; text-align:right; font-size:12px; color:#9ca3af;",
          paste0(gain_pct, "%")
        )
      )
    })

    do.call(tagList, rows)
  }

  # ---- Policy variable list ----
  output$ai_var_policy_list <- renderUI({
    req(rv$importance_df)
    build_var_list(rv$importance_df, ai_var_policy_vars, show_recommend = FALSE)
  })

  # ---- Candidate variable list ----
  output$ai_var_candidate_list <- renderUI({
    req(rv$importance_df)
    build_var_list(rv$importance_df, ai_var_candidate_vars, show_recommend = TRUE)
  })

  # ---- Partial Dependence Plot ----
  output$ai_var_pdp <- renderPlotly({
    req(rv$model, rv$feature_matrix, input$ai_var_detail_select)

    sel_var <- input$ai_var_detail_select
    if (!(sel_var %in% colnames(rv$feature_matrix))) return(NULL)

    df <- rv$analysis_df
    fm <- rv$feature_matrix
    model <- rv$model

    # Values for selected variable
    var_vals <- fm[, sel_var]

    # Bucket into 10 equal-frequency bins
    bins <- tryCatch(
      dplyr::ntile(var_vals, 10),
      error = function(e) rep(1, length(var_vals))
    )

    # Compute predictions
    dtrain <- xgboost::xgb.DMatrix(data = fm)
    preds  <- predict(model, dtrain)

    # Compute mean prediction per bin
    pdp_df <- data.frame(
      bin      = bins,
      var_val  = var_vals,
      pred     = preds,
      stringsAsFactors = FALSE
    )

    pdp_agg <- pdp_df %>%
      dplyr::group_by(bin) %>%
      dplyr::summarise(
        midpoint  = median(var_val, na.rm = TRUE),
        mean_pred = mean(pred, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      dplyr::arrange(midpoint)

    var_label <- if (sel_var %in% names(ai_var_labels)) {
      ai_var_labels[sel_var]
    } else {
      gsub("_", " ", sel_var)
    }

    y_label <- if (rv$target_type == "delinq") {
      "Mean Predicted P(Delinquency)"
    } else {
      "Mean Predicted IRR"
    }

    p <- ggplot(pdp_agg, aes(x = midpoint, y = mean_pred)) +
      geom_line(color = "#4E79A7", linewidth = 1.2) +
      geom_point(color = "#4E79A7", size = 3) +
      labs(x = var_label, y = y_label,
           title = paste("Partial Dependence:", var_label)) +
      theme_custom +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

    ggplotly(p, tooltip = c("x", "y")) %>%
      plotly::layout(
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font   = list(family = "Source Sans Pro, sans-serif", color = "#334155"),
        margin = list(l = 60, r = 20, t = 40, b = 60)
      )
  })

  # ---- Bivariate Bad Rate Chart ----
  output$ai_var_bivariate <- renderPlotly({
    req(rv$analysis_df, input$ai_var_detail_select)

    sel_var <- input$ai_var_detail_select
    df <- rv$analysis_df

    if (!(sel_var %in% names(df))) return(NULL)
    if (!("performance_delinquency_flag" %in% names(df))) return(NULL)

    var_vals <- df[[sel_var]]
    bad_flag <- as.numeric(df$performance_delinquency_flag)

    # Remove NAs
    valid <- !is.na(var_vals) & !is.na(bad_flag)
    var_vals <- var_vals[valid]
    bad_flag <- bad_flag[valid]

    if (length(var_vals) < 10) return(NULL)

    # Bucket into 10 equal-frequency bins
    bins <- tryCatch(
      dplyr::ntile(var_vals, 10),
      error = function(e) rep(1, length(var_vals))
    )

    biv_df <- data.frame(
      bin     = bins,
      var_val = var_vals,
      bad     = bad_flag,
      stringsAsFactors = FALSE
    )

    biv_agg <- biv_df %>%
      dplyr::group_by(bin) %>%
      dplyr::summarise(
        midpoint = median(var_val, na.rm = TRUE),
        bad_rate = mean(bad, na.rm = TRUE),
        n        = dplyr::n(),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(midpoint)

    # Create bin labels
    biv_agg$bin_label <- paste0("Bin ", seq_len(nrow(biv_agg)))
    biv_agg$bin_label <- factor(biv_agg$bin_label, levels = biv_agg$bin_label)

    var_label <- if (sel_var %in% names(ai_var_labels)) {
      ai_var_labels[sel_var]
    } else {
      gsub("_", " ", sel_var)
    }

    p <- ggplot(biv_agg, aes(x = bin_label, y = bad_rate)) +
      geom_col(fill = "#fb6e36", width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      labs(x = var_label, y = "Bad Rate",
           title = paste("Bad Rate by", var_label, "Decile")) +
      theme_custom +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

    ggplotly(p, tooltip = c("x", "y")) %>%
      plotly::layout(
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font   = list(family = "Source Sans Pro, sans-serif", color = "#334155"),
        margin = list(l = 60, r = 20, t = 40, b = 80)
      )
  })
}
