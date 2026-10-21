# ============================================================
# Tab: AI SHAP Analysis
# XGBoost model training + SHAP-based feature importance,
# dependence plots, and per-loan waterfall explanations.
# ============================================================

# ---- UI Function ----
ai_shap_ui <- function() {
  tabItem(tabName = "ai_shap",

    header_panel(
      "AI-Powered SHAP Analysis",
      paste("Train an XGBoost model on your portfolio and use SHAP values",
            "to understand which features drive delinquency or IRR.")
    ),

    # Product selector
    fluidRow(column(12, product_selector_ui("aish"))),

    # ---- Top control bar ----
    fluidRow(column(12, div(class = "card",
      fluidRow(
        column(3,
          radioGroupButtons("ai_shap_target", "Prediction Target",
            choices  = c("Delinquency" = "delinq", "IRR" = "irr"),
            selected = "delinq",
            status   = "default",
            justified = TRUE,
            individual = TRUE)
        ),
        column(6,
          pickerInput("ai_shap_vars", "Analysis Variables",
            choices = c(
              "FICO Score"              = "fico_score",
              "Annual Income"           = "annual_income",
              "Monthly Debt Payment"    = "monthly_debt_payment",
              "Debt-to-Income Ratio"    = "debt_to_income_ratio",
              "Credit File Age (Mo)"    = "credit_file_age_months",
              "Delinquent Accounts"     = "delinquent_accounts_count",
              "Mo Since Last Delinq"    = "months_since_last_delinquency",
              "Internal File Age (Mo)"  = "internal_file_age_months",
              "Loan Term (Mo)"          = "term_months",
              "Risk Score 1"            = "risk_model_score_1",
              "Risk Score 2"            = "risk_model_score_2",
              "Risk Score 3"            = "risk_model_score_3"
            ),
            selected = c(
              "fico_score", "annual_income", "monthly_debt_payment",
              "debt_to_income_ratio", "credit_file_age_months",
              "delinquent_accounts_count", "months_since_last_delinquency",
              "internal_file_age_months", "term_months",
              "risk_model_score_1", "risk_model_score_2", "risk_model_score_3"
            ),
            multiple = TRUE,
            options  = list(`actions-box` = TRUE))
        ),
        column(3,
          div(style = "margin-top: 25px;",
            actionButton("ai_shap_run", "Run SHAP Analysis",
              class = "custom-button", icon = icon("bolt"),
              width = "100%")
          )
        )
      )
    ))),

    # ---- Row 1: KPI boxes ----
    fluidRow(
      column(4, uiOutput("ai_shap_kpi1")),
      column(4, uiOutput("ai_shap_kpi2")),
      column(4, uiOutput("ai_shap_kpi3"))
    ),

    # ---- Row 2: SHAP importance bar chart ----
    fluidRow(column(12, div(class = "card", style = "padding: 20px;",
      h4("Feature Importance (Mean |SHAP|)",
         style = "font-weight: bold; margin-bottom: 12px;"),
      plotly::plotlyOutput("ai_shap_importance", height = "420px")
    ))),

    # ---- Row 3: Dependence + Waterfall ----
    fluidRow(
      column(6, div(class = "card", style = "padding: 20px;",
        h4("SHAP Dependence Plot",
           style = "font-weight: bold; margin-bottom: 12px;"),
        plotly::plotlyOutput("ai_shap_dependence", height = "400px")
      )),
      column(6, div(class = "card", style = "padding: 20px;",
        h4("SHAP Waterfall (Single Loan)",
           style = "font-weight: bold; margin-bottom: 12px;"),
        selectInput("ai_shap_loan_select", "Select Loan ID",
          choices = NULL, selected = NULL),
        plotly::plotlyOutput("ai_shap_waterfall", height = "360px")
      ))
    )
  )
}


# ---- Server Function ----
ai_shap_server <- function(input, output, session, credit_data, loan_irr_cache) {

  # Reactive store for model results
  rv <- reactiveValues(
    shap_obj        = NULL,   # shapviz object
    shap_values     = NULL,   # SHAP value matrix
    feature_matrix  = NULL,   # Feature matrix used for training
    model           = NULL,   # Trained xgboost model
    predictions     = NULL,   # Model predictions
    actuals         = NULL,   # Actual target values
    target_type     = NULL,   # "delinq" or "irr"
    metric_value    = NULL,   # AUC or RMSE
    n_features      = NULL,   # Number of features
    n_loans         = NULL,   # Number of loans analyzed
    analysis_df     = NULL,   # Filtered data used for analysis
    base_value      = NULL    # Expected value (model baseline)
  )

  # ---- Main computation block ----
  observeEvent(input$ai_shap_run, {

    sel_vars <- input$ai_shap_vars
    target   <- input$ai_shap_target

    # Validate inputs
    if (is.null(sel_vars) || length(sel_vars) < 2) {
      showNotification("Please select at least 2 analysis variables.",
                       type = "error")
      return()
    }

    withProgress(message = "Training model...", value = 0, {

      # -- Step 1: Filter to issued loans --
      incProgress(0.1, detail = "Preparing data...")

      df <- credit_data[credit_data$issued_flag == 1, , drop = FALSE]

      if (nrow(df) == 0) {
        showNotification("No issued loans found in credit_data.",
                         type = "error")
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
          showNotification("Too few loans with IRR data (need >= 50).",
                           type = "error")
          return()
        }
        target_vector <- df$loan_irr
      } else {
        target_vector <- as.numeric(df$performance_delinquency_flag)
        df <- df[!is.na(target_vector), , drop = FALSE]
        target_vector <- target_vector[!is.na(target_vector)]
      }

      # -- Step 3: Build feature matrix --
      incProgress(0.2, detail = "Building feature matrix...")

      # Keep only selected vars that exist in the data
      valid_vars <- sel_vars[sel_vars %in% names(df)]
      if (length(valid_vars) < 2) {
        showNotification("Fewer than 2 selected variables found in data.",
                         type = "error")
        return()
      }

      feat_df <- df[, valid_vars, drop = FALSE]

      # Replace NAs with column median
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
      incProgress(0.3, detail = "Training XGBoost model...")

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

        # -- Step 5: Compute predictions --
        preds <- predict(model, dtrain)

        # -- Step 6: Compute performance metric --
        if (target == "delinq") {
          # AUC via Mann-Whitney U statistic
          pos <- preds[target_vector == 1]
          neg <- preds[target_vector == 0]
          if (length(pos) > 0 && length(neg) > 0) {
            # Sample for speed if very large
            n_pos <- min(length(pos), 5000)
            n_neg <- min(length(neg), 5000)
            set.seed(42)
            pos_s <- sample(pos, n_pos)
            neg_s <- sample(neg, n_neg)
            auc_val <- mean(outer(pos_s, neg_s, ">")) +
                       0.5 * mean(outer(pos_s, neg_s, "=="))
          } else {
            auc_val <- NA_real_
          }
          metric_value <- round(auc_val, 4)
        } else {
          rmse_val <- sqrt(mean((preds - target_vector)^2))
          metric_value <- round(rmse_val, 4)
        }

        # -- Step 7: Compute SHAP values --
        incProgress(0.5, detail = "Computing SHAP values...")

        shp <- shapviz::shapviz(model, X_pred = feature_matrix)
        shap_values <- shapviz::get_shap_values(shp)
        base_value  <- shapviz::get_baseline(shp)

        # -- Store results in reactiveValues --
        rv$shap_obj       <- shp
        rv$shap_values    <- shap_values
        rv$feature_matrix <- feature_matrix
        rv$model          <- model
        rv$predictions    <- preds
        rv$actuals        <- target_vector
        rv$target_type    <- target
        rv$metric_value   <- metric_value
        rv$n_features     <- length(valid_vars)
        rv$n_loans        <- nrow(df)
        rv$analysis_df    <- df
        rv$base_value     <- base_value

        # -- Update loan selector --
        loan_ids <- as.character(df$loan_id[1:min(100, nrow(df))])
        updateSelectInput(session, "ai_shap_loan_select",
          choices  = loan_ids,
          selected = loan_ids[1])

        incProgress(1.0, detail = "Done!")

      }, error = function(e) {
        showNotification(
          paste("Model training failed:", conditionMessage(e)),
          type = "error", duration = 10)
      })
    })
  })

  # ---- KPI 1: Model metric (AUC / RMSE) ----
  output$ai_shap_kpi1 <- renderUI({
    req(rv$metric_value)
    if (rv$target_type == "delinq") {
      kpi_box(rv$metric_value, "Model AUC", "Binary classification")
    } else {
      kpi_box(rv$metric_value, "Model RMSE", "Regression")
    }
  })

  # ---- KPI 2: Number of features ----
  output$ai_shap_kpi2 <- renderUI({
    req(rv$n_features)
    kpi_box(rv$n_features, "Features Used")
  })

  # ---- KPI 3: Number of loans analyzed ----
  output$ai_shap_kpi3 <- renderUI({
    req(rv$n_loans)
    kpi_box(
      format(rv$n_loans, big.mark = ","),
      "Loans Analyzed",
      "Issued loans only"
    )
  })

  # ---- SHAP Importance Bar Chart ----
  output$ai_shap_importance <- plotly::renderPlotly({
    req(rv$shap_values)

    # Compute mean |SHAP| per feature
    mean_abs_shap <- colMeans(abs(rv$shap_values))
    imp_df <- data.frame(
      feature    = names(mean_abs_shap),
      importance = as.numeric(mean_abs_shap),
      stringsAsFactors = FALSE
    )
    imp_df <- imp_df[order(imp_df$importance), , drop = FALSE]
    imp_df$feature <- factor(imp_df$feature, levels = imp_df$feature)

    # Pretty labels for features
    var_labels <- c(
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
      "risk_model_score_3"            = "Risk Score 3"
    )
    imp_df$label <- ifelse(
      imp_df$feature %in% names(var_labels),
      var_labels[as.character(imp_df$feature)],
      gsub("_", " ", imp_df$feature)
    )
    imp_df$label <- factor(imp_df$label, levels = imp_df$label)

    # Color gradient: low importance = muted, high importance = orange
    n <- nrow(imp_df)
    fill_colors <- colorRampPalette(c("#9ca3af", "#fb6e36"))(n)

    p <- ggplot(imp_df, aes(x = label, y = importance, fill = importance)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_gradient(low = "#9ca3af", high = "#fb6e36", guide = "none") +
      labs(x = NULL, y = "Mean |SHAP Value|",
           title = "Feature Importance by SHAP") +
      theme_custom +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

    plotly::ggplotly(p, tooltip = c("y")) %>%
      plotly::layout(
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font = list(family = "Source Sans Pro, sans-serif", color = "#334155"),
        margin = list(l = 150, r = 20, t = 40, b = 40)
      )
  })

  # ---- SHAP Dependence Plot ----
  output$ai_shap_dependence <- plotly::renderPlotly({
    req(rv$shap_values, rv$feature_matrix)

    # Find top variable (highest mean |SHAP|)
    mean_abs_shap <- colMeans(abs(rv$shap_values))
    sorted_vars   <- names(sort(mean_abs_shap, decreasing = TRUE))
    top_var       <- sorted_vars[1]
    color_var     <- if (length(sorted_vars) >= 2) sorted_vars[2] else top_var

    # Sample for performance (max 5000 points)
    n <- nrow(rv$feature_matrix)
    if (n > 5000) {
      set.seed(123)
      idx <- sample(n, 5000)
    } else {
      idx <- seq_len(n)
    }

    dep_df <- data.frame(
      feature_val = rv$feature_matrix[idx, top_var],
      shap_val    = rv$shap_values[idx, top_var],
      color_val   = rv$feature_matrix[idx, color_var],
      stringsAsFactors = FALSE
    )

    # Pretty labels
    var_labels <- c(
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
      "risk_model_score_3"            = "Risk Score 3"
    )
    top_label   <- var_labels[top_var]   %||% gsub("_", " ", top_var)
    color_label <- var_labels[color_var] %||% gsub("_", " ", color_var)

    p <- ggplot(dep_df, aes(x = feature_val, y = shap_val, color = color_val)) +
      geom_point(alpha = 0.3, size = 1) +
      scale_color_gradient(low = "#4E79A7", high = "#fb6e36",
                           name = color_label) +
      labs(x = top_label,
           y = paste0("SHAP Value (", top_label, ")"),
           title = paste0("Dependence: ", top_label)) +
      theme_custom +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

    plotly::ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      plotly::layout(
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font = list(family = "Source Sans Pro, sans-serif", color = "#334155"),
        margin = list(l = 60, r = 20, t = 40, b = 60),
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
  })

  # ---- SHAP Waterfall for Selected Loan ----
  output$ai_shap_waterfall <- plotly::renderPlotly({
    req(rv$shap_values, rv$feature_matrix, rv$base_value)
    req(input$ai_shap_loan_select)

    # Find the row index for the selected loan
    loan_ids <- as.character(rv$analysis_df$loan_id)
    sel_id   <- input$ai_shap_loan_select
    row_idx  <- which(loan_ids == sel_id)

    if (length(row_idx) == 0) {
      row_idx <- 1
    } else {
      row_idx <- row_idx[1]
    }

    # Get SHAP values for this row
    shap_row <- rv$shap_values[row_idx, ]
    feat_row <- rv$feature_matrix[row_idx, ]

    # Sort by |SHAP| descending, take top 10
    ord <- order(abs(shap_row), decreasing = TRUE)
    top_n <- min(10, length(ord))
    ord   <- ord[1:top_n]

    shap_sorted <- shap_row[ord]
    feat_sorted <- feat_row[ord]
    feat_names  <- names(shap_sorted)

    # Pretty labels
    var_labels <- c(
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
      "risk_model_score_3"            = "Risk Score 3"
    )

    # Build waterfall data: each bar shows the SHAP contribution
    # Label includes the feature value for context
    wf_df <- data.frame(
      feature = ifelse(
        feat_names %in% names(var_labels),
        var_labels[feat_names],
        gsub("_", " ", feat_names)
      ),
      shap    = as.numeric(shap_sorted),
      value   = as.numeric(feat_sorted),
      stringsAsFactors = FALSE
    )

    # Create display label: "Feature = value"
    wf_df$label <- paste0(wf_df$feature, " = ",
                          ifelse(abs(wf_df$value) >= 1000,
                                 format(round(wf_df$value, 0), big.mark = ","),
                                 round(wf_df$value, 3)))

    # Reverse so top-importance is at the top of the horizontal bar chart
    wf_df <- wf_df[rev(seq_len(nrow(wf_df))), , drop = FALSE]
    wf_df$label <- factor(wf_df$label, levels = wf_df$label)

    # Color: positive SHAP = orange (increases prediction), negative = blue (decreases)
    wf_df$direction <- ifelse(wf_df$shap >= 0, "Increases", "Decreases")

    base_val <- round(rv$base_value, 4)
    pred_val <- round(rv$base_value + sum(shap_row), 4)

    p <- ggplot(wf_df, aes(x = label, y = shap, fill = direction)) +
      geom_col(width = 0.6) +
      coord_flip() +
      scale_fill_manual(
        values = c("Increases" = "#fb6e36", "Decreases" = "#4E79A7"),
        name   = "Direction"
      ) +
      labs(
        x     = NULL,
        y     = "SHAP Value",
        title = paste0("Loan ", sel_id,
                       "  |  Base: ", base_val,
                       "  |  Prediction: ", pred_val)
      ) +
      theme_custom +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

    plotly::ggplotly(p, tooltip = c("y")) %>%
      plotly::layout(
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#FFFFFF",
        font = list(family = "Source Sans Pro, sans-serif", color = "#334155"),
        margin = list(l = 200, r = 20, t = 50, b = 40),
        legend = list(orientation = "h", x = 0, y = -0.15)
      )
  })
}
