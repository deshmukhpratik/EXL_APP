# ============================================================
# Tab: Portfolio Insights — 2 Sub-tabs
#   1. Customer Acquisition (originations funnel, distribution,
#      decline summary, customer profiling)
#   2. Customer Performance (delinquency trend, vintage curves,
#      MOB breakdown)
# Ported from dashboard_shiny/app.R into modular format
# ============================================================

# ---- EXL Brand Colors (scoped to this module) ----
PI_ORANGE        <- "#FF4B00"
PI_MIDNIGHT      <- "#005071"
PI_MIDNIGHT_70   <- "#4F8199"
PI_MIDNIGHT_40   <- "#9FBBC7"
PI_LIGHT_BLUE    <- "#E6F5FA"
PI_SLATE         <- "#2E3643"
PI_ORANGE_20     <- "#FF6426"
PI_ORANGE_40     <- "#FF7D4D"
PI_ORANGE_60     <- "#FF9673"
PI_ORANGE_80     <- "#FFAF99"
PI_ORANGE_90     <- "#FFC8BF"

PI_PRODUCT_COLORS <- c(
  "Personal Loan" = PI_ORANGE,
  "Auto Loan"     = PI_MIDNIGHT,
  "Home Equity"   = PI_MIDNIGHT_70,
  "Credit Card"   = PI_ORANGE_40
)
PI_PRODUCTS <- c("Personal Loan", "Auto Loan", "Home Equity", "Credit Card")

# ---- Data is built from real credit_data inside portfolio_insights_server() ----
# Synthetic generators removed per adversarial review F4/MED-3.
# The 5 data frames (pi_funnel_data, pi_profiling_data, pi_decline_data,
# pi_delinquency_ts, pi_vintage_data) are now constructed from baseline
# credit_data at server init time. See portfolio_insights_server() below.
# Placeholder globals for backward compat (overwritten in server):
pi_funnel_data <- data.frame()
pi_profiling_data <- data.frame()
pi_decline_data <- data.frame()
pi_delinquency_ts <- data.frame()
pi_vintage_data <- data.frame()

# (All synthetic generators removed — data built from credit_data in server)

# ---- Plotly layout helper (migrated from 02b_globals_utils.R) ----
.pi_layout <- function(p, title = "", xlab = "", ylab = "", tickformat_y = NULL) {
  ly <- list(
    title  = list(text = title, font = list(size = 14)),
    xaxis  = list(title = xlab),
    yaxis  = list(title = ylab),
    paper_bgcolor = "#FFFFFF",
    plot_bgcolor  = "#FFFFFF",
    font   = list(family = "Source Sans Pro, sans-serif", color = "#334155"),
    margin = list(l = 60, r = 20, t = 40, b = 60),
    legend = list(orientation = "h", x = 0, y = -0.15)
  )
  if (!is.null(tickformat_y)) ly$yaxis$tickformat <- tickformat_y
  do.call(plotly::layout, c(list(p), ly))
}

# ---- Helpers (pi_ prefixed to avoid collisions) ----
pi_fmt_num <- function(x) {
  ifelse(abs(x) >= 1e6, paste0(round(x / 1e6, 1), "M"),
    ifelse(abs(x) >= 1e3, paste0(round(x / 1e3, 1), "K"),
      format(round(x), big.mark = ",")))
}

pi_fmt_dollar <- function(x) {
  ifelse(abs(x) >= 1e9, paste0("$", round(x / 1e9, 1), "B"),
    ifelse(abs(x) >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
      ifelse(abs(x) >= 1e3, paste0("$", round(x / 1e3, 1), "K"),
        paste0("$", format(round(x), big.mark = ",")))))
}

pi_compute_delta <- function(vec) {
  n <- length(vec)
  if (n < 2 || is.na(vec[n - 1]) || is.na(vec[n]) || vec[n - 1] == 0) return(NA_real_)
  round((vec[n] - vec[n - 1]) / vec[n - 1] * 100, 1)
}

pi_kpi_card <- function(label, value, delta = NULL, accent = "orange", period_label = "period") {
  accent_class <- switch(accent,
    "orange" = "pi-accent-left", "blue" = "pi-accent-blue",
    "teal"   = "pi-accent-teal", "light" = "pi-accent-light", "pi-accent-left")
  delta_html <- ""
  if (!is.null(delta) && !is.na(delta)) {
    dir_class <- if (delta >= 0) "up" else "down"
    arrow <- if (delta >= 0) "&#9650;" else "&#9660;"
    delta_html <- sprintf(
      '<span class="pi-kpi-delta %s">%s %s%% <span class="pi-kpi-delta-label">vs prior %s</span></span>',
      dir_class, arrow, abs(delta), period_label)
  }
  sprintf(
    '<div class="pi-kpi-card %s">
       <div class="pi-kpi-label">%s</div>
       <div class="pi-kpi-value">%s</div>
       %s
     </div>', accent_class, label, value, delta_html)
}

# ---- Module CSS ----
pi_css <- '
/* ---------- Portfolio Insights (inherits Source Sans Pro from shinydashboard) ---------- */

/* ---------- Product Pills ---------- */
.pi-product-toggle-bar { display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 12px; }
.pi-product-pill {
  padding: 7px 18px; border-radius: 20px; font-size: 13px; font-weight: 600;
  cursor: pointer; border: 2px solid #005071; background: #FFFFFF; color: #005071;
  transition: all 0.2s ease; user-select: none;
}
.pi-product-pill:hover { background: #005071; color: #FFFFFF; }
.pi-product-pill.active { background: #005071; color: #FFFFFF; border-color: #005071; }

/* ---------- Filter Bar ---------- */
.pi-filter-bar {
  display: flex; align-items: flex-end; gap: 16px; flex-wrap: wrap;
  padding: 14px 20px; background: #FFFFFF; border-radius: 10px;
  border: 1px solid #E2E6ED; margin-bottom: 20px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.04);
}
.pi-filter-group { display: flex; flex-direction: column; gap: 4px; }
.pi-filter-group label {
  color: #475569; font-size: 13px; margin-bottom: 0;
}
.pi-filter-bar .form-control, .pi-filter-bar select {
  font-size: 13px; padding: 6px 12px; border-radius: 6px;
  border: 1.5px solid #E2E6ED; min-width: 130px; height: 34px;
}
.pi-filter-bar .form-control:focus { border-color: #005071; box-shadow: 0 0 0 2px rgba(0,80,113,0.12); }

/* ---------- KPI Cards ---------- */
.pi-kpi-grid { display: grid; gap: 16px; margin-bottom: 24px; }
.pi-kpi-grid-4 { grid-template-columns: repeat(4, 1fr); }
.pi-kpi-card {
  background: #FFFFFF; border: 1px solid #E2E6ED; border-radius: 10px;
  padding: 18px 20px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06), 0 1px 2px rgba(0,0,0,0.04);
  transition: transform 0.15s, box-shadow 0.15s;
}
.pi-kpi-card:hover {
  transform: translateY(-1px);
  box-shadow: 0 4px 12px rgba(0,0,0,0.08);
}
.pi-kpi-label {
  font-size: 11px; font-weight: 600; text-transform: uppercase;
  letter-spacing: 0.1em; color: #6E747D; margin-bottom: 8px;
}
.pi-kpi-value {
  font-size: 28px; font-weight: 800; line-height: 1.1;
  font-family: "Source Sans Pro", sans-serif;
  color: #005071;
}
.pi-kpi-delta {
  display: inline-flex; align-items: center; gap: 3px;
  font-size: 12px; font-weight: 600; margin-top: 6px;
  padding: 2px 8px; border-radius: 12px;
}
.pi-kpi-delta.up { background: #E6F9F0; color: #0E9F6E; }
.pi-kpi-delta.down { background: #FDE8E8; color: #E02424; }
.pi-accent-left  { border-left: 4px solid #FF4B00; }
.pi-accent-blue  { border-left: 4px solid #005071; }
.pi-accent-teal  { border-left: 4px solid #4F8199; }
.pi-accent-light { border-left: 4px solid #9FBBC7; }

/* ---------- Metric Selector Panel ---------- */
.pi-metric-panel {
  display: flex; gap: 20px; background: #FFFFFF; border-radius: 10px;
  border: 1px solid #E2E6ED; padding: 20px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06);
  min-height: 420px; margin-bottom: 20px;
}
.pi-metric-sidebar {
  min-width: 200px; max-width: 220px; display: flex; flex-direction: column; gap: 14px;
}
.pi-metric-sidebar .pi-sidebar-section-label {
  font-size: 11px; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.1em; color: #6E747D; margin-bottom: 4px;
}
.pi-metric-btn-group { display: flex; flex-direction: column; gap: 6px; }
.pi-metric-btn {
  padding: 10px 14px; border-radius: 8px; font-size: 13px; font-weight: 600;
  cursor: pointer; background: #F4F6F9; color: #2E3643; border: 1.5px solid transparent;
  transition: all 0.15s; text-align: left; user-select: none;
}
.pi-metric-btn:hover { background: #E6F5FA; border-color: #4F8199; }
.pi-metric-btn.active { background: #005071; color: #FFFFFF; border-color: #005071; }
.pi-metric-chart-area { flex: 1; min-width: 0; }

/* ---------- Section ---------- */
.pi-section-heading {
  font-size: 15px; font-weight: 700; color: #005071;
  margin: 18px 0 10px 0; padding-bottom: 6px;
  border-bottom: 2px solid #FF4B00;
}

/* ---------- Chart boxes ---------- */
.pi-chart-box {
  background: #FFFFFF; border-radius: 10px; padding: 16px 20px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06); border: 1px solid #E2E6ED;
  margin-bottom: 16px;
}

/* ---------- Insight Box ---------- */
.pi-insight-box {
  background: linear-gradient(135deg, #E6F5FA 0%, #F4F6F9 100%);
  border: 1px solid #9FBBC7; border-left: 4px solid #005071;
  border-radius: 8px; padding: 14px 18px; margin-bottom: 18px;
  font-size: 13px; line-height: 1.6; color: #2E3643;
}
.pi-insight-box .pi-insight-icon { color: #005071; font-weight: 700; margin-right: 6px; }
.pi-insight-box strong { color: #005071; }
.pi-kpi-delta-label {
  font-size: 10px; color: #6E747D; font-weight: 500;
  margin-left: 4px; letter-spacing: 0.03em;
}

/* ---------- Responsive ---------- */
@media(max-width: 1100px) {
  .pi-kpi-grid-4 { grid-template-columns: repeat(2, 1fr); }
  .pi-metric-panel { flex-direction: column; }
  .pi-metric-sidebar { max-width: 100%; min-width: 100%; flex-direction: row; flex-wrap: wrap; }
}
@media(max-width: 640px) {
  .pi-kpi-grid-4 { grid-template-columns: 1fr; }
}
'

# ============================================================
# UI FUNCTIONS (one per sidebar sub-item)
# ============================================================
pi_acquisition_ui <- function() {
  tabItem(tabName = "pi_acquisition",
    tags$head(tags$style(HTML(pi_css))),

    header_panel(
      "Customer Acquisition",
      "Originations funnel, distribution, decline summary, and customer profiling"
    ),

    scope_toggle_ui("pia_data_scope"),

        # Product selector
        fluidRow(column(12, product_selector_ui("pia"))),

        # Filter bar
        tags$div(class = "pi-filter-bar",
          tags$div(class = "pi-filter-group",
            tags$label("Frequency"),
            selectInput("pi_orig_frequency", NULL,
              choices = c("Daily", "Weekly", "Monthly", "Quarterly", "Yearly"),
              selected = "Monthly", width = "140px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Date Range"),
            dateRangeInput("pi_orig_date_range", NULL,
              start = "2022-01-01", end = "2025-12-31",
              min = "2020-01-01", max = "2026-12-31",
              format = "dd M yyyy")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Grade"),
            selectInput("pi_orig_grade", NULL,
              choices = c("All", "High", "Med", "Low"),
              selected = "All", width = "130px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Repeat Type"),
            selectInput("pi_orig_repeat", NULL,
              choices = c("All", "New", "Repeat"),
              selected = "All", width = "130px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Partner Channel"),
            selectInput("pi_orig_channel", NULL,
              choices = c("All", "Direct", "Partner A", "Partner B", "Retail"),
              selected = "All", width = "150px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("View"),
            radioGroupButtons("pi_view_mode", NULL,
              choices = c("\U0023 Volume" = "volume", "$ Dollar" = "dollar"),
              selected = "volume", size = "sm")
          )
        ),

        # Dynamic Insight
        uiOutput("pi_orig_insight"),

        # KPI Cards
        uiOutput("pi_orig_kpis"),

        # Metric Selector Panel + Chart
        tags$div(class = "pi-section-heading", "Origination Funnel Metrics"),
        uiOutput("pi_orig_metric_panel"),

        # Distribution Section
        tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;",
          tags$div(class = "pi-section-heading", style = "margin:0; border:none;", "Distribution"),
          selectInput("pi_orig_dist_by", NULL,
            choices = c("Grade", "Channel", "Repeat Type"),
            selected = "Grade", width = "180px")
        ),
        fluidRow(
          column(6, tags$div(class = "pi-chart-box",
            plotlyOutput("pi_orig_dist_stacked", height = "360px")
          )),
          column(6, tags$div(class = "pi-chart-box",
            plotlyOutput("pi_orig_dist_pct", height = "360px")
          ))
        ),

        # Decline Summary
        tags$div(class = "pi-section-heading", "Decline Summary"),
        uiOutput("pi_dec_kpis"),
        fluidRow(
          column(6, tags$div(class = "pi-chart-box",
            plotlyOutput("pi_dec_reasons_chart", height = "360px")
          )),
          column(6, tags$div(class = "pi-chart-box",
            plotlyOutput("pi_dec_trend_chart", height = "360px")
          ))
        ),

        # Customer Profiling
        tags$div(class = "pi-section-heading", "Customer Profiling"),
        tags$div(class = "pi-filter-bar", style = "margin-bottom: 16px;",
          tags$div(class = "pi-filter-group",
            tags$label("Dimension"),
            selectInput("pi_orig_prof_dimension", NULL,
              choices = c("FICO Range", "Income Band", "Channel", "Risk Tier", "MOB"),
              selected = "FICO Range", width = "200px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Loan Stage"),
            radioGroupButtons("pi_prof_stage", NULL,
              choices = c("Approved" = "approved", "Issued" = "issued"),
              selected = "approved", size = "sm")
          )
        ),
        uiOutput("pi_prof_kpis"),
        fluidRow(
          column(6, tags$div(class = "pi-chart-box",
            plotlyOutput("pi_prof_dist_chart", height = "360px")
          )),
          column(6, tags$div(class = "pi-chart-box",
            plotlyOutput("pi_prof_stacked_chart", height = "360px")
          ))
        )
  )
}

pi_performance_ui <- function() {
  tabItem(tabName = "pi_performance",
    tags$head(tags$style(HTML(pi_css))),

    header_panel(
      "Customer Performance",
      "Delinquency trends, vintage curves, and MOB breakdown analytics"
    ),

    scope_toggle_ui("pip_data_scope"),

        # Product selector
        fluidRow(column(12, product_selector_ui("pip"))),

        tags$div(class = "pi-filter-bar",
          tags$div(class = "pi-filter-group",
            tags$label("Frequency"),
            selectInput("pi_vin_frequency", NULL,
              choices = c("Monthly", "Quarterly", "Yearly"),
              selected = "Monthly", width = "140px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Date Range"),
            dateRangeInput("pi_vin_date_range", NULL,
              start = "2022-01-01", end = "2025-12-31",
              min = "2020-01-01", max = "2026-12-31",
              format = "dd M yyyy")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Grade"),
            selectInput("pi_vin_grade", NULL,
              choices = c("All", "High", "Med", "Low"),
              selected = "All", width = "130px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Repeat Type"),
            selectInput("pi_vin_repeat", NULL,
              choices = c("All", "New", "Repeat"),
              selected = "All", width = "130px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("Partner Channel"),
            selectInput("pi_vin_channel", NULL,
              choices = c("All", "Direct", "Partner A", "Partner B", "Retail"),
              selected = "All", width = "150px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("MOB Range"),
            selectInput("pi_vin_mob_range", NULL,
              choices = c("All", "0-3", "0-6", "0-9", "0-12"),
              selected = "All", width = "130px")
          ),
          tags$div(class = "pi-filter-group",
            tags$label("View"),
            radioGroupButtons("pi_vin_view_mode", NULL,
              choices = c("\U0023 Volume" = "volume", "$ Dollar" = "dollar"),
              selected = "volume", size = "sm")
          )
        ),

        # KPI Cards
        uiOutput("pi_vin_kpis"),

        # Insight
        uiOutput("pi_vin_insight"),

        # Charts
        tags$div(class = "pi-section-heading", "+2 Rate Overview"),
        fluidRow(
          column(6, tags$div(class = "pi-chart-box",
            plotlyOutput("pi_vin_del_trend", height = "360px"))),
          column(6, tags$div(class = "pi-chart-box",
            plotlyOutput("pi_vin_vintage_chart", height = "360px")))
        ),

        # MOB Breakdown
        tags$div(class = "pi-section-heading", "MOB Breakdown"),
        tags$div(class = "pi-filter-bar", style = "margin-bottom: 16px;",
          tags$div(class = "pi-filter-group",
            tags$label("MOB Buckets"),
            pickerInput("pi_vin_mob_select", NULL,
              choices = c("0-3", "4-6", "7-12", "13-18", "19-24"),
              selected = c("0-3", "4-6", "7-12", "13-18", "19-24"),
              multiple = TRUE,
              options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                             `count-selected-text` = "{0} MOBs selected"))
          )
        ),
        tags$div(class = "pi-chart-box",
          plotlyOutput("pi_vin_mob_del_chart", height = "400px")
        )
  )
}

# ============================================================
# SERVER FUNCTION
# ============================================================
portfolio_insights_server <- function(input, output, session, credit_data, perf_tape) {

  # ---- Preflight column checks (defensive init) ----
  .pi_required_cols <- c("date", "maximum_offered_amount", "first_delinquency_mob",
                          "issued_flag", "issued_amount", "performance_delinquency_flag",
                          "booking_vintage", "fico_band", "risk_grade", "partner_channel",
                          "borrower_repeat_type", "interest_rate_apr")
  .pi_missing <- setdiff(.pi_required_cols, names(credit_data))
  if (length(.pi_missing) > 0) {
    message("[PortfolioInsights] Missing columns: ", paste(.pi_missing, collapse = ", "),
            " \u2014 some charts may be disabled")
  }
  .pi_has_col <- function(col) col %in% names(credit_data)

  # ---- Shared safe fallbacks for renderers ----
  .pi_empty_plotly <- function(msg = "Data unavailable") {
    plotly::plot_ly() %>% plotly::layout(
      title = list(text = msg, font = list(size = 12, color = "#888")),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      plot_bgcolor = "#FFF", paper_bgcolor = "#FFF"
    ) %>% plotly::config(displayModeBar = FALSE)
  }
  .pi_empty_ui <- function(msg = "Data unavailable") {
    tags$div(class = "pi-insight-box",
             style = "color:#888;font-size:12px;padding:12px;",
             msg)
  }

  # ---- Build data frames from real credit_data ----
  # These replace the former synthetic globals with identical column structures.
  cd <- credit_data

  # 1. pi_funnel_data: one row per application with indicator columns
  pi_funnel_data <- cd %>%
    mutate(
      product = "Personal Loan",
      day_date = date,
      month_date = lubridate::floor_date(date, "month"),
      month_label = format(lubridate::floor_date(date, "month"), "%b %Y"),
      day_label = format(date, "%d %b"),
      week_label = paste0("W", lubridate::isoweek(date), " ", lubridate::year(date)),
      quarter = paste0("Q", lubridate::quarter(date), " ", lubridate::year(date)),
      year = as.character(lubridate::year(date)),
      grade = risk_grade,
      repeat_type = borrower_repeat_type,
      applications = 1L,
      approvals = as.integer(offered_decision_flag == "APPR"),
      conversions = as.integer(issued_flag == 1),
      quotes = 1L,
      app_dollars = ifelse(!is.na(maximum_offered_amount), maximum_offered_amount, 0),
      appr_dollars = ifelse(offered_decision_flag == "APPR" & !is.na(maximum_offered_amount),
                            maximum_offered_amount, 0),
      conv_dollars = ifelse(issued_flag == 1 & !is.na(issued_amount), issued_amount, 0),
      issued_dollars = ifelse(issued_flag == 1 & !is.na(issued_amount), issued_amount, 0),
      avg_apr = ifelse(issued_flag == 1, interest_rate_apr, NA_real_),
      avg_amount = ifelse(issued_flag == 1, issued_amount, NA_real_)
    )

  # 2. pi_decline_data: monthly decline counts by reason
  pi_decline_data <- cd %>%
    filter(offered_decision_flag != "APPR") %>%
    mutate(
      product = "Personal Loan",
      month_date = lubridate::floor_date(date, "month"),
      month_label = format(lubridate::floor_date(date, "month"), "%b %Y"),
      quarter = paste0("Q", lubridate::quarter(date), " ", lubridate::year(date)),
      year = as.character(lubridate::year(date)),
      reason = if ("decline_reason" %in% names(.))
                  ifelse(is.na(decline_reason) | decline_reason == "", "Other",
                         gsub("^\\d+\\.\\s*", "", decline_reason))
                else "Unspecified"
    ) %>%
    group_by(product, month_date, month_label, quarter, year, reason) %>%
    summarise(declined = n(), .groups = "drop")

  # 3. pi_profiling_data: monthly by dimension/segment
  pi_build_profiling <- function(cd, dim_name, seg_col) {
    cd %>%
      mutate(
        product = "Personal Loan",
        month_date = lubridate::floor_date(date, "month"),
        month_label = format(lubridate::floor_date(date, "month"), "%b %Y"),
        quarter = paste0("Q", lubridate::quarter(date), " ", lubridate::year(date)),
        year = as.character(lubridate::year(date)),
        dimension = dim_name,
        segment = !!sym(seg_col)
      ) %>%
      filter(!is.na(segment)) %>%
      group_by(product, month_date, month_label, quarter, year, dimension, segment) %>%
      summarise(
        volume = n(),
        approved_volume = sum(offered_decision_flag == "APPR", na.rm = TRUE),
        issued_volume = sum(issued_flag == 1, na.rm = TRUE),
        approved_amount = sum(ifelse(offered_decision_flag == "APPR", maximum_offered_amount, 0), na.rm = TRUE),
        issued_amount = sum(ifelse(issued_flag == 1, issued_amount, 0), na.rm = TRUE),
        del_rate = round(mean(performance_delinquency_flag, na.rm = TRUE) * 100, 2),
        avg_dti = round(mean(debt_to_income_ratio * 100, na.rm = TRUE), 1),
        avg_term = round(mean(term_months, na.rm = TRUE)),
        joint_pct = 0,
        .groups = "drop"
      )
  }

  # Build income_band for profiling
  cd_prof <- cd %>%
    mutate(
      income_band = cut(annual_income,
        breaks = c(-Inf, 30000, 50000, 75000, 100000, Inf),
        labels = c("<30K", "30-50K", "50-75K", "75-100K", "100K+"),
        right = FALSE)
    )

  pi_profiling_data <- bind_rows(
    pi_build_profiling(cd_prof, "FICO Range", "fico_band"),
    pi_build_profiling(cd_prof, "Income Band", "income_band"),
    pi_build_profiling(cd_prof, "Channel", "partner_channel"),
    pi_build_profiling(cd_prof, "Risk Tier", "risk_grade"),
    pi_build_profiling(cd_prof, "MOB", "mob_bins")
  )

  # 4. pi_delinquency_ts: monthly delinquency rate by grade/repeat/channel (issued loans)
  tape <- isolate(perf_tape())
  if (!is.null(tape)) {
    # risk_grade already joined into tape$rows by 05_global_data_prep.R
    tape_rows <- tape$rows

    pi_delinquency_ts <- tape_rows %>%
      mutate(
        product = "Personal Loan",
        month_date = payment_month,
        month_label = format(payment_month, "%b %Y"),
        quarter = paste0("Q", lubridate::quarter(payment_month), " ", lubridate::year(payment_month)),
        year = as.character(lubridate::year(payment_month)),
        grade = risk_grade,
        repeat_type = borrower_repeat_type
      ) %>%
      group_by(product, grade, repeat_type, partner_channel,
               month_date, month_label, quarter, year) %>%
      summarise(
        del_rate = round(mean(ever_del, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      )
  } else {
    pi_delinquency_ts <- cd %>%
      filter(issued_flag == 1) %>%
      mutate(
        product = "Personal Loan",
        month_date = lubridate::floor_date(date, "month"),
        month_label = format(lubridate::floor_date(date, "month"), "%b %Y"),
        quarter = paste0("Q", lubridate::quarter(date), " ", lubridate::year(date)),
        year = as.character(lubridate::year(date)),
        grade = risk_grade,
        repeat_type = borrower_repeat_type
      ) %>%
      group_by(product, grade, repeat_type, partner_channel,
               month_date, month_label, quarter, year) %>%
      summarise(
        del_rate = round(mean(performance_delinquency_flag, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      )
  }

  # 5. pi_vintage_data: cumulative bad rate by cohort and MOB
  if (!is.null(tape)) {
    rows_sub <- tape$rows
    min_dq_mob <- 4L

    cohort_size <- rows_sub %>%
      filter(month_on_books == 0) %>%
      group_by(vintage_quarter) %>%
      summarise(n_loans = n_distinct(loan_id), .groups = "drop")

    first_dq <- rows_sub %>%
      filter(co_amount > 0) %>%
      group_by(loan_id, vintage_quarter) %>%
      summarise(first_dq_mob = min(month_on_books), .groups = "drop")
    flagged_no_co <- rows_sub %>%
      filter(performance_delinquency_flag == 1) %>%
      distinct(loan_id, vintage_quarter) %>%
      anti_join(first_dq, by = "loan_id") %>%
      mutate(first_dq_mob = min_dq_mob)
    first_dq <- bind_rows(first_dq, flagged_no_co) %>%
      mutate(first_dq_mob = pmax(first_dq_mob, min_dq_mob))

    max_mob_v <- rows_sub %>%
      group_by(vintage_quarter) %>%
      summarise(max_mob = max(month_on_books), .groups = "drop")

    grid <- max_mob_v %>%
      rowwise() %>%
      do({ data.frame(vintage_quarter = .$vintage_quarter,
                      mob = 0:.$max_mob, stringsAsFactors = FALSE) }) %>%
      ungroup() %>%
      left_join(cohort_size, by = "vintage_quarter")

    dq_counts <- first_dq %>%
      group_by(vintage_quarter) %>%
      arrange(first_dq_mob) %>%
      mutate(cum_bad = row_number()) %>%
      group_by(vintage_quarter, first_dq_mob) %>%
      summarise(cum_bad = max(cum_bad), .groups = "drop")

    pi_vintage_data <- grid %>%
      left_join(dq_counts %>% rename(mob = first_dq_mob),
                by = c("vintage_quarter", "mob")) %>%
      arrange(vintage_quarter, mob) %>%
      group_by(vintage_quarter) %>%
      tidyr::fill(cum_bad, .direction = "down") %>%
      mutate(cum_bad = ifelse(is.na(cum_bad), 0, cum_bad),
             del_rate = round(cum_bad / pmax(n_loans, 1) * 100, 2)) %>%
      ungroup() %>%
      mutate(product = "Personal Loan", cohort = vintage_quarter) %>%
      select(product, cohort, mob, del_rate)
  } else {
    vd_issued <- cd %>%
      filter(issued_flag == 1, !is.na(booking_vintage)) %>%
      mutate(
        bv_date = as.Date(booking_vintage, format = "%d/%m/%Y"),
        cohort_qtr = lubridate::floor_date(bv_date, "quarter"),
        cohort = format(cohort_qtr, "%b %Y")
      ) %>%
      filter(!is.na(cohort_qtr))

    cohort_sizes <- vd_issued %>% count(cohort, name = "cohort_size")
    max_mob <- 12
    mob_seq <- 0:max_mob
    pi_vintage_data <- expand.grid(
      cohort = unique(vd_issued$cohort),
      mob = mob_seq,
      stringsAsFactors = FALSE
    ) %>%
      left_join(cohort_sizes, by = "cohort") %>%
      left_join(
        vd_issued %>%
          filter(!is.na(first_delinquency_mob)) %>%
          group_by(cohort) %>%
          summarise(
            del_by_0 = 0,
            del_by_1 = sum(first_delinquency_mob <= 1),
            del_by_2 = sum(first_delinquency_mob <= 2),
            del_by_3 = sum(first_delinquency_mob <= 3),
            del_by_4 = sum(first_delinquency_mob <= 4),
            del_by_5 = sum(first_delinquency_mob <= 5),
            del_by_6 = sum(first_delinquency_mob <= 6),
            del_by_7 = sum(first_delinquency_mob <= 7),
            del_by_8 = sum(first_delinquency_mob <= 8),
            del_by_9 = sum(first_delinquency_mob <= 9),
            del_by_10 = sum(first_delinquency_mob <= 10),
            del_by_11 = sum(first_delinquency_mob <= 11),
            del_by_12 = sum(first_delinquency_mob <= 12),
            .groups = "drop"
          ) %>%
          tidyr::pivot_longer(cols = starts_with("del_by_"),
                              names_to = "mob_key", values_to = "cum_del") %>%
          mutate(mob = as.integer(gsub("del_by_", "", mob_key))) %>%
          select(cohort, mob, cum_del),
        by = c("cohort", "mob")
      ) %>%
      mutate(
        product = "Personal Loan",
        cum_del = ifelse(is.na(cum_del), 0, cum_del),
        del_rate = round(cum_del / pmax(cohort_size, 1) * 100, 2)
      ) %>%
      select(product, cohort, mob, del_rate)
  }

  # ---- Shared reactive state ----
  pi_selected_product <- reactiveVal("Personal Loan")
  pi_selected_metric  <- reactiveVal("Applications")
  pi_selected_mode    <- reactiveVal("volume")

  # ---- Product selectors — uses shared product_selector_ui("pia") / product_selector_ui("pip") ----
  observeEvent(input$pia_product, { pi_selected_product(input$pia_product) })
  observeEvent(input$pip_product, { pi_selected_product(input$pip_product) })
  observeEvent(input$pi_metric_click,  { pi_selected_metric(input$pi_metric_click) })
  observeEvent(input$pi_view_mode,     { pi_selected_mode(input$pi_view_mode) })
  observeEvent(input$pi_vin_view_mode, { pi_selected_mode(input$pi_vin_view_mode) })

  # =========================================================================
  # ORIGINATIONS: Filtered data
  # =========================================================================
  pi_orig_filtered <- reactive({
    fd <- pi_funnel_data[pi_funnel_data$product == pi_selected_product(), ]

    if (!is.null(input$pi_orig_grade) && input$pi_orig_grade != "All")
      fd <- fd[fd$grade == input$pi_orig_grade, ]
    if (!is.null(input$pi_orig_repeat) && input$pi_orig_repeat != "All")
      fd <- fd[fd$repeat_type == input$pi_orig_repeat, ]
    if (!is.null(input$pi_orig_channel) && input$pi_orig_channel != "All")
      fd <- fd[fd$partner_channel == input$pi_orig_channel, ]
    if (!is.null(input$pi_orig_date_range)) {
      fd <- fd[fd$day_date >= input$pi_orig_date_range[1] & fd$day_date <= input$pi_orig_date_range[2], ]
    }

    freq <- input$pi_orig_frequency %||% "Monthly"

    if (freq == "Daily") {
      fd <- fd %>% group_by(day_date) %>%
        summarise(month_label = first(day_label),
          applications = sum(applications), approvals = sum(approvals),
          conversions = sum(conversions), quotes = sum(quotes),
          app_dollars = sum(app_dollars), appr_dollars = sum(appr_dollars),
          conv_dollars = sum(conv_dollars), issued_dollars = sum(issued_dollars),
          avg_apr = mean(avg_apr), avg_amount = mean(avg_amount), .groups = "drop") %>%
        arrange(day_date) %>%
        mutate(app_rate = ifelse(quotes > 0, applications / quotes, 0),
               approval_rate = ifelse(applications > 0, approvals / applications, 0),
               conv_rate = ifelse(approvals > 0, conversions / approvals, 0))
    } else if (freq == "Weekly") {
      fd <- fd %>% group_by(week_label) %>%
        summarise(month_label = first(week_label), sort_date = min(day_date),
          applications = sum(applications), approvals = sum(approvals),
          conversions = sum(conversions), quotes = sum(quotes),
          app_dollars = sum(app_dollars), appr_dollars = sum(appr_dollars),
          conv_dollars = sum(conv_dollars), issued_dollars = sum(issued_dollars),
          avg_apr = mean(avg_apr), avg_amount = mean(avg_amount), .groups = "drop") %>%
        arrange(sort_date) %>%
        mutate(app_rate = ifelse(quotes > 0, applications / quotes, 0),
               approval_rate = ifelse(applications > 0, approvals / applications, 0),
               conv_rate = ifelse(approvals > 0, conversions / approvals, 0))
    } else if (freq == "Monthly") {
      fd <- fd %>% group_by(month_date) %>%
        summarise(month_label = first(month_label),
          applications = sum(applications), approvals = sum(approvals),
          conversions = sum(conversions), quotes = sum(quotes),
          app_dollars = sum(app_dollars), appr_dollars = sum(appr_dollars),
          conv_dollars = sum(conv_dollars), issued_dollars = sum(issued_dollars),
          avg_apr = mean(avg_apr), avg_amount = mean(avg_amount), .groups = "drop") %>%
        arrange(month_date) %>%
        mutate(app_rate = ifelse(quotes > 0, applications / quotes, 0),
               approval_rate = ifelse(applications > 0, approvals / applications, 0),
               conv_rate = ifelse(approvals > 0, conversions / approvals, 0))
    } else {
      group_col <- if (freq == "Quarterly") "quarter" else "year"
      fd <- fd %>% group_by(across(all_of(group_col))) %>%
        summarise(month_label = first(!!sym(group_col)), sort_date = min(day_date),
          applications = sum(applications), approvals = sum(approvals),
          conversions = sum(conversions), quotes = sum(quotes),
          app_dollars = sum(app_dollars), appr_dollars = sum(appr_dollars),
          conv_dollars = sum(conv_dollars), issued_dollars = sum(issued_dollars),
          avg_apr = mean(avg_apr), avg_amount = mean(avg_amount), .groups = "drop") %>%
        arrange(sort_date) %>%
        mutate(app_rate = ifelse(quotes > 0, applications / quotes, 0),
               approval_rate = ifelse(applications > 0, approvals / applications, 0),
               conv_rate = ifelse(approvals > 0, conversions / approvals, 0))
    }
    fd
  })

  # =========================================================================
  # KPI Cards (8)
  # =========================================================================
  output$pi_orig_kpis <- renderUI({
    tryCatch({
    fd <- pi_orig_filtered()
    freq <- input$pi_orig_frequency %||% "Monthly"
    plabel <- switch(freq, "Daily" = "day", "Weekly" = "week", "Monthly" = "month",
                     "Quarterly" = "quarter", "Yearly" = "year", "period")
    total_apps <- sum(fd$applications)
    total_appr <- sum(fd$approvals)
    total_issued <- sum(fd$conversions)
    avg_appr_rate <- mean(fd$approval_rate, na.rm = TRUE)
    avg_take_rate <- mean(fd$conv_rate, na.rm = TRUE)
    avg_apr_val <- mean(fd$avg_apr, na.rm = TRUE)
    app_appr_dropoff <- (1 - avg_appr_rate) * 100
    appr_conv_pct <- avg_take_rate * 100
    cards <- paste0(
      '<div class="pi-kpi-grid pi-kpi-grid-4">',
      pi_kpi_card("Applications", pi_fmt_num(total_apps),
                  pi_compute_delta(fd$applications), "orange", plabel),
      pi_kpi_card("Approvals", pi_fmt_num(total_appr),
                  pi_compute_delta(fd$approvals), "blue", plabel),
      pi_kpi_card("Issued", pi_fmt_num(total_issued),
                  pi_compute_delta(fd$conversions), "teal", plabel),
      pi_kpi_card("Avg APR", paste0(round(avg_apr_val, 1), "%"),
                  pi_compute_delta(fd$avg_apr), "light", plabel),
      pi_kpi_card("Approval Rate", paste0(round(avg_appr_rate * 100, 1), "%"),
                  pi_compute_delta(fd$approval_rate * 100), "orange", plabel),
      pi_kpi_card("Take Rate", paste0(round(avg_take_rate * 100, 1), "%"),
                  pi_compute_delta(fd$conv_rate * 100), "blue", plabel),
      pi_kpi_card("App \u2192 Approval Drop-off", paste0(round(app_appr_dropoff, 1), "%"),
                  NULL, "teal"),
      pi_kpi_card("Approval \u2192 Conversion", paste0(round(appr_conv_pct, 1), "%"),
                  NULL, "light"),
      '</div>')
    HTML(cards)
    }, error = function(e) {
      message("[PortfolioInsights] pi_orig_kpis: ", e$message)
      .pi_empty_ui("KPIs unavailable")
    })
  })

  # =========================================================================
  # Metric Selector Panel
  # =========================================================================
  output$pi_orig_metric_panel <- renderUI({
    tryCatch({
    metric <- pi_selected_metric()
    metrics <- c("Applications", "Approvals", "Issued", "Approval Rate", "Take Rate")
    metric_btns <- lapply(metrics, function(m) {
      cls <- if (m == metric) "pi-metric-btn active" else "pi-metric-btn"
      tags$div(class = cls,
        onclick = sprintf("Shiny.setInputValue('pi_metric_click', '%s', {priority:'event'})", m), m)
    })
    tags$div(class = "pi-metric-panel",
      tags$div(class = "pi-metric-sidebar",
        tags$div(class = "pi-sidebar-section-label", "Metric"),
        tags$div(class = "pi-metric-btn-group", metric_btns)
      ),
      tags$div(class = "pi-metric-chart-area",
        plotlyOutput("pi_orig_main_chart", height = "380px")
      )
    )
    }, error = function(e) {
      message("[PortfolioInsights] pi_orig_metric_panel: ", e$message)
      .pi_empty_ui("Metric panel unavailable")
    })
  })

  # =========================================================================
  # Main Chart (bar for volumes, combo for rates)
  # =========================================================================
  output$pi_orig_main_chart <- renderPlotly({
    tryCatch({
    fd <- pi_orig_filtered()
    metric <- pi_selected_metric()
    mode <- pi_selected_mode()
    is_rate <- metric %in% c("Approval Rate", "Take Rate")

    if (is_rate) {
      rate_col <- if (metric == "Approval Rate") "approval_rate" else "conv_rate"
      rate_vals <- fd[[rate_col]] * 100
      rate_label <- metric
      if (metric == "Take Rate") {
        bar_y <- if (mode == "dollar") fd$appr_dollars else fd$approvals
        bar_name <- if (mode == "dollar") "Approvals ($)" else "Approvals"
        bar_fmt <- if (mode == "dollar") "$,.0f" else ","
        bar_hover <- if (mode == "dollar") {
          paste0("%{x}<br>Approvals: ", pi_fmt_dollar(bar_y), "<extra></extra>")
        } else { "%{x}<br>Approvals: %{y:,}<extra></extra>" }
      } else {
        bar_y <- if (mode == "dollar") fd$app_dollars else fd$applications
        bar_name <- if (mode == "dollar") "Applications ($)" else "Applications"
        bar_fmt <- if (mode == "dollar") "$,.0f" else ","
        bar_hover <- if (mode == "dollar") {
          paste0("%{x}<br>Applications: ", pi_fmt_dollar(bar_y), "<extra></extra>")
        } else { "%{x}<br>Applications: %{y:,}<extra></extra>" }
      }
      plot_ly() %>%
        add_bars(x = fd$month_label, y = bar_y, name = bar_name,
                 marker = list(color = PI_MIDNIGHT, line = list(width = 0), cornerradius = 4),
                 hovertemplate = bar_hover) %>%
        add_trace(x = fd$month_label, y = rate_vals, name = rate_label,
                  type = "scatter", mode = "lines+markers", yaxis = "y2",
                  line = list(color = PI_ORANGE, width = 3),
                  marker = list(color = PI_ORANGE, size = 8),
                  hovertemplate = paste0("%{x}<br>", rate_label, ": %{y:.1f}%<extra></extra>")) %>%
        layout(
          title = list(text = paste0("<b>", bar_name, " & ", rate_label, "</b>"),
                       font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
          xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Source Sans Pro"),
                       categoryorder = "array", categoryarray = fd$month_label),
          yaxis = list(title = bar_name, tickfont = list(size = 11, family = "Source Sans Pro"),
                       gridcolor = "#EEF0F2", tickformat = bar_fmt),
          yaxis2 = list(title = paste0(rate_label, " (%)"), overlaying = "y", side = "right",
                        tickfont = list(size = 11, family = "Source Sans Pro", color = PI_ORANGE),
                        titlefont = list(color = PI_ORANGE), showgrid = FALSE,
                        range = c(max(0, min(rate_vals) - 5), min(100, max(rate_vals) + 5)),
                        ticksuffix = "%"),
          legend = list(orientation = "h", y = -0.25, font = list(size = 11, family = "Source Sans Pro")),
          plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
          margin = list(t = 40, b = 90, l = 60, r = 70), bargap = 0.3
        ) %>% config(displayModeBar = FALSE)

    } else {
      y_col <- switch(metric,
        "Applications" = if (mode == "dollar") "app_dollars" else "applications",
        "Approvals"    = if (mode == "dollar") "appr_dollars" else "approvals",
        "Issued"       = if (mode == "dollar") "issued_dollars" else "conversions",
        "applications")
      y_vals <- fd[[y_col]]
      is_dollar <- mode == "dollar"
      y_title <- if (is_dollar) "Amount ($)" else "Volume"
      chart_label <- if (is_dollar) paste0(metric, " ($)") else metric
      hover_text <- if (is_dollar) {
        paste0(fd$month_label, "<br>", chart_label, ": ", pi_fmt_dollar(y_vals))
      } else {
        paste0(fd$month_label, "<br>", chart_label, ": ", pi_fmt_num(y_vals))
      }
      plot_ly(x = fd$month_label, y = y_vals, type = "bar",
              marker = list(color = PI_MIDNIGHT, line = list(width = 0), cornerradius = 4),
              hovertext = hover_text, hoverinfo = "text") %>%
        layout(
          title = list(text = paste0("<b>", chart_label, " by Period</b>"),
                       font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
          xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Source Sans Pro"),
                       categoryorder = "array", categoryarray = fd$month_label),
          yaxis = list(title = y_title, tickfont = list(size = 11, family = "Source Sans Pro"),
                       gridcolor = "#EEF0F2", tickformat = if (is_dollar) "$,.0f" else ","),
          plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
          margin = list(t = 40, b = 80, l = 60, r = 20), bargap = 0.3
        ) %>% config(displayModeBar = FALSE)
    }
    }, error = function(e) {
      message("[PortfolioInsights] pi_orig_main_chart: ", e$message)
      .pi_empty_plotly("Chart unavailable")
    })
  })

  # =========================================================================
  # Distribution
  # =========================================================================
  pi_dist_data <- reactive({
    fd <- pi_funnel_data[pi_funnel_data$product == pi_selected_product(), ]
    if (!is.null(input$pi_orig_grade) && input$pi_orig_grade != "All")
      fd <- fd[fd$grade == input$pi_orig_grade, ]
    if (!is.null(input$pi_orig_repeat) && input$pi_orig_repeat != "All")
      fd <- fd[fd$repeat_type == input$pi_orig_repeat, ]
    if (!is.null(input$pi_orig_channel) && input$pi_orig_channel != "All")
      fd <- fd[fd$partner_channel == input$pi_orig_channel, ]
    if (!is.null(input$pi_orig_date_range)) {
      fd <- fd[fd$day_date >= input$pi_orig_date_range[1] & fd$day_date <= input$pi_orig_date_range[2], ]
    }
    dim_col <- switch(input$pi_orig_dist_by %||% "Grade",
      "Grade" = "grade", "Channel" = "partner_channel", "Repeat Type" = "repeat_type", "grade")
    freq <- input$pi_orig_frequency %||% "Monthly"
    time_col <- switch(freq,
      "Daily" = "day_label", "Weekly" = "week_label",
      "Monthly" = "month_label", "Quarterly" = "quarter", "Yearly" = "year", "month_label")
    sort_col <- switch(freq,
      "Daily" = "day_date", "Weekly" = "day_date",
      "Monthly" = "month_date", "Quarterly" = "day_date", "Yearly" = "day_date", "day_date")
    fd %>%
      group_by(period = !!sym(time_col), segment = !!sym(dim_col)) %>%
      summarise(sort_key = min(!!sym(sort_col)),
        applications = sum(applications), approvals = sum(approvals),
        conversions = sum(conversions),
        app_dollars = sum(app_dollars), appr_dollars = sum(appr_dollars),
        issued_dollars = sum(issued_dollars), avg_apr = mean(avg_apr), .groups = "drop") %>%
      mutate(approval_rate = ifelse(applications > 0, approvals / applications, 0),
             conv_rate = ifelse(approvals > 0, conversions / approvals, 0)) %>%
      arrange(sort_key)
  })

  pi_dist_vol_col <- reactive({
    metric <- pi_selected_metric()
    mode <- pi_selected_mode()
    switch(metric,
      "Applications"  = if (mode == "dollar") "app_dollars" else "applications",
      "Approvals"     = if (mode == "dollar") "appr_dollars" else "approvals",
      "Issued"        = if (mode == "dollar") "issued_dollars" else "conversions",
      "Approval Rate" = "applications", "Take Rate" = "approvals", "applications")
  })

  pi_dist_chart_label <- reactive({
    metric <- pi_selected_metric()
    mode <- pi_selected_mode()
    is_rate <- metric %in% c("Approval Rate", "Take Rate")
    if (!is_rate && mode == "dollar") paste0(metric, " ($)")
    else if (metric == "Take Rate") "Approvals"
    else if (metric == "Approval Rate") "Applications"
    else metric
  })

  output$pi_orig_dist_stacked <- renderPlotly({
    tryCatch({
    dd <- pi_dist_data()
    dist_by <- input$pi_orig_dist_by %||% "Grade"
    metric <- pi_selected_metric()
    is_rate <- metric %in% c("Approval Rate", "Take Rate")
    mode <- pi_selected_mode()
    segments <- unique(dd$segment)
    seg_pal <- c(PI_MIDNIGHT, PI_ORANGE, PI_MIDNIGHT_70, PI_ORANGE_40, PI_MIDNIGHT_40)
    period_order <- unique(dd$period[order(dd$sort_key)])

    if (is_rate) {
      rate_col <- if (metric == "Approval Rate") "approval_rate" else "conv_rate"
      p <- plot_ly()
      for (i in seq_along(segments)) {
        seg <- segments[i]
        sd <- dd[dd$segment == seg, ]
        col <- seg_pal[(i - 1) %% length(seg_pal) + 1]
        p <- p %>% add_trace(x = sd$period, y = round(sd[[rate_col]] * 100, 1), name = seg,
                              type = "scatter", mode = "lines+markers",
                              line = list(color = col, width = 2.5),
                              marker = list(color = col, size = 6),
                              hovertemplate = paste0("%{x}<br>", seg, ": %{y:.1f}%<extra></extra>"))
      }
      p %>% layout(
        title = list(text = paste0("<b>Distribution of ", metric, " by ", dist_by, "</b>"),
                     font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
        xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Source Sans Pro"),
                     categoryorder = "array", categoryarray = period_order),
        yaxis = list(title = paste0(metric, " (%)"), tickfont = list(size = 11, family = "Source Sans Pro"),
                     gridcolor = "#EEF0F2", ticksuffix = "%"),
        legend = list(orientation = "h", y = -0.25, font = list(size = 11, family = "Source Sans Pro")),
        plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
        margin = list(t = 40, b = 90, l = 60, r = 20)
      ) %>% config(displayModeBar = FALSE)
    } else {
      y_col <- pi_dist_vol_col()
      chart_label <- pi_dist_chart_label()
      is_dollar <- mode == "dollar"
      p <- plot_ly()
      for (i in seq_along(segments)) {
        seg <- segments[i]
        sd <- dd[dd$segment == seg, ]
        col <- seg_pal[(i - 1) %% length(seg_pal) + 1]
        p <- p %>% add_bars(x = sd$period, y = sd[[y_col]], name = seg,
                             marker = list(color = col),
                             hovertemplate = paste0("%{x}<br>", seg, ": %{y:,.0f}<extra></extra>"))
      }
      p %>% layout(
        barmode = "stack",
        title = list(text = paste0("<b>Distribution of ", chart_label, " by ", dist_by, "</b>"),
                     font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
        xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Source Sans Pro"),
                     categoryorder = "array", categoryarray = period_order),
        yaxis = list(title = if (is_dollar) "Amount ($)" else "Volume",
                     tickfont = list(size = 11, family = "Source Sans Pro"), gridcolor = "#EEF0F2",
                     tickformat = if (is_dollar) "$,.0f" else ","),
        legend = list(orientation = "h", y = -0.25, font = list(size = 11, family = "Source Sans Pro")),
        plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
        margin = list(t = 40, b = 90, l = 60, r = 20), bargap = 0.25
      ) %>% config(displayModeBar = FALSE)
    }
    }, error = function(e) {
      message("[PortfolioInsights] pi_orig_dist_stacked: ", e$message)
      .pi_empty_plotly("Distribution unavailable")
    })
  })

  output$pi_orig_dist_pct <- renderPlotly({
    tryCatch({
    dd <- pi_dist_data()
    dist_by <- input$pi_orig_dist_by %||% "Grade"
    metric <- pi_selected_metric()
    is_rate <- metric %in% c("Approval Rate", "Take Rate")
    segments <- unique(dd$segment)
    seg_pal <- c(PI_MIDNIGHT, PI_ORANGE, PI_MIDNIGHT_70, PI_ORANGE_40, PI_MIDNIGHT_40)
    period_order <- unique(dd$period[order(dd$sort_key)])
    y_col <- if (is_rate) {
      if (metric == "Approval Rate") "applications" else "approvals"
    } else { pi_dist_vol_col() }
    chart_label <- if (is_rate) {
      if (metric == "Approval Rate") "Applications" else "Approvals"
    } else { pi_dist_chart_label() }
    dd <- dd %>% group_by(period) %>%
      mutate(pct = !!sym(y_col) / sum(!!sym(y_col)) * 100) %>% ungroup()
    p <- plot_ly()
    for (i in seq_along(segments)) {
      seg <- segments[i]
      sd <- dd[dd$segment == seg, ]
      col <- seg_pal[(i - 1) %% length(seg_pal) + 1]
      p <- p %>% add_bars(x = sd$period, y = sd$pct, name = seg,
                           marker = list(color = col),
                           hovertemplate = paste0("%{x}<br>", seg, ": %{y:.1f}%<extra></extra>"))
    }
    p %>% layout(
      barmode = "stack",
      title = list(text = paste0("<b>", chart_label, " Mix Distribution by ", dist_by, " (%)</b>"),
                   font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
      xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Source Sans Pro"),
                   categoryorder = "array", categoryarray = period_order),
      yaxis = list(title = "Share (%)", tickfont = list(size = 11, family = "Source Sans Pro"),
                   gridcolor = "#EEF0F2", ticksuffix = "%", range = c(0, 100)),
      legend = list(orientation = "h", y = -0.25, font = list(size = 11, family = "Source Sans Pro")),
      plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
      margin = list(t = 40, b = 90, l = 60, r = 20), bargap = 0.25
    ) %>% config(displayModeBar = FALSE)
    }, error = function(e) {
      message("[PortfolioInsights] pi_orig_dist_pct: ", e$message)
      .pi_empty_plotly("Mix distribution unavailable")
    })
  })

  # Dynamic insight
  output$pi_orig_insight <- renderUI({
    tryCatch({
    fd <- pi_orig_filtered()
    metric <- pi_selected_metric()
    mode <- pi_selected_mode()
    prod <- pi_selected_product()
    if (nrow(fd) < 2) return(NULL)
    n <- nrow(fd)
    if (metric %in% c("Approval Rate", "Take Rate")) {
      rate_col <- if (metric == "Approval Rate") "approval_rate" else "conv_rate"
      current <- round(fd[[rate_col]][n] * 100, 1)
      prev <- round(fd[[rate_col]][n - 1] * 100, 1)
      avg_val <- round(mean(fd[[rate_col]]) * 100, 1)
      trend_dir <- if (current > prev) "improved" else "declined"
      change <- round(abs(current - prev), 1)
      insight <- sprintf(
        '<span class="pi-insight-icon">&#x1F4CA;</span> <strong>%s</strong> %s %s by <strong>%s%%</strong> to <strong>%s%%</strong> in the latest period. The average across the selected timeframe is <strong>%s%%</strong>.',
        prod, metric, trend_dir, change, current, avg_val)
    } else {
      vol_col <- switch(metric,
        "Applications" = if (mode == "dollar") "app_dollars" else "applications",
        "Approvals"    = if (mode == "dollar") "appr_dollars" else "approvals",
        "Issued"       = if (mode == "dollar") "issued_dollars" else "conversions",
        "applications")
      current <- fd[[vol_col]][n]
      prev <- fd[[vol_col]][n - 1]
      total <- sum(fd[[vol_col]])
      pct_change <- round((current - prev) / prev * 100, 1)
      trend_dir <- if (current > prev) "increased" else "decreased"
      fmt_fn <- if (mode == "dollar") pi_fmt_dollar else pi_fmt_num
      insight <- sprintf(
        '<span class="pi-insight-icon">&#x1F4CA;</span> <strong>%s</strong> %s %s by <strong>%s%%</strong> to <strong>%s</strong> in the latest period. Total across the selected timeframe: <strong>%s</strong>.',
        prod, metric, trend_dir, abs(pct_change), fmt_fn(current), fmt_fn(total))
    }
    tags$div(class = "pi-insight-box", HTML(insight))
    }, error = function(e) {
      message("[PortfolioInsights] pi_orig_insight: ", e$message)
      .pi_empty_ui("Insight unavailable")
    })
  })

  # =========================================================================
  # Decline Analysis
  # =========================================================================
  pi_dec_filtered <- reactive({
    dd <- pi_decline_data[pi_decline_data$product == pi_selected_product(), ]
    freq <- input$pi_orig_frequency %||% "Monthly"
    if (freq %in% c("Daily", "Weekly", "Monthly")) return(dd)
    group_col <- if (freq == "Quarterly") "quarter" else "year"
    dd %>% group_by(across(all_of(c(group_col, "reason")))) %>%
      summarise(declined = sum(declined), month_label = first(!!sym(group_col)),
                month_date = min(month_date), .groups = "drop")
  })

  output$pi_dec_kpis <- renderUI({
    tryCatch({
    dd <- pi_dec_filtered()
    fd <- pi_orig_filtered()
    total_dec <- sum(dd$declined)
    total_apps <- sum(fd$applications)
    dec_rate <- if (total_apps > 0) round(total_dec / total_apps * 100, 1) else 0
    cards <- paste0(
      '<div class="pi-kpi-grid pi-kpi-grid-4">',
      pi_kpi_card("Total Declined", pi_fmt_num(total_dec), NULL, "orange"),
      pi_kpi_card("Decline Rate", paste0(dec_rate, "%"), NULL, "blue"),
      pi_kpi_card("Total Applications", pi_fmt_num(total_apps), NULL, "teal"),
      pi_kpi_card("Avg APR", paste0(round(mean(fd$avg_apr), 1), "%"), NULL, "light"),
      '</div>')
    HTML(cards)
    }, error = function(e) {
      message("[PortfolioInsights] pi_dec_kpis: ", e$message)
      .pi_empty_ui("Decline KPIs unavailable")
    })
  })

  output$pi_dec_reasons_chart <- renderPlotly({
    tryCatch({
    dd <- pi_dec_filtered()
    agg <- dd %>% group_by(reason) %>% summarise(vol = sum(declined), .groups = "drop") %>%
      mutate(pct = round(vol / sum(vol) * 100, 1)) %>% arrange(pct)
    colors <- c(PI_ORANGE, PI_ORANGE_20, PI_ORANGE_40, PI_ORANGE_60, PI_ORANGE_80)
    agg$reason <- factor(agg$reason, levels = agg$reason)
    plot_ly(agg, y = ~reason, x = ~pct, type = "bar", orientation = "h",
            marker = list(color = rev(colors)), text = ~paste0(pct, "%"),
            textposition = "outside") %>%
      layout(title = list(text = "<b>Top Decline Reasons</b>",
                          font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
             xaxis = list(title = "% of Declines", gridcolor = "#EEF0F2"),
             yaxis = list(title = ""), plot_bgcolor = "#FFF", paper_bgcolor = "#FFF",
             margin = list(l = 120, t = 40, b = 40)) %>%
      config(displayModeBar = FALSE)
    }, error = function(e) {
      message("[PortfolioInsights] pi_dec_reasons_chart: ", e$message)
      .pi_empty_plotly("Decline reasons unavailable")
    })
  })

  output$pi_dec_trend_chart <- renderPlotly({
    tryCatch({
    dd <- pi_dec_filtered()
    trend <- dd %>% group_by(month_label, month_date) %>%
      summarise(dec = sum(declined), .groups = "drop") %>% arrange(month_date)
    fd_monthly <- pi_funnel_data[pi_funnel_data$product == pi_selected_product(), ] %>%
      group_by(month_date) %>% summarise(apps = sum(applications), .groups = "drop")
    merged <- merge(trend, fd_monthly, by = "month_date", all.x = TRUE) %>%
      mutate(rate = round(dec / apps * 100, 1)) %>% arrange(month_date)
    period_order <- merged$month_label
    plot_ly(merged, x = ~month_label, y = ~rate, type = "scatter", mode = "lines+markers",
            line = list(color = PI_ORANGE, width = 2.5),
            marker = list(color = PI_ORANGE, size = 8),
            hovertemplate = "%{x}<br>Decline Rate: %{y:.1f}%<extra></extra>") %>%
      layout(title = list(text = "<b>Decline Rate Trend</b>",
                          font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
             xaxis = list(title = "", tickangle = -45, categoryorder = "array", categoryarray = period_order),
             yaxis = list(title = "Decline Rate (%)", gridcolor = "#EEF0F2", rangemode = "tozero", ticksuffix = "%"),
             plot_bgcolor = "#FFF", paper_bgcolor = "#FFF", margin = list(t = 40, b = 80)) %>%
      config(displayModeBar = FALSE)
    }, error = function(e) {
      message("[PortfolioInsights] pi_dec_trend_chart: ", e$message)
      .pi_empty_plotly("Decline trend unavailable")
    })
  })

  # =========================================================================
  # Customer Profiling
  # =========================================================================
  pi_prof_filtered <- reactive({
    pd <- pi_profiling_data[pi_profiling_data$product == pi_selected_product() &
                            pi_profiling_data$dimension == (input$pi_orig_prof_dimension %||% "FICO Range"), ]
    if (!is.null(input$pi_orig_date_range)) {
      pd <- pd[pd$month_date >= input$pi_orig_date_range[1] & pd$month_date <= input$pi_orig_date_range[2], ]
    }
    freq <- input$pi_orig_frequency %||% "Monthly"
    if (freq %in% c("Daily", "Weekly", "Monthly")) return(pd)
    group_col <- if (freq == "Quarterly") "quarter" else "year"
    pd %>% group_by(across(all_of(c(group_col, "segment")))) %>%
      summarise(volume = sum(volume),
                approved_volume = sum(approved_volume), issued_volume = sum(issued_volume),
                approved_amount = sum(approved_amount), issued_amount = sum(issued_amount),
                del_rate = mean(del_rate), month_label = first(!!sym(group_col)),
                month_date = min(month_date), .groups = "drop")
  })

  output$pi_prof_kpis <- renderUI({
    tryCatch({
    pd <- pi_profiling_data[pi_profiling_data$product == pi_selected_product(), ]
    dim_name <- input$pi_orig_prof_dimension %||% "FICO Range"
    cards <- paste0(
      '<div class="pi-kpi-grid pi-kpi-grid-4">',
      pi_kpi_card("Avg DTI", paste0(round(mean(pd$avg_dti), 1), "%"), NULL, "orange"),
      pi_kpi_card("Avg Term", paste0(round(mean(pd$avg_term)), " mo"), NULL, "blue"),
      pi_kpi_card("Joint App Mix", paste0(round(mean(pd$joint_pct), 1), "%"), NULL, "teal"),
      pi_kpi_card(paste0(dim_name, " Segments"), length(unique(pd$segment[pd$dimension == dim_name])), NULL, "light"),
      '</div>')
    HTML(cards)
    }, error = function(e) {
      message("[PortfolioInsights] pi_prof_kpis: ", e$message)
      .pi_empty_ui("Profile KPIs unavailable")
    })
  })

  output$pi_prof_dist_chart <- renderPlotly({
    tryCatch({
    pd <- pi_prof_filtered()
    dim_name <- input$pi_orig_prof_dimension %||% "FICO Range"
    mode <- pi_selected_mode()
    stage <- input$pi_prof_stage %||% "approved"
    is_dollar <- mode == "dollar"
    val_col <- if (is_dollar) {
      if (stage == "issued") "issued_amount" else "approved_amount"
    } else {
      if (stage == "issued") "issued_volume" else "approved_volume"
    }
    stage_label <- if (stage == "issued") "Issued" else "Approved"
    y_title <- if (is_dollar) "Amount ($)" else "Volume"
    chart_prefix <- if (is_dollar) paste0(stage_label, " Amount") else paste0(stage_label, " Volume")
    period_order <- unique(pd$month_label[order(pd$month_date)])
    segments <- unique(pd$segment)
    seg_pal <- c(PI_MIDNIGHT, PI_ORANGE, PI_MIDNIGHT_70, PI_ORANGE_40, PI_MIDNIGHT_40)
    p <- plot_ly()
    for (i in seq_along(segments)) {
      seg <- segments[i]
      sd <- pd[pd$segment == seg, ] %>%
        group_by(month_label, month_date) %>%
        summarise(val = sum(!!sym(val_col)), .groups = "drop") %>% arrange(month_date)
      col <- seg_pal[(i - 1) %% length(seg_pal) + 1]
      hover_fmt <- if (is_dollar) paste0("%{x}<br>", seg, ": $%{y:,.0f}<extra></extra>")
                   else paste0("%{x}<br>", seg, ": %{y:,}<extra></extra>")
      p <- p %>% add_bars(x = sd$month_label, y = sd$val, name = seg,
                           marker = list(color = col), hovertemplate = hover_fmt)
    }
    p %>% layout(
      barmode = "stack",
      title = list(text = paste0("<b>Customer Profiling: ", chart_prefix, " by ", dim_name, "</b>"),
                   font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
      xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Source Sans Pro"),
                   categoryorder = "array", categoryarray = period_order),
      yaxis = list(title = y_title, gridcolor = "#EEF0F2", tickfont = list(size = 11, family = "Source Sans Pro"),
                   tickformat = if (is_dollar) "$,.0f" else ","),
      legend = list(orientation = "h", y = -0.25, font = list(size = 11, family = "Source Sans Pro")),
      plot_bgcolor = "#FFF", paper_bgcolor = "#FFF", margin = list(t = 40, b = 90)
    ) %>% config(displayModeBar = FALSE)
    }, error = function(e) {
      message("[PortfolioInsights] pi_prof_dist_chart: ", e$message)
      .pi_empty_plotly("Profile distribution unavailable")
    })
  })

  output$pi_prof_stacked_chart <- renderPlotly({
    tryCatch({
    pd <- pi_prof_filtered()
    dim_name <- input$pi_orig_prof_dimension %||% "FICO Range"
    period_order <- unique(pd$month_label[order(pd$month_date)])
    segments <- unique(pd$segment)
    seg_pal <- c(PI_MIDNIGHT, PI_ORANGE, PI_MIDNIGHT_70, PI_ORANGE_40, PI_MIDNIGHT_40)
    p <- plot_ly()
    for (i in seq_along(segments)) {
      seg <- segments[i]
      sd <- pd[pd$segment == seg, ] %>%
        group_by(month_label, month_date) %>%
        summarise(del = mean(del_rate), .groups = "drop") %>% arrange(month_date)
      col <- seg_pal[(i - 1) %% length(seg_pal) + 1]
      p <- p %>% add_trace(x = sd$month_label, y = sd$del, name = seg,
                            type = "scatter", mode = "lines+markers",
                            line = list(color = col, width = 2.5),
                            marker = list(color = col, size = 6),
                            hovertemplate = paste0("%{x}<br>", seg, ": %{y:.1f}%<extra></extra>"))
    }
    p %>% layout(
      title = list(text = paste0("<b>Customer Profiling: +2 Rate by ", dim_name, "</b>"),
                   font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
      xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Source Sans Pro"),
                   categoryorder = "array", categoryarray = period_order),
      yaxis = list(title = "+2 Rate (%)", gridcolor = "#EEF0F2", rangemode = "tozero", ticksuffix = "%",
                   tickfont = list(size = 11, family = "Source Sans Pro")),
      legend = list(orientation = "h", y = -0.25, font = list(size = 11, family = "Source Sans Pro")),
      plot_bgcolor = "#FFF", paper_bgcolor = "#FFF", margin = list(t = 40, b = 90)
    ) %>% config(displayModeBar = FALSE)
    }, error = function(e) {
      message("[PortfolioInsights] pi_prof_stacked_chart: ", e$message)
      .pi_empty_plotly("Profile +2 rate unavailable")
    })
  })

  # =========================================================================
  # Delinquency & Vintage (Customer Performance sub-tab)
  # =========================================================================
  pi_vin_del_filtered <- reactive({
    dd <- pi_delinquency_ts[pi_delinquency_ts$product == pi_selected_product(), ]
    if (!is.null(input$pi_vin_date_range)) {
      dd <- dd[dd$month_date >= input$pi_vin_date_range[1] & dd$month_date <= input$pi_vin_date_range[2], ]
    }
    vin_grade <- input$pi_vin_grade %||% "All"
    if (vin_grade != "All") dd <- dd[dd$grade == vin_grade, ]
    vin_repeat <- input$pi_vin_repeat %||% "All"
    if (vin_repeat != "All") dd <- dd[dd$repeat_type == vin_repeat, ]
    vin_channel <- input$pi_vin_channel %||% "All"
    if (vin_channel != "All") dd <- dd[dd$partner_channel == vin_channel, ]
    dd <- dd %>%
      group_by(month_date, month_label, quarter, year) %>%
      summarise(del_rate = round(mean(del_rate), 2), .groups = "drop")
    freq <- input$pi_vin_frequency %||% "Monthly"
    if (freq == "Quarterly") {
      dd <- dd %>% group_by(quarter) %>%
        summarise(month_label = first(quarter), month_date = min(month_date),
                  del_rate = mean(del_rate), .groups = "drop")
    } else if (freq == "Yearly") {
      dd <- dd %>% group_by(year) %>%
        summarise(month_label = first(year), month_date = min(month_date),
                  del_rate = mean(del_rate), .groups = "drop")
    }
    dd[order(dd$month_date), ]
  })

  output$pi_vin_kpis <- renderUI({
    tryCatch({
    dd <- pi_vin_del_filtered()
    vd <- pi_vintage_data[pi_vintage_data$product == pi_selected_product(), ]
    freq <- input$pi_vin_frequency %||% "Monthly"
    plabel <- switch(freq, "Monthly" = "month", "Quarterly" = "quarter", "Yearly" = "year", "period")
    current_del <- round(tail(dd$del_rate, 1), 2)
    avg_del <- round(mean(dd$del_rate), 2)
    peak_del <- round(max(dd$del_rate), 2)
    peak_period <- dd$month_label[which.max(dd$del_rate)]
    delta_del <- if (nrow(dd) >= 2) round(dd$del_rate[nrow(dd)] - dd$del_rate[nrow(dd) - 1], 2) else NA
    cards <- paste0(
      '<div class="pi-kpi-grid pi-kpi-grid-4">',
      pi_kpi_card("Current +2 Rate", paste0(current_del, "%"), delta_del, "orange", plabel),
      pi_kpi_card("Avg +2 Rate", paste0(avg_del, "%"), NULL, "blue"),
      pi_kpi_card(paste0("Peak +2 Rate (", peak_period, ")"), paste0(peak_del, "%"), NULL, "teal"),
      pi_kpi_card("Vintage Cohorts", length(unique(vd$cohort)), NULL, "light"),
      '</div>')
    HTML(cards)
    }, error = function(e) {
      message("[PortfolioInsights] pi_vin_kpis: ", e$message)
      .pi_empty_ui("Vintage KPIs unavailable")
    })
  })

  output$pi_vin_insight <- renderUI({
    tryCatch({
    dd <- pi_vin_del_filtered()
    if (nrow(dd) < 2) return(NULL)
    current <- round(tail(dd$del_rate, 1), 2)
    prev <- dd$del_rate[nrow(dd) - 1]
    avg_val <- round(mean(dd$del_rate), 2)
    peak_val <- round(max(dd$del_rate), 2)
    peak_period <- dd$month_label[which.max(dd$del_rate)]
    trend_dir <- if (current > prev) "increased" else "decreased"
    change <- round(abs(current - prev), 2)
    prod <- pi_selected_product()
    insight <- sprintf(
      '<span class="pi-insight-icon">&#x1F4CA;</span> <strong>%s</strong> +2 rate %s by <strong>%s%%</strong> to <strong>%s%%</strong> in the latest period. Average rate stands at <strong>%s%%</strong> with the peak of <strong>%s%%</strong> observed in <strong>%s</strong>.',
      prod, trend_dir, change, current, avg_val, peak_val, peak_period)
    tags$div(class = "pi-insight-box", HTML(insight))
    }, error = function(e) {
      message("[PortfolioInsights] pi_vin_insight: ", e$message)
      .pi_empty_ui("Vintage insight unavailable")
    })
  })

  output$pi_vin_del_trend <- renderPlotly({
    tryCatch({
    dd <- pi_vin_del_filtered()
    dd <- dd[order(dd$month_date), ]
    period_order <- dd$month_label
    plot_ly(dd, x = ~month_label, y = ~del_rate, type = "scatter", mode = "lines+markers",
            fill = "tozeroy", fillcolor = "rgba(255,75,0,0.08)",
            line = list(color = PI_ORANGE, width = 2.5),
            marker = list(color = PI_ORANGE, size = 8),
            hovertemplate = "%{x}<br>+2 Rate: %{y:.1f}%<extra></extra>") %>%
      layout(title = list(text = "<b>+2 Rate Over Time</b>",
                          font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
             xaxis = list(title = "", tickangle = -45, categoryorder = "array", categoryarray = period_order),
             yaxis = list(title = "+2 Rate (%)", gridcolor = "#EEF0F2", rangemode = "tozero", ticksuffix = "%"),
             plot_bgcolor = "#FFF", paper_bgcolor = "#FFF", margin = list(t = 40, b = 80)) %>%
      config(displayModeBar = FALSE)
    }, error = function(e) {
      message("[PortfolioInsights] pi_vin_del_trend: ", e$message)
      .pi_empty_plotly("Delinquency trend unavailable")
    })
  })

  output$pi_vin_vintage_chart <- renderPlotly({
    tryCatch({
    vd <- pi_vintage_data[pi_vintage_data$product == pi_selected_product(), ]
    mob_range <- input$pi_vin_mob_range %||% "All"
    if (mob_range != "All") {
      max_mob <- as.numeric(sub("0-", "", mob_range))
      vd <- vd[vd$mob <= max_mob, ]
    }
    cohorts <- unique(vd$cohort)
    pal <- colorRampPalette(c(PI_MIDNIGHT, PI_MIDNIGHT_70, PI_ORANGE_40, PI_ORANGE))(length(cohorts))
    p <- plot_ly()
    for (i in seq_along(cohorts)) {
      cd <- vd[vd$cohort == cohorts[i], ]
      cd <- cd[order(cd$mob), ]
      p <- p %>% add_trace(x = cd$mob, y = cd$del_rate, name = cohorts[i],
                            type = "scatter", mode = "lines+markers",
                            line = list(color = pal[i], width = 2),
                            marker = list(color = pal[i], size = 5),
                            hovertemplate = paste0("MOB %{x}<br>+2 Rate: %{y:.1f}%<extra></extra>"))
    }
    p %>% layout(
      title = list(text = "<b>Vintage Curves (+2 Rate by MOB)</b>",
                   font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
      xaxis = list(title = "Months on Book (MOB)", dtick = 1, gridcolor = "#EEF0F2"),
      yaxis = list(title = "+2 Rate (%)", gridcolor = "#EEF0F2", rangemode = "tozero", ticksuffix = "%"),
      legend = list(orientation = "h", y = -0.22),
      plot_bgcolor = "#FFF", paper_bgcolor = "#FFF", margin = list(t = 40, b = 80)
    ) %>% config(displayModeBar = FALSE)
    }, error = function(e) {
      message("[PortfolioInsights] pi_vin_vintage_chart: ", e$message)
      .pi_empty_plotly("Vintage curves unavailable")
    })
  })

  # MOB Breakdown
  output$pi_vin_mob_del_chart <- renderPlotly({
    tryCatch({
    tape <- perf_tape()
    if (!is.null(tape)) {
      # risk_grade already joined into tape$rows by 05_global_data_prep.R
      mob_data <- tape$rows %>%
        mutate(
          mob_bins = case_when(
            month_on_books <= 3  ~ "0-3",
            month_on_books <= 6  ~ "4-6",
            month_on_books <= 12 ~ "7-12",
            month_on_books <= 18 ~ "13-18",
            month_on_books <= 24 ~ "19-24",
            TRUE ~ NA_character_
          ),
          month_date = payment_month,
          month_label = format(payment_month, "%b %Y"),
          quarter = paste0("Q", lubridate::quarter(payment_month), " ", lubridate::year(payment_month)),
          year = as.character(lubridate::year(payment_month))
        ) %>%
        filter(!is.na(mob_bins)) %>%
        group_by(mob_bins, month_date, month_label, quarter, year) %>%
        summarise(del_rate = round(mean(ever_del, na.rm = TRUE) * 100, 2), .groups = "drop") %>%
        rename(segment = mob_bins)
      pd <- mob_data
    } else {
      pd <- pi_profiling_data[pi_profiling_data$product == pi_selected_product() &
                              pi_profiling_data$dimension == "MOB", ]
    }
    if (!is.null(input$pi_vin_date_range)) {
      pd <- pd[pd$month_date >= input$pi_vin_date_range[1] & pd$month_date <= input$pi_vin_date_range[2], ]
    }
    freq <- input$pi_vin_frequency %||% "Monthly"
    if (freq == "Quarterly") {
      pd <- pd %>% group_by(quarter, segment) %>%
        summarise(month_label = first(quarter), month_date = min(month_date),
                  del_rate = mean(del_rate), .groups = "drop")
    } else if (freq == "Yearly") {
      pd <- pd %>% group_by(year, segment) %>%
        summarise(month_label = first(year), month_date = min(month_date),
                  del_rate = mean(del_rate), .groups = "drop")
    }
    period_order <- unique(pd$month_label[order(pd$month_date)])
    mob_all <- c("0-3", "4-6", "7-12", "13-18", "19-24")
    mob_selected <- input$pi_vin_mob_select %||% mob_all
    mob_pal_full <- c("0-3" = PI_MIDNIGHT, "4-6" = PI_MIDNIGHT_70,
                      "7-12" = PI_MIDNIGHT_40, "13-18" = PI_ORANGE_40, "19-24" = PI_ORANGE)
    p <- plot_ly()
    for (seg in mob_selected) {
      sd <- pd[pd$segment == seg, ]
      if (nrow(sd) == 0) next
      sd <- sd[order(sd$month_date), ]
      p <- p %>% add_trace(x = sd$month_label, y = sd$del_rate, name = paste0("MOB ", seg),
                            type = "scatter", mode = "lines+markers",
                            line = list(color = mob_pal_full[seg], width = 2.5),
                            marker = list(color = mob_pal_full[seg], size = 7),
                            hovertemplate = paste0("%{x}<br>MOB ", seg, ": %{y:.1f}%<extra></extra>"))
    }
    p %>% layout(
      title = list(text = "<b>+2 Rate by MOB Bucket</b>",
                   font = list(size = 14, color = PI_MIDNIGHT, family = "Source Sans Pro"), x = 0, xanchor = "left"),
      xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Source Sans Pro"),
                   categoryorder = "array", categoryarray = period_order),
      yaxis = list(title = "+2 Rate (%)", gridcolor = "#EEF0F2", rangemode = "tozero", ticksuffix = "%",
                   tickfont = list(size = 11, family = "Source Sans Pro")),
      legend = list(orientation = "h", y = -0.18, font = list(size = 11, family = "Source Sans Pro")),
      plot_bgcolor = "#FFF", paper_bgcolor = "#FFF", margin = list(t = 40, b = 80)
    ) %>% config(displayModeBar = FALSE)
    }, error = function(e) {
      message("[PortfolioInsights] pi_vin_mob_del_chart: ", e$message)
      .pi_empty_plotly("MOB breakdown unavailable")
    })
  })
}
