# ============================================================
# R/services/report_service.R — Unified export report
#
# Addition 5 from the spec. Produces a self-contained HTML report
# summarizing portfolio state, active scenario (if any), SQL,
# and (where available) AI narration. Uses plain R string
# concatenation rather than rmarkdown to avoid the pandoc
# dependency.
#
# Output: a single HTML file with inlined CSS, no external
# assets. Can be opened directly in a browser.
# ============================================================

#' Build the unified export report HTML.
#'
#' @param context A named list. Expected fields (all optional — the
#'   renderer handles missing pieces gracefully):
#'     - product         character, e.g. "Personal Loan"
#'     - active_policy   character, e.g. "PL-CURRENT"
#'     - portfolio_kpis  list, the return value of measure_service::portfolio_kpis
#'     - scan_result     list, the return value of discover_service::auto_scan
#'     - scenario_result list, the return value of simulate_service::run_scenario
#'     - scenario_narration character, the return value of ai_service::narrate_scenario
#'     - active_sql      character, the generated SQL for active_policy
#'     - active_sql_label character, the label used in the SQL header
#'     - comparison      list, the return value of govern_service::compare_policies
#' @return character(1) — the full HTML document
build_export_report <- function(context = list()) {

  # ---- helpers ----
  .esc <- function(x) {
    if (is.null(x)) return("")
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x
  }
  .fmt_pct <- function(x, d = 2) {
    if (is.null(x) || is.na(x)) return("n/a")
    sprintf(paste0("%.", d, "f%%"), as.numeric(x) * 100)
  }
  .fmt_k <- function(x) {
    if (is.null(x) || is.na(x)) return("n/a")
    v <- as.numeric(x)
    if (abs(v) >= 1e6) sprintf("%.1fM", v / 1e6)
    else if (abs(v) >= 1e3) sprintf("%.1fK", v / 1e3)
    else sprintf("%d", as.integer(round(v)))
  }
  .kpi_card <- function(value, label) {
    sprintf(
      '<div class="kpi"><div class="kpi-label">%s</div><div class="kpi-val">%s</div></div>',
      .esc(label), .esc(value)
    )
  }

  product       <- context$product %||% "Personal Loan"
  active_policy <- context$active_policy %||% "PL-CURRENT"
  now_str       <- format(Sys.time(), "%Y-%m-%d %H:%M")

  # ---- 1. Cover ----
  cover <- sprintf('
    <section class="cover">
      <div class="brand">CREDIT POLICY TOOL</div>
      <h1>Policy snapshot report</h1>
      <div class="cover-meta">
        <span><strong>Product:</strong> %s</span>
        <span><strong>Active policy:</strong> %s</span>
        <span><strong>Generated:</strong> %s</span>
      </div>
    </section>',
    .esc(product), .esc(active_policy), now_str
  )

  # ---- 2. Portfolio KPIs ----
  k <- context$portfolio_kpis
  portfolio_section <- if (is.null(k)) {
    '<section><h2>Portfolio snapshot</h2><p class="muted">No portfolio KPIs available.</p></section>'
  } else {
    cards <- paste(
      .kpi_card(.fmt_pct(k$portfolio_irr),     "Portfolio IRR"),
      .kpi_card(.fmt_pct(k$approval_rate, 1),  "Approval rate"),
      .kpi_card(.fmt_pct(k$avg_loss_rate, 2),  "Avg loss rate"),
      .kpi_card(paste0("$", .fmt_k(k$at_risk_usd)), "$ At risk"),
      .kpi_card(.fmt_k(k$n_loans),             "Loans"),
      collapse = ""
    )
    sprintf('
      <section>
        <h2>Portfolio snapshot</h2>
        <div class="kpi-row">%s</div>
      </section>',
      cards
    )
  }

  # ---- 3. Auto Scan (if present) ----
  sr <- context$scan_result
  scan_section <- if (is.null(sr) || length(sr$segments) == 0) {
    '<section><h2>Discovery \u2014 Auto Scan</h2><p class="muted">No scan results for this session.</p></section>'
  } else {
    top <- utils::head(sr$segments, 10)
    seg_rows <- vapply(seq_along(top), function(i) {
      s <- top[[i]]
      lift <- s$lift %||% NA_real_
      lift_txt <- if (is.na(lift)) "n/a" else sprintf("%.2fx", lift)
      sprintf(
        '<tr><td>%d</td><td class="rule">%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
        i,
        .esc(s$display_rule %||% s$rule),
        .fmt_k(s$n %||% 0),
        .fmt_pct(s$bad_rate, 1),
        .fmt_pct(s$mean_irr, 1),
        lift_txt
      )
    }, character(1))
    sprintf('
      <section>
        <h2>Discovery \u2014 Auto Scan</h2>
        <p class="muted">Top %d segments by action score (scanned in %.1fs).</p>
        <table>
          <thead><tr><th>#</th><th>Rule</th><th>n</th><th>bad%%</th><th>IRR</th><th>Lift</th></tr></thead>
          <tbody>%s</tbody>
        </table>
      </section>',
      length(top),
      sr$elapsed_seconds %||% 0,
      paste(seg_rows, collapse = "")
    )
  }

  # ---- 4. Scenario (if present) ----
  scen <- context$scenario_result
  narr <- context$scenario_narration
  scenario_section <- if (is.null(scen)) {
    '<section><h2>Simulate \u2014 Current scenario</h2><p class="muted">No scenario has been run in this session.</p></section>'
  } else {
    bk <- scen$baseline_kpis %||% list()
    sk <- scen$scenario_kpis %||% list()
    metrics <- c("portfolio_irr", "approval_rate", "avg_loss_rate", "at_risk_usd", "n_loans")
    metric_rows <- vapply(metrics, function(m) {
      a <- bk[[m]]; b <- sk[[m]]
      fmt <- function(x) {
        if (is.null(x) || is.na(x)) return("n/a")
        if (m %in% c("portfolio_irr", "approval_rate", "avg_loss_rate")) .fmt_pct(x, 2)
        else if (m == "at_risk_usd") paste0("$", .fmt_k(x))
        else .fmt_k(x)
      }
      delta <- if (!is.null(a) && !is.null(b) && !is.na(a) && !is.na(b)) {
        if (m %in% c("portfolio_irr", "approval_rate", "avg_loss_rate"))
          sprintf("%+.2f pts", (b - a) * 100)
        else sprintf("%+s", .fmt_k(b - a))
      } else ""
      sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td class="delta">%s</td></tr>',
              .esc(m), fmt(a), fmt(b), .esc(delta))
    }, character(1))
    narration_html <- if (!is.null(narr) && nzchar(narr)) {
      sprintf('<div class="narration">%s</div>', .esc(narr))
    } else ""
    n_changed <- length(scen$changed_params %||% list())
    sprintf('
      <section>
        <h2>Simulate \u2014 Current scenario</h2>
        <p class="muted">%d parameter(s) changed \u00b7 ran in %.2fs</p>
        %s
        <table>
          <thead><tr><th>Metric</th><th>Baseline</th><th>Scenario</th><th>Delta</th></tr></thead>
          <tbody>%s</tbody>
        </table>
      </section>',
      n_changed,
      scen$elapsed_seconds %||% 0,
      narration_html,
      paste(metric_rows, collapse = "")
    )
  }

  # ---- 5. Generated SQL ----
  sql_text <- context$active_sql %||% ""
  sql_label <- context$active_sql_label %||% active_policy
  sql_section <- if (!nzchar(sql_text)) {
    '<section><h2>Generated SQL</h2><p class="muted">No SQL provided.</p></section>'
  } else {
    sprintf('
      <section>
        <h2>Generated SQL \u2014 %s</h2>
        <p class="muted">%s characters \u00b7 L10\u2013L90 CTE waterfall</p>
        <details><summary>Show SQL</summary><pre><code>%s</code></pre></details>
      </section>',
      .esc(sql_label),
      nchar(sql_text),
      .esc(sql_text)
    )
  }

  # ---- 6. Footer ----
  footer <- '
    <footer>
      Credit Policy Tool \u00b7 Generated by build_export_report()
    </footer>'

  # ---- Assemble ----
  css <- '
<style>
  body { font-family: Georgia, "Times New Roman", serif; color: #1c1917; background: #faf9f7; margin: 0; padding: 0; }
  .container { max-width: 900px; margin: 0 auto; padding: 40px 36px; }
  .cover { border-bottom: 3px solid #fb6e36; padding-bottom: 18px; margin-bottom: 28px; }
  .brand { font-family: "IBM Plex Mono", monospace; font-size: 11px; letter-spacing: 0.15em; color: #fb6e36; font-weight: 700; text-transform: uppercase; }
  .cover h1 { font-size: 32px; margin: 8px 0 12px 0; font-weight: 600; letter-spacing: -0.3px; }
  .cover-meta { font-size: 12px; color: #57534e; display: flex; gap: 20px; flex-wrap: wrap; font-family: "IBM Plex Mono", monospace; }
  .cover-meta span { white-space: nowrap; }
  section { margin-bottom: 32px; }
  h2 { font-size: 18px; color: #1c1917; font-weight: 600; border-bottom: 1px solid #e7e5e4; padding-bottom: 8px; margin-bottom: 14px; }
  .muted { color: #78716c; font-size: 12px; margin-bottom: 12px; font-style: italic; }
  .kpi-row { display: flex; gap: 10px; flex-wrap: wrap; }
  .kpi { background: #ffffff; border: 1px solid #e7e5e4; border-radius: 7px; padding: 12px 14px; min-width: 140px; flex: 1; }
  .kpi-label { font-size: 9px; color: #78716c; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 600; font-family: "IBM Plex Mono", monospace; }
  .kpi-val { font-size: 22px; font-family: "IBM Plex Mono", monospace; color: #1c1917; margin-top: 6px; font-weight: 600; }
  table { width: 100%; border-collapse: collapse; font-size: 12px; font-family: Georgia, serif; }
  th { text-align: left; padding: 8px 10px; border-bottom: 2px solid #1c1917; font-weight: 600; background: #fafaf9; }
  td { padding: 6px 10px; border-bottom: 1px solid #e7e5e4; }
  td.rule { font-family: "IBM Plex Mono", monospace; font-size: 10.5px; color: #1c1917; }
  td.delta { font-family: "IBM Plex Mono", monospace; font-size: 11px; color: #b91c1c; }
  .narration { background: #fff7ed; border-left: 3px solid #fb6e36; padding: 12px 14px; font-size: 13px; line-height: 1.55; color: #44403c; border-radius: 4px; margin-bottom: 14px; }
  details { background: #ffffff; border: 1px solid #e7e5e4; border-radius: 7px; padding: 10px 12px; }
  summary { cursor: pointer; font-size: 11px; color: #78716c; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 600; font-family: "IBM Plex Mono", monospace; }
  details[open] summary { margin-bottom: 10px; }
  pre { background: #0f0d0b; color: #e7e5e4; padding: 16px; border-radius: 6px; overflow: auto; max-height: 520px; font-size: 10.5px; line-height: 1.5; }
  code { font-family: "IBM Plex Mono", "JetBrains Mono", Consolas, monospace; }
  footer { border-top: 1px solid #e7e5e4; padding-top: 14px; font-size: 10px; color: #a8a29e; font-family: "IBM Plex Mono", monospace; text-align: center; letter-spacing: 0.3px; }
</style>'

  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Credit Policy Report \u2014 %s</title>
  %s
</head>
<body>
  <div class="container">
    %s
    %s
    %s
    %s
    %s
    %s
  </div>
</body>
</html>',
    .esc(product),
    css,
    cover,
    portfolio_section,
    scan_section,
    scenario_section,
    sql_section,
    footer
  )

  html
}
