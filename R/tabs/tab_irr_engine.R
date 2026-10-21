# ============================================================
# Tab: IRR Engine
# Interactive flowchart, single-loan waterfall demo,
# and DCOR/DPOR curve upload with editable product parameters.
# ============================================================

# ---------------------------------------------------------------------------
# Flowchart helper — HTML/CSS/SVG inline
# ---------------------------------------------------------------------------

irr_flowchart_html <- function() {
  # ── CSS for the two-part flowchart ──────────────────────────────────────────
  flowchart_css <- "
  .irr-fc { font-family: 'Segoe UI', system-ui, -apple-system, sans-serif; color: #e2e8f0; }
  .irr-fc .section-label { text-transform: uppercase; font-size: 11px; font-weight: 700; letter-spacing: 1.5px; color: #64748b; margin-bottom: 16px; text-align: center; }

  /* Pipeline */
  .irr-fc .pipeline { display: flex; align-items: center; justify-content: center; gap: 0; margin-bottom: 12px; flex-wrap: wrap; }
  .irr-fc .pipe-card { background: #1e293b; border: 1px solid #334155; border-radius: 12px; padding: 20px 24px; text-align: center; min-width: 180px; max-width: 220px; position: relative; transition: transform 0.2s, border-color 0.2s; }
  .irr-fc .pipe-card:hover { transform: translateY(-2px); border-color: #64748b; }
  .irr-fc .pipe-icon { font-size: 28px; margin-bottom: 8px; display: block; }
  .irr-fc .pipe-title { font-size: 14px; font-weight: 700; color: #f1f5f9; margin-bottom: 4px; }
  .irr-fc .pipe-sub { font-size: 11px; color: #94a3b8; line-height: 1.4; }
  .irr-fc .pipe-arrow { font-size: 24px; color: #475569; margin: 0 8px; flex-shrink: 0; }
  .irr-fc .pipe-card.input  { border-top: 3px solid #3b82f6; }
  .irr-fc .pipe-card.curves { border-top: 3px solid #22c55e; }
  .irr-fc .pipe-card.engine { border-top: 3px solid #f59e0b; }
  .irr-fc .pipe-card.output { border-top: 3px solid #a855f7; }
  .irr-fc .pipe-step { position: absolute; top: -10px; left: 14px; width: 20px; height: 20px; border-radius: 50%; font-size: 10px; font-weight: 700; color: white; display: flex; align-items: center; justify-content: center; line-height: 1; }
  .irr-fc .pipe-card.input  .pipe-step { background: #3b82f6; }
  .irr-fc .pipe-card.curves .pipe-step { background: #22c55e; }
  .irr-fc .pipe-card.engine .pipe-step { background: #f59e0b; }
  .irr-fc .pipe-card.output .pipe-step { background: #a855f7; }

  /* Divider */
  .irr-fc .part-divider { display: flex; align-items: center; gap: 16px; margin: 8px auto 20px; max-width: 960px; }
  .irr-fc .part-divider::before, .irr-fc .part-divider::after { content: ''; flex: 1; height: 1px; background: linear-gradient(90deg, transparent, #334155, transparent); }
  .irr-fc .part-divider span { font-size: 10px; color: #475569; letter-spacing: 2px; text-transform: uppercase; white-space: nowrap; }

  /* Expandable detail panel */
  .irr-fc .expand-panel { max-width: 960px; margin: 0 auto 12px; }
  .irr-fc .expand-toggle { display: flex; align-items: center; justify-content: center; gap: 10px; width: 100%; background: #1e293b; border: 1px solid #334155; border-radius: 10px; padding: 12px 20px; color: #94a3b8; font-size: 13px; font-weight: 600; cursor: pointer; transition: background 0.2s, border-color 0.2s, color 0.2s; text-align: center; letter-spacing: 0.3px; }
  .irr-fc .expand-toggle:hover { background: #263347; border-color: #475569; color: #cbd5e1; }
  .irr-fc .expand-chevron { display: inline-block; transition: transform 0.3s ease; font-size: 11px; }
  .irr-fc .expand-panel[open] .expand-chevron { transform: rotate(180deg); }
  .irr-fc .expand-panel[open] .expand-toggle { border-radius: 10px 10px 0 0; border-bottom: 1px solid #1e293b; background: #151d2e; }
  .irr-fc .expand-body { background: #111827; border: 1px solid #334155; border-top: none; border-radius: 0 0 12px 12px; padding: 28px 28px 24px; }
  .irr-fc .detail-section-title { font-size: 14px; font-weight: 700; color: #e2e8f0; margin: 0 0 16px; display: flex; align-items: center; gap: 8px; }
  .irr-fc .dt-badge { display: inline-flex; align-items: center; justify-content: center; width: 22px; height: 22px; border-radius: 6px; font-size: 11px; font-weight: 700; color: white; flex-shrink: 0; }
  .irr-fc .source-options { display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 24px; }
  .irr-fc .source-card { flex: 1; min-width: 200px; max-width: 300px; background: #1e293b; border: 1.5px solid #334155; border-radius: 10px; padding: 16px; transition: transform 0.15s, border-color 0.15s; }
  .irr-fc .source-card:hover { transform: translateY(-2px); border-color: #475569; }
  .irr-fc .sc-header { display: flex; align-items: center; gap: 8px; margin-bottom: 8px; }
  .irr-fc .sc-icon { font-size: 20px; flex-shrink: 0; }
  .irr-fc .sc-title { font-size: 13px; font-weight: 700; color: #f1f5f9; }
  .irr-fc .sc-desc { font-size: 11px; color: #94a3b8; line-height: 1.5; }
  .irr-fc .sc-tag { display: inline-block; font-size: 9px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.8px; padding: 2px 8px; border-radius: 4px; margin-top: 8px; }
  .irr-fc .sc-tag.default { background: #14532d; color: #6ee7b7; }
  .irr-fc .sc-tag.advanced { background: #1e3a5f; color: #93c5fd; }
  .irr-fc .sc-tag.manual { background: #3b0764; color: #d8b4fe; }
  .irr-fc .curve-pipeline { display: flex; align-items: stretch; gap: 0; margin: 16px 0; flex-wrap: wrap; justify-content: center; }
  .irr-fc .cp-step { background: #1e293b; border: 1px solid #334155; border-radius: 8px; padding: 14px 16px; min-width: 180px; max-width: 220px; flex: 1; position: relative; }
  .irr-fc .cp-num { position: absolute; top: -8px; left: 12px; background: #22c55e; color: white; font-size: 9px; font-weight: 700; width: 16px; height: 16px; border-radius: 50%; display: flex; align-items: center; justify-content: center; }
  .irr-fc .cp-title { font-size: 12px; font-weight: 700; color: #6ee7b7; margin-bottom: 4px; }
  .irr-fc .cp-desc { font-size: 10px; color: #94a3b8; line-height: 1.4; }
  .irr-fc .cp-arrow { display: flex; align-items: center; color: #475569; font-size: 18px; padding: 0 6px; flex-shrink: 0; }
  .irr-fc .scenario-table { width: 100%; border-collapse: collapse; font-size: 11px; margin-top: 12px; }
  .irr-fc .scenario-table th { text-align: left; font-weight: 700; color: #94a3b8; padding: 6px 12px; border-bottom: 1px solid #334155; text-transform: uppercase; font-size: 10px; letter-spacing: 0.5px; }
  .irr-fc .scenario-table td { padding: 8px 12px; color: #cbd5e1; border-bottom: 1px solid #1e293b; }
  .irr-fc .scenario-table tr:hover td { background: #1e293b; }
  .irr-fc .scenario-badge { display: inline-block; padding: 2px 8px; border-radius: 4px; font-size: 10px; font-weight: 600; }
  .irr-fc .scenario-badge.base { background: #14532d; color: #6ee7b7; }
  .irr-fc .scenario-badge.stress { background: #7f1d1d; color: #fca5a5; }
  .irr-fc .scenario-badge.optimistic { background: #1e3a5f; color: #93c5fd; }
  .irr-fc .detail-sep { border: none; border-top: 1px solid #1e293b; margin: 20px 0; }

  /* Waterfall container */
  .irr-fc .wf-container { background: #111827; border: 1px solid #1e293b; border-radius: 16px; padding: 32px 28px; max-width: 960px; margin: 0 auto; }
  .irr-fc .wf-title { text-align: center; font-size: 18px; font-weight: 700; color: #f8fafc; margin-bottom: 4px; }
  .irr-fc .wf-subtitle { text-align: center; font-size: 12px; color: #64748b; margin-bottom: 28px; }

  /* Balance boxes */
  .irr-fc .balance-row { display: flex; justify-content: center; margin-bottom: 8px; }
  .irr-fc .balance-box { background: #1e293b; border: 2px solid #475569; border-radius: 10px; padding: 14px 28px; text-align: center; min-width: 320px; }
  .irr-fc .balance-box .box-label { font-size: 15px; font-weight: 700; color: #f1f5f9; }
  .irr-fc .balance-box .box-detail { font-size: 11px; color: #94a3b8; margin-top: 3px; }
  .irr-fc .box-acronym { font-size: 10px; color: #64748b; margin-top: 2px; font-style: italic; }

  /* Arrows */
  .irr-fc .center-arrow { text-align: center; font-size: 22px; color: #475569; margin: 8px 0; line-height: 1; }

  /* Lane labels */
  .irr-fc .split-label-row { display: flex; justify-content: center; gap: 80px; margin-bottom: 6px; }
  .irr-fc .lane-label { font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: 1.2px; display: flex; align-items: center; gap: 6px; }
  .irr-fc .lane-label.outflow { color: #f87171; }
  .irr-fc .lane-label.inflow  { color: #34d399; }
  .irr-fc .lane-dot { width: 8px; height: 8px; border-radius: 50%; display: inline-block; }
  .irr-fc .lane-dot.red   { background: #ef4444; }
  .irr-fc .lane-dot.green { background: #10b981; }

  /* Two-column lanes */
  .irr-fc .lanes { display: flex; justify-content: center; gap: 32px; margin-bottom: 8px; }
  .irr-fc .lane { display: flex; flex-direction: column; gap: 10px; min-width: 300px; max-width: 380px; flex: 1; }

  /* Cashflow cards */
  .irr-fc .cf-card { border-radius: 10px; padding: 14px 16px; position: relative; transition: transform 0.15s; }
  .irr-fc .cf-card:hover { transform: translateX(3px); }
  .irr-fc .cf-card.outflow { background: #1c1017; border: 1.5px solid #7f1d1d; border-left: 4px solid #ef4444; }
  .irr-fc .cf-card.inflow  { background: #0d1f17; border: 1.5px solid #14532d; border-left: 4px solid #10b981; }
  .irr-fc .cf-label { font-size: 14px; font-weight: 700; margin-bottom: 2px; }
  .irr-fc .cf-card.outflow .cf-label { color: #fca5a5; }
  .irr-fc .cf-card.inflow  .cf-label { color: #6ee7b7; }
  .irr-fc .cf-formula { font-size: 12px; color: #cbd5e1; margin-bottom: 4px; line-height: 1.4; }
  .irr-fc .cf-source { display: inline-block; font-size: 10px; color: #94a3b8; background: #1e293b; padding: 2px 8px; border-radius: 4px; margin-top: 2px; }
  .irr-fc .cf-acronym { position: absolute; top: 8px; right: 10px; font-size: 10px; color: #475569; font-style: italic; }
  .irr-fc .cross-link { display: inline-flex; align-items: center; gap: 4px; font-size: 9px; color: #6366f1; background: #1e1b4b; padding: 1px 7px; border-radius: 3px; margin-left: 4px; vertical-align: middle; }

  /* Bridge */
  .irr-fc .bridge-row { display: flex; justify-content: center; margin: 8px 0; }
  .irr-fc .bridge-box { background: #172033; border: 1.5px dashed #475569; border-radius: 8px; padding: 10px 24px; text-align: center; }
  .irr-fc .bridge-box .box-label { font-size: 13px; font-weight: 600; color: #94a3b8; }
  .irr-fc .bridge-box .box-detail { font-size: 11px; color: #64748b; margin-top: 2px; }

  /* Result cards */
  .irr-fc .result-row { display: flex; justify-content: center; gap: 20px; flex-wrap: wrap; }
  .irr-fc .result-card { border-radius: 12px; padding: 16px 24px; text-align: center; min-width: 260px; flex: 1; max-width: 380px; }
  .irr-fc .result-card.ncf { background: linear-gradient(135deg, #1a1507 0%, #1f1a08 100%); border: 2px solid #d97706; box-shadow: 0 0 20px rgba(217,119,6,0.15), 0 0 40px rgba(217,119,6,0.05); }
  .irr-fc .result-card.ebb { background: #1e293b; border: 2px solid #475569; }
  .irr-fc .result-card .box-label { font-size: 15px; font-weight: 700; }
  .irr-fc .result-card.ncf .box-label { color: #fbbf24; }
  .irr-fc .result-card.ebb .box-label { color: #e2e8f0; }
  .irr-fc .result-card .box-detail { font-size: 12px; color: #94a3b8; margin-top: 4px; line-height: 1.5; }

  /* Feed arrow & repeat */
  .irr-fc .feed-arrow { text-align: center; margin: 14px 0 4px; }
  .irr-fc .feed-arrow span { display: inline-block; background: #1e293b; border: 1px solid #334155; border-radius: 20px; padding: 4px 16px; font-size: 11px; color: #64748b; }
  .irr-fc .repeat-indicator { text-align: center; margin-top: 10px; }
  .irr-fc .repeat-badge { display: inline-block; background: #0f172a; border: 1px dashed #475569; border-radius: 20px; padding: 6px 20px; font-size: 11px; color: #64748b; letter-spacing: 0.5px; }

  /* Legend */
  .irr-fc .legend { display: flex; justify-content: center; gap: 24px; margin-top: 24px; flex-wrap: wrap; }
  .irr-fc .legend-item { display: flex; align-items: center; gap: 6px; font-size: 11px; color: #94a3b8; }
  .irr-fc .legend-swatch { width: 14px; height: 14px; border-radius: 4px; flex-shrink: 0; }

  /* XIRR outputs */
  .irr-fc .xirr-row { display: flex; justify-content: center; gap: 16px; margin-top: 24px; flex-wrap: wrap; }
  .irr-fc .xirr-card { background: #1a0f2e; border: 1.5px solid #7c3aed; border-radius: 10px; padding: 14px 20px; text-align: center; min-width: 180px; flex: 1; max-width: 240px; }
  .irr-fc .xirr-card .box-label { font-size: 13px; font-weight: 700; color: #c4b5fd; margin-bottom: 3px; }
  .irr-fc .xirr-card .box-detail { font-size: 11px; color: #a78bfa; line-height: 1.4; }

  /* Responsive */
  @media (max-width: 740px) {
    .irr-fc .lanes { flex-direction: column; align-items: center; }
    .irr-fc .lane  { max-width: 100%; min-width: unset; }
    .irr-fc .split-label-row { gap: 24px; }
    .irr-fc .pipeline { gap: 8px; }
    .irr-fc .pipe-arrow { display: none; }
    .irr-fc .result-row { flex-direction: column; align-items: center; }
  }
  "

  # ── Helper: cashflow card ───────────────────────────────────────────────────
  cf_card <- function(type, acronym, label, formula, source, extra_style = NULL, extra_label_style = NULL, extra_tag = NULL) {
    tags$div(class = paste("cf-card", type), style = extra_style,
      tags$span(class = "cf-acronym", acronym),
      tags$div(class = "cf-label", style = extra_label_style, label),
      tags$div(class = "cf-formula", HTML(formula)),
      tags$span(class = "cf-source", HTML(source)),
      extra_tag
    )
  }

  # ── Build the flowchart ─────────────────────────────────────────────────────
  tags$div(class = "irr-fc",
    tags$style(HTML(flowchart_css)),

    # ═══ PART 1: PIPELINE ═══
    tags$div(class = "section-label", "Part 1 \u2014 How the IRR Engine Works"),
    tags$div(class = "pipeline",
      tags$div(class = "pipe-card input",
        tags$span(class = "pipe-step", "1"),
        tags$span(class = "pipe-icon", HTML("&#128203;")),
        tags$div(class = "pipe-title", "Loan Data"),
        tags$div(class = "pipe-sub", HTML("Loan amounts, APRs, terms,<br>observed payments &amp; defaults"))
      ),
      tags$span(class = "pipe-arrow", HTML("&#10132;")),
      tags$div(class = "pipe-card curves",
        tags$span(class = "pipe-step", "2"),
        tags$span(class = "pipe-icon", HTML("&#128200;")),
        tags$div(class = "pipe-title", "Default & Prepay Curves"),
        tags$div(class = "pipe-sub", HTML("Monthly default &amp; prepayment<br>rates by segment &amp; vintage"))
      ),
      tags$span(class = "pipe-arrow", HTML("&#10132;")),
      tags$div(class = "pipe-card engine",
        tags$span(class = "pipe-step", "3"),
        tags$span(class = "pipe-icon", HTML("&#9881;")),
        tags$div(class = "pipe-title", "Monthly Cashflow Projection"),
        tags$div(class = "pipe-sub", HTML("Roll balance forward month<br>by month to loan maturity"))
      ),
      tags$span(class = "pipe-arrow", HTML("&#10132;")),
      tags$div(class = "pipe-card output",
        tags$span(class = "pipe-step", "4"),
        tags$span(class = "pipe-icon", HTML("&#128202;")),
        tags$div(class = "pipe-title", "Portfolio IRR"),
        tags$div(class = "pipe-sub", HTML("Annualised return on the<br>loan portfolio (XIRR)"))
      )
    ),

    # ═══ EXPANDABLE PART 1 DETAIL ═══
    tags$details(class = "expand-panel",
      tags$summary(class = "expand-toggle",
        tags$span(class = "expand-chevron", HTML("&#9660;")),
        "How are Default & Prepayment Curves Generated?",
        tags$span(style = "font-size:10px; color:#64748b; font-weight:400;", HTML("&mdash; click to expand"))
      ),
      tags$div(class = "expand-body",

        # Section A: Curve Source Options
        tags$div(class = "detail-section-title",
          tags$span(class = "dt-badge", style = "background:#3b82f6;", "A"),
          "Where Do the Curves Come From?"
        ),
        tags$p(style = "font-size:12px; color:#94a3b8; margin:0 0 14px; line-height:1.6;",
          "The engine needs a monthly ",
          tags$strong(style = "color:#cbd5e1;", "Default Rate (DCOR)"),
          " and ",
          tags$strong(style = "color:#cbd5e1;", "Prepayment Rate (DPOR)"),
          " for each loan segment. There are three ways to provide them:"
        ),
        tags$div(class = "source-options",
          tags$div(class = "source-card",
            tags$div(class = "sc-header",
              tags$span(class = "sc-icon", HTML("&#128202;")),
              tags$span(class = "sc-title", "Auto-Derived from Tape")),
            tags$div(class = "sc-desc",
              "The engine extracts actual default and prepayment rates from your performance tape, then fits parametric curves to project beyond observed months."),
            tags$span(class = "sc-tag default", HTML("Default &mdash; No Action Needed"))
          ),
          tags$div(class = "source-card",
            tags$div(class = "sc-header",
              tags$span(class = "sc-icon", HTML("&#128203;")),
              tags$span(class = "sc-title", "Upload Custom Curves")),
            tags$div(class = "sc-desc",
              HTML("Upload a CSV with your own DCOR/DPOR values by Product &times; Term &times; Vintage &times; MOB. Use this when you have proprietary risk models or actuarial projections.")),
            tags$span(class = "sc-tag manual", "Manual Upload")
          ),
          tags$div(class = "source-card",
            tags$div(class = "sc-header",
              tags$span(class = "sc-icon", HTML("&#9881;")),
              tags$span(class = "sc-title", "Scenario Adjustments")),
            tags$div(class = "sc-desc",
              "Apply stress or optimistic multipliers to the base curves to model different economic environments (recession, benign credit cycle, etc.)."),
            tags$span(class = "sc-tag advanced", "Advanced")
          )
        ),

        tags$hr(class = "detail-sep"),

        # Section B: Auto-Derivation Pipeline
        tags$div(class = "detail-section-title",
          tags$span(class = "dt-badge", style = "background:#22c55e;", "B"),
          "How Auto-Derivation Works (Step by Step)"
        ),
        tags$p(style = "font-size:12px; color:#94a3b8; margin:0 0 14px; line-height:1.6;",
          "When using the performance tape, the engine builds curves in three steps:"),
        tags$div(class = "curve-pipeline",
          tags$div(class = "cp-step",
            tags$span(class = "cp-num", "1"),
            tags$div(class = "cp-title", "Empirical Extraction"),
            tags$div(class = "cp-desc",
              HTML("Average observed DCOR &amp; DPOR per<br>"),
              tags$strong(style = "color:#cbd5e1;", HTML("Product &times; Term &times; Vintage &times; MOB")),
              HTML("<br>from loan-level performance tape.<br><br>"),
              tags$span(style = "color:#64748b;", HTML("Covers MOBs 0&ndash;24 (observed window)"))
            )
          ),
          tags$span(class = "cp-arrow", HTML("&#10132;")),
          tags$div(class = "cp-step",
            tags$span(class = "cp-num", "2"),
            tags$div(class = "cp-title", "Parametric Tail Fit"),
            tags$div(class = "cp-desc",
              tags$strong(style = "color:#cbd5e1;", "DCOR:"), " Exponential decay",
              HTML("<br>"),
              tags$span(style = "color:#64748b;", HTML("a &times; e<sup>-b&times;t</sup> + c")),
              HTML("<br>(captures the &ldquo;hump then decline&rdquo; default pattern)<br><br>"),
              tags$strong(style = "color:#cbd5e1;", "DPOR:"), " Constant average",
              HTML("<br>"),
              tags$span(style = "color:#64748b;", "(observed ~1%/month, very flat)")
            )
          ),
          tags$span(class = "cp-arrow", HTML("&#10132;")),
          tags$div(class = "cp-step",
            tags$span(class = "cp-num", "3"),
            tags$div(class = "cp-title", "Young Vintage Fallback"),
            tags$div(class = "cp-desc",
              "Vintages with <6 observed months",
              HTML("<br>"),
              tags$strong(style = "color:#cbd5e1;", "inherit the curve shape"),
              HTML("<br>from the nearest older vintage<br>"),
              HTML("in the same Product &times; Term segment.<br><br>"),
              tags$span(style = "color:#64748b;", "Prevents noisy short-history curves")
            )
          )
        ),
        tags$p(style = "font-size:11px; color:#64748b; text-align:center; margin:4px 0 0;",
          "Result: complete DCOR/DPOR curve from MOB 0 to loan maturity (e.g. 72 months) for every segment"),

        tags$hr(class = "detail-sep"),

        # Section C: Scenario Multipliers
        tags$div(class = "detail-section-title",
          tags$span(class = "dt-badge", style = "background:#f59e0b;", "C"),
          "Scenario Multipliers"
        ),
        tags$p(style = "font-size:12px; color:#94a3b8; margin:0 0 10px; line-height:1.6;",
          "Scenarios adjust the base curves to model different credit environments:"),
        tags$table(class = "scenario-table",
          tags$thead(tags$tr(
            tags$th("Scenario"), tags$th("Default Rate (DCOR)"), tags$th("Prepay Rate (DPOR)"),
            tags$th("Recovery Adjustment"), tags$th("Use Case")
          )),
          tags$tbody(
            tags$tr(
              tags$td(tags$span(class = "scenario-badge base", "Base")),
              tags$td(HTML("1.0&times; (unchanged)")), tags$td(HTML("1.0&times; (unchanged)")),
              tags$td("No change"), tags$td("Current economic environment")
            ),
            tags$tr(
              tags$td(tags$span(class = "scenario-badge stress", "Stress")),
              tags$td(HTML("1.5&times; (50% higher defaults)")), tags$td(HTML("0.7&times; (30% fewer prepays)")),
              tags$td(HTML("&minus;5pp recovery")), tags$td("Recession / downturn modelling")
            ),
            tags$tr(
              tags$td(tags$span(class = "scenario-badge optimistic", "Optimistic")),
              tags$td(HTML("0.7&times; (30% lower defaults)")), tags$td(HTML("1.2&times; (20% more prepays)")),
              tags$td("+3pp recovery"), tags$td("Benign credit cycle / expansion")
            )
          )
        )
      )
    ),

    # ═══ DIVIDER ═══
    tags$div(class = "part-divider", tags$span("Detailed View")),

    # ═══ PART 2: WATERFALL ═══
    tags$div(class = "section-label", "Part 2 \u2014 Monthly Cashflow Waterfall (One Loan, One Month)"),
    tags$div(class = "wf-container",
      tags$div(class = "wf-title", "What Happens to the Balance Each Month?"),
      tags$div(class = "wf-subtitle", "Each loan\u2019s balance is projected forward from origination to maturity, month by month"),

      # Opening Balance
      tags$div(class = "balance-row",
        tags$div(class = "balance-box",
          tags$div(class = "box-label", "Opening Book Balance"),
          tags$div(class = "box-detail", "The outstanding loan balance at the start of this month"),
          tags$div(class = "box-acronym", "BBB \u2014 Beginning Book Balance")
        )
      ),
      tags$div(class = "center-arrow", HTML("&#9660;")),

      # Lane labels
      tags$div(class = "split-label-row",
        tags$div(class = "lane-label outflow",
          tags$span(class = "lane-dot red"), "Outflows \u2014 Money Leaving the Book"),
        tags$div(class = "lane-label inflow",
          tags$span(class = "lane-dot green"), "Inflows \u2014 Money Back to Lender")
      ),

      # Two-column lanes
      tags$div(class = "lanes",
        # LEFT: OUTFLOWS
        tags$div(class = "lane",
          cf_card("outflow", "ED", "Defaults",
                  "Balance &times; Monthly Default Rate",
                  "&#8592; from DCOR curve (varies by month-on-books)",
                  extra_tag = tags$span(class = "cross-link",
                    HTML("&#8594; feeds Write-offs, Recoveries, Collection Fees"))),
          cf_card("outflow", "EWF", "Net Write-offs",
                  "Defaults &minus; Recoveries",
                  "&#8592; the portion of defaults that is permanently lost"),
          cf_card("outflow", "EUPP", "Prepayments",
                  "(Balance &minus; Defaults) &times; Monthly Prepay Rate",
                  "&#8592; from DPOR curve &mdash; borrowers paying off early"),
          cf_card("outflow", "ESF + ECF + ERF", "Fees & Costs",
                  "Servicing: Surviving Bal &times; 0.06%/mo<br>Collection: Defaults &times; 10%<br>Recovery: Recoveries &times; 15%",
                  "&#8592; product parameters (editable per product)",
                  extra_style = "border-left-color: #f97316;")
        ),
        # RIGHT: INFLOWS
        tags$div(class = "lane",
          cf_card("inflow", "ADJ_EIP", "Interest Income",
                  "Surviving Balance &times; Monthly Interest Rate",
                  "&#8592; from loan APR (e.g. 12% &divide; 12 = 1%/mo)"),
          cf_card("inflow", "ESPP", "Scheduled Principal",
                  "Standard amortisation payment on surviving balance",
                  "&#8592; from loan term &amp; APR (level-pay amortisation)"),
          cf_card("inflow", "ER", "Recoveries",
                  "Defaults &times; Recovery Rate",
                  "&#8592; product parameter (PL: 20%, AL: 40%)"),
          cf_card("inflow", "EUPP", "Prepayments (returned principal)",
                  "Prepaid principal flows back but earns no future interest",
                  "&#8592; reduces balance, counted as cash returned",
                  extra_style = "border-left-color: #3b82f6; background: #0d1520; border-color: #1e3a5f;",
                  extra_label_style = "color: #93c5fd;")
        )
      ),

      # Surviving Balance bridge
      tags$div(class = "bridge-row",
        tags$div(class = "bridge-box",
          tags$div(class = "box-label", HTML("Surviving Balance = Opening &minus; Defaults &minus; Prepayments")),
          tags$div(class = "box-detail", "The performing balance that continues to generate interest & principal")
        )
      ),
      tags$div(class = "center-arrow", HTML("&#9660;")),

      # Net Cashflow & Ending Balance
      tags$div(class = "result-row",
        tags$div(class = "result-card ncf",
          tags$div(class = "box-label", "Net Cashflow to Lender"),
          tags$div(class = "box-detail", HTML(
            "Interest + Principal + Prepayments + Recoveries<br>",
            "&minus; Servicing Fees &minus; Collection Fees &minus; Recovery Fees"
          )),
          tags$div(class = "box-acronym", "NCF \u2014 this is the cashflow used in the IRR calculation")
        ),
        tags$div(class = "result-card ebb",
          tags$div(class = "box-label", "Closing Book Balance"),
          tags$div(class = "box-detail", HTML(
            "Surviving Balance &minus; Scheduled Principal<br>",
            "This becomes next month\u2019s Opening Balance"
          )),
          tags$div(class = "box-acronym", "EBB \u2014 Ending Book Balance")
        )
      ),

      # Feed-next & repeat
      tags$div(class = "feed-arrow",
        tags$span(HTML("&#8635; Closing Balance feeds into next month\u2019s Opening Balance"))
      ),
      tags$div(class = "repeat-indicator",
        tags$span(class = "repeat-badge", "Repeated for every month from origination to loan maturity (e.g. 72 months)")
      ),

      # Legend
      tags$div(class = "legend",
        tags$div(class = "legend-item",
          tags$span(class = "legend-swatch", style = "background:#ef4444;"), "Outflows (losses, prepayments, fees)"),
        tags$div(class = "legend-item",
          tags$span(class = "legend-swatch", style = "background:#10b981;"), "Inflows (interest, principal, recoveries)"),
        tags$div(class = "legend-item",
          tags$span(class = "legend-swatch", style = "background:#d97706;"), "Net Cashflow (used for IRR)"),
        tags$div(class = "legend-item",
          tags$span(class = "legend-swatch", style = "background:#475569;"), "Balance (carried forward)")
      ),

      # XIRR Outputs
      tags$div(style = "margin-top:28px; border-top: 1px solid #1e293b; padding-top: 20px;",
        tags$div(style = "text-align:center; font-size:12px; color:#64748b; margin-bottom:12px; font-weight:600; letter-spacing:1px; text-transform:uppercase;",
          "After all months are projected, the engine computes"),
        tags$div(class = "xirr-row",
          tags$div(class = "xirr-card",
            tags$div(class = "box-label", "Loan-Level IRR"),
            tags$div(class = "box-detail", "Each loan\u2019s own rate of return based on its cashflow stream")
          ),
          tags$div(class = "xirr-card",
            tags$div(class = "box-label", "Portfolio IRR"),
            tags$div(class = "box-detail", "Aggregate return across all loans, weighted by size & timing")
          ),
          tags$div(class = "xirr-card",
            tags$div(class = "box-label", "Segment IRR"),
            tags$div(class = "box-detail", "Breakdowns by FICO, vintage, channel, risk grade, etc.")
          )
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Demo inputs panel
# ---------------------------------------------------------------------------

irr_demo_inputs_ui <- function() {
  div(class = "card",
    tags$div(
      style = "background:#fef3c7; border:1px solid #f59e0b; border-radius:8px; padding:10px 14px; margin-bottom:15px;",
      tags$span(style = "color:#92400e; font-size:12px;",
        icon("info-circle"),
        " This is a simplified example using flat DCOR/DPOR rates. In production, the engine uses empirical curves that vary by MOB."
      )
    ),
    h4("Example Loan Parameters", style = "font-weight:bold; margin-top:0;"),

    numericInput("irr_demo_amount", "Loan Amount ($)",
                 value = 25000, min = 1000, max = 100000, step = 1000),

    sliderInput("irr_demo_apr", "Annual Percentage Rate (APR)",
                min = 0.05, max = 0.30, value = 0.12, step = 0.005),

    selectInput("irr_demo_term", "Term (months)",
                choices = c(24, 36, 48, 60, 72), selected = 36),

    hr(style = "border-color:#e2e8f0;"),
    h5("DCOR / DPOR Assumptions", style = "font-weight:bold; color:#6b7280;"),

    sliderInput("irr_demo_dcor", "Monthly Default Rate (DCOR)",
                min = 0, max = 0.05, value = 0.001, step = 0.0001),

    sliderInput("irr_demo_dpor", "Monthly Prepayment Rate (DPOR)",
                min = 0, max = 0.10, value = 0.01, step = 0.001),

    hr(style = "border-color:#e2e8f0;"),
    h5("Recovery & Fees", style = "font-weight:bold; color:#6b7280;"),

    sliderInput("irr_demo_recovery", "Recovery Rate",
                min = 0, max = 1.0, value = 0.20, step = 0.01),

    sliderInput("irr_demo_coll_fee", "Collection Fee Rate",
                min = 0, max = 0.30, value = 0.10, step = 0.01),

    sliderInput("irr_demo_rec_fee", "Recovery Fee Rate",
                min = 0, max = 0.30, value = 0.15, step = 0.01),

    numericInput("irr_demo_svc_fee", "Servicing Fee Rate (monthly)",
                 value = 0.0006, min = 0, max = 0.01, step = 0.0001),

    br(),
    actionButton("irr_demo_compute", "Compute IRR",
                 class = "custom-button", width = "100%",
                 icon = icon("calculator")),
    br(), br(),
    uiOutput("irr_demo_result_kpi")
  )
}

# ---------------------------------------------------------------------------
# Upload panel
# ---------------------------------------------------------------------------

irr_upload_ui <- function() {
  div(class = "card",
    h4("DCOR/DPOR Curve Upload", style = "font-weight:bold; margin-top:0;"),
    tags$div(
      style = "background:#eff6ff; border:1px solid #3b82f6; border-radius:8px; padding:10px 14px; margin-bottom:15px;",
      tags$span(style = "color:#1e40af; font-size:12px;",
        icon("info-circle"),
        " Optional: Upload custom DCOR/DPOR curves by Product x Term x Vintage x Scenario x MOB. ",
        "If not uploaded, the engine uses parametric curves fitted from the performance tape."
      )
    ),

    fluidRow(
      column(6,
        downloadButton("irr_download_template", "Download CSV Template",
                       class = "custom-button", style = "width:100%; margin-bottom:10px;")
      ),
      column(6,
        actionButton("irr_clear_curves", "Clear Uploaded Curves",
                     class = "btn btn-default", width = "100%",
                     icon = icon("times"))
      )
    ),

    fileInput("irr_curve_file", "Choose DCOR/DPOR CSV",
              accept = ".csv", width = "100%"),

    uiOutput("irr_curve_status"),
    br(),
    uiOutput("irr_curve_segment_info")
  )
}

# ---------------------------------------------------------------------------
# Product parameters panel
# ---------------------------------------------------------------------------

irr_product_params_ui <- function() {
  div(class = "card",
    h4("Product Parameters", style = "font-weight:bold; margin-top:0;"),

    # Personal Loan
    tags$div(
      style = "background:#f8fafc; border:1px solid #e2e8f0; border-radius:8px; padding:12px; margin-bottom:12px;",
      h5("Personal Loan", style = "font-weight:bold; margin:0 0 8px; color:#1f2937;"),
      fluidRow(
        column(6, sliderInput("irr_pl_recovery", "Recovery Rate",
                              min = 0, max = 1, value = 0.20, step = 0.01)),
        column(6, numericInput("irr_pl_svc_fee", "Servicing Fee (monthly)",
                               value = 0.0006, min = 0, max = 0.01, step = 0.0001))
      ),
      fluidRow(
        column(6, sliderInput("irr_pl_coll_fee", "Collection Fee Rate",
                              min = 0, max = 0.30, value = 0.10, step = 0.01)),
        column(6, sliderInput("irr_pl_rec_fee", "Recovery Fee Rate",
                              min = 0, max = 0.30, value = 0.15, step = 0.01))
      )
    ),

    # Auto Loan
    tags$div(
      style = "background:#f8fafc; border:1px solid #e2e8f0; border-radius:8px; padding:12px; margin-bottom:12px;",
      h5("Auto Loan", style = "font-weight:bold; margin:0 0 8px; color:#1f2937;"),
      fluidRow(
        column(6, sliderInput("irr_al_recovery", "Recovery Rate",
                              min = 0, max = 1, value = 0.40, step = 0.01)),
        column(6, numericInput("irr_al_svc_fee", "Servicing Fee (monthly)",
                               value = 0.0005, min = 0, max = 0.01, step = 0.0001))
      ),
      fluidRow(
        column(6, sliderInput("irr_al_coll_fee", "Collection Fee Rate",
                              min = 0, max = 0.30, value = 0.08, step = 0.01)),
        column(6, sliderInput("irr_al_rec_fee", "Recovery Fee Rate",
                              min = 0, max = 0.30, value = 0.10, step = 0.01))
      )
    ),

    # Scenario selector
    selectInput("irr_scenario", "Scenario",
                choices = c("Base" = "base", "Stress" = "stress", "Optimistic" = "optimistic"),
                selected = "base"),
    uiOutput("irr_scenario_description"),

    br(),
    actionButton("irr_apply_portfolio", "Apply to Portfolio",
                 class = "custom-button", width = "100%",
                 icon = icon("play")),
    br(), br(),
    uiOutput("irr_apply_status")
  )
}

# ---------------------------------------------------------------------------
# Main UI
# ---------------------------------------------------------------------------

irr_engine_ui <- function() {
  tabItem(tabName = "irr_engine",

    tags$style(HTML("
      #irr_demo_apr .irs-single, #irr_demo_dcor .irs-single,
      #irr_demo_dpor .irs-single, #irr_demo_recovery .irs-single,
      #irr_demo_coll_fee .irs-single, #irr_demo_rec_fee .irs-single,
      #irr_pl_recovery .irs-single, #irr_pl_coll_fee .irs-single,
      #irr_pl_rec_fee .irs-single, #irr_al_recovery .irs-single,
      #irr_al_coll_fee .irs-single, #irr_al_rec_fee .irs-single {
        font-size: 11px;
      }
    ")),

    header_panel(
      "IRR Engine",
      "Understand how portfolio IRR is computed — explore the cashflow waterfall, run a live demo, and configure DCOR/DPOR curves.",
      icon_name = "calculator",
      data_scope = "issued_perf"
    ),

    scope_toggle_ui("irr_data_scope"),

    # ---- Section 1: How It Works ----
    irr_flowchart_html(),

    # ---- Section 2: Live Demo ----
    fluidRow(
      column(4, irr_demo_inputs_ui()),
      column(8,
        div(class = "card",
          h4("Waterfall Projection", style = "font-weight:bold; margin-top:0;"),
          DT::dataTableOutput("irr_demo_table"),
          br(),
          plotly::plotlyOutput("irr_demo_chart", height = "350px"),
          br(),
          fluidRow(
            column(3, uiOutput("irr_demo_kpi_irr")),
            column(3, uiOutput("irr_demo_kpi_ncf")),
            column(3, uiOutput("irr_demo_kpi_defaults")),
            column(3, uiOutput("irr_demo_kpi_fees"))
          )
        )
      )
    ),

    # ---- Section 3: Curve Upload & Parameters ----
    fluidRow(
      column(5, irr_upload_ui()),
      column(7, irr_product_params_ui())
    ),

    # Curve Preview Chart
    fluidRow(
      column(12,
        div(class = "card",
          h4("Curve Preview", style = "font-weight:bold; margin-top:0;"),
          fluidRow(
            column(4, selectInput("irr_curve_segment", "Select Segment",
                                  choices = NULL))
          ),
          plotly::plotlyOutput("irr_curve_preview_chart", height = "300px")
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

irr_engine_server <- function(input, output, session, perf_tape) {

  # ==================================================================
  # Section 2: Live Demo
  # ==================================================================

  demo_waterfall <- eventReactive(input$irr_demo_compute, {
    amt   <- input$irr_demo_amount
    apr   <- input$irr_demo_apr
    term  <- as.integer(input$irr_demo_term)
    dcor  <- input$irr_demo_dcor
    dpor  <- input$irr_demo_dpor
    rec_r <- input$irr_demo_recovery
    coll  <- input$irr_demo_coll_fee
    recf  <- input$irr_demo_rec_fee
    svc   <- input$irr_demo_svc_fee

    # Build flat DCOR/DPOR curve
    curves_df <- data.frame(
      mob       = 0:term,
      dcor_rate = c(0, rep(dcor, term)),
      dpor_rate = c(0, rep(dpor, term)),
      curve_source = "demo",
      stringsAsFactors = FALSE
    )

    loan <- list(
      loan_id           = "DEMO_001",
      product_type      = "Personal Loan",
      issued_amount     = amt,
      interest_rate_apr = apr,
      term_months       = term,
      current_mob       = 0L,
      remaining_balance = amt,
      last_obs_date     = Sys.Date()
    )

    params <- list(
      recovery_rate       = rec_r,
      servicing_fee_rate  = svc,
      collection_fee_rate = coll,
      recovery_fee_rate   = recf
    )

    wf <- project_loan_cashflows(loan, curves_df, params)

    irr_val <- tryCatch(
      xirr(wf$ncf, wf$date),
      error = function(e) NA_real_
    )

    list(wf = wf, irr = irr_val)
  })

  # IRR result KPI (large display)
  output$irr_demo_result_kpi <- renderUI({
    res <- demo_waterfall()
    irr_pct <- if (!is.na(res$irr)) paste0(round(res$irr * 100, 1), "%") else "N/A"
    div(class = "custom-value-box",
      tags$h5("Computed IRR"),
      tags$h3(irr_pct)
    )
  })

  # Waterfall Table
  output$irr_demo_table <- DT::renderDataTable({
    res <- demo_waterfall()
    wf <- res$wf
    disp <- wf %>%
      mutate(across(c(bbb, ed, er, ewf, eupp, espp, adj_eip, esf, ecf, erf, ncf, ebb),
                    ~ round(., 2))) %>%
      select(MOB = mob, BBB = bbb, ED = ed, ER = er, EWF = ewf,
             EUPP = eupp, ESPP = espp, ADJ_EIP = adj_eip,
             ESF = esf, ECF = ecf, ERF = erf, NCF = ncf, EBB = ebb)

    DT::datatable(disp,
      options = list(
        pageLength = 6,
        scrollX = TRUE,
        dom = "tp",
        ordering = FALSE
      ),
      rownames = FALSE,
      class = "compact stripe"
    ) %>%
      DT::formatCurrency(c("BBB", "ED", "ER", "EWF", "EUPP", "ESPP",
                            "ADJ_EIP", "ESF", "ECF", "ERF", "NCF", "EBB"),
                         currency = "$", digits = 2)
  })

  # Balance & Cashflow Chart
  output$irr_demo_chart <- plotly::renderPlotly({
    res <- demo_waterfall()
    wf <- res$wf

    plotly::plot_ly(wf, x = ~mob) %>%
      plotly::add_trace(y = ~ebb, name = "Ending Balance",
                        type = "scatter", mode = "lines",
                        line = list(color = "#3b82f6", width = 2.5)) %>%
      plotly::add_trace(y = ~ed, name = "Expected Default",
                        type = "bar",
                        marker = list(color = "rgba(239,68,68,0.5)")) %>%
      plotly::add_trace(y = ~ncf, name = "Net Cashflow",
                        type = "bar", yaxis = "y2",
                        marker = list(color = "rgba(251,110,54,0.7)")) %>%
      plotly::layout(
        xaxis = list(title = "Month on Books (MOB)"),
        yaxis = list(title = "Balance ($)", side = "left"),
        yaxis2 = list(title = "Net Cashflow ($)", side = "right",
                      overlaying = "y"),
        legend = list(orientation = "h", y = -0.15),
        margin = list(t = 30),
        barmode = "group",
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })

  # Summary KPIs
  output$irr_demo_kpi_irr <- renderUI({
    res <- demo_waterfall()
    v <- if (!is.na(res$irr)) paste0(round(res$irr * 100, 1), "%") else "N/A"
    kpi_box(v, "IRR (Annualised)")
  })
  output$irr_demo_kpi_ncf <- renderUI({
    res <- demo_waterfall()
    v <- paste0("$", formatC(round(sum(res$wf$ncf)), format = "d", big.mark = ","))
    kpi_box(v, "Total Net Cashflow")
  })
  output$irr_demo_kpi_defaults <- renderUI({
    res <- demo_waterfall()
    v <- paste0("$", formatC(round(sum(res$wf$ed)), format = "d", big.mark = ","))
    kpi_box(v, "Total Defaults")
  })
  output$irr_demo_kpi_fees <- renderUI({
    res <- demo_waterfall()
    total_fees <- sum(res$wf$esf) + sum(res$wf$ecf) + sum(res$wf$erf)
    v <- paste0("$", formatC(round(total_fees), format = "d", big.mark = ","))
    kpi_box(v, "Total Fees")
  })

  # ==================================================================
  # Section 3: Curve Upload & Parameters
  # ==================================================================

  # Reactive to hold uploaded curves (NULL = use parametric)
  uploaded_curves <- reactiveVal(NULL)

  # Download template
  output$irr_download_template <- downloadHandler(
    filename = function() { "dcor_dpor_template.csv" },
    content = function(file) {
      products <- c("Personal Loan", "Auto Loan")
      terms    <- c(36L, 48L, 60L)
      vintages <- c("2024Q1", "2024Q2", "2025Q1")
      template <- expand.grid(
        product_type    = products,
        term_months     = terms,
        vintage_quarter = vintages,
        scenario        = "base",
        stringsAsFactors = FALSE
      )
      rows <- do.call(rbind, lapply(seq_len(nrow(template)), function(i) {
        seg <- template[i, ]
        data.frame(
          product_type    = seg$product_type,
          term_months     = seg$term_months,
          vintage_quarter = seg$vintage_quarter,
          scenario        = seg$scenario,
          mob             = 0:seg$term_months,
          dcor_rate       = c(0, rep(0.001, seg$term_months)),
          dpor_rate       = c(0, rep(0.010, seg$term_months)),
          stringsAsFactors = FALSE
        )
      }))
      write.csv(rows, file, row.names = FALSE)
    }
  )

  # File upload handler
  observeEvent(input$irr_curve_file, {
    req(input$irr_curve_file$datapath)

    if (file.size(input$irr_curve_file$datapath) > 5 * 1024^2) {
      showNotification("File too large (max 5MB)", type = "error", duration = 8)
      return()
    }

    tryCatch({
      df <- read.csv(input$irr_curve_file$datapath,
                     stringsAsFactors = FALSE, check.names = FALSE)

      required <- c("product_type", "term_months", "vintage_quarter",
                     "scenario", "mob", "dcor_rate", "dpor_rate")
      missing_cols <- setdiff(required, names(df))
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing columns:", paste(missing_cols, collapse = ", ")),
          type = "error", duration = 8)
        return()
      }

      df$term_months <- as.integer(df$term_months)
      df$mob         <- as.integer(df$mob)
      df$dcor_rate   <- as.numeric(df$dcor_rate)
      df$dpor_rate   <- as.numeric(df$dpor_rate)
      df$curve_source <- "uploaded"

      uploaded_curves(df)

      n_segments <- nrow(dplyr::distinct(df, product_type, term_months, vintage_quarter, scenario))
      showNotification(
        paste("Curves loaded:", n_segments, "segments across",
              length(unique(df$product_type)), "products"),
        type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste("Error reading CSV:", e$message),
                       type = "error", duration = 8)
    })
  })

  # Clear curves
  observeEvent(input$irr_clear_curves, {
    uploaded_curves(NULL)
    showNotification("Custom curves cleared. Using parametric defaults.",
                     type = "message", duration = 4)
  })

  # Curve status display
  output$irr_curve_status <- renderUI({
    uc <- uploaded_curves()
    if (!is.null(uc)) {
      n_seg <- nrow(dplyr::distinct(uc, product_type, term_months, vintage_quarter, scenario))
      tags$div(
        style = "background:#dcfce7; border:1px solid #22c55e; border-radius:6px; padding:8px 12px; font-size:12px; color:#166534;",
        icon("check-circle"),
        paste("Custom Curves Active:", n_seg, "segments loaded")
      )
    } else {
      tags$div(
        style = "background:#f3f4f6; border:1px solid #d1d5db; border-radius:6px; padding:8px 12px; font-size:12px; color:#6b7280;",
        icon("circle"),
        "Using Parametric Defaults"
      )
    }
  })

  # Segment info (after upload)
  output$irr_curve_segment_info <- renderUI({
    uc <- uploaded_curves()
    if (is.null(uc)) return(NULL)
    tags$div(style = "font-size:11px; color:#6b7280;",
      tags$b("Coverage: "),
      paste("Products:", paste(unique(uc$product_type), collapse = ", ")),
      tags$br(),
      paste("Terms:", paste(sort(unique(uc$term_months)), collapse = ", "), "months"),
      tags$br(),
      paste("Vintages:", paste(sort(unique(uc$vintage_quarter)), collapse = ", ")),
      tags$br(),
      paste("Scenarios:", paste(unique(uc$scenario), collapse = ", "))
    )
  })

  # Scenario description
  output$irr_scenario_description <- renderUI({
    sc <- input$irr_scenario
    desc <- switch(sc,
      "base"       = "Base: DCOR x 1.0, DPOR x 1.0, Recovery +0pp",
      "stress"     = "Stress: DCOR x 1.5, DPOR x 0.7, Recovery -5pp",
      "optimistic" = "Optimistic: DCOR x 0.7, DPOR x 1.2, Recovery +3pp"
    )
    tags$div(style = "font-size:11px; color:#6b7280; margin-top:-10px; margin-bottom:10px;", desc)
  })

  # Apply to Portfolio
  observeEvent(input$irr_apply_portfolio, {
    tape <- perf_tape()
    if (is.null(tape)) {
      showNotification("No performance tape loaded. Upload one first via Data Upload tab.",
                       type = "warning", duration = 6)
      return()
    }

    # Build custom params from sliders
    custom_params <- list(
      "Personal Loan" = list(
        recovery_rate       = input$irr_pl_recovery,
        servicing_fee_rate  = input$irr_pl_svc_fee,
        collection_fee_rate = input$irr_pl_coll_fee,
        recovery_fee_rate   = input$irr_pl_rec_fee
      ),
      "Auto Loan" = list(
        recovery_rate       = input$irr_al_recovery,
        servicing_fee_rate  = input$irr_al_svc_fee,
        collection_fee_rate = input$irr_al_coll_fee,
        recovery_fee_rate   = input$irr_al_rec_fee
      )
    )

    # Get the raw tape data — re-read from CSV
    raw_file <- "inst/data/irr_performance_tape_FIXED_v2.csv"
    if (!file.exists(raw_file)) {
      showNotification("Performance tape CSV not found.", type = "error", duration = 6)
      return()
    }

    showNotification("Re-computing portfolio IRR with new parameters...",
                     type = "message", duration = 3)

    tryCatch({
      raw <- read.csv(raw_file, stringsAsFactors = FALSE, check.names = FALSE)

      uc <- uploaded_curves()
      scenario_name <- input$irr_scenario

      new_tape <- build_perf_tape_v2(
        raw,
        curves   = uc,
        params   = custom_params,
        scenario = scenario_name
      )

      perf_tape(new_tape)

      v <- compute_portfolio_xirr_v2(new_tape)
      irr_str <- if (!is.na(v)) paste0(round(v * 100, 1), "%") else "N/A"
      showNotification(
        paste("Portfolio updated! IRR:", irr_str, "(", scenario_name, "scenario)"),
        type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
    })
  })

  # Apply status
  output$irr_apply_status <- renderUI({
    tape <- perf_tape()
    if (is.null(tape) || is.null(tape$curves)) return(NULL)
    n_seg <- nrow(dplyr::distinct(tape$curves, product_type, term_months, vintage_quarter))
    tags$div(style = "font-size:11px; color:#6b7280; text-align:center;",
      paste("Current tape:", n_seg, "curve segments,",
            nrow(tape$loan_view), "loans")
    )
  })

  # Curve Preview — update segment choices
  observe({
    tape <- perf_tape()
    uc   <- uploaded_curves()
    curves <- if (!is.null(uc)) uc else if (!is.null(tape) && !is.null(tape$curves)) tape$curves else NULL
    if (is.null(curves)) return()

    segments <- dplyr::distinct(curves, product_type, term_months, vintage_quarter)
    choices <- paste(segments$product_type, segments$term_months, "mo",
                     segments$vintage_quarter)
    names(choices) <- choices
    updateSelectInput(session, "irr_curve_segment",
                      choices = choices, selected = choices[1])
  })

  output$irr_curve_preview_chart <- plotly::renderPlotly({
    tape <- perf_tape()
    uc   <- uploaded_curves()
    curves <- if (!is.null(uc)) uc else if (!is.null(tape) && !is.null(tape$curves)) tape$curves else NULL
    req(curves, input$irr_curve_segment)

    # Parse selection: "Personal Loan 36 mo 2024Q1"
    parts <- strsplit(input$irr_curve_segment, " ")[[1]]
    n <- length(parts)
    vq <- parts[n]                       # last token = vintage
    tm <- as.integer(parts[n - 2])       # 3rd from end = term
    pt <- paste(parts[1:(n - 3)], collapse = " ")  # everything before

    seg <- curves %>%
      dplyr::filter(product_type == pt, term_months == tm, vintage_quarter == vq)

    if (nrow(seg) == 0) return(plotly::plotly_empty())

    plotly::plot_ly(seg, x = ~mob) %>%
      plotly::add_trace(y = ~dcor_rate, name = "DCOR",
                        type = "scatter", mode = "lines+markers",
                        line = list(color = "#ef4444", width = 2),
                        marker = list(size = 4)) %>%
      plotly::add_trace(y = ~dpor_rate, name = "DPOR",
                        type = "scatter", mode = "lines+markers",
                        line = list(color = "#3b82f6", width = 2),
                        marker = list(size = 4)) %>%
      plotly::layout(
        xaxis = list(title = "Month on Books (MOB)"),
        yaxis = list(title = "Rate", tickformat = ".3%"),
        legend = list(orientation = "h", y = -0.15),
        margin = list(t = 30),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
}
