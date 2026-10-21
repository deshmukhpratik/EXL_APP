# ============================================================
# R/ui/analysis_pipeline.R — Analysis Engine Pipeline Component
#
# A visual step-by-step readout that shows each analytical stage
# (calibration lookup, bad rate projection, elasticity estimation,
# policy cascade, IRR projection) as it executes. Replaces the
# generic withProgress() spinner with a transparent view of the
# engine's work.
#
# Usage:
#   UI: place pipeline_container("my_id") in the tab layout
#   Server:
#     pipeline_show(session, "my_id", steps)
#     pipeline_step_done(session, "my_id", step_index, result)
#     pipeline_complete(session, "my_id")
#     pipeline_hide(session, "my_id")
# ============================================================

# ---- CSS (injected once via pipeline_head_tags) ----
.pipeline_css <- '
/* ---- Analysis Pipeline ---- */
.analysis-pipeline {
  display: none;
  background: #111827;
  border-left: 3px solid #fb6e36;
  border-radius: 7px;
  padding: 14px 18px 12px;
  margin: 0 15px 14px;
  font-family: "IBM Plex Mono", "JetBrains Mono", "Consolas", monospace;
  overflow: hidden;
}
.analysis-pipeline.ap-visible {
  display: block;
  animation: ap-fade-in 0.3s ease-out;
}
@keyframes ap-fade-in {
  from { opacity: 0; transform: translateY(-8px); }
  to   { opacity: 1; transform: translateY(0); }
}

/* Header */
.ap-header {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 10px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 1.2px;
  text-transform: uppercase;
  color: #fb6e36;
}
.ap-header-dot {
  width: 6px; height: 6px;
  border-radius: 50%;
  background: #fb6e36;
  animation: ap-header-pulse 1.5s ease-in-out infinite;
}
.ap-header.ap-done .ap-header-dot {
  background: #16a34a;
  animation: none;
}
.ap-header.ap-done { color: #16a34a; }

@keyframes ap-header-pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.3; }
}

/* Step row */
.ap-step {
  display: flex;
  align-items: flex-start;
  gap: 10px;
  padding: 5px 0;
  opacity: 0;
  transform: translateX(-6px);
  transition: opacity 0.25s ease, transform 0.25s ease;
}
.ap-step.ap-revealed {
  opacity: 1;
  transform: translateX(0);
}

/* Status indicator */
.ap-indicator {
  flex-shrink: 0;
  width: 16px; height: 16px;
  margin-top: 1px;
  display: flex;
  align-items: center;
  justify-content: center;
}
.ap-dot {
  width: 7px; height: 7px;
  border-radius: 50%;
  background: #374151;
  transition: all 0.3s ease;
}
.ap-step.ap-running .ap-dot {
  background: #fb6e36;
  box-shadow: 0 0 8px rgba(251, 110, 54, 0.5);
  animation: ap-dot-pulse 1s ease-in-out infinite;
}
@keyframes ap-dot-pulse {
  0%, 100% { transform: scale(1); box-shadow: 0 0 8px rgba(251, 110, 54, 0.5); }
  50% { transform: scale(1.3); box-shadow: 0 0 14px rgba(251, 110, 54, 0.7); }
}
.ap-step.ap-done .ap-dot { display: none; }
.ap-check {
  display: none;
  color: #16a34a;
  font-size: 13px;
  line-height: 1;
}
.ap-step.ap-done .ap-check { display: block; }

/* Label */
.ap-label {
  font-size: 11.5px;
  color: #6b7280;
  transition: color 0.2s ease;
  line-height: 1.4;
}
.ap-step.ap-running .ap-label { color: #d1d5db; }
.ap-step.ap-done .ap-label { color: #9ca3af; }

/* Result (appears on completion) */
.ap-result {
  margin-left: auto;
  font-size: 11px;
  font-weight: 600;
  color: #fb6e36;
  white-space: nowrap;
  opacity: 0;
  transform: translateX(8px);
  transition: opacity 0.3s ease 0.1s, transform 0.3s ease 0.1s;
}
.ap-step.ap-done .ap-result {
  opacity: 1;
  transform: translateX(0);
}

/* Collapse animation when done */
.analysis-pipeline.ap-collapsing {
  animation: ap-collapse 0.4s ease-in forwards;
  animation-delay: 0.8s;
}
@keyframes ap-collapse {
  to { max-height: 0; padding: 0 18px; margin-bottom: 0; opacity: 0; overflow: hidden; }
}

/* ---- Static variant (post-action recap modals) ---- */
.analysis-pipeline.ap-static { animation: none; margin: 0; }
.analysis-pipeline.ap-static .ap-step {
  opacity: 1; transform: none; transition: none;
}

/* Skipped step (not applicable for this action) */
.ap-step.ap-skipped .ap-dot {
  display: block; background: #374151;
  animation: none; box-shadow: none; transform: none;
}
.ap-step.ap-skipped .ap-check { display: none; }
.ap-step.ap-skipped .ap-label { color: #6b7280; font-style: italic; }
.ap-step.ap-skipped .ap-result { color: #6b7280; font-weight: 500; }

/* Summary strip between header and steps */
.ap-summary {
  padding: 4px 0 10px;
  border-bottom: 1px solid #1f2937;
  margin-bottom: 10px;
  display: flex; flex-wrap: wrap; gap: 22px;
  font-size: 11.5px;
}
.ap-summary-item { display: flex; flex-direction: column; gap: 2px; }
.ap-summary-label {
  color: #9ca3af; text-transform: uppercase;
  letter-spacing: 0.5px; font-size: 9.5px; font-weight: 700;
}
.ap-summary-value { color: #fb6e36; font-weight: 700; font-size: 13px; }
.ap-summary-value.ap-muted { color: #6b7280; }
'

# ---- JavaScript (injected once via pipeline_head_tags) ----
.pipeline_js <- '
// Analysis Pipeline handlers
(function() {
  // Show pipeline with step definitions
  Shiny.addCustomMessageHandler("ap-show", function(msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    var steps = msg.steps; // [{label: "...", id: "..."}]
    // Build DOM
    var html = \'<div class="ap-header"><div class="ap-header-dot"></div>ANALYSIS ENGINE</div>\';
    for (var i = 0; i < steps.length; i++) {
      html += \'<div class="ap-step" data-step="\' + i + \'" id="\' + msg.id + \'-step-\' + i + \'">\' +
        \'<div class="ap-indicator"><div class="ap-dot"></div><div class="ap-check">\\u2713</div></div>\' +
        \'<div class="ap-label">\' + steps[i].label + \'</div>\' +
        \'<div class="ap-result"></div>\' +
        \'</div>\';
    }
    el.innerHTML = html;
    el.classList.remove("ap-collapsing");
    el.classList.add("ap-visible");
    // Stagger reveal
    var rows = el.querySelectorAll(".ap-step");
    rows.forEach(function(row, idx) {
      setTimeout(function() {
        row.classList.add("ap-revealed");
        if (idx === 0) row.classList.add("ap-running");
      }, 80 + idx * 60);
    });
  });

  // Update a step: mark done + set result, activate next
  Shiny.addCustomMessageHandler("ap-step", function(msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    var step = el.querySelector("[data-step=\\"" + msg.step + "\\"]");
    if (step) {
      step.classList.remove("ap-running");
      step.classList.add("ap-done");
      var res = step.querySelector(".ap-result");
      if (res && msg.result) res.textContent = msg.result;
    }
    // Activate next step
    var next = el.querySelector("[data-step=\\"" + (msg.step + 1) + "\\"]");
    if (next) next.classList.add("ap-running");
  });

  // Mark pipeline complete
  Shiny.addCustomMessageHandler("ap-done", function(msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    var hdr = el.querySelector(".ap-header");
    if (hdr) {
      hdr.classList.add("ap-done");
      hdr.innerHTML = \'<div class="ap-header-dot"></div>ANALYSIS COMPLETE\';
    }
    // Optional: auto-collapse after delay
    if (msg.collapse) {
      el.classList.add("ap-collapsing");
    }
  });

  // Hide pipeline
  Shiny.addCustomMessageHandler("ap-hide", function(msg) {
    var el = document.getElementById(msg.id);
    if (el) {
      el.classList.remove("ap-visible", "ap-collapsing");
      el.style.display = "none";
    }
  });
})();
'

# ---- Head tags (include once in app UI) ----
pipeline_head_tags <- function() {
  tagList(
    tags$style(HTML(.pipeline_css)),
    tags$script(HTML(.pipeline_js))
  )
}

# ---- UI container (place in tab layout) ----
pipeline_container <- function(id) {
  div(id = id, class = "analysis-pipeline")
}

# ---- Server helpers ----

#' Show the pipeline with step definitions.
#' @param session Shiny session
#' @param id Pipeline container id (matches pipeline_container)
#' @param steps Named list of lists: list(list(label="...", id="..."), ...)
pipeline_show <- function(session, id, steps) {
  session$sendCustomMessage("ap-show", list(
    id = id,
    steps = steps
  ))
  # Small flush delay so the DOM updates before R continues
  Sys.sleep(0.05)
}

#' Mark a step as done with an optional result string.
#' @param step_index 0-based step index
#' @param result Character string to show as the result (e.g. "3.7% observed")
pipeline_step_done <- function(session, id, step_index, result = "") {
  session$sendCustomMessage("ap-step", list(
    id = id,
    step = as.integer(step_index),
    result = result
  ))
  Sys.sleep(0.05)
}

#' Mark pipeline as complete.
#' @param collapse If TRUE, pipeline auto-collapses after ~1s
pipeline_complete <- function(session, id, collapse = FALSE) {
  session$sendCustomMessage("ap-done", list(
    id = id,
    collapse = collapse
  ))
}

#' Hide pipeline immediately.
pipeline_hide <- function(session, id) {
  session$sendCustomMessage("ap-hide", list(id = id))
}

#' Render a static "analysis engine" card (post-action recap).
#'
#' Matches the look of the live progress pipeline but is rendered fully
#' complete — every step shows its checkmark, the header reads
#' "ANALYSIS COMPLETE", and the optional summary strip shows the
#' action-level deltas above the step list.
#'
#' @param steps list of list(label=..., result=..., status="done"|"skipped")
#' @param title header text (default "ANALYSIS COMPLETE")
#' @param summary optional list of list(label=..., value=..., muted=TRUE/FALSE)
#'   rendered as a summary strip between header and steps.
analysis_engine_static <- function(steps, title = "ANALYSIS COMPLETE",
                                   summary = NULL) {
  summary_el <- NULL
  if (!is.null(summary) && length(summary) > 0) {
    summary_el <- tags$div(class = "ap-summary",
      lapply(summary, function(it) {
        val_cls <- paste0("ap-summary-value",
                          if (isTRUE(it$muted)) " ap-muted" else "")
        tags$div(class = "ap-summary-item",
          tags$div(class = "ap-summary-label", it$label),
          tags$div(class = val_cls, it$value))
      })
    )
  }

  step_els <- lapply(steps, function(s) {
    status <- s$status %||% "done"
    step_class <- paste0("ap-step ap-revealed ",
                         if (status == "skipped") "ap-skipped" else "ap-done")
    tags$div(class = step_class,
      tags$div(class = "ap-indicator",
        tags$div(class = "ap-dot"),
        tags$div(class = "ap-check", "\u2713")),
      tags$div(class = "ap-label", s$label),
      tags$div(class = "ap-result", s$result %||% ""))
  })

  tags$div(class = "analysis-pipeline ap-visible ap-static",
    tags$div(class = "ap-header ap-done",
      tags$div(class = "ap-header-dot"), title),
    summary_el,
    do.call(tagList, step_els)
  )
}
