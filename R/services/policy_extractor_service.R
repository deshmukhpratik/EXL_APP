# R/services/policy_extractor_service.R
# Pure functions — no Shiny deps. Sourced into .GlobalEnv by app.R.

# Extract plain text from an uploaded policy doc.
# Returns character(1) on success, NULL on failure (unreadable, wrong type,
# < 1 KB, errored extraction).
extract_text_from_file <- function(path, ext) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  size <- tryCatch(file.info(path)$size, error = function(e) 0)
  if (is.na(size) || size < 1024) return(NULL)

  ext <- tolower(ext)
  if (ext == "pdf")  return(.extract_pdf_text(path))
  if (ext %in% c("docx", "doc")) return(.extract_docx_text(path))
  NULL
}

.extract_pdf_text <- function(path) {
  pdftotext <- Sys.which("pdftotext")
  if (!nzchar(pdftotext)) return(NULL)
  out <- tryCatch(
    system2(pdftotext, args = c(shQuote(path), "-"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(out) || length(out) == 0) return(NULL)
  txt <- paste(out, collapse = "\n")
  if (!nzchar(trimws(txt))) return(NULL)
  # Normalize Unicode MINUS SIGN (U+2212) — the pdf() R device emits this
  # for ASCII '-' — back to ASCII hyphen so downstream regex patterns can
  # use plain hyphens.
  txt <- gsub("−", "-", txt, fixed = TRUE)
  txt
}

.extract_docx_text <- function(path) {
  if (!requireNamespace("xml2", quietly = TRUE)) return(NULL)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- tryCatch({
    utils::unzip(path, files = "word/document.xml", exdir = tmp)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) return(NULL)
  xml_path <- file.path(tmp, "word", "document.xml")
  if (!file.exists(xml_path)) return(NULL)
  doc <- tryCatch(xml2::read_xml(xml_path), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  nodes <- xml2::xml_find_all(doc, "//w:t", ns = ns)
  if (length(nodes) == 0) return(NULL)
  txt <- paste(xml2::xml_text(nodes), collapse = "\n")
  if (!nzchar(trimws(txt))) return(NULL)
  txt
}

# Run regex passes on extracted text and return both human-readable lines
# (per modal field) and structured per-lever values (for diffing).
# Return shape:
#   list(
#     lines  = list(grading=character, pricing=character,
#                   decision=character, r_logic=character),
#     values = list(fico_min=NA_integer_, fico_max=…, dti_max=…, hard_inq_max=…,
#                   util_max=…, base_apr=…, auto_approve=…,
#                   manual_review=c(NA,NA), decline=…,
#                   min_credit_history_months=…, no_bankruptcies=NA),
#     n_levers = 0L
#   )
extract_levers <- function(text) {
  if (is.null(text) || !is.character(text) || !nzchar(trimws(text))) {
    return(.empty_lever_result())
  }
  # Normalize whitespace for easier matching
  ntext <- gsub("[ \t]+", " ", text)

  values <- .empty_values()
  lines  <- list(grading = character(),
                 pricing = character(),
                 decision = character(),
                 r_logic  = character())

  # ---- FICO floor ----
  m <- regmatches(ntext, regexpr(
    "(?i)FICO\\s*(>=?|>\\s*=|≥|min(?:imum)?|of|at\\s*least)\\s*(\\d{3})",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.integer(sub(".*?(\\d{3}).*", "\\1", m[1], perl = TRUE))
    if (!is.na(n) && n >= 300 && n <= 850) {
      values$fico_min <- n
      lines$grading <- c(lines$grading, sprintf("FICO >= %d", n))
    }
  }

  # ---- FICO ceiling (rare) ----
  m <- regmatches(ntext, regexpr(
    "(?i)FICO\\s*(<=?|≤|max(?:imum)?|cap(?:ped)?\\s*at)\\s*(\\d{3})",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.integer(sub(".*?(\\d{3}).*", "\\1", m[1], perl = TRUE))
    if (!is.na(n) && n >= 300 && n <= 850) {
      values$fico_max <- n
      lines$grading <- c(lines$grading, sprintf("FICO <= %d", n))
    }
  }

  # ---- DTI cap ----
  m <- regmatches(ntext, regexpr(
    "(?i)DTI\\s*(<=?|≤|max(?:imum)?|cap(?:ped)?\\s*at|of)\\s*(\\d{1,2})\\s*%?",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.numeric(sub(".*?(\\d{1,2}).*", "\\1", m[1], perl = TRUE))
    if (!is.na(n) && n >= 1 && n <= 99) {
      values$dti_max <- n
      lines$grading <- c(lines$grading, sprintf("DTI <= %g%%", n))
    }
  }

  # ---- Thin file (months of credit history) ----
  m <- regmatches(ntext, regexpr(
    "(?i)(credit\\s*history|trade\\s*line|account\\s*age|thin\\s*file|min(?:imum)?\\s*tenure)[^\\n]{0,40}?(\\d{1,3})\\s*month",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.integer(sub(".*?(\\d{1,3})\\s*month.*", "\\1", m[1], perl = TRUE))
    if (!is.na(n) && n >= 0 && n <= 240) {
      values$min_credit_history_months <- n
      lines$grading <- c(lines$grading,
                         sprintf("Min credit history: %d months", n))
    }
  }

  # ---- Bankruptcy presence flag ----
  if (grepl("(?i)bankrupt", ntext)) {
    values$no_bankruptcies <- TRUE
    lines$grading <- c(lines$grading, "No bankruptcies")
  }

  # ---- Hard inquiries (6M) ----
  m <- regmatches(ntext, regexpr(
    "(?i)(hard\\s*)?inquir(?:y|ies)[^\\n]{0,40}?(<=?|≤|max(?:imum)?|=)\\s*(\\d{1,2})",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.integer(sub(".*(<=?|≤|max(?:imum)?|=)\\s*(\\d{1,2}).*", "\\2", m[1], perl = TRUE))
    if (!is.na(n) && n >= 0 && n <= 50) {
      values$hard_inq_max <- n
      lines$r_logic <- c(lines$r_logic, sprintf("Hard inquiries (6M) <= %d", n))
    }
  }
  # Also try compact "HardInq6M max=3" form (matches govern_service describe text)
  if (is.na(values$hard_inq_max)) {
    m <- regmatches(ntext, regexpr(
      "(?i)HardInq[0-9A-Za-z_]*\\s*max\\s*=\\s*(\\d{1,2})",
      ntext, perl = TRUE))
    if (length(m) > 0) {
      n <- as.integer(sub("(?i).*max\\s*=\\s*(\\d{1,2}).*", "\\1", m[1], perl = TRUE))
      if (!is.na(n) && n >= 0 && n <= 50) {
        values$hard_inq_max <- n
        lines$r_logic <- c(lines$r_logic, sprintf("Hard inquiries (6M) <= %d", n))
      }
    }
  }

  # ---- Utilization cap ----
  m <- regmatches(ntext, regexpr(
    "(?i)utili[sz]ation[^\\n]{0,40}?(<=?|≤|>=?|over|above|max(?:imum)?)\\s*(\\d{1,3})\\s*%",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.numeric(sub(".*?(\\d{1,3})\\s*%.*", "\\1", m[1], perl = TRUE))
    if (!is.na(n) && n >= 0 && n <= 100) {
      values$util_max <- n
      lines$r_logic <- c(lines$r_logic, sprintf("Utilization <= %g%%", n))
    }
  }

  # ---- Base APR ----
  m <- regmatches(ntext, regexpr(
    "(?i)(?:base\\s*APR|APR|interest\\s*rate|base\\s*rate)\\s*[:=]?\\s*(\\d{1,2}(?:\\.\\d+)?)\\s*%",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.numeric(sub(".*?(\\d{1,2}(?:\\.\\d+)?)\\s*%.*", "\\1", m[1], perl = TRUE))
    if (!is.na(n) && n >= 0 && n <= 99) {
      values$base_apr <- n
      lines$pricing <- c(lines$pricing, sprintf("Base APR: %g%%", n))
    }
  }

  # ---- Auto-approve cutoff ----
  m <- regmatches(ntext, regexpr(
    "(?i)auto.?approve[^\\n]{0,40}?(\\d{3})",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.integer(sub(".*?(\\d{3}).*", "\\1", m[1], perl = TRUE))
    if (!is.na(n) && n >= 300 && n <= 850) {
      values$auto_approve <- n
      lines$decision <- c(lines$decision, sprintf("Auto-approve: score > %d", n))
    }
  }

  # ---- Manual review band ----
  m <- regmatches(ntext, regexpr(
    "(?i)manual\\s*review[^\\n]{0,40}?(\\d{3})\\s*[-–]\\s*(\\d{3})",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    nums <- as.integer(regmatches(m[1], gregexpr("\\d{3}", m[1]))[[1]])
    if (length(nums) >= 2 && all(!is.na(nums)) &&
        all(nums >= 300) && all(nums <= 850)) {
      values$manual_review <- c(nums[1], nums[2])
      lines$decision <- c(lines$decision,
                          sprintf("Manual review: %d-%d", nums[1], nums[2]))
    }
  }

  # ---- Decline cutoff ----
  m <- regmatches(ntext, regexpr(
    "(?i)decline[^\\n]{0,40}?(\\d{3})",
    ntext, perl = TRUE))
  if (length(m) > 0) {
    n <- as.integer(sub(".*?(\\d{3}).*", "\\1", m[1], perl = TRUE))
    if (!is.na(n) && n >= 300 && n <= 850) {
      values$decline <- n
      lines$decision <- c(lines$decision, sprintf("Decline: < %d", n))
    }
  }

  values_set <- vapply(values, function(v) {
    if (length(v) == 0) return(FALSE)
    if (length(v) == 2) return(all(!is.na(v)))  # manual_review pair: count only if both endpoints known
    !is.na(v[1])
  }, logical(1))
  n_levers <- sum(values_set)

  list(
    lines = lapply(lines, function(v) paste(v, collapse = "\n")),
    values = values,
    n_levers = as.integer(n_levers)
  )
}

.empty_values <- function() {
  list(
    fico_min = NA_integer_, fico_max = NA_integer_,
    dti_max = NA_real_, hard_inq_max = NA_integer_,
    util_max = NA_real_, base_apr = NA_real_,
    auto_approve = NA_integer_, manual_review = c(NA_integer_, NA_integer_),
    decline = NA_integer_, min_credit_history_months = NA_integer_,
    no_bankruptcies = NA
  )
}

.empty_lever_result <- function() {
  list(
    lines = list(grading = "", pricing = "", decision = "", r_logic = ""),
    values = .empty_values(),
    n_levers = 0L
  )
}
