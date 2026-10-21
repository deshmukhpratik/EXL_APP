# ============================================================
# R/services/root_cause_service.R
# Orchestrates the root-cause engine. Pure, no Shiny imports.
# ============================================================

.rc_cache <- new.env(parent = emptyenv())

.rc_hash <- function(...) {
  bits <- list(...)
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(bits))
  paste(vapply(bits, function(x) paste(deparse(x), collapse = "|"), character(1)),
        collapse = "|")
}

run_root_cause <- function(df, metric, comparison = c("none", "baseline_policy", "prior_vintage"),
                           candidates = NULL,
                           baseline_df = NULL,
                           irr_cache = NULL,
                           max_depth = 4, min_seg_pct = 0.02,
                           cache = TRUE) {
  comparison <- match.arg(comparison)
  registry   <- rc_metric_registry()
  if (!metric %in% names(registry)) {
    stop(sprintf("Unknown metric '%s'. Valid: %s",
                 metric, paste(names(registry), collapse = ", ")))
  }
  if (is.null(candidates) || length(candidates) == 0) {
    candidates <- rc_default_candidates()
  }
  candidates <- intersect(candidates, names(df))

  if (!is.null(irr_cache)) attr(df, "irr_cache") <- irr_cache

  key <- .rc_hash(metric, comparison, candidates, max_depth, min_seg_pct,
                  nrow(df), digest_col_sig(df))
  if (cache && !is.null(.rc_cache[[key]])) return(.rc_cache[[key]])

  metric_fn <- registry[[metric]]$fn

  path <- rc_compute_pareto_path(df, metric_fn, candidates,
                                 max_depth = max_depth,
                                 min_seg_pct = min_seg_pct)

  alts <- collect_alternatives(path)

  shift <- NULL
  if (comparison == "prior_vintage" && "booking_vintage" %in% names(df)) {
    vq <- sort(unique(df$booking_vintage))
    if (length(vq) >= 2) {
      cur_v  <- vq[length(vq)]
      base_v <- vq[length(vq) - 1]
      cur_df  <- df[df$booking_vintage == cur_v,  , drop = FALSE]
      base_df <- df[df$booking_vintage == base_v, , drop = FALSE]
      seg_var <- candidates[1]
      if (!is.null(seg_var)) {
        cur_df$.seg  <- as.character(rc_bin_variable(cur_df,  seg_var))
        base_df$.seg <- as.character(rc_bin_variable(base_df, seg_var))
        if (!is.null(irr_cache)) {
          attr(cur_df,  "irr_cache") <- irr_cache
          attr(base_df, "irr_cache") <- irr_cache
        }
        shift <- rc_decompose_shift(cur_df, base_df, metric_fn, seg_col = ".seg")
      }
    }
  } else if (comparison == "baseline_policy" && !is.null(baseline_df)) {
    if (!is.null(irr_cache)) attr(baseline_df, "irr_cache") <- irr_cache
    seg_var <- candidates[1]
    if (!is.null(seg_var)) {
      cur2  <- df;          cur2$.seg  <- as.character(rc_bin_variable(df,          seg_var))
      base2 <- baseline_df; base2$.seg <- as.character(rc_bin_variable(baseline_df, seg_var))
      shift <- rc_decompose_shift(cur2, base2, metric_fn, seg_col = ".seg")
    }
  }

  result <- list(
    path = path,
    shift = shift,
    alternatives = alts,
    metric = registry[[metric]]
  )
  if (cache) .rc_cache[[key]] <- result
  result
}

collect_alternatives <- function(path, k = 3) {
  out <- list()
  node <- path
  while (!is.null(node) && !is.null(node$alternatives)) {
    alts <- node$alternatives
    if (nrow(alts) > 1) {
      ru <- alts[2:min(nrow(alts), 1 + k), , drop = FALSE]
      ru$at_depth <- node$depth
      out[[length(out) + 1]] <- ru
    }
    node <- node$child
  }
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

digest_col_sig <- function(df) paste(sort(names(df)), collapse = ",")
