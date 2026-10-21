# ============================================================
# Engine: Portfolio Scanner
# Single source of truth for CART-based segment discovery.
# Called by: tab_smart_recommender.R, discover_service.R, demo_rehearsal.R
# No Shiny dependency — sources cleanly in non-interactive R.
# ============================================================

# CV-based pruning helper. rpart's xval=10 already runs 10-fold CV during
# fit; cptable holds (CP, xerror, xstd). We pick the cp that minimises
# xerror and prune to it. If pruning would collapse the tree to a root-only
# stub (no splits remain), keep the unpruned tree — honest segments from
# CV beats over-fit splits, but zero output is worse than either.
.cv_prune_or_keep <- function(tree) {
  cp_table <- tryCatch(tree$cptable, error = function(e) NULL)
  if (is.null(cp_table) || !is.matrix(cp_table) || nrow(cp_table) < 2) return(tree)
  if (!"xerror" %in% colnames(cp_table)) return(tree)

  best_idx <- which.min(cp_table[, "xerror"])
  best_cp  <- cp_table[best_idx, "CP"]
  pruned   <- tryCatch(rpart::prune(tree, cp = best_cp), error = function(e) NULL)
  if (is.null(pruned)) return(tree)

  # Only adopt the pruned tree if it still has at least one internal split
  has_split <- sum(pruned$frame$var != "<leaf>") > 0
  if (has_split) pruned else tree
}

scan_portfolio <- function(
  df_issued,
  avail_vars,
  perf_tape        = NULL,
  irr_cache        = NULL,
  sweep_cols       = c("risk_grade", "partner_channel", "borrower_repeat_type"),
  cross_slices     = TRUE,
  objective        = c("blended", "delinquency", "irr"),
  anchor_fico_max  = 760,
  jaccard_threshold = 0.70,
  max_segments     = 15,
  progress_fn      = NULL
) {
  objective <- match.arg(objective)
  t0 <- Sys.time()

  # ── Early exit: too few rows ──
  if (nrow(df_issued) < 50) {
    return(list(
      segments = list(), portfolio_bad = NA_real_, portfolio_proj = NA_real_,
      portfolio_irr = NA_real_, variable_importance = numeric(0),
      elapsed_seconds = 0, n_slices_scanned = 0L,
      recommended_demo_segment = NULL
    ))
  }

  # ── Attach IRR from cache if provided ──
  if (!is.null(irr_cache) && is.data.frame(irr_cache) &&
      "loan_irr" %in% names(irr_cache)) {
    if (!"loan_irr" %in% names(df_issued)) {
      irr_lookup <- setNames(irr_cache$loan_irr, as.character(irr_cache$loan_id))
      df_issued$loan_irr <- irr_lookup[as.character(df_issued$loan_id)]
    }
  } else if (!"loan_irr" %in% names(df_issued)) {
    df_issued$loan_irr <- NA_real_
  }

  # ── Portfolio baseline ──
  portfolio_bad <- mean(df_issued$performance_delinquency_flag, na.rm = TRUE)
  portfolio_irr <- mean(df_issued$loan_irr, na.rm = TRUE)
  if (is.na(portfolio_irr)) portfolio_irr <- 0

  # ── Variable validation ──
  avail_vars <- avail_vars[avail_vars %in% names(df_issued)]
  avail_vars <- avail_vars[vapply(avail_vars, function(v) is.numeric(df_issued[[v]]), logical(1))]
  if (length(avail_vars) < 3) {
    return(list(
      segments = list(), portfolio_bad = portfolio_bad, portfolio_proj = portfolio_bad,
      portfolio_irr = portfolio_irr, variable_importance = numeric(0),
      elapsed_seconds = as.numeric(Sys.time() - t0, units = "secs"),
      n_slices_scanned = 0L, recommended_demo_segment = NULL
    ))
  }

  # ── Formula helper ──
  make_formula <- function(target) {
    as.formula(paste(target, "~", paste(avail_vars, collapse = " + ")))
  }

  # ── Build slice list ──
  slices <- list(list(label = "All Data", rows = seq_len(nrow(df_issued))))

  # Map sweep_cols to display prefixes
  known_prefixes <- c(risk_grade = "Grade", partner_channel = "Partner",
                      borrower_repeat_type = "Repeat Type")
  sweep_dims <- lapply(sweep_cols, function(col) {
    prefix <- if (col %in% names(known_prefixes)) {
      known_prefixes[[col]]
    } else {
      # Auto-generate prefix from column name
      prefix_raw <- gsub("_", " ", sub("_.*", "", col))
      paste0(toupper(substring(prefix_raw, 1, 1)), substring(prefix_raw, 2))
    }
    list(col = col, prefix = prefix)
  })

  # 1-way slices (store row indices, not data copies, to save memory)
  for (dim in sweep_dims) {
    if (!(dim$col %in% names(df_issued))) next
    levels_found <- unique(df_issued[[dim$col]])
    levels_found <- levels_found[!is.na(levels_found)]
    for (lev in levels_found) {
      idx <- which(df_issued[[dim$col]] == lev)
      if (length(idx) >= 100) {
        slices <- c(slices, list(list(
          label = paste0(dim$prefix, ": ", lev),
          rows  = idx
        )))
      }
    }
  }

  # 2-way cross slices
  if (cross_slices) {
    valid_dims <- Filter(function(d) d$col %in% names(df_issued), sweep_dims)
    if (length(valid_dims) >= 2) {
      for (a in seq_len(length(valid_dims) - 1)) {
        for (b in (a + 1):length(valid_dims)) {
          dim_a <- valid_dims[[a]]
          dim_b <- valid_dims[[b]]
          levs_a <- unique(df_issued[[dim_a$col]])
          levs_a <- levs_a[!is.na(levs_a)]
          levs_b <- unique(df_issued[[dim_b$col]])
          levs_b <- levs_b[!is.na(levs_b)]
          for (la in levs_a) {
            for (lb in levs_b) {
              idx <- which(df_issued[[dim_a$col]] == la &
                           df_issued[[dim_b$col]] == lb)
              if (length(idx) >= 200) {
                slices <- c(slices, list(list(
                  label = paste0(dim_a$prefix, ": ", la, " \u00d7 ",
                                 dim_b$prefix, ": ", lb),
                  rows  = idx
                )))
              }
            }
          }
        }
      }
    }
  }

  n_slices <- length(slices)
  segments <- list()
  all_var_importance <- numeric(0)
  run_delinq <- objective %in% c("delinquency", "blended")
  run_irr    <- objective %in% c("irr", "blended")

  # ── Sweep CART across all slices ──
  for (si in seq_along(slices)) {
    slice       <- slices[[si]]
    slice_df    <- df_issued[slice$rows, , drop = FALSE]
    slice_label <- slice$label
    n_slice     <- nrow(slice_df)
    mb_val      <- max(15L, min(30L, floor(n_slice / 20)))

    # Tree A: Delinquency (classification)
    if (run_delinq) {
      tryCatch({
        tree_del <- rpart::rpart(
          make_formula("performance_delinquency_flag"),
          data    = slice_df,
          method  = "class",
          parms   = list(split = "information", prior = c(0.3, 0.7)),
          control = rpart::rpart.control(maxdepth = 5, minbucket = mb_val,
                                          cp = 0.001, xval = 10)
        )
        # Cross-validation pruning: pick cp that minimises 10-fold xerror.
        # Falls back to the unpruned tree if pruning would collapse it to a
        # root-only stub (which would yield zero segments). Honest segments
        # from CV beats over-fit splits, but we never want zero output.
        tree_del <- .cv_prune_or_keep(tree_del)

        # Accumulate variable importance across delinquency trees
        vi <- tree_del$variable.importance
        if (length(vi) > 0) {
          for (nm in names(vi)) {
            prev <- if (nm %in% names(all_var_importance)) all_var_importance[[nm]] else 0
            all_var_importance[[nm]] <- prev + vi[[nm]]
          }
        }
        new_segs <- extract_segments(tree_del, slice_df, avail_vars,
                                     "delinquency", portfolio_bad, portfolio_irr)
        for (k in seq_along(new_segs)) new_segs[[k]]$source_filter <- slice_label
        segments <- c(segments, new_segs)
      }, error = function(e) {
        message("[scan_portfolio] delinquency CART slice '", slice_label,
                "' failed: ", e$message)
        NULL
      })
    }

    # Tree B: IRR (regression) — only if enough non-NA IRR values
    if (run_irr && sum(!is.na(slice_df$loan_irr)) > 50) {
      tryCatch({
        df_irr <- slice_df[!is.na(slice_df$loan_irr), ]
        tree_irr <- rpart::rpart(
          make_formula("loan_irr"),
          data    = df_irr,
          method  = "anova",
          control = rpart::rpart.control(maxdepth = 5, minbucket = mb_val,
                                          cp = 0.001, xval = 10)
        )
        tree_irr <- .cv_prune_or_keep(tree_irr)

        new_segs <- extract_segments(tree_irr, df_irr, avail_vars,
                                     "irr", portfolio_bad, portfolio_irr)
        for (k in seq_along(new_segs)) new_segs[[k]]$source_filter <- slice_label
        segments <- c(segments, new_segs)
      }, error = function(e) {
        message("[scan_portfolio] IRR CART slice '", slice_label,
                "' failed: ", e$message)
        NULL
      })
    }

    # Progress callback
    if (!is.null(progress_fn)) {
      tryCatch(progress_fn(si, n_slices, slice_label), error = function(e) NULL)
    }
  }

  # ── Rank and deduplicate ──
  if (length(segments) > 0) {
    scores <- vapply(segments, function(s) s$action_score, numeric(1))
    segments <- segments[order(scores, decreasing = TRUE)]

    # Pre-trim to top 50 before expensive dedup
    segments <- head(segments, 50)

    # Jaccard dedup: remove segments with > jaccard_threshold loan overlap
    keep <- rep(TRUE, length(segments))
    for (i in seq_along(segments)) {
      if (!keep[i]) next
      ids_i <- segments[[i]]$loan_ids
      for (j in seq_len(i - 1)) {
        if (!keep[j]) next
        common <- length(intersect(ids_i, segments[[j]]$loan_ids))
        total  <- length(ids_i) + length(segments[[j]]$loan_ids) - common
        if (total > 0 && common / total > jaccard_threshold) { keep[i] <- FALSE; break }
      }
    }
    segments <- segments[keep]
  }

  # ── Anchor FICO filter: remove high-FICO IRR segments ──
  if (!is.null(anchor_fico_max)) {
    segments <- Filter(function(seg) {
      if (!isTRUE(seg$source == "irr")) return(TRUE)  # Keep delinquency segments
      seg_fico <- df_issued$fico_score[df_issued$loan_id %in% seg$loan_ids]
      median_fico <- median(seg_fico, na.rm = TRUE)
      is.na(median_fico) || median_fico < anchor_fico_max
    }, segments)
  }

  # Take top N
  top_segments <- head(segments, max_segments)

  # ── Enrich with projected bad rate and pooled XIRR ──
  portfolio_proj <- portfolio_bad
  if (!is.null(perf_tape)) {
    portfolio_proj <- tryCatch({
      proj <- compute_projected_bad_rate(
        as.character(df_issued$loan_id), perf_tape, df_issued)
      if (!is.null(proj) && nrow(proj) > 0)
        mean(proj$projected_lifetime_prob, na.rm = TRUE)
      else portfolio_bad
    }, error = function(e) {
      message("[scan_portfolio] portfolio_proj calc failed: ", e$message)
      portfolio_bad
    })

    for (si in seq_along(top_segments)) {
      # Projected bad rate per segment
      seg_proj <- tryCatch({
        compute_segment_projected_bad_rate(
          as.character(top_segments[[si]]$loan_ids), perf_tape, df_issued)
      }, error = function(e) NULL)
      if (!is.null(seg_proj) && !is.na(seg_proj$segment_mean_projected)) {
        top_segments[[si]]$projected_bad_rate <- seg_proj$segment_mean_projected
      } else {
        top_segments[[si]]$projected_bad_rate <- top_segments[[si]]$bad_rate
      }

      # Pooled XIRR (replaces biased mean-of-per-loan IRR).
      # Tiny-n guard: with <50 loans the pooled XIRR is dominated by a handful
      # of cashflows and can produce wildly negative/positive readings
      # (e.g. −25% on 45 loans) that mislead reviewers. Flag as NA so the UI
      # can suppress or annotate rather than display a spurious IRR.
      seg_n <- length(top_segments[[si]]$loan_ids)
      top_segments[[si]]$irr_tiny_n <- seg_n < 50
      if (seg_n >= 50) {
        seg_pool_irr <- tryCatch({
          compute_portfolio_xirr_v2(
            perf_tape, loan_ids = as.character(top_segments[[si]]$loan_ids))
        }, error = function(e) NA_real_)
        if (!is.na(seg_pool_irr)) {
          top_segments[[si]]$mean_irr <- seg_pool_irr
        }
      } else {
        top_segments[[si]]$mean_irr <- NA_real_
      }

      # Projected lift: distinguishes from observed lift so downstream
      # UIs can show the right number for the right decision context.
      if (!is.na(portfolio_proj) && portfolio_proj > 0) {
        top_segments[[si]]$projected_lift <- round(
          top_segments[[si]]$projected_bad_rate / portfolio_proj, 2)
      } else {
        top_segments[[si]]$projected_lift <- NA_real_
      }
    }

    # Upgrade portfolio IRR to pooled XIRR
    portfolio_irr_pooled <- tryCatch(
      compute_portfolio_xirr_v2(
        perf_tape, loan_ids = as.character(df_issued$loan_id)),
      error = function(e) NA_real_)
    if (!is.na(portfolio_irr_pooled)) portfolio_irr <- portfolio_irr_pooled
  } else {
    for (si in seq_along(top_segments)) {
      top_segments[[si]]$projected_bad_rate <- top_segments[[si]]$bad_rate
    }
  }

  # ── Re-score delinquency segments by projected bad rate ──
  for (si in seq_along(top_segments)) {
    s <- top_segments[[si]]
    if (s$source == "delinquency") {
      effective_bad <- s$projected_bad_rate
      top_segments[[si]]$action_score <- (effective_bad - portfolio_proj) * sqrt(s$n)
    }
  }
  # Re-sort by updated action scores
  if (length(top_segments) > 0) {
    scores <- vapply(top_segments, function(s) s$action_score, numeric(1))
    top_segments <- top_segments[order(scores, decreasing = TRUE)]
  }

  # ── Recommended demo segment ──
  # Prefer: highest action_score with >50% Low grade + N_HARD_INQ_6M involvement
  recommended <- NULL
  inq_in_importance <- "N_HARD_INQ_6M" %in% names(all_var_importance)
  for (i in seq_along(top_segments)) {
    s <- top_segments[[i]]
    seg_loans <- df_issued[df_issued$loan_id %in% s$loan_ids, ]
    low_pct <- mean(seg_loans$risk_grade == "Low", na.rm = TRUE)
    has_inq <- grepl("N_HARD_INQ_6M", s$rule %||% "")
    if (low_pct > 0.5 && (has_inq || inq_in_importance)) {
      recommended <- i
      break
    }
  }

  # Sort variable importance descending
  all_var_importance <- sort(all_var_importance, decreasing = TRUE)

  list(
    segments                 = top_segments,
    portfolio_bad            = portfolio_bad,
    portfolio_proj           = portfolio_proj,
    portfolio_irr            = portfolio_irr,
    variable_importance      = all_var_importance,
    elapsed_seconds          = as.numeric(Sys.time() - t0, units = "secs"),
    n_slices_scanned         = n_slices,
    recommended_demo_segment = recommended
  )
}
