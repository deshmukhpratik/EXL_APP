# Engine: Curves
# Reference product curve sourcing for products with insufficient data.
#
# Extends the curve fallback ladder:
#   1. Same product, same vintage -> empirical (build_complete_curves)
#   2. Same product, older vintage -> inherit + overlay (build_complete_curves)
#   3. Different product, grade-matched -> donor curves + auto-stress (THIS FILE)
#
# Functions: get_donor_product, get_donor_stress, select_donor_curves,
#            resolve_curves_with_donor_fallback, enhance_tape_curves_with_donors

# ---------------------------------------------------------------------------
# Donor product lookup
# ---------------------------------------------------------------------------

#' Look up the donor product for a target product
#'
#' Reads the donor_product mapping from consultant_config (originally parsed
#' from consultant_assumptions.csv rows with parameter=="donor_product").
#'
#' @param target_product Character, e.g. "Top-Up"
#' @param consultant_config List with $donor_product named character vector
#' @return Character scalar (donor product name), or NULL if no mapping exists
get_donor_product <- function(target_product, consultant_config) {
  if (is.null(consultant_config) ||
      is.null(consultant_config$donor_product) ||
      length(consultant_config$donor_product) == 0) {
    return(NULL)
  }
  donor <- consultant_config$donor_product[target_product]
  if (is.na(donor) || is.null(donor)) return(NULL)
  unname(donor)
}

#' Look up the auto-stress multiplier for a target product
#'
#' Reads the donor_stress mapping from consultant_config (originally parsed
#' from consultant_assumptions.csv rows with parameter=="donor_stress").
#'
#' @param target_product Character, e.g. "Top-Up"
#' @param consultant_config List with $donor_stress named numeric vector
#' @return Numeric scalar (stress multiplier, e.g. 1.15), or 1.0 if not configured
get_donor_stress <- function(target_product, consultant_config) {
  if (is.null(consultant_config) ||
      is.null(consultant_config$donor_stress) ||
      length(consultant_config$donor_stress) == 0) {
    return(1.0)
  }
  stress <- consultant_config$donor_stress[target_product]
  if (is.na(stress) || is.null(stress)) return(1.0)
  unname(stress)
}

# ---------------------------------------------------------------------------
# Donor curve selection
# ---------------------------------------------------------------------------

#' Select donor curves for a target product with grade-matching and auto-stress
#'
#' When a product has insufficient MOBs for its own DCOR/DPOR curves, this
#' function finds the donor product (from consultant_config), extracts its
#' curves from the tape, optionally filters to the closest matching term, pools
#' across vintages for robustness, and applies the configured auto-stress
#' multiplier to DCOR rates.
#'
#' @param target_product Character, the product needing curves (e.g. "Top-Up")
#' @param target_term Integer, the loan term in months (e.g. 36)
#' @param target_grade Character or NULL. If provided (e.g. "Med"), used for
#'   grade-filtered curve selection when risk_grade is available in tape_curves.
#'   Currently unused since standard curves are indexed by vintage_quarter, not
#'   grade. Reserved for future grade-stratified curves.
#' @param tape_curves Data frame of DCOR/DPOR curves with columns:
#'   product_type, term_months, vintage_quarter, scenario, mob, dcor_rate,
#'   dpor_rate, curve_source. This is the $curves element from build_perf_tape_v2().
#' @param consultant_config List with $donor_product and $donor_stress mappings
#' @return List with:
#'   - curves: data.frame with mob, dcor_rate, dpor_rate, curve_source columns
#'     (curve_source = "donor" for all rows)
#'   - donor_product: character, the donor product used
#'   - stress_factor: numeric, the stress multiplier applied
#'   - target_product: character, echo of the target product
#'   - target_term: integer, echo of the target term
#'   - match_type: character, one of "exact_term", "closest_term", "pooled_product"
#'   Or NULL if no donor mapping exists or no donor curves are available.
select_donor_curves <- function(target_product, target_term, target_grade = NULL,
                                tape_curves, consultant_config) {
  # 1. Find donor product from config

  donor_product <- get_donor_product(target_product, consultant_config)
  if (is.null(donor_product)) return(NULL)

  # 2. Filter tape_curves to donor product
  donor_all <- tape_curves[tape_curves$product_type == donor_product, , drop = FALSE]
  if (nrow(donor_all) == 0) return(NULL)

  # 3. Get stress factor
  stress_factor <- get_donor_stress(target_product, consultant_config)

  # 4. Try exact term match first
  donor_exact <- donor_all[donor_all$term_months == target_term, , drop = FALSE]
  match_type <- "exact_term"

  if (nrow(donor_exact) == 0) {
    # Try closest available term
    available_terms <- sort(unique(donor_all$term_months))
    if (length(available_terms) > 0) {
      closest_term <- available_terms[which.min(abs(available_terms - target_term))]
      donor_exact <- donor_all[donor_all$term_months == closest_term, , drop = FALSE]
      match_type <- "closest_term"
    }
  }

  if (nrow(donor_exact) == 0) {
    # Use all donor product curves (pooled across terms)
    donor_exact <- donor_all
    match_type <- "pooled_product"
  }

  # 5. Pool across vintages to get a single robust curve
  #    (Averaging across vintages gives the most stable donor curve)
  pooled <- donor_exact %>%
    dplyr::group_by(mob = as.integer(.data$mob)) %>%
    dplyr::summarise(
      dcor_rate = mean(.data$dcor_rate, na.rm = TRUE),
      dpor_rate = mean(.data$dpor_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$mob)

  if (nrow(pooled) == 0) return(NULL)

  # 6. Apply auto-stress to DCOR (delinquency charge-off rate)
  #    Stress only applies to MOB > 0 (MOB 0 is always 0%)
  nonzero_mob <- pooled$mob > 0
  pooled$dcor_rate[nonzero_mob] <- pooled$dcor_rate[nonzero_mob] * stress_factor

  # 7. Mark curve source

  pooled$curve_source <- "donor"

  list(
    curves         = as.data.frame(pooled),
    donor_product  = donor_product,
    stress_factor  = stress_factor,
    target_product = target_product,
    target_term    = target_term,
    match_type     = match_type
  )
}

# ---------------------------------------------------------------------------
# Full fallback resolver
# ---------------------------------------------------------------------------

#' Resolve curves for a loan, falling back to donor product if needed
#'
#' Implements the complete 3-level fallback ladder:
#'   1. Same product + term + vintage (exact match in tape_curves)
#'   2. Same product + term, pooled across vintages
#'   3. Donor product via select_donor_curves()
#'
#' This function does NOT modify tape_curves. It returns a single curve
#' data.frame suitable for project_loan_cashflows().
#'
#' @param product_type Character, the loan's product type
#' @param term_months Integer, the loan's term
#' @param vintage_quarter Character or NULL, the loan's vintage quarter
#' @param grade Character or NULL, the loan's risk grade (for future use)
#' @param tape_curves Data frame of all curves ($curves from build_perf_tape_v2())
#' @param consultant_config List with donor mappings
#' @return List with:
#'   - curves: data.frame with mob, dcor_rate, dpor_rate, curve_source
#'   - source_detail: character description of which fallback was used
#'   Or NULL if no curves found at any level.
resolve_curves_with_donor_fallback <- function(product_type, term_months,
                                                vintage_quarter = NULL,
                                                grade = NULL,
                                                tape_curves,
                                                consultant_config) {
  # Level 1: Exact match (product + term + vintage)
  if (!is.null(vintage_quarter)) {
    exact <- tape_curves[tape_curves$product_type == product_type &
                           tape_curves$term_months == term_months &
                           tape_curves$vintage_quarter == vintage_quarter, , drop = FALSE]
    if (nrow(exact) > 0) {
      return(list(
        curves = exact[, c("mob", "dcor_rate", "dpor_rate", "curve_source"), drop = FALSE],
        source_detail = paste0("empirical: ", product_type, " ", term_months, "mo ",
                               vintage_quarter)
      ))
    }
  }

  # Level 2: Same product + term, pooled across vintages
  same_pt <- tape_curves[tape_curves$product_type == product_type &
                            tape_curves$term_months == term_months, , drop = FALSE]
  if (nrow(same_pt) > 0) {
    pooled <- same_pt %>%
      dplyr::group_by(mob = as.integer(.data$mob)) %>%
      dplyr::summarise(
        dcor_rate = mean(.data$dcor_rate, na.rm = TRUE),
        dpor_rate = mean(.data$dpor_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$mob)
    pooled$curve_source <- "pooled"
    return(list(
      curves = as.data.frame(pooled),
      source_detail = paste0("pooled: ", product_type, " ", term_months, "mo across vintages")
    ))
  }

  # Level 3: Donor product fallback
  donor_result <- select_donor_curves(
    target_product   = product_type,
    target_term      = term_months,
    target_grade     = grade,
    tape_curves      = tape_curves,
    consultant_config = consultant_config
  )
  if (!is.null(donor_result)) {
    return(list(
      curves = donor_result$curves,
      source_detail = paste0("donor: ", donor_result$donor_product, " ",
                             donor_result$target_term, "mo (",
                             donor_result$match_type, ", stress=",
                             donor_result$stress_factor, ")")
    ))
  }

  # No curves found at any level
  NULL
}

# ---------------------------------------------------------------------------
# Tape-level donor curve enrichment (post-processing after tape build)
# ---------------------------------------------------------------------------

#' Enrich a performance tape's curves with donor product curves for thin-data products
#'
#' Called once at startup after the tape is built AND consultant_config is available.
#' For each product with a donor mapping in consultant_config, checks every
#' term x vintage combination. If a target product/term has fewer than 6 own MOBs,
#' the donor product's curves are used to fill in the gaps, with the configured
#' stress multiplier applied to DCOR rates.
#'
#' Safe to call when consultant_config has no donor mappings (returns tape unchanged).
#' Safe to call when tape has no $curves (returns tape unchanged).
#'
#' @param tape List returned by build_perf_tape_v2(), must have $curves data.frame
#' @param consultant_config List with $donor_product (named char vec) and
#'   $donor_stress (named numeric vec)
#' @return The tape list with $curves potentially enriched with donor rows
enhance_tape_curves_with_donors <- function(tape, consultant_config) {
  if (is.null(tape$curves) || nrow(tape$curves) == 0) return(tape)

  # Find products with donor mappings
  donor_map <- consultant_config$donor_product
  if (is.null(donor_map) || length(donor_map) == 0) return(tape)

  min_own_mobs <- 6L  # Threshold: fewer own MOBs triggers donor fallback

  for (target in names(donor_map)) {
    donor <- donor_map[[target]]
    stress <- if (!is.null(consultant_config$donor_stress[[target]])) {
      consultant_config$donor_stress[[target]]
    } else {
      1.0
    }

    # Get target and donor curves from the tape
    target_curves <- tape$curves[tape$curves$product_type == target, , drop = FALSE]
    donor_curves  <- tape$curves[tape$curves$product_type == donor, , drop = FALSE]

    if (nrow(donor_curves) == 0) next  # No donor data either — skip silently

    # Determine which terms to process
    target_terms <- unique(target_curves$term_months)
    if (length(target_terms) == 0) {
      # No curves at all for target — use ALL donor terms
      target_terms <- unique(donor_curves$term_months)
    }

    for (tm in target_terms) {
      target_term_curves <- target_curves[target_curves$term_months == tm, , drop = FALSE]
      n_own_mobs <- length(unique(target_term_curves$mob[target_term_curves$mob > 0]))

      if (n_own_mobs >= min_own_mobs) next  # Sufficient own data — no donor needed

      # Get donor curves for this term
      donor_term <- donor_curves[donor_curves$term_months == tm, , drop = FALSE]

      if (nrow(donor_term) == 0) {
        # Try closest available term from donor
        available_terms <- unique(donor_curves$term_months)
        closest <- available_terms[which.min(abs(available_terms - tm))]
        donor_term <- donor_curves[donor_curves$term_months == closest, , drop = FALSE]
      }

      if (nrow(donor_term) == 0) next

      # Pool donor curves by MOB (average across vintages for robustness)
      pooled <- aggregate(cbind(dcor_rate, dpor_rate) ~ mob,
                          data = donor_term, FUN = mean, na.rm = TRUE)

      # Apply stress to DCOR (not MOB 0, which must stay at 0%)
      pooled$dcor_rate <- ifelse(pooled$mob > 0,
                                  pooled$dcor_rate * stress,
                                  pooled$dcor_rate)

      # Determine target vintages to enrich
      target_vintages <- unique(target_term_curves$vintage_quarter)
      if (length(target_vintages) == 0) target_vintages <- "pooled"

      for (vq in target_vintages) {
        own <- target_term_curves[target_term_curves$vintage_quarter == vq, , drop = FALSE]
        own_mobs <- own$mob[own$mob > 0]

        # Build combined curve: start with donor, overlay own actuals
        combined <- pooled
        combined$product_type    <- target
        combined$term_months     <- tm
        combined$vintage_quarter <- vq
        combined$scenario        <- "base"
        combined$curve_source    <- "donor"

        # Overlay own data where available (own actuals override donor)
        if (nrow(own) > 0) {
          for (m in own_mobs) {
            idx <- combined$mob == m
            if (any(idx)) {
              combined$dcor_rate[idx]   <- own$dcor_rate[own$mob == m][1]
              combined$dpor_rate[idx]   <- own$dpor_rate[own$mob == m][1]
              combined$curve_source[idx] <- "own"
            }
          }
        }

        # Remove existing rows for this target+term+vintage, then append combined
        tape$curves <- tape$curves[!(tape$curves$product_type == target &
                                      tape$curves$term_months == tm &
                                      tape$curves$vintage_quarter == vq), , drop = FALSE]
        tape$curves <- rbind(tape$curves, combined)
      }

      message("  Donor curves: ", target, " term=", tm,
              " using ", donor, " (", stress, "x stress, ",
              n_own_mobs, " own MOBs overlaid)")
    }
  }

  tape
}
