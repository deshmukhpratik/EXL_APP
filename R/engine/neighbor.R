# Engine: Neighbor Band
# API contract for swap-in neighbor band analysis.
#
# These functions currently live in irr_cashflow_engine.R (sourced via 01_packages.R).
# They will be moved here when IRR engines are fully migrated to engine/.
# For now this file documents the API contract.
#
# Functions available from irr_cashflow_engine.R:
#
#   get_neighbor_metrics(tape, credit_data, decline_reason, threshold, ...)
#     - Identifies issued loans near a decline boundary (neighbor band)
#     - Returns: list(n_loans, observed_irr, delinq_rate, avg_months_to_default,
#                     avg_lgd, avg_amount, avg_apr, avg_term, neighbor_loan_ids)
#     - Neighbor band definitions:
#       FICO:        issued with FICO in [threshold, threshold+40)
#       Thin file:   issued with file age in (threshold, threshold+18]
#       Delinquency: issued with exactly threshold delinquent accounts
#       DTI:         issued with DTI in (threshold-0.10, threshold]
#
#   build_synthetic_cashflows(n_loans, avg_amount, apr, term,
#                              delinq_rate, avg_months_to_default,
#                              observation_months, lgd, origination_date)
#     - Projects cashflows for hypothetical swap-in cohort
#     - Uses constant hazard default model: lambda = -ln(1-delinq_rate)/avg_mtd
#     - Returns: data.frame(date, cashflow, performing_cf, loss_cf)
#
#   compute_synthetic_irr(...)
#     - Wrapper: builds synthetic cashflows then computes XIRR
#     - Returns: scalar IRR or NA_real_
