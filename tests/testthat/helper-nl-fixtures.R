# Synthetic loan data for NL compute tests. ~100 rows, all 6 shapes exercised.

.nl_fixture_loans <- function() {
  set.seed(42)
  n <- 100
  df <- data.frame(
    loan_id                       = sprintf("L%04d", 1:n),
    fico_score                    = sample(c(620, 660, 700, 740, 780), n, replace = TRUE),
    fico_score_band               = NA_character_,
    partner_channel               = sample(c("Direct", "Indirect", "Broker", "Digital"), n, replace = TRUE,
                                            prob = c(0.4, 0.3, 0.2, 0.1)),
    risk_grade                    = sample(c("A", "B", "C", "D"), n, replace = TRUE),
    booking_vintage               = sample(c("2025_Q3", "2025_Q4", "2026_Q1"), n, replace = TRUE),
    product_type                  = sample(c("PL-V3", "PL-V4"), n, replace = TRUE),
    borrower_repeat_type          = sample(c("new", "repeat"), n, replace = TRUE),
    test_group                    = sample(c("test", "control"), n, replace = TRUE),
    underwriting_risk_tier        = sample(c("prime", "near-prime", "subprime"), n, replace = TRUE),
    N_HARD_INQ_6M                 = sample(0:6, n, replace = TRUE,
                                            prob = c(0.35, 0.25, 0.15, 0.1, 0.08, 0.05, 0.02)),
    performance_delinquency_flag  = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.92, 0.08)),
    offered_decision_flag         = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.3, 0.7)),
    issued_flag                   = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.5, 0.5)),
    offer_accepted_flag           = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.6, 0.4)),
    stringsAsFactors              = FALSE
  )
  df$fico_score_band <- cut(df$fico_score,
    breaks = c(-Inf, 660, 700, 740, Inf),
    labels = c("620-660", "661-700", "701-740", "741-780"),
    right = TRUE) |> as.character()
  df
}

# Inject channel-bad-rate signal so ranking tests are deterministic.
# Direct always gets the highest bad rate (0.20), Digital the lowest (0.02).
.nl_fixture_loans_with_channel_signal <- function() {
  df <- .nl_fixture_loans()
  df$performance_delinquency_flag <- ifelse(df$partner_channel == "Direct",
                                            rbinom(nrow(df), 1, 0.20),
                                            ifelse(df$partner_channel == "Digital",
                                                   rbinom(nrow(df), 1, 0.02),
                                                   rbinom(nrow(df), 1, 0.08)))
  df
}
