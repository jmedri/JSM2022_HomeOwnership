PRIOR_TYPE_LIST <- c("1", "2", "3", "4") # All possible prior settings
FINAL_PRIOR_TYPE <- "1" # The prior setting used in the final model

if (PRIOR_TYPE == "1") {
  # Weakly informative priors (the final prior used).
  # The theta intercept is set to the overall home ownership mean (log-odds scale).
  # The phi intercept is set to 3 (log-scale).

  PRIOR_THETA_INTERCEPT <- "normal(0.60, 0.25)"
  PRIOR_THETA_B <- "normal(0, 0.25)"
  PRIOR_PHI_INTERCEPT <- "normal(3.25, 0.25)"
  PRIOR_PHI_B <- "normal(0, 0.25)"
} else if (PRIOR_TYPE == "2") {
  # Weakly informative priors (the lower precision prior).
  # The theta intercept is set to the overall home ownership mean (log-odds scale).
  # The phi intercept is set to 1 (log-scale).

  PRIOR_THETA_INTERCEPT <- "normal(0.60, 0.25)" # To mimic overall home ownership mean (log-odds scale)
  PRIOR_THETA_B <- "normal(0, 0.25)"
  PRIOR_PHI_INTERCEPT <- "normal(1.25, 0.25)"
  PRIOR_PHI_B <- "normal(0, 0.25)"
} else if (PRIOR_TYPE == "3") {
  # Weakly informative priors (the higher precision prior).
  # The theta intercept is set to the overall home ownership mean (log-odds scale).
  # The phi intercept is set to 5 (log-scale).

  PRIOR_THETA_INTERCEPT <- "normal(0.60, 0.25)"
  PRIOR_THETA_B <- "normal(0, 0.25)"
  PRIOR_PHI_INTERCEPT <- "normal(5.25, 0.25)"
  PRIOR_PHI_B <- "normal(0, 0.25)"
} else if (PRIOR_TYPE == "4") {
  # Non-informative priors.
  # The theta intercept is set to 0 (log-odds scale).
  # The phi intercept is set to 0 (log-scale).

  PRIOR_THETA_INTERCEPT <- "normal(0, 100)"
  PRIOR_THETA_B <- "normal(0, 100)"
  PRIOR_PHI_INTERCEPT <- "normal(0, 100)"
  PRIOR_PHI_B <- "normal(0, 100)"
} else {
  stop("Invalid prior type: ", PRIOR_TYPE)
}

PRIOR_BETA_BIN_1 <- (
  # This verision for there is a phi covariate.
  brms::set_prior(PRIOR_THETA_INTERCEPT, class = "Intercept") +
    brms::set_prior(PRIOR_THETA_B, class = "b") +
    brms::set_prior(PRIOR_PHI_INTERCEPT, class = "Intercept", dpar = "phi") +
    brms::set_prior(PRIOR_PHI_B, class = "b", dpar = "phi")
)
PRIOR_BETA_BIN_2 <- (
  # This version for when there is no phi covariate.
  brms::set_prior(PRIOR_THETA_INTERCEPT, class = "Intercept") +
    brms::set_prior(PRIOR_THETA_B, class = "b") +
    brms::set_prior(PRIOR_PHI_INTERCEPT, class = "Intercept", dpar = "phi")
)

RACES_MODEL <- c(
  "WhiteNH",
  "Hispanic",
  "Black",
  "Asian"
)

MODEL_INFO <- list(
  # The exploratory binomial model
  "0" = list(
    formula = (
      cbind(hom.own.count, size - hom.own.count) ~
        state +
        race +
        edu.hs +
        emp.ue +
        inc.inc.trans
    )
  ),

  # The base model
  "1" = list(
    formula = brms::bf(
      hom.own.count | trials(size) ~
        state +
        race +
        edu.hs +
        emp.ue +
        inc.inc.trans,
      phi ~ hom.tot.log
    ),
    prior = PRIOR_BETA_BIN_1
  ),

  # Model 1 with race interactions
  "2" = list(
    formula = brms::bf(
      hom.own.count | trials(size) ~
        state +
        race +
        edu.hs : race +
        emp.ue : race +
        inc.inc.trans : race,
      phi ~ hom.tot.log
    ),
    prior = PRIOR_BETA_BIN_1
  ),

  # Model 2 with emp.ue removed because it was not significant.
  # However, you have to see the model effects table or Model 1 summary to see it.
  # For some reason the WhiteNH coefficient is significant and the other groups are
  # significant in the opposite direction making the sum not significant.
  "3" = list(
    formula = brms::bf(
      hom.own.count | trials(size) ~
        state +
        race +
        edu.hs : race +
        inc.inc.trans : race,
      phi ~ hom.tot.log
    ),
    prior = PRIOR_BETA_BIN_1
  ),

  # Model 3 with edu.hs removed because it had an inexplicable
  # negative association with home ownership.
  "4" = list(
    formula = brms::bf(
      hom.own.count | trials(size) ~
        state +
        race +
        inc.inc.trans : race,
      phi ~ hom.tot.log
    ),
    prior = PRIOR_BETA_BIN_1
  ),

  # Model 4 with inc.inc.trans removed to test for significance.
  "5" = list(
    formula = brms::bf(
      hom.own.count | trials(size) ~
        state +
        race,
      phi ~ hom.tot.log
    ),
    prior = PRIOR_BETA_BIN_1
  ),

  # Model 4 with state removed to test for significance.
  "6" = list(
    formula = brms::bf(
      hom.own.count | trials(size) ~
        race +
        inc.inc.trans : race,
      phi ~ hom.tot.log
    ),
    prior = PRIOR_BETA_BIN_1
  ),

  # Model 4 with no interaction to test for significance.
  "7" = list(
    formula = brms::bf(
      hom.own.count | trials(size) ~
        state +
        race +
        inc.inc.trans,
      phi ~ hom.tot.log
    ),
    prior = PRIOR_BETA_BIN_1
  ),

  # Model 4 with hom.tot.log removed to test for significance.
  "8" = list(
    formula = brms::bf(
      hom.own.count | trials(size) ~
        state +
        race +
        inc.inc.trans : race,
      phi ~ 1
    ),
    prior = PRIOR_BETA_BIN_2
  )
)

# Only do the full analysis with the final prior.
# Otherwise do the analysis with the first two models.
if (PRIOR_TYPE == FINAL_PRIOR_TYPE) {
  # Omit Model 0 (binomial model) since it is not a Bayesian model.
  MODEL_NAMES <- names(MODEL_INFO)[2:length(MODEL_INFO)] 
} else {
  MODEL_NAMES <- FINAL_MODEL
}

# Final model information.
FINAL_MODEL <- "4"
FINAL_MODEL_MCMC_ITERS <- 8000
