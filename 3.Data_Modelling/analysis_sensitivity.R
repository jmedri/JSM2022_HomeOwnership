sensitivity_analysis <- function() {
  initialize(FINAL_PRIOR_TYPE, FALSE)

  model_post <- load_model(FINAL_MODEL)

  initialize(FINAL_PRIOR_TYPE, TRUE)

  model_prior <- load_model(FINAL_MODEL)

  
}