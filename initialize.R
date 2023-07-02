initialize <- function(
  PRIOR_TYPE = "1",
  SAMPLE_PRIOR = FALSE,
  MCMC_ITERS = 2000
) {
  #' @title Initialize the R environment for the analysis.
  #' @description This function initializes the R environment for the analysis.
  #' @param PRIOR_TYPE The type of prior to use for the analysis. 
  #'  "1" for noninformative, "2" for weakly informative, "3" for strongly informative.
  #' @param SAMPLE_PRIOR Whether to sample from the prior (TRUE) or posterior (FALSE).
  #' @param MCMC_ITERS The number of MCMC iterations to run.
  #' @return NULL
  
  # Do global settings
  assign("PRIOR_TYPE", PRIOR_TYPE, envir = .GlobalEnv)
  assign("SAMPLE_PRIOR", SAMPLE_PRIOR, envir = .GlobalEnv)
  assign("MCMC_ITERS", MCMC_ITERS, envir = .GlobalEnv)

  # Load libraries and set globals variables/functions.
  source("do_load_libraries.R")
  source("directories.R")
  source("constants_common.R")
  source("constants_model.R")
  source("constants_app.R")
  source("utils_common.R")
  source("utils_shape_data.R")
  source("utils_census_data.R")
  source("utils_app.R")
  source("analysis_exploratory.R")
  source("analysis_exploratory_app.R")
  source("analysis_model.R")
  source("analysis_sensitivity.R")
  source("plot_prediction_old.R")

  # Load data
  load_shape_data()
  load_census_data()
  load_model_data()
  load_app_data()
  load_state_data_old()

  options(scipen = 999) # Turn off scientific notation
}
