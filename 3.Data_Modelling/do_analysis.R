source("3.Data_Modelling/initialize.R")

initialize()
do_all_exploratory_analyses()

for (prior_type in PRIOR_TYPE_LIST) {
  for (sample_prior in c(TRUE, FALSE)) {
    initialize(PRIOR_TYPE = prior_type, SAMPLE_PRIOR = sample_prior)
    do_all_compute_models()
    do_all_model_analyses()
  }
}

sensitivity_analysis()
