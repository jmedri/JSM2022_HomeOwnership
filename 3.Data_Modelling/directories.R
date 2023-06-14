OUTPUT_DIR <- file.path(
  "3.Data_Modelling_Output",
  paste0("output_", PRIOR_TYPE, if (SAMPLE_PRIOR) "_prior" else "_posterior")
)
OUTPUT_EXPLORATORY_DIR <- file.path("3.Data_Modelling_Output", "output_exploratory")
INPUT_DIR <- file.path("3.Data_Modelling_Output", "input")
INPUT_PROCESSED_DIR <- file.path("3.Data_Modelling_Output", "input_processed")
