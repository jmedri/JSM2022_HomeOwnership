OUTPUT_DIR <- file.path(
  "data",
  paste0("output_", PRIOR_TYPE, if (SAMPLE_PRIOR) "_prior" else "_posterior")
)
OUTPUT_EXPLORATORY_DIR <- file.path("data", "output_exploratory")
OUTPUT_SENSITIVITY_DIR <- file.path("data", "output_sensitivity")
INPUT_DIR <- file.path("data", "input")
INPUT_PROCESSED_DIR <- file.path("data", "input_processed")
