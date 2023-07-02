sensitivity_analysis <- function() {

  # Make the RMSE and Pearson's R tables
  initialize(FINAL_PRIOR_TYPE, FALSE)
  means_final <- (
    load_model(FINAL_MODEL) |>
    brms::posterior_epred(ndraws = 4000) |>
    colMeans() |>
    x => x / MODEL_DATA[["size"]]
  )

  purrr::map(
    PRIOR_TYPE_LIST,
    function (prior_type) {
      if (prior_type == FINAL_PRIOR_TYPE) {
        return()
      }

      initialize(prior_type, FALSE)

      means <- (
        load_model(FINAL_MODEL) |>
        brms::posterior_epred(ndraws = 4000) |>
        colMeans() |>
        x => x / MODEL_DATA[["size"]]
      )

      tibble::tibble(
        prior_type = prior_type,
        rmse = sqrt(mean((means_final - means)^2)),
        pearson_r = cor(means_final, means)
      )
    }
  ) |>
  purrr::list_rbind() |>
  save_csv(file.path(OUTPUT_SENSITIVITY_DIR, "sensitivity_analysis.csv"))
}
