MODEL_FILES <- list(
  "mle_gaus" = file.path(INPUT_DIR, "model_mle_gaus.rds"),
  "mle_bin" = file.path(INPUT_DIR, "model_mle_bin.rds"),
  "bay_gaus2" = file.path(INPUT_DIR, "model_bay_gaus2.rds"),
  "bay_betabin" = file.path(INPUT_DIR, "model_bay_betabin.rds")
)

RACES_OLD <- c("White", "Black", "Asian", "Other")
RACE_COLORS_OLD <- c(
  "White" = "#e41a1c",
  "Black" = "#377eb8",
  "Asian" = "#4daf4a",
  "Other" = "#984ea3"
)

prepare_model_data_old <- function(data) {
  data |>
  dplyr::filter(race %in% RACES_OLD) |>
  dplyr::mutate(
    inc.inc.trans = inc.inc / 100000,
    val.tax.trans = val.tax / 10000,
    val.hom.trans = val.hom / 1000000,
    val.inc.trans = val.inc / 100000,
    year.trans = year - min(YEARS),
    hom.tot.log = log(hom.tot),
    weights = hom.tot
  )
}

plot_prediction_old <- function(
  model,
  data,
  model_which,
  new_data,
  separate_y = FALSE,
  x_lim_full = FALSE,
  fill_alpha = 0.3,
  title = NULL,
  show_subtitle = TRUE,
  base_size = 20,
  interval_size = 5
) {
  #' @title plot_prediction_old()
  #' @description Main function for plotting predicted homeownsership
  #' @note This function needs library(ggdist) and library(brms)!
  #' @param model The fitted model (e.g. output of lm function)
  #' @param data Data model was fitted on
  #' @param model_which One of the strings "mle_gaus", "mle_bin", "bay_gaus2", "bay_betabin".
  #' @param new_data list of new covariates for prediction. Must have keys "state", "race", "year"
  #' and can optionally have any other keys including:
  #' "edu.hs", "emp.ue", "inc.inc", "val.hom", "pop.share.ratio", "hom.tot".
  #' If any of these are missing the default values will be taken from the real data.
  #' If "state" = "All" or "year" = "All" the average prediction over all states or years
  #' is given. "race" must be a string vector which is a subset of RACES_OLD. Only the
  #' races in "race" will be displayed.
  #' @param separate_y If TRUE show predicted distribution for each race on a separate baseline.
  #' If FALSE overlay them on the same baseline.
  #' @param x_lim_full If TRUE show the entire x-axis from 0 to 1. If FALSE show only the range of
  #' of the plotted values.
  #' @param fill_alpha Alpha value to fill in the density.
  #' @param title Title of plot. If NULL not title is plotted.
  #' @param show_subtitle If TRUE show a subtitle explaining the plotted point and interval within the density.
  #' @param base_size The base size of the ggplot2 theme.
  #' @param interval_size Thickness of the part showing the median and .95 interval.
  #' @return ggplot2 object

  NUM_DRAWS <- 1000
  ALL_LIST <- list(
    race = RACES_OLD,
    state = STATES,
    year = YEARS
  )
  for (var_name in c("race", "state", "year")) {
    if (
      !(var_name %in% names(new_data)) ||
      (
        (length(new_data[[var_name]]) == 1) &&
        (new_data[[var_name]] == "All")
      )
    ) {
      new_data[[var_name]] <- ALL_LIST[[var_name]]
    }
  }

  new_data <- do.call(tidyr::expand_grid, new_data)

  new_data <- (
    dplyr::inner_join(
      new_data,
      data[
        c(
          "year",
          "state",
          "race",
          setdiff(colnames(data), colnames(new_data))
        )
      ],
      by = c("year", "state", "race")
    ) |>
    prepare_model_data_old()
  )

  if (model_which %in% c("mle_gaus", "mle_bin")) {
    pred <- predict(model, newdata = new_data, se.fit = TRUE)
    pred_draws <- do.call(
      cbind,
      purrr::map2(
        pred[["fit"]],
        pred[["se.fit"]],
        function(fit, se_fit) plogis(rnorm(NUM_DRAWS, fit, se_fit))
      )
    )
  } else if (model_which == "mle_beta") {
    pred_mean <- predict(model, newdata = new_data, type = "response")
    pred_precision <- predict(model, newdata = new_data, type = "precision")
    pred_draws <- do.call(
      cbind,
      purrr::map2(
        pred_mean,
        pred_precision,
        function(mean, precision) rbeta(
          NUM_DRAWS,
          mean * precision,
          (1 - mean) * precision
        )
      )
    )
  } else if (stringr::str_starts(model_which, "bay_")) {
    pred_draws <- brms::posterior_predict(model, newdata = new_data, ndraws = NUM_DRAWS)
    if (model_which %in% c("bay_gaus", "bay_gaus2")) {
      pred_draws <- plogis(pred_draws)
    } else if (model_which %in% c("bay_bin", "bay_betabin")) {
      pred_draws <- sweep(pred_draws, 2, new_data[["hom.tot"]], `/`)
    }
  } else {
    stop(stringr::str_c("Unhandled model: ", model_which))
  }

  pred_draws <- purrr::map_dfr(
    unique(new_data[["race"]]),
    function(race) {
      pred_draws_race <- pred_draws[, new_data[["race"]] == race, drop = FALSE]
      hom_tot_race <- new_data[new_data[["race"]] == race, "hom.tot", drop = TRUE]
      pred_draws_race <- sweep(pred_draws_race, 2, hom_tot_race, `*`)
      pred_draws_race <- rowSums(pred_draws_race) / sum(hom_tot_race)
      tibble::tibble(
        value = pred_draws_race,
        race = factor(race, levels = c("White", "Black", "Asian", "Other"))
      )
    }
  )

  if (separate_y) {
    ggplot_out <- (
      ggplot2::ggplot(
        pred_draws,
        ggplot2::aes(x = value * 100, y = race, fill = race, color = race)
      ) +
      ggplot2::ylab("Race")
    )
  } else {
    ggplot_out <- (
      ggplot2::ggplot(
        pred_draws,
        ggplot2::aes(x = value * 100, fill = race, color = race)
      ) +
      ggplot2::ylab("Density")
    )
  }

  ggplot_out <- (
    ggplot_out +
    ggdist::stat_halfeye(alpha = fill_alpha, .width = .95,
      interval_size = interval_size,
      interval_alpha = 1,
      point_alpha = 1
    ) +
    ggplot2::xlab("Home Ownership Rate (%)") +
    ggplot2::theme_classic(base_size = base_size)
  )

  if (!separate_y) {
    ggplot_out <- ggplot_out + ggplot2::expand_limits(y = 0)
  }

  if (!is.null(title)) {
    ggplot_out <- ggplot_out + ggplot2::labs(title = title)
  }

  if (show_subtitle) {
    ggplot_out <- (
      ggplot_out +
      ggplot2::labs(
        subtitle = stringr::str_c("Density, median, and 95% interval")
      )
    )
  }

  if (x_lim_full) {
    ggplot_out <- ggplot_out + ggplot2::coord_cartesian(xlim = c(0, 1))
  }

  # Colors
  ggplot_out <- (
    ggplot_out +
    ggplot2::scale_discrete_manual(
      values = RACE_COLORS_OLD,
      aesthetics = c("fill", "color")
    )
  )

  ggplot_out
}

plot_prediction_app_old <- function(
  model_which,
  race = RACES_OLD,
  state = "All",
  year = "All",
  edu.hs = NULL,
  emp.ue = NULL,
  inc.inc = NULL,
  val.hom = NULL,
  pop.share.ratio = NULL,
  separate_y = FALSE,
  x_lim_full = FALSE,
  fill_alpha = 0.3,
  title = NULL,
  show_subtitle = TRUE,
  base_size = 20
) {
  #' @title plot_prediction_app_old()
  #' @description Helper function for app interface to plot predictions
  #' @param model_which One of the strings "mle_gaus", "mle_bin", "bay_gaus", "bay_betabin".
  #' @param race Races to predict and plot. If "All" plots all races, else must be a subset
  #' of RACES_OLD.
  #' @param state Which state to predict. If "All" will show the predicition for whole US.
  #' @param year Which year to predict. If "All" will show the prediciton for all years.
  #' @param edu.hs Covariate value. If omitted gets default value from data.
  #' @param emp.ue Covariate value. If NULL gets default value from data.
  #' @param inc.inc Covariate value. If NULL gets default value from data.
  #' @param val.hom Covariate value. If NULL gets default value from data.
  #' @param pop.share.ratio Covariate value. If NULL gets default value from data.
  #' @param separate_y Refer to plot_prediction_old() documentation.
  #' @param x_lim_full Refer to plot_prediction_old() documentation.
  #' @param fill_alpha Refer to plot_prediction_old() documentation.
  #' @param title Refer to plot_prediction_old() documentation.
  #' @param show_subtitle Refer to plot_prediction_old() documentation.
  #' @param interval_size Refer to plot_prediction_old() documentation.
  #' @param base_size Refer to plot_prediction_old() documentation.

  new_data <- (
    list(
      race = race,
      state = state,
      year = year,
      edu.hs = edu.hs / 100,
      emp.ue = emp.ue / 100,
      inc.inc = inc.inc,
      val.hom = val.hom,
      pop.share.ratio = pop.share.ratio / 100
    ) |>
    purrr::discard(\(x) length(x) == 0)
  )

  model <- readRDS(MODEL_FILES[[model_which]])

  plot_prediction_old(
    model = model,
    data = STATE_DATA_OLD,
    model_which = model_which,
    new_data = new_data,
    separate_y = separate_y,
    x_lim_full = x_lim_full,
    fill_alpha = fill_alpha,
    title = title,
    show_subtitle = show_subtitle,
    base_size = base_size
  )
}

load_state_data_old <- function() {
  STATE_DATA_OLD <<- readr::read_csv(file.path(INPUT_DIR, "state_data_old.csv"))
}
