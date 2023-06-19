library(tidyverse)
library(brms)
library(ggdist)

# Please fix the directory!
setwd("C:/Users/jhona/OneDrive/Documents/Classes/USF/ASA Data Challenge 2022/04. R Scripts/plot_prediction_app")

MODEL_FILES <- list(
  "mle_gaus" = file.path("model_mle_gaus.RDS"),
  "mle_bin" = file.path("model_mle_bin.RDS"),
  "bay_gaus" = file.path("model_bay_gaus.RDS"),
  "bay_betabin" = file.path("model_bay_betabin.RDS")
)

YEARS <- 2015:2020

RACES_MODEL <- c("White", "Black", "Asian", "Other")

STATES <- c(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "West Virginia",
  "Wisconsin",
  "Wyoming"
)

prepare_model_data <- function(data) {
  data |>
  dplyr::filter(race %in% RACES_MODEL) |>
  dplyr::mutate(
    inc.inc.trans = inc.inc / 100000,
    val.tax.trans = val.tax / 10000,
    val.hom.trans = val.hom / 1000000,
    val.inc.trans = val.inc / 100000,
    year.trans = year - min(YEARS),
    hom.tot.log = log(hom.tot)
  )
}

# plot_prediction: main function for plotting predicted homeownsership
# Note: this function needs library(ggdist) and library(brms)!
# Parameters:
#     model: the fitted model (e.g. output of lm function)
#     data: data model was fitted on
#     model_which: one of the strings "mle_gaus", "mle_bin", "bay_gaus", "bay_betabin".
#     new_data: list of new covariates for prediction. Must have keys "state", "race", "year"
#               and can optionally have any other keys including:
#               "edu.hs", "emp.ue", "inc.inc", "val.hom", "pop.share.ratio", "hom.tot".
#               If any of these are missing the default values will be taken from the real data.
#               If "state" = "All" or "year" = "All" the average prediction over all states or years
#               is given. "race" must be a string vector which is a subset of RACES_MODEL. Only the
#               races in "race" will be displayed.
#     separate_y: If TRUE show predicted distribution for each race on a separate baseline.
#                 If FALSE overlay them on the same baseline.
#     x_lim_full: If TRUE show the entire x-axis from 0 to 1. If FALSE show only the range of
#                 of the plotted values.
#     fill_alpha: Alpha value to fill in the density.
#     title: title of plot. If NULL not title is plotted.
#     show_subtitle: If TRUE show a subtitle explaining the plotted point and interval within the density.
#     base_size: The base size of the ggplot2 theme.
#     interval_size: thickness of the part showing the median and .95 interval
plot_prediction <- function(
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
  NUM_DRAWS <- 1000
  ALL_LIST <- list(
    race = RACES_MODEL,
    state = STATES,
    year = YEARS
  )
  for (var_name in c("race", "state", "year")) {
    if (
      !(var_name %in% names(new_data)) ||
      (new_data[[var_name]] == "All")
    ) {
      new_data[[var_name]] <- ALL_LIST[[var_name]]
    }
  }

  new_data <- do.call(tidyr::expand_grid, new_data)

  new_data <- (
    dplyr::inner_join(
      new_data,
      data[c(
        "year",
        "state",
        "race",
        setdiff(colnames(data),colnames(new_data))
      )],
      by = c("year", "state", "race")
    ) |>
    dplyr::mutate(hom.tot.log = log(hom.tot)) |>
    prepare_model_data()
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
    pred_draws <- posterior_epred(model, newdata = new_data, ndraws = NUM_DRAWS)
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
        aes(x = value, y = race, fill = race, color = race)
      ) +
      ggplot2::ylab("race")
    )
  } else {
    ggplot_out <- (
      ggplot2::ggplot(
        pred_draws,
        aes(x = value, fill = race, color = race)
      ) +
      ggplot2::ylab("density")
    )
  }
  ggplot_out <- (
    ggplot_out +
    ggdist::stat_halfeye(alpha = fill_alpha, .width = .95,
      interval_size = interval_size,
      interval_alpha = 1,
      point_alpha = 1
    ) +
    ggplot2::xlab("hom.own") +
    ggplot2::theme_classic(base_size = base_size)
  )

  if (!separate_y) {
    ggplot_out <- ggplot_out + ggplot2::expand_limits(y = 0)
  }

  if (!is.null(title)) {
    ggplot_out <- ggplot_out + ggplot2::labs(title = title)
  }

  if (show_subtitle) {
    ggplot_out <- ggplot_out + ggplot2::labs(
      subtitle = stringr::str_c("with median point and 0.95 interval")
    )
  }

  if (x_lim_full) {
    ggplot_out <- ggplot_out + ggplot2::coord_cartesian(xlim = c(0, 1))
  }
  ggplot_out
}

# plot_prediction_app: helper function for app interface to plot predictions
# Parameters:
#     model_which: one of the strings "mle_gaus", "mle_bin", "bay_gaus", "bay_betabin".
#     race: races to predict and plot. If "All" plots all races, else must be a subset
#           of RACES_MODEL.
#     state: which state to predict. If "All" will show the predicition for whole US.
#     year: which year to predict. If "All" will show the prediciton for all years.
#     edu.hs: covariate value. If omitted gets default value from data.
#     emp.ue: covariate value. If NULL gets default value from data.
#     inc.inc: covariate value. If NULL gets default value from data.
#     val.hom: covariate value. If NULL gets default value from data.
#     pop.share.ratio: covariate value. If NULL gets default value from data.
#     separate_y: refer to plot_prediction() documentation.
#     x_lim_full: refer to plot_prediction() documentation.
#     fill_alpha: refer to plot_prediction() documentation.
#     title: refer to plot_prediction() documentation.
#     show_subtitle: refer to plot_prediction() documentation.
#     interval_size: refer to plot_prediction() documentation.
plot_prediction_app <- function(
  model_which,
  race = RACES_MODEL,
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
  new_data <- (
    list(
      race = race,
      state = state,
      year = year,
      edu.hs = edu.hs / 100,
      emp.ue = emp.ue / 100,
      inc.inc = inc.inc,
      val.hom = val.hom,
      pop.share.ratio = pop.share.ratio
    ) |>
    purrr::discard(is.null)
  )

  model <- readRDS(MODEL_FILES[[model_which]])

  plot_prediction(
    model = model[["model"]],
    data = model[["data"]],
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

do_examples <- function() {
  # Some races, Alaska, 2019, other covariates default, MLE Gaussian
  ggplot_out <- plot_prediction_app(
    "mle_gaus",
    race = c("White", "Black", "Asian"),
    state = "Alaska",
    year = 2019,
    separate_y = TRUE,
    title = "Predicted homeownership by race"
  )
  ggplot2::ggsave("temp1.png", ggplot_out, width = 10, height = 10)

  # All races, all states, 2019, other covariates default, MLE Gaussian
  ggplot_out <- plot_prediction_app(
    "mle_gaus",
    race = "All",
    state = "Alaska",
    year = 2019,
    separate_y = TRUE,
    title = "Predicted homeownership by race"
  )
  ggplot2::ggsave("temp2.png", ggplot_out, width = 10, height = 10)

  # All races, all states, all years, other covariates default, MLE Gaussian
  ggplot_out <- plot_prediction_app(
    "mle_gaus",
    race = "All",
    state = "Alaska",
    year = "All",
    separate_y = TRUE,
    title = "Predicted homeownership by race"
  )
  ggplot2::ggsave("temp3.png", ggplot_out, width = 10, height = 10)

  # All races, all states, all years, some covariates set, MLE Gaussian
  ggplot_out <- plot_prediction_app(
    "mle_gaus",
    race = "All",
    state = "All",
    year = "All",
    edu.hs = 0.6,
    emp.ue = .1,
    inc.inc = 100000,
    val.hom = 1000000,
    pop.share.ratio = 1,
    separate_y = TRUE,
    title = "Predicted homeownership by race"
  )
  ggplot2::ggsave("temp4.png", ggplot_out, width = 10, height = 10)

  # All races, all states, all years, some covariates set, Bayesian Gaussian
  ggplot_out <- plot_prediction_app(
    "bay_gaus",
    race = "All",
    state = "All",
    year = "All",
    edu.hs = 0.6,
    emp.ue = .1,
    inc.inc = 100000,
    val.hom = 1000000,
    pop.share.ratio = 1,
    separate_y = TRUE,
    title = "Predicted homeownership by race"
  )
  ggplot2::ggsave("temp5.png", ggplot_out, width = 10, height = 10)

  # All races, all states, all years, some covariates set, Bayesian Gaussian, same y
  ggplot_out <- plot_prediction_app(
    "bay_gaus",
    race = "All",
    state = "All",
    year = "All",
    edu.hs = 0.6,
    emp.ue = .1,
    inc.inc = 100000,
    val.hom = 1000000,
    pop.share.ratio = 1,
    separate_y = FALSE,
    title = "Predicted homeownership by race"
  )
  ggplot2::ggsave("temp6.png", ggplot_out, width = 10, height = 10)
}

# do_examples()
