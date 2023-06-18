# Functions for performing the data analysis/making plots

plot_var_hist <- function() {
  (
    MODEL_DATA |>
      dplyr::select(
        dplyr::any_of(c(ID_COLUMNS, COLUMNS_RENAME_PAPER))
      ) |>
      dplyr::select(!c(pop, owners)) |>
      dplyr::filter(unemp < 0.2) |>
      tidyr::pivot_longer(
        dplyr::any_of(setdiff(COLUMNS_PAPER_NEW, "sampsize"))
      ) |>
      ggplot2::ggplot(mapping = aes(x = value, weight = sampsize, fill = race)) +
      ggplot2::geom_histogram(bins = 10) +
      ggplot2::theme_classic(base_size = GGPLOT_BASE_SIZE_BIG) +
      ggplot2::facet_grid(rows = vars(race), cols = vars(name), scales = "free") +
      ggplot2::scale_color_manual(values = RACE_COLORS) +
      ggplot2::xlab("Value") +
      ggplot2::ylab("Count") +
      ggplot2::theme(
        legend.position = "none",
        panel.background = ggplot2::element_rect(fill = NA, color = "black")
      )
  ) |>
    save_ggplot(
      file.path(OUTPUT_DIR, "var_hist", "var_hist.pdf"),
      width = GGPLOT_WIDTH * 1.5,
      height = GGPLOT_HEIGHT * 0.75
    )
}

get_model_data <- function(data = CENSUS_DATA,
                           drop_size_0 = TRUE,
                           drop_na = TRUE,
                           race_list = RACES_MODEL,
                           column_list = MODEL_COLUMNS) {
  data <- (
    data |>
      dplyr::filter(race %in% race_list) |>
      dplyr::select(dplyr::all_of(column_list)) |>
      dplyr::mutate(
        inc.inc.trans = inc.inc / 1e5,
        hom.tot.log = log(hom.tot),
        hom.own.count = round(hom.own * size)
      )
  )

  if (drop_size_0) {
    data <- data |> dplyr::filter(size > 0)
  }

  if (drop_na) {
    data <- data |> tidyr::drop_na()
  }

  data
}

load_model_data <- function() {
  MODEL_DATA <<- get_model_data()
}

compute_model <- function(model_name,
                          iter = MCMC_ITERS,
                          overwrite = FALSE) {
  #' @description Compute a Bayesian model.
  #' @param model_name Name of the model to compute.
  #' @param iter Number of MCMC iterations.
  #' @param overwrite Whether to overwrite an existing model.
  #' @return A brmsfit object.

  if (!(model_name %in% MODEL_NAMES)) {
    stop(stringr::str_c("Unknown model: ", model_name))
  }
  file_out <- file.path(
    OUTPUT_DIR,
    "model_rds",
    stringr::str_c("model_", model_name, ".rds")
  )
  if (file.exists(file_out) && !overwrite) {
    cat("Model", model_name, "already exists. Skipping.\n")
    return(invisible(NULL))
  }
  brms::brm(
    formula = MODEL_INFO[[model_name]][["formula"]],
    data = MODEL_DATA,
    family = brms::beta_binomial,
    prior = MODEL_INFO[[model_name]][["prior"]],
    sample_prior = if (SAMPLE_PRIOR) "only" else "no",
    init = 0,
    iter = iter
  ) |>
  save_rds(file = file_out)
}

load_model <- function(model_name) {
  #' @title Load a model.
  #' @description This function is used to load a model that has already been computed.
  #' @param model_name Name of the model to load.
  #' @return A brmsfit object (for a Bayesian model) or else a glm object (for Model 0).
  if (model_name == "0") {
    readRDS(file = file.path(
      OUTPUT_EXPLORATORY_DIR,
      "model_rds",
      stringr::str_c("model_", model_name, ".rds")
    ))
  } else {
    readRDS(file = file.path(
      OUTPUT_DIR,
      "model_rds",
      stringr::str_c("model_", model_name, ".rds")
    ))
  }
}

plot_post_pred_density <- function(brms_out) {
  n_obs <- nrow(brms_out[["data"]])
  n_draws <- 100

  size <- brms_out[["data"]][["size"]]
  size_rep <- rep(size, n_draws)
  race <- brms_out[["data"]][["race"]]
  race_rep <- rep(race, n_draws)
  hom.own.count <- brms_out[["data"]][["hom.own.count"]]
  hom.own <- hom.own.count / size
  hom.own.count_rep <- (
    brms::posterior_predict(brms_out, ndraws = n_draws) |>
      t() |>
      as.vector()
  )
  hom.own_rep <- hom.own.count_rep / size_rep
  iter_rep <- rep(seq_len(n_draws), each = n_obs)

  data_rep <- tibble::tibble(
    hom.own = hom.own_rep,
    size = size_rep,
    race = factor(race_rep, RACES_MODEL),
    iter = iter_rep
  )

  data <- tibble::tibble(
    hom.own = hom.own,
    size = size,
    race = factor(race, RACES_MODEL)
  )

  breaks <- seq(0, 1, by = 0.05)
  labels <- as.character(breaks)
  labels[((seq_along(labels) - 1) %% 4) != 0] <- ""
  limits <- c(0, 1)

  ggplot2::ggplot() +
    ggplot2::geom_freqpoly(
      data = data_rep,
      mapping = ggplot2::aes(
        x = hom.own,
        group = iter,
        weight = size,
        color = race
      ),
      breaks = breaks,
      alpha = 0.1
    ) +
    ggplot2::scale_color_manual(values = RACE_COLORS) +
    ggplot2::geom_freqpoly(
      data = data,
      mapping = ggplot2::aes(x = hom.own, weight = size),
      color = "black",
      linewidth = 1,
      linetype = "dashed",
      breaks = breaks
    ) +
    ggplot2::scale_x_continuous(
      breaks = breaks,
      labels = labels,
      limits = limits
    ) +
    ggplot2::ggtitle("Density") +
    ggplot2::xlab("Home ownership rate") +
    ggplot2::ylab("Sample size") +
    ggplot2::facet_wrap(vars(race), scales = "free", ncol = 4) +
    ggplot2::theme_classic(base_size = GGPLOT_BASE_SIZE_SMALL) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

plot_post_pred_density_resid <- function(brms_out, n_draws = 100) {
  num_obs <- nrow(brms_out[["data"]])
  race <- brms_out[["data"]][["race"]]
  size <- brms_out[["data"]][["size"]]

  race_rep <- t(matrix(rep(race, n_draws), ncol = n_draws))
  size_rep <- t(matrix(rep(size, n_draws), ncol = n_draws))
  hom.own_pred_rep <- brms::posterior_predict(brms_out, draw_ids = seq_len(n_draws))
  hom.own_rep <- t(matrix(rep(brms_out[["data"]][["hom.own.count"]], n_draws), ncol = n_draws))
  mu_pred_rep <- brms::posterior_epred(brms_out, dpar = "mu", draw_ids = seq_len(n_draws))
  phi_pred_rep <- brms::posterior_epred(brms_out, dpar = "phi", draw_ids = seq_len(n_draws))
  var_pred_rep <- size_rep * mu_pred_rep * (1 - mu_pred_rep) * (phi_pred_rep + size_rep) / (phi_pred_rep + 1)
  std_resid_rep <- (hom.own_pred_rep - hom.own_rep) / sqrt(var_pred_rep)

  data_rep <- tibble::tibble(
    std_resid = as.vector(t(std_resid_rep)),
    size = as.vector(t(size_rep)),
    race = factor(as.vector(t(race_rep)), RACES_MODEL),
    iter = rep(seq_len(n_draws), each = num_obs)
  )

  breaks <- seq(-5, 5, by = 0.5)
  labels <- as.character(breaks)
  labels[((seq_along(labels) - 1) %% 2) != 0] <- ""
  limits <- c(-5, 5)

  ggplot2::ggplot() +
    ggplot2::geom_freqpoly(
      data = data_rep,
      mapping = ggplot2::aes(
        x = std_resid,
        group = iter,
        weight = size,
        color = race
      ),
      breaks = breaks,
      alpha = 0.1
    ) +
    ggplot2::scale_color_manual(values = RACE_COLORS) +
    ggplot2::scale_x_continuous(
      breaks = breaks,
      labels = labels,
      limits = limits
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 2
    ) +
    ggplot2::xlab("Home ownership rate") +
    ggplot2::ylab("Count") +
    ggplot2::facet_wrap(vars(race), scales = "free") +
    ggplot2::theme_classic(base_size = GGPLOT_BASE_SIZE_SMALL) +
    ggplot2::theme(legend.position = "none")
}

plot_post_pred_stat <- function(brms_out,
                                stat_func,
                                x_label,
                                y_label,
                                title,
                                n_draws = 100) {
  num_obs <- nrow(brms_out[["data"]])
  race <- brms_out[["data"]][["race"]]
  race_rep <- t(matrix(rep(race, n_draws), ncol = n_draws))
  size <- brms_out[["data"]][["size"]]
  size_rep <- t(matrix(rep(size, n_draws), ncol = n_draws))
  hom.own_rep <- brms::posterior_predict(brms_out, draw_ids = seq_len(n_draws)) / size_rep
  hom.own <- brms_out[["data"]][["hom.own.count"]] / size

  data <- (
    tibble::tibble(
      hom.own = hom.own,
      size = size,
      race = factor(race, RACES_MODEL)
    ) |>
      dplyr::group_by(race) |>
      dplyr::summarize(
        stat = stat_func(hom.own, size),
        .groups = "drop"
      )
  )

  data_rep <- (
    tibble::tibble(
      hom.own = as.vector(t(hom.own_rep)),
      size = as.vector(t(size_rep)),
      race = factor(as.vector(t(race_rep)), RACES_MODEL),
      iter = rep(seq_len(n_draws), each = num_obs)
    ) |>
      dplyr::group_by(iter, race) |>
      dplyr::summarize(
        stat = stat_func(hom.own, size),
        .groups = "drop"
      )
  )
  ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data = data_rep,
      mapping = ggplot2::aes(
        x = stat,
        group = iter,
        fill = race
      ),
      bins = 20
    ) +
    ggplot2::geom_vline(
      data = data,
      mapping = ggplot2::aes(xintercept = stat),
      color = "black",
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::scale_fill_manual(values = RACE_COLORS) +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_label) +
    ggplot2::ggtitle(title) +
    ggplot2::facet_wrap(vars(race), ncol = 4, scales = "free") +
    ggplot2::theme_classic(base_size = GGPLOT_BASE_SIZE_SMALL) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

compute_model_loo <- function(model_name, cores = 1, overwrite = FALSE) {
  model <- load_model(model_name = model_name)
  file_out <- file.path(
    OUTPUT_DIR,
    "loo_rds",
    stringr::str_c("loo_", model_name, ".rds")
  )
  if (file.exists(file_out) && !overwrite) {
    cat("File", file_out, "already exists. Skipping.\n")
    return(invisible(NULL))
  }
  (
    if ("brmsfit" %in% class(model)) {
      loo::loo(model, cores = cores)
    } else {
      NULL
    }) |>
    save_rds(file_out)
}

load_model_loo <- function(model_name) {
  readRDS(
    file.path(
      OUTPUT_DIR,
      "loo_rds",
      stringr::str_c("loo_", model_name, ".rds")
    )
  )
}

make_model_loo_table <- function() {
  loo_data <- purrr::map_dfr(
    MODEL_NAMES,
    function(model_name) {
      loo_out <- load_model_loo(model_name = model_name)
      tibble::tibble(
        Model = model_name,
        LOOIC = loo_out[["estimates"]][["looic", "Estimate"]],
        SE = loo_out[["estimates"]][["looic", "SE"]]
      )
    }
  )

  loo_data |>
    (function(x) {
      min_idx <- which.min(x[["LOOIC"]])
      min_looic <- x[["LOOIC"]][[min_idx]]
      min_se <- x[["SE"]][[min_idx]]

      x |>
        dplyr::mutate(` ` = ifelse(LOOIC <= min_looic + min_se, "†", "")) |>
        dplyr::mutate(` ` = stringr::str_c(` `, ifelse(Model == FINAL_MODEL, "*", "")))
    })() |>
    (function(x) {
      dplyr::bind_cols(
        (
          x |>
            dplyr::slice(1:4) |>
            dplyr::rename_with(function(y) stringr::str_c(y, "_1"))
        ),
        (
          x |>
            dplyr::slice(5:8) |>
            dplyr::rename_with(function(y) stringr::str_c(y, "_2"))
        )
      )
    })() |>
    xtable::xtable(align = "rlrrl|lrrl") |>
    save_tex(
      file.path(OUTPUT_DIR, "loo_analysis", "loo_table.tex"),
      sanitize.colnames.function = function(x) stringr::str_replace(x, "_\\d", "")
    )

  loo_data |>
    save_csv(file.path(OUTPUT_DIR, "loo_analysis", "loo_table.csv"))
}

plot_model_loo_figure <- function() {
  loo_data <- purrr::map_dfr(
    MODEL_NAMES,
    function(model_name) {
      loo_out <- load_model_loo(model_name = model_name)
      tibble::tibble(
        Model = model_name,
        LOOIC = loo_out[["estimates"]][["looic", "Estimate"]],
        SE = loo_out[["estimates"]][["looic", "SE"]]
      )
    }
  )

  min_idx <- which.min(loo_data[["LOOIC"]])
  min_looic <- loo_data[["LOOIC"]][[min_idx]]
  min_se <- loo_data[["SE"]][[min_idx]]

  loo_data <- loo_data |>
    dplyr::mutate(annotation = ifelse(LOOIC <= min_looic + min_se, "†", "")) |>
    dplyr::mutate(annotation = stringr::str_c(annotation, ifelse(Model == FINAL_MODEL, "*", "")))
  
  span_x <- (
    max(loo_data[["LOOIC"]] + loo_data[["SE"]]) -
    min(loo_data[["LOOIC"]] - loo_data[["SE"]])
  )
  annotation_x <- min(loo_data[["LOOIC"]] - loo_data[["SE"]]) - 0.1 * span_x 

  (
    ggplot2::ggplot(data = loo_data) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = LOOIC, y = Model),
      size = 4
    ) +
    ggplot2::geom_errorbarh(
      mapping = ggplot2::aes(xmin = LOOIC - SE, xmax = LOOIC + SE, y = Model),
      height = 0.5
    ) +
    ggplot2::xlab("LOOIC") +
    ggplot2::ylab("Model") +
    ggplot2::geom_vline(xintercept = min_looic + min_se, linetype = "dashed") +
    ggplot2::geom_text(
      mapping = ggplot2::aes(x = annotation_x, y = Model, label = annotation),
      hjust = 0,
      size = 8
    ) +
    ggplot2::theme_classic(base_size = GGPLOT_BASE_SIZE_SMALL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  ) |>
  save_ggplot(
    file.path(OUTPUT_DIR, "loo_analysis", "loo_figure.pdf"),
    width = GGPLOT_WIDTH / 2,
    height = GGPLOT_HEIGHT / 2
  )
}

make_model_summaries <- function() {
  purrr::walk(
    MODEL_NAMES,
    function(model_name) {
      model <- load_model(model_name = model_name)

      save_output(
        {
          cat("Posterior summary:\n")
          print(summary(model), width = 10000)
          cat(strrep("-", 100), "\n")
          cat("Prior summary:\n")
          print(brms::prior_summary(model), width = 10000)
        },
        file_name = file.path(
          OUTPUT_DIR,
          "model_summary",
          stringr::str_c("summary_", model_name, ".txt")
        )
      )
    }
  )
}

plot_post_pred_density_all <- function() {
  purrr::walk(
    MODEL_NAMES,
    function(model_name) {
      model_out <- load_model(model_name)
      plot_post_pred_density(model_out) |>
        save_ggplot(
          file.path(
            OUTPUT_DIR,
            "post_pred_density",
            stringr::str_c("post_pred_density_", model_name, ".pdf")
          ),
          width = GGPLOT_WIDTH * 1.5,
          height = GGPLOT_HEIGHT * 1.5 / 4
        )

      plot_post_pred_density_resid(model_out) |>
        save_ggplot(
          file.path(
            OUTPUT_DIR,
            "post_pred_density_resid",
            stringr::str_c("post_pred_density_resid_", model_name, ".pdf")
          )
        )
    }
  )
}


plot_post_pred_stat_all <- function() {
  purrr::walk(
    MODEL_NAMES,
    function(model_name) {
      model <- load_model(model_name)

      dplyr::bind_rows(
        list(stat_name = "mean", stat_func = list(matrixStats::weightedMean)),
        list(stat_name = "median", stat_func = list(matrixStats::weightedMedian)),
        list(stat_name = "variance", stat_func = list(matrixStats::weightedVar))
      ) |>
        purrr::pwalk(
          function(stat_name, stat_func) {
            file_prefix <- stringr::str_c("post_pred_stat_", stat_name)
            plot_post_pred_stat(
              model,
              stat_func = stat_func,
              x_label = stringr::str_c(
                "Home ownership rate ",
                stat_name
              ),
              y_label = "Count",
              title = stringr::str_to_title(stat_name)
            ) |>
              save_ggplot(
                file.path(
                  OUTPUT_DIR,
                  file_prefix,
                  stringr::str_c(file_prefix, "_", model_name, ".pdf")
                ),
                width = GGPLOT_WIDTH * 1.5,
                height = GGPLOT_HEIGHT * 1.5 / 4
              )
          }
        )
    }
  )
}

plot_loo_pareto_k_all <- function() {
  purrr::walk(
    MODEL_NAMES,
    function(model_name) {
      loo_out <- load_model_loo(model_name)

      data <- tibble::tibble(
        pareto_k = loo_out[["diagnostics"]][["pareto_k"]],
        index = seq_along(pareto_k),
        race = MODEL_DATA[["race"]]
      )
      data_outlier <- (
        data |>
          dplyr::filter(pareto_k > 0.7)
      )

      (
        ggplot2::ggplot() +
          ggplot2::geom_point(
            data = data,
            mapping = ggplot2::aes(
              x = index,
              y = pareto_k,
              color = race
            )
          ) +
          ggplot2::geom_text(
            data = data_outlier,
            mapping = ggplot2::aes(
              x = index,
              y = pareto_k,
              color = race,
              label = index
            ),
            nudge_x = 500
          ) +
          ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed") +
          ggplot2::ylim(c(-0.5, 1)) +
          ggplot2::xlab("Index") +
          ggplot2::ylab("LOO PSIS Pareto K") +
          ggplot2::facet_wrap(ggplot2::vars(race), ncol = 2) +
          ggplot2::theme_classic(base_size = GGPLOT_BASE_SIZE_SMALL) +
          ggplot2::theme(legend.position = "none")
      ) |>
        save_ggplot(
          file.path(
            OUTPUT_DIR,
            "loo_psis_pareto_k",
            stringr::str_c("loo_psis_pareto_k_", model_name, ".pdf")
          )
        )
      }
  )
}

plot_prediction <- function(
  model,
  data,
  new_data,
  separate_y = FALSE,
  x_lim_full = FALSE,
  fill_alpha = 0.3,
  title = NULL,
  show_subtitle = TRUE,
  base_size = 20,
  interval_size = 5,
  num_draws = 1000
) {
#' @title Plot predicted home ownership.
#' @description Plots posterior predicted distributions for Bayesian models.
#' @note This function needs library(ggdist) and library(brms)!
#' 
#' @param model the fitted model (e.g. output of lm function)
#' @param data data model was fitted on
#' @param new_data list of new covariates for prediction. Must have keys "state", "race", "year"
#'                 and can optionally have any other keys including:
#'                 "edu.hs", "emp.ue", "inc.inc", "val.hom", "hom.tot".
#'                 If any of these are missing the default values will be taken from the real data.
#'                 If "state" = "All" or "year" = "All" the average prediction over all state or years
#'                 is given. "race" must be a string vector which is a subset of RACES_MODEL. Only the
#'                 races in "race" will be displayed.
#' @param separate_y If TRUE show predicted distribution for each race on a separate baseline.
#'                   If FALSE overlay them on the same baseline.
#' @param x_lim_full If TRUE show the entire x-axis from 0 to 1. If FALSE show only the range of
#'                   of the plotted values.
#' @param fill_alpha Alpha value to fill in the density.
#' @param title title of plot. If NULL not title is plotted.
#' @param show_subtitle If TRUE show a subtitle explaining the plotted point and interval within the density.
#' @param base_size The base size of the ggplot2 theme.
#' @param interval_size thickness of the part showing the median and .95 interval.
#' @param num_draws number of posterior draws to use when estimating the prediction

  # Check which variables need to be expanded to all possible values
  for (var_name in c("race", "state")) {
    if (
      !(var_name %in% names(new_data)) ||
      (
        (length(new_data[[var_name]]) == 1) &&
        (new_data[[var_name]] == "All")
      )
    ) {
      new_data[[var_name]] <- if (var_name == "race") {
        RACES_MODEL
      } else if (var_name == "state") {
        STATES
      } else {
        stop("Impossible.")
      }
    }
  }

  new_data <- do.call(tidyr::expand_grid, new_data)

  new_data <- (
    dplyr::inner_join(
      new_data,
      data[c(
        "state",
        "race",
        setdiff(colnames(data), colnames(new_data))
      )],
      by = c("state", "race")
    ) |>
      get_model_data()
  )

  pred_draws <- (
    brms::posterior_epred(model, newdata = new_data, ndraws = num_draws) |>
      sweep(2, new_data[["size"]], `/`)
  )

  pred_draws <- purrr::map_dfr(
    unique(new_data[["race"]]),
    function(race) {
      pred_draws_race <- pred_draws[, new_data[["race"]] == race, drop = FALSE]
      size_race <- new_data[new_data[["race"]] == race, "size", drop = TRUE]
      pred_draws_race <- sweep(pred_draws_race, 2, size_race, `*`)
      pred_draws_race <- rowSums(pred_draws_race) / sum(size_race)
      tibble::tibble(
        value = pred_draws_race,
        race = factor(race, levels = RACES_MODEL)
      )
    }
  )

  if (separate_y) {
    ggplot_out <- (
      ggplot2::ggplot(
        pred_draws,
        ggplote::aes(
          x = value * 100,
          y = race,
          fill = race,
          color = race,
          slab_color = race
        )
      ) +
        ggplot2::ylab("race")
    )
  } else {
    ggplot_out <- (
      ggplot2::ggplot(
        pred_draws,
        ggplot2::aes(
          x = value * 100,
          fill = race,
          color = race,
          slab_color = race
        )
      ) +
        ggplot2::ylab("Density")
    )
  }
  ggplot_out <- (
    ggplot_out +
      ggdist::stat_halfeye(
        alpha = fill_alpha,
        .width = .95,
        interval_size = interval_size,
        interval_alpha = 1,
        point_alpha = 1
      ) +
      ggplot2::xlab("Home ownership rate (%)") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::scale_color_manual(
        values = RACE_COLORS,
        name = "Group",
        aesthetics = c("color", "fill", "slab_color")
      )
  )

  if (!separate_y) {
    ggplot_out <- ggplot_out + ggplot2::expand_limits(y = 0)
  }

  if (!is.null(title)) {
    ggplot_out <- ggplot_out + ggplot2::labs(title = title)
  }

  if (show_subtitle) {
    ggplot_out <- ggplot_out + ggplot2::labs(
      subtitle = stringr::str_c("Density, median, and 95% interval")
    )
  }

  if (x_lim_full) {
    ggplot_out <- ggplot_out + ggplot2::coord_cartesian(xlim = c(0, 1))
  }

  ggplot_out
}

make_outlier_tables <- function() {
  CENSUS_DATA |>
    dplyr::filter(
      race == "Hispanic",
      (0.3 <= hom.own) & (hom.own <= 0.4)
    ) |>
    dplyr::arrange(dplyr::desc(size)) |>
    dplyr::select(county, hom.own, pop.share, pop.share.ratio, size) |>
    save_csv(file.path(OUTPUT_DIR, "outliers", "hispanic_homown_0.3_0.4.csv"))

  CENSUS_DATA |>
    dplyr::filter(
      race == "Black",
      (0.55 <= hom.own) & (hom.own <= 0.7)
    ) |>
    dplyr::arrange(dplyr::desc(size)) |>
    dplyr::select(county, hom.own, pop.share, pop.share.ratio, size) |>
    save_csv(file.path(OUTPUT_DIR, "outliers", "black_homown_0.55_0.7.csv"))
}

get_model_draws <- function(model_name, with_state) {
  load_model(model_name) |>
  posterior::as_draws_df() |>
  dplyr::select(dplyr::starts_with("b_")) |>
  dplyr::mutate(iter = seq_len(dplyr::n())) |>
  x => (
    if (with_state) {
      x
    } else {
      x |>
      dplyr::select(!dplyr::starts_with("b_state"))
    }
  ) |>
  tidyr::pivot_longer(!iter) |>
  dplyr::mutate(name = stringr::str_replace(name, "^b_", "")) |>
  dplyr::mutate(
    component = ifelse(
      stringr::str_detect(name, "^phi_"),
      "precision",
      "response"
    )
  ) |>
  dplyr::mutate(
    name = (
      name |>
        stringr::str_replace("^phi_", "") |>
        stringr::str_replace("^race", "") |>
        stringr::str_replace("^state", "") |>
        replace_all_pairs(
          COLUMNS_PAPER_OLD,
          COLUMNS_PAPER_NEW
        )
    )
  ) |>
  dplyr::mutate(
    name_split = (
      stringr::str_split(name, ":") |>
        purrr::map(rev)
    ),
    name_base = (
      name_split |>
        purrr::map_chr(purrr::chuck, 1)
    ),
    name_race = (
      name_split |>
        purrr::map_chr(purrr::pluck, 2, .default = NA)
    )
  ) |>
  dplyr::select(!name_split) |>
  dplyr::mutate(
    name = ifelse(
      is.na(name_race),
      name_base,
      stringr::str_c(name_base, ":", name_race)
    )
  )
}

make_model_coef_table <- function() {
  purrr::pwalk(
    tidyr::expand_grid(
      model_name = MODEL_NAMES,
      with_state = c(FALSE, TRUE)
    ),
    function(model_name, with_state) {
      data_draws <- get_model_draws(model_name, with_state)

      data_coef <- (
        data_draws |>
          dplyr::group_by(name, name_base, name_race, component) |>
          dplyr::summarize(
            Mean = mean(value),
            SD = sd(value),
            `2.5%` = quantile(value, 0.025),
            `97.5%` = quantile(value, 0.975),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            name_base = factor(
              name_base,
              c("Intercept", RACES_MODEL, COLUMNS_PAPER_NEW)
            ),
            name_race = factor(name_race, RACES_MODEL),
            component = factor(component, c("response", "precision"))
          ) |>
          dplyr::arrange(component, name_base, name_race) |>
          dplyr::mutate(component = stringr::str_to_title(component)) |>
          dplyr::transmute(
            Component = component,
            Name = name,
            Mean,
            SD,
            `2.5%`,
            `97.5%`,
            `Mean*` = exp(Mean),
            `2.5%*` = exp(`2.5%`),
            `97.5%*` = exp(`97.5%`)
          )
      )

      data_coef |>
        save_csv(
          file.path(
            OUTPUT_DIR,
            "model_coef",
            stringr::str_c(
              "model_coef_",
              model_name,
              if (with_state) "_state" else "",
              ".csv"
            )
          )
        )

      data_coef_tex <- (
        data_coef |>
          dplyr::mutate(
            Component = forcats::fct_relabel(
              Component,
              function(x) {
                c(
                  Response = "$\\mathrm{logit}(\\theta)$",
                  Precision = "$\\log(\\phi)$"
                )[x]
              }
            ),
            Name = stringr::str_replace_all(
              Name,
              "\\w+",
              "$\\\\mathit{\\0}$"
            ),
            Mean = sprintf("$%.2f$", Mean),
            SD = sprintf("$%.2f$", SD),
            `2.5%` = sprintf("$%.2f$", `2.5%`),
            `97.5%` = sprintf("$%.2f$", `97.5%`),
            `Mean*` = sprintf("$%.2f$", `Mean*`),
            `2.5%*` = sprintf("$%.2f$", `2.5%*`),
            `97.5%*` = sprintf("$%.2f$", `97.5%*`)
          )
      )

      data_coef_tex |>
        xtable::xtable(align = "rll|rrrr|rrr") |>
        save_tex_formatted(
          file.path(
            OUTPUT_DIR,
            "model_coef",
            stringr::str_c(
              "model_coef_",
              model_name,
              if (with_state) "_state" else "",
              ".tex"
            )
          ),
          hline_after_extra = max(which(as.integer(data_coef_tex[["Component"]]) == 2)),
          sanitize.text.function = base::I,
          sanitize.colnames.function = xtable::sanitize
        )

      if (!with_state) {
        hom.own_mean <- (
          CENSUS_DATA |>
            dplyr::select(hom.own, hom.tot) |>
            tidyr::drop_na() |>
            dplyr::summarize(hom.own_mean = weighted.mean(hom.own, hom.tot)) |>
            dplyr::pull(hom.own_mean)
        )
        
        data_effect <- (
          data_draws |>
            dplyr::filter(component == "response", name_base != "Intercept") |>
            dplyr::select(!component) |>
            dplyr::mutate(
              unit = list(
                Hispanic = list(unit_value = 1, Unit = "N/A"),
                Black = list(unit_value = 1, Unit = "N/A"),
                Asian = list(unit_value = 1, Unit = "N/A"),
                popshareratio = list(unit_value = 0.1, Unit = "0.1"),
                income = list(unit_value = 0.1, Unit = "$10,000"),
                hsedu = list(unit_value = 0.1, Unit = "10%"),
                unemp = list(unit_value = 0.1, Unit = "10%")
              )[name_base]
            ) |>
            tidyr::unnest_wider(unit) |>
            dplyr::mutate(
              effect = plogis(qlogis(hom.own_mean) + unit_value * value) - hom.own_mean
            ) |>
            dplyr::group_by(name, name_base, name_race, Unit) |>
            dplyr::summarize(
              Effect = mean(effect),
              `2.5%` = quantile(effect, 0.025),
              `97.5%` = quantile(effect, 0.975),
              .groups = "drop"
            ) |>
            dplyr::mutate(
              name_base = factor(
                name_base,
                c("Intercept", RACES_MODEL, COLUMNS_PAPER_NEW)
              ),
              name_race = factor(name_race, RACES_MODEL)
            ) |>
            dplyr::arrange(name_base, name_race) |>
            dplyr::select(
              Name = name,
              Unit,
              Effect,
              `2.5%`,
              `97.5%`
            )
        )

        data_effect |>
          save_csv(
            file.path(
              OUTPUT_DIR,
              "model_effect",
              stringr::str_c(
                "model_effect_",
                model_name,
                ".csv"
              )
            )
          )

        data_effect |>
          dplyr::mutate(
            Name = stringr::str_replace_all(
              Name,
              "\\w+",
              "$\\\\mathit{\\0}$"
            ),
            Unit = xtable::sanitize(Unit),
            Effect = sprintf("$%.1f\\%%$", 100 * Effect),
            `2.5%` = sprintf("$%.1f\\%%$", 100 * `2.5%`),
            `97.5%` = sprintf("$%.1f\\%%$", 100 * `97.5%`),
          ) |>
          xtable::xtable(
            align = "rllrrr",
            caption = xtable::sanitize(
              sprintf("Home ownership mean: %.2f%%", 100 * hom.own_mean)
            )
          ) |>
          save_tex_formatted(
            file.path(
              OUTPUT_DIR,
              "model_effect",
              stringr::str_c(
                "model_effect_",
                model_name,
                ".tex"
              )
            ),
            sanitize.text.function = base::I,
            sanitize.colnames.function = xtable::sanitize
          )
      }
    }
  )
}

make_mean_vs_model_table <- function() {
  MODEL_NAMES |>
  purrr::walk(
    function(model_name) {
      data_fitted <- (
        readr::read_csv(
          file.path(OUTPUT_DIR, "model_effect",
                    paste0("model_effect_", model_name, ".csv"))
        ) |>
          dplyr::filter(Name %in% RACES_MODEL) |>
          dplyr::select(
            race = Name,
            hom.own_fitted = Effect
          )
      )

      readr::read_csv(
        file.path(OUTPUT_EXPLORATORY_DIR, "census_data_by_race_means.csv")
      ) |>
        dplyr::filter(race != "Total") |>
        dplyr::select(race, hom.own) |>
        dplyr::mutate(
          hom.own_diff = hom.own - hom.own[race == "WhiteNH"]
        ) |>
        dplyr::left_join(
          data_fitted,
          by = "race"
        ) |>
        dplyr::transmute(
          Group = race,
          Rate = sprintf("$%.2f\\%%$", 100 * hom.own),
          Difference = ifelse(race == "WhiteNH", "", sprintf("$%.2f\\%%$", 100 * hom.own_diff)),
          `Model Effect` = ifelse(race == "WhiteNH", "", sprintf("$%.2f\\%%$", 100 * hom.own_fitted))
        ) |>
        xtable::xtable(align = "rlrrr") |>
        save_tex_formatted(
          file.path(OUTPUT_DIR, "mean_vs_model", paste0("mean_vs_model_", model_name, ".tex")),
          sanitize.text.function = I
        )
      }
  )
}

make_model_effect_county <- function(county) {
  data <- (
    get_model_data(drop_size_0 = FALSE, drop_na = FALSE, column_list = COLUMN_NAMES) |>
      dplyr::filter(county == !!county) |>
      dplyr::mutate(race = factor(race, RACES_MODEL))
  )
  effect_list <- list(
    list(variable = "inc.inc.trans", delta = 0.1, name = "Income_$+\\$10$k"),
    list(variable = "inc.inc.trans", delta = 0.25, name = "Income_$+\\$25$k"),
    list(variable = "inc.inc.trans", delta = 0.5, name = "Income_$+\\$50$k")
  )

  MODEL_NAMES |>
  purrr::walk(
    function(model_name) {
      model <- load_model(model_name)
      (
        purrr::map_dfr(
          effect_list,
          function(effect) {
            data_new <- data
            data_new[[effect[["variable"]]]] <- data_new[[effect[["variable"]]]] + effect[["delta"]]

            predict_orig <- predict(model, newdata = data)[, "Estimate"] / data[["size"]]
            predict_new <- predict(model, newdata = data_new)[, "Estimate"] / data[["size"]]
            predict_delta <- predict_new - predict_orig

            dplyr::bind_cols(
              Group = data[["race"]],
              Name = effect[["name"]],
              Delta = predict_delta
            )
          }
        ) |>
          tidyr::pivot_wider(id_cols = Group, names_from = Name, values_from = Delta) |>
          dplyr::inner_join(
            tibble::tibble(
              Group = data[["race"]],
              HOwnR_Empirical = data[["hom.own"]],
              Income_Empirical = data[["inc.inc.trans"]]
            ),
            by = "Group"
          ) |>
          dplyr::inner_join(
            (
              tibble::tibble(
                Group = data[["race"]],
                predict_out = (
                  predict(model, newdata = data)[, c("Estimate", "Est.Error")] /
                    data[["size"]]
                ),
                HOwnR_Fitted = predict_out[, 1],
                HOwnR_SD = predict_out[, 2]
              ) |>
                dplyr::select(!predict_out)
            ),
            by = "Group"
          ) |>
          dplyr::mutate(
            HOwnR_Empirical = sprintf("$%.1f\\%%$", 100 * HOwnR_Empirical),
            HOwnR_Fitted = sprintf("$%.1f\\%%$", 100 * HOwnR_Fitted),
            Income_Empirical = sprintf("$\\$%d$k", round(100 * Income_Empirical))
          ) |>
          dplyr::mutate(
            dplyr::across(
              tidyselect::where(is.numeric),
              function(x) {
                sprintf("$%.2f\\%%$", 100 * x)
              }
            )
          ) |>
          x => {
            first_row <- (
              colnames(x) |>
                stringr:::str_split("_") |>
                purrr::map(purrr::pluck, 2, .default = "")
            )
            dplyr::bind_rows(
              first_row |> rlang::set_names(colnames(x)),
              x
            )
          } |>
          dplyr::relocate(
            Group,
            HOwnR_Empirical,
            HOwnR_Fitted,
            HOwnR_SD,
            Income_Empirical
          ) |>
          xtable::xtable(align = "rlrrrr|rrr") |>
          x => save_tex(
            x,
            file.path(
              OUTPUT_DIR,
              "model_effect_cases",
              stringr::str_c(
                (
                  county |>
                    remove_punct() |>
                    stringr::str_replace_all(" ", "_")
                ),
                "_",
                model_name,
                ".tex"
              )
            ),
            hline.after = c(-1, -1, 1, nrow(x)),
            sanitize.text.function = I,
            sanitize.colnames.function = function(y) {
              y |>
                stringr:::str_split("_") |>
                purrr::map(purrr::pluck, 1)
            }
          )
      )
    }
  )
}

plot_model_residual_chloropleth <- function(standardize = FALSE) {
  model_data <- get_model_data(
    column_list = union(
      c("state", "county", "geoid", "race"),
      MODEL_COLUMNS
    )
  )
  id_data <- (
    CENSUS_DATA |>
      dplyr::filter(race %in% RACES_MODEL) |>
      dplyr::select(state, county, geoid, race)
  )
  purrr::walk(
    MODEL_NAMES,
    function(model_name) {
      model <- load_model(model_name)
      if (standardize) {
        pred_out <- predict(model)
        hom.own_pred <- pred_out[, "Estimate"] / model_data[["size"]]
        hom.own_pred_se <- pred_out[, "Est.Error"] / model_data[["size"]]
        hom.own_actual <- model_data[["hom.own"]]
        residual <- (hom.own_actual - hom.own_pred) / hom.own_pred_se
      } else {
        fitted_out <- fitted(model)
        hom.own_fitted <- fitted_out[, "Estimate"] / model_data[["size"]]
        hom.own_actual <- model_data[["hom.own"]]
        residual <- hom.own_actual - hom.own_fitted
      }
      tibble::tibble(
        state = model_data[["state"]],
        county = model_data[["county"]],
        geoid = model_data[["geoid"]],
        race = model_data[["race"]],
        residual = residual
      ) |>
        dplyr::right_join(
          id_data,
          by = c("state", "county", "geoid", "race")
        ) |>
        join_with_shape() |>
        plot_choropleth(
          "residual",
          outline = TRUE,
          breaks = if (standardize) {
            c(-2, -1, 0, 1, 2)
          } else {
            c(-0.1, -0.05, 0, 0.05, 0.1)
          }
        ) |>
        save_tmap(
          file.path(
            OUTPUT_DIR,
            "model_residuals",
            stringr::str_c(
              "model_residuals_",
              model_name,
              if (standardize) "_std" else "_raw",
              ".png"
            )
          )
        )
    }
  )
}

do_all_compute_models <- function(model_names = MODEL_NAMES, overwrite = FALSE) {
  purrr::walk(
    MODEL_NAMES,
    function(model_name) {
      if (
        (PRIOR_TYPE == FINAL_PRIOR_TYPE) &&
        (SAMPLE_PRIOR == FALSE) &&
        (model_name == FINAL_MODEL)
      ) {
        compute_model(
          model_name,
          iter = FINAL_MODEL_MCMC_ITERS,
          overwrite = overwrite
        )
      } else {
        compute_model(model_name, overwrite = overwrite)
      }
    }
  )

  if (!SAMPLE_PRIOR) {
    MODEL_NAMES |> purrr::walk(compute_model_loo, overwrite = overwrite)
  }
}

do_convergence_analysis <- function() {
  purrr::map(
    MODEL_NAMES,
    function(model) {
      model_summary <- model |> load_model() |> summary() |> purrr::pluck("fixed")
      rhat_range <- model_summary[["Rhat"]] |> range()
      tail_ess_range <- model_summary[["Tail_ESS"]] |> range()
      bulk_ess_range <- model_summary[["Bulk_ESS"]] |> range()
      
      tibble::tibble(
        model = model,
        rhat_min = rhat_range[1],
        rhat_max = rhat_range[2],
        tail_ess_min = tail_ess_range[1],
        tail_ess_max = tail_ess_range[2],
        bulk_ess_min = bulk_ess_range[1],
        bulk_ess_max = bulk_ess_range[2]
      )
    }
  ) |>
  purrr::list_rbind() |>
  save_csv(
    file_name = file.path(
      OUTPUT_DIR,
      "model_summary",
      "summary_all_convergence.csv"
    )
  )
}

do_fitted_correlation <- function() {
  c("0", MODEL_NAMES) |>
  x => setNames(x, x) |>
  purrr::map(
    function(model_name) {
      if (model_name == "0") {
        load_model(model_name) |>
        fitted()
      } else {
        load_model(model_name) |>
        brms::posterior_epred(ndraws = 100) |>
        apply(2, mean) |>
        x => (x / MODEL_DATA[["size"]])
      }
    }
  ) |>
  x => do.call(cbind, x) |>
  cor() |>
  x => {
    x |>
      xtable::xtable() |>
      save_tex(
        file.path(
          OUTPUT_DIR,
          "fitted_correlation",
          "fitted_correlation.tex"
        ),
        .include.rownames = TRUE
      )

    x |>
      tibble::as_tibble() |>
      dplyr::mutate(model = rownames(x), .before = 1) |>
      save_csv(
        file.path(
          OUTPUT_DIR,
          "fitted_correlation",
          "fitted_correlation.csv"
        )
      )
  }
}

do_all_model_analyses <- function() {
  #' @title Do all model analyses.
  #' @description Do all model analyses once the models have been computed.

  make_model_summaries()
  if ((PRIOR_TYPE == FINAL_PRIOR_TYPE) && !SAMPLE_PRIOR) {
    make_model_loo_table()
    plot_model_loo_figure()
  }
  plot_post_pred_density_all()
  plot_post_pred_stat_all()
  plot_var_hist()
  do_missing_value_analysis()
  make_outlier_tables()
  make_model_coef_table()
  make_mean_vs_model_table()
  make_model_effect_county("New York/New York County")
  make_model_effect_county("Florida/Hillsborough County")
  plot_model_residual_chloropleth(FALSE)
  plot_model_residual_chloropleth(TRUE)
  do_convergence_analysis()
  do_fitted_correlation()
}
