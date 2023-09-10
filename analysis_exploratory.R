#################### Functions ##################

plot_choropleth <- function(
  data,
  fill_var,
  title = NULL,
  facet_var = "race",
  facet_num_cols = NA,
  breaks = NULL,
  outline = TRUE,
  color_na = "black",
  free_scales = FALSE,
  log_scale = FALSE,
  legend_show = TRUE,
  legend_portrait = FALSE,
  main_title_size = 3,
  panel_label_size = 3,
  legend_title_size = 3,
  legend_text_size = 3,
  legend_height = 0.3,
  palette = NULL
) {
#' @title Plot choropleth
#' @param data The data frame with spatial geometry.
#'        Must have a "geometry" column and have class "sf".
#' @param fill_var The variable used to color the choropleth.
#' @param title Main title of the plot.
#' @param facet_var The variable to use to facet the plots.
#' @param facet_num_cols Number of columns for facets.
#' @param breaks List of numbers to manually specify the breaks for the color bar.
#' @param outline Show the outline of spatial units.
#' @param color_na Color for missing values. Set to NA for no color.
#' @param free_scales Set to TRUE if want to show different color scales in each panel.
#'                    Otherwise all panels follow the same
#' @param log_scale Set to TRUE to take log10 before plotting color.
#'                  The builtin log scale in tmap is buggy and gives errors,
#'                  so I just decided to show log scale like this.
#' @param legend_show Show the legend.
#' @param legend_portrait Set to TRUE to show legend in portrait mode.
#' @param main_title_size Size of the main title.
#' @param panel_label_size Size of the panel labels.
#' @param legend_title_size Size of the legend title.
#' @param legend_text_size Size of the legend text.
#' @param legend_height Height of the legend.
#' @param palette Color palette to use. If NULL, use the default palette.
#' @return A tmap object.
#' @note The size arguments are selected so that the plots look good at a
#'       5000 x 5000 pixel resolution.

  # keep only contiguous states for plotting
  data <- data |> dplyr::filter(state %in% STATES_MAINLAND)
  if (log_scale) {
    data[[fill_var]] <- log10(
      replace(data[[fill_var]], data[[fill_var]] <= 0, NA)
    )
  }
  tmap_out <- (
    tmap::tm_shape(data) +
    tmap::tm_fill(
      col = fill_var,
      style = "cont",
      breaks = breaks,
      colorNA = color_na,
      legend.is.portrait = legend_portrait,
      palette = palette
    )
  )
  if (outline) {
    tmap_out <- tmap_out + tmap::tm_borders()
  }
  if (!is.null(facet_var)) {
    tmap_out <- (
      tmap_out +
      tmap::tm_facets(
        facet_var,
        free.scales = free_scales,
        ncol = facet_num_cols
      )
    )
  }
  tmap_out <- (
    tmap_out +
    tmap::tm_layout(
      main.title = if (is.null(title)) fill_var else title,
      main.title.size = main_title_size,
      panel.label.size = panel_label_size,
      legend.show = legend_show,
      legend.outside = !free_scales,
      legend.outside.position = "bottom",
      legend.position = c("left", "bottom"),
      legend.title.size = legend_title_size,
      legend.text.size = legend_text_size,
      legend.height = legend_height
    )
  )
  tmap_out
}

plot_pair <- function(
  data,
  x_var,
  y_var,
  title = NULL,
  facet_var = NULL,
  facet_num_cols = NULL,
  facet_scales = "free",
  weight_var = NULL,
  fill_var = NULL
) {
  purrr::walk(
    c(x_var, y_var),
    function(var) {
      if (!is.numeric(data[[var]]) && !is.factor(data[[var]])) {
        stop("Variables must be numeric or factor.")
      }
    }
  )

  if (is.null(title)) {
    title <- stringr::str_c(y_var, " vs. ", x_var)
  }

  aes_obj <- (
    list(
      x = x_var,
      y = y_var,
      weight = weight_var,
      fill = fill_var
    ) |>
    purrr::discard(is.null) |>
    purrr::map(as.symbol) |>
    x => do.call(ggplot2::aes, x)
  )

  ggplot_out <- ggplot2::ggplot(
    data = data,
    mapping = aes_obj
  )
  if (!is.numeric(data[[x_var]]) && !is.numeric(data[[y_var]])) {
    ggplot_out <- ggplot_out + ggplot2::geom_tile()
  } else if (is.numeric(data[[x_var]]) && is.numeric(data[[y_var]])) {
    ggplot_out <- (
      ggplot_out +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
    )
  } else {
    # one numeric, the other is factor
    ggplot_out <- ggplot_out + ggplot2::geom_boxplot()
  }
  ggplot_out <- (
    ggplot_out +
    ggplot2::theme_classic(
      base_size = if (is.null(facet_var)) {
        GGPLOT_BASE_SIZE_BIG
      } else {
        GGPLOT_BASE_SIZE_SMALL
      }
    ) +
    ggplot2::xlab(x_var) +
    ggplot2::ylab(y_var) +
    ggplot2::labs(title = title)
  )
  if (!is.null(facet_var)) {
    ggplot_out <- (
      ggplot_out +
      ggplot2::facet_wrap(facet_var, ncol = facet_num_cols, scales = facet_scales)
    )
  }
  ggplot_out
}

plot_histogram <- function(
  data,
  value_var,
  weight_var = NULL,
  facet_var = NULL,
  color_var = NULL,
  title = NULL,
  y_expand = NULL,
  density = FALSE,
  line_size = 1,
  facet_scales = "free",
  x_lim = NULL
) {
  if (is.null(title)) {
    title <- value_var
  }

  is_numeric <- is.numeric(data[[value_var]])
  is_factor <- is.factor(data[[value_var]])
  if (!is_numeric && !is_factor) {
    stop(stringr::str_c("Need a numeric or factor variable"))
  }

  if (is_numeric) {
    data_median <- data
    if (!is.null(facet_var)) {
      data_median <- (
        data_median |>
        dplyr::group_by(dplyr::across(dplyr::all_of(facet_var)))
      )
    }
    data_median <- (
      data_median |>
      dplyr::summarize(median = median(.data[[value_var]], na.rm = TRUE))
    )
  }

  aes_obj <- (
    list(
      x = value_var,
      fill = color_var,
      weight_var = weight_var
    ) |>
    purrr::discard(is.null) |>
    purrr::map(as.symbol) |>
    x => do.call(ggplot2::aes, x)
  )

  ggplot_out <- ggplot2::ggplot(
    data = data,
    mapping = aes_obj
  )

  if (!is.null(color_var)) {
    ggplot_out <- (
      ggplot_out +
      ggplot2::guides(fill = ggplot2::guide_legend(title = value_var))
    )
  }

  if (is_numeric) {
    if (density) {
      ggplot_out <- (
        ggplot_out +
        ggplot2::geom_density() +
        ggplot2::geom_vline(
          data = data_median,
          ggplot2::aes(xintercept = median),
          size = line_size
        ) +
        ggplot2::labs(subtitle = "with median line")
      )
    } else {
      ggplot_out <- ggplot_out + ggplot2::geom_histogram(bins = 20)
    }
  } else if (is_factor) {
    ggplot_out <- ggplot_out + ggplot2::geom_bar()
  } else {
    stop("Impossible.")
  }

  ggplot_out <- (
    ggplot_out +
    ggplot2::theme_classic(
      base_size = if (is.null(facet_var)) {
        GGPLOT_BASE_SIZE_BIG
      } else {
        GGPLOT_BASE_SIZE_SMALL
      }
    ) +
    ggplot2::labs(title = title) +
    ggplot2::xlab(value_var)
  )

  if (!is.null(facet_var)) {
    ggplot_out <- (
      ggplot_out +
      ggplot2::facet_grid(rows = facet_var, scales = facet_scales)
    )
    if (!is.null(color_var) && (color_var == facet_var)) {
      ggplot_out <- ggplot_out + ggplot2::guides(fill = "none")
    }
  }

  if (!is.null(y_expand)) {
    ggplot_out <- ggplot_out + ggplot2::expand_limits(y = y_expand)
  }

  if (!is.null(x_lim)) {
    ggplot_out <- ggplot_out + ggplot2::xlim(x_lim)
  }

  ggplot_out
}

plot_histogram_all <- function(data, dir) {
  purrr::walk(
    MEAN_COLUMNS,
    function(value_var) {
      if (is.numeric(data[[value_var]])) {
        plot_histogram(
          data = data,
          value_var = value_var,
          facet_var = "race",
          weight_var = "size",
          x_lim = if (value_var == "emp.ue") {
            c(0, 0.25)
          } else {
            NULL
          }
        ) |>
        save_ggplot(
          file.path(
            dir,
            stringr::str_c("hist_", remove_punct(value_var), ".pdf")
          )
        )
      }
    }
  )

  purrr::walk(
    c("size", "hom.tot"),
    function(weight_var) {
      plot_histogram(
        data = data,
        value_var = "race",
        weight_var = weight_var
      ) |>
      save_ggplot(
        file.path(
          dir,
          stringr::str_c("hist_race_", remove_punct(weight_var), ".pdf")
        )
      )
    }
  )
}

plot_pair_all <- function(data, dir, facet_num_cols) {
  y_var <- "hom.own"
  purrr::walk(
    c(MEAN_COLUMNS, "race"),
    function(x_var) {
      plot_pair(
        data = data,
        x_var = x_var,
        y_var = y_var,
        facet_var = if (x_var == "race") NULL else "race",
        facet_num_cols = facet_num_cols,
        weight_var = "size"
      ) |>
      save_ggplot(
        file.path(
          dir,
          stringr::str_c(
            remove_punct(y_var),
            "_vs_",
            remove_punct(x_var),
            ".pdf"
          )
        )
      )
    }
  )
}

plot_choropleth_all <- function(
  data,
  dir,
  area = "county",
  facet_num_cols = 3
) {
  data <- (
    data |>
    dplyr::filter(state %in% STATES_MAINLAND) |>
    join_with_shape(area = area)
  )
  purrr::walk(
    VALUE_COLUMNS,
    function(fill_var) {
      plot_choropleth(
        data,
        fill_var,
        title = fill_var,
        facet_var = "race",
        facet_num_cols = facet_num_cols,
        outline = FALSE,
        color_na = "black",
        free_scales = fill_var == "pop.share",
        log_scale = fill_var %in% c(TOTAL_COLUMNS, "inc.inc"),
        breaks = if (fill_var == "pop.share.ratio") {
          c(0, 1, 2)
        } else if (fill_var == "emp.ue") {
          c(0, 0.05, 0.1, 0.15)
        } else {
          NULL
        }
      ) |>
      save_tmap(
        file.path(
          dir,
          stringr::str_c("choro_", remove_punct(fill_var), ".pdf")
        )
      )
    }
  )
}

make_var_ranges_table <- function() {
  CENSUS_DATA |>
  dplyr::filter(race == "Total") |>
  dplyr::select(
    edu.hs,
    emp.ue,
    hom.own,
    inc.inc,
    pop.tot,
    size
  ) |>
  tidyr::pivot_longer(dplyr::everything()) |>
  dplyr::nest_by(name) |>
  purrr::pmap_dfr(
    function(name, data) {
      tibble::tibble(
        name = !!name,
        low = quantile(data[["value"]], 0.1, na.rm = TRUE),
        high = quantile(data[["value"]], 0.9, na.rm = TRUE)
      )
    }
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    `Variable Name` = c(
      edu.hs = "High school attainment",
      emp.ue = "Unemployment",
      inc.inc = "Annual income",
      hom.own = "Home ownership",
      pop.tot = "Population",
      size = "Sample size"
    )[name],
    `Description (units)` = c(
      edu.hs = "Percentage of 18+ population",
      emp.ue = "Percentage of total labor force",
      inc.inc = "Annual amount in current US\\$",
      hom.own = "Percentage of occupied households",
      pop.tot = "Total inhabitants",
      size = "Total interviews"
    )[name],
    `County Range` = (
      if (
        name %in% c(
          "edu.hs",
          "emp.ue",
          "fin.cost",
          "hom.own",
          "occ.fam",
          "val.mort"
        )
      ) {
        sprintf("%.1f\\%% - %.1f\\%%", low * 100, high * 100)
      } else if (name %in% c("inc.inc", "val.hom", "val.tax")) {
        paste0("\\$", format(signif(round(low), 3), big.mark = ","), " - \\$",
          format(signif(round(high), 3), big.mark = ","))
      } else if (name %in% c("size", "pop.tot")) {
        paste0(format(signif(round(low), 3), big.mark = ","), " - ",
          format(signif(round(high), 3), big.mark = ","))
      } else {
        stop("Impossible.")
      }
    )
  ) |>
  dplyr::ungroup() |>
  tibble::add_row(
    name = "housing_header",
    `Variable Name` = "\\textbf{Housing Variables}"
  ) |>
  tibble::add_row(
    name = "sociodemographic_header",
    `Variable Name` = "\\textbf{Sociodemographic Variables}"
  ) |>
  tibble::add_row(
    name = "survey_header", `Variable Name` = "\\textbf{Survey Variables}"
  ) |>
  dplyr::mutate(
    name = factor(
      name,
      c(
        "housing_header",
        "hom.own",
        "sociodemographic_header",
        "edu.hs",
        "inc.inc",
        "emp.ue",
        "pop.tot",
        "survey_header",
        "size"
      )
    )
  ) |>
  dplyr::arrange(name) |>
  dplyr::select(`Variable Name`, `Description (units)`, `County Range`) |>
  xtable::xtable(align = "rllc") |>
  save_tex_formatted(
    file.path(OUTPUT_EXPLORATORY_DIR, "county_ranges.tex"),
    sanitize.text.function = I
  )
}

make_data_summary <- function() {
  print_summary_column <- function(data, name) {
    cat("Name: ", name, "\n", sep = "")
    cat("Type: ", stringr::str_c(class(data), collapse = ", "), "\n", sep = "")
    if (is.character(data) || is.factor(data)) {
      cat("Levels: ", length(levels(factor(data))), "\n", sep = "")
    } else if (is.numeric(data)) {
      p_values <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)
      quantiles <- quantile(data, p_values, na.rm = TRUE)
      cat(
        stringr::str_c(format(p_values * 100, width = 3), "%: ", quantiles, "\n"),
        sep = ""
      )
    } else {
      stop(stringr::str_c("Unhandled type: ", class(data)))
    }
    cat("NA: ", sum(is.na(data)), "\n\n", sep = "")
  }
  print_summary <- function(data) {
    cat("Num rows: ", nrow(data), "\n", sep = "")
    cat("Num cols: ", ncol(data), "\n", sep = "")
    cat("Col names: ", stringr::str_c(colnames(data), collapse = ", "), "\n\n", sep = "")
    purrr::iwalk(data, print_summary_column)
  }
  save_output(
    (
      CENSUS_DATA |>
      dplyr::mutate(
        state = factor(state),
        county = factor(county),
        geoid = factor(geoid)
      ) |>
      print_summary()
    ),
    file_name = file.path(OUTPUT_EXPLORATORY_DIR, "census_data_summary.txt")
  )
  save_output(
    (
      MODEL_DATA |>
      dplyr::mutate(state = factor(state)) |>
      print_summary()
    ),
    file_name = file.path(OUTPUT_EXPLORATORY_DIR, "model_data_summary.txt")
  )

  data_by_race_mean <- (
    CENSUS_DATA |>
    dplyr::group_by(race) |>
    dplyr::summarize(
      edu.hs = matrixStats::weightedMean(edu.hs, edu.tot, na.rm = TRUE),
      emp.ue = matrixStats::weightedMean(emp.ue, emp.tot, na.rm = TRUE),
      fin.cost = matrixStats::weightedMean(fin.cost, fin.tot, na.rm = TRUE),
      fin.inc = matrixStats::weightedMean(fin.inc, fin.tot, na.rm = TRUE),
      hom.own = matrixStats::weightedMean(hom.own, hom.tot),
      inc.inc = matrixStats::weightedMean(inc.inc, inc.tot, na.rm = TRUE),
      occ.fam = matrixStats::weightedMean(occ.fam, occ.tot),
      pop.share = matrixStats::weightedMean(pop.share, pop.tot.all),
      val.hom = matrixStats::weightedMean(val.hom, val.tot, na.rm = TRUE),
      val.mort = matrixStats::weightedMean(val.mort, val.tot, na.rm = TRUE),
      val.tax = matrixStats::weightedMean(val.tax, val.tot, na.rm = TRUE),
      edu.tot = mean(edu.tot),
      emp.tot = mean(emp.tot),
      fin.tot = mean(fin.tot),
      hom.tot = mean(hom.tot),
      inc.tot = mean(inc.tot),
      occ.tot = mean(occ.tot),
      pop.tot = mean(pop.tot),
      size = mean(size),
      val.tot = mean(val.tot)
    )
  )
  data_by_race_sd <- (
    CENSUS_DATA |>
    dplyr::group_by(race) |>
    dplyr::summarize(
      edu.hs = matrixStats::weightedSd(edu.hs, edu.tot, na.rm = TRUE),
      emp.ue = matrixStats::weightedSd(emp.ue, emp.tot, na.rm = TRUE),
      fin.cost = matrixStats::weightedSd(fin.cost, fin.tot, na.rm = TRUE),
      fin.inc = matrixStats::weightedSd(fin.inc, fin.tot, na.rm = TRUE),
      hom.own = matrixStats::weightedSd(hom.own, hom.tot),
      inc.inc = matrixStats::weightedSd(inc.inc, inc.tot, na.rm = TRUE),
      occ.fam = matrixStats::weightedSd(occ.fam, occ.tot),
      pop.share = matrixStats::weightedSd(pop.share, pop.tot.all),
      val.hom = matrixStats::weightedSd(val.hom, val.tot, na.rm = TRUE),
      val.mort = matrixStats::weightedSd(val.mort, val.tot, na.rm = TRUE),
      val.tax = matrixStats::weightedSd(val.tax, val.tot, na.rm = TRUE),
      edu.tot = sd(edu.tot),
      emp.tot = sd(emp.tot),
      fin.tot = sd(fin.tot),
      hom.tot = sd(hom.tot),
      inc.tot = sd(inc.tot),
      occ.tot = sd(occ.tot),
      pop.tot = sd(pop.tot),
      size = sd(size),
      val.tot = sd(val.tot)
    )
  )

  data_by_race_mean |>
  save_csv(file.path(OUTPUT_EXPLORATORY_DIR, "census_data_by_race_means.csv"))

  # Get the race dot chart data
  data_by_race_mean <- (
    data_by_race_mean |>
    dplyr::select(
      race,
      hom.own,
      edu.hs,
      inc.inc,
      emp.ue,
      pop.share
    ) |>
    dplyr::mutate(
      hom.own = 100 * hom.own,
      pop.share = 100 * pop.share,
      edu.hs = 100 * edu.hs,
      emp.ue = 100 * emp.ue,
      inc.inc = inc.inc / 1000
    ) |>
    tidyr::pivot_longer(
      cols = !race,
      names_to = "var",
      values_to = "val"
    )
  )
  data_by_race_sd <- (
    data_by_race_sd |>
    dplyr::select(
      race,
      hom.own,
      edu.hs,
      inc.inc,
      emp.ue,
      pop.share
    ) |>
    dplyr::mutate(
      hom.own = 100 * hom.own,
      pop.share = 100 * pop.share,
      edu.hs = 100 * edu.hs,
      emp.ue = 100 * emp.ue,
      inc.inc = inc.inc / 1000
    ) |>
    tidyr::pivot_longer(
      cols = !race,
      names_to = "var",
      values_to = "val"
    )
  )

  data_by_race <- (
    dplyr::inner_join(
      data_by_race_mean,
      data_by_race_sd,
      by = c("race", "var"),
      suffix = c("_mean", "_sd")
    ) |>
    dplyr::mutate(var = factor(var)) |>
    dplyr::mutate(
      var = forcats::fct_recode(
        var,
        "Home Ownership (%)" = "hom.own",
        "High School Education (%)" = "edu.hs",
        "Income ($1000)" = "inc.inc",
        "Unemployment (%)" = "emp.ue",
        "Population Share (%)" = "pop.share"
      )
    )
  )

  # Make the dot chart
  ggplot_out <- (
    data_by_race |>
    dplyr::nest_by(var, .keep = TRUE) |>
    purrr::pmap(
      function(var, data) {
        # For ordering from lowest to highest in each panel
        data <- (
          data |>
          dplyr::arrange(val_mean) |>
          dplyr::mutate(race_2 = paste(1:nrow(data), race))
        )

        # Order by frequency and make the plot
        list(
          ggplot2::geom_hline( # The dotted line
            data = data,
            mapping = ggplot2::aes(yintercept = race_2),
            linetype = "dotted",
            alpha = 0.3
          ),
          ggplot2::geom_point( # The mean points
            data = data,
            mapping = ggplot2::aes(
              x = val_mean,
              y = race_2,
              color = race
            ),
            size = 2
          ),
          ggplot2::geom_errorbarh( # The SD error bars
            data = data,
            mapping = ggplot2::aes(
              y = race_2,
              xmin = val_mean - val_sd,
              xmax = val_mean + val_sd,
              color = race
            ),
            height = 0.5,
            linewidth = 1
          )
        )
      }
    ) |>
    purrr::list_c() |>
    purrr::reduce(`+`, .init = ggplot2::ggplot())
  )
  
  (
    ggplot_out +
    ggplot2::scale_color_manual(values = c(RACE_COLORS, Total = "#000000")) +
    ggplot2::facet_wrap(dplyr::vars(var), ncol = 1, scales = "free") +
    ggplot2::theme_classic(GGPLOT_BASE_SIZE_BIG) +
    ggplot2::ylab("Group") +
    ggplot2::xlab("Mean +/- SD") +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        x |>
        stringr::str_split(" ") |>
        purrr::map_chr(\(x) x[[2]])
      }
    ) +
    ggplot2::theme(legend.position = "none")
  ) |>
  save_ggplot(
    file.path(OUTPUT_EXPLORATORY_DIR, "census_data_by_race_dot_chart.pdf"),
    height = 2 * GGPLOT_HEIGHT
  )

  data_by_state <- (
    CENSUS_DATA |>
    dplyr::group_by(state) |>
    dplyr::summarize(
      edu.hs = matrixStats::weightedMean(edu.hs, edu.tot, na.rm = TRUE),
      emp.ue = matrixStats::weightedMean(emp.ue, emp.tot, na.rm = TRUE),
      fin.cost = matrixStats::weightedMean(fin.cost, fin.tot, na.rm = TRUE),
      fin.inc = matrixStats::weightedMean(fin.inc, fin.tot, na.rm = TRUE),
      hom.own = matrixStats::weightedMean(hom.own, hom.tot),
      inc.inc = matrixStats::weightedMean(inc.inc, inc.tot, na.rm = TRUE),
      occ.fam = matrixStats::weightedMean(occ.fam, occ.tot),
      pop.share = matrixStats::weightedMean(pop.share, pop.tot.all),
      val.hom = matrixStats::weightedMean(val.hom, val.tot, na.rm = TRUE),
      val.mort = matrixStats::weightedMean(val.mort, val.tot, na.rm = TRUE),
      val.tax = matrixStats::weightedMean(val.tax, val.tot, na.rm = TRUE),
      edu.tot = mean(edu.tot),
      emp.tot = mean(emp.tot),
      fin.tot = mean(fin.tot),
      hom.tot = mean(hom.tot),
      inc.tot = mean(inc.tot),
      occ.tot = mean(occ.tot),
      pop.tot = mean(pop.tot),
      size = mean(size),
      val.tot = mean(val.tot)
    )
  )

  data_by_state |>
  save_csv(file.path(OUTPUT_EXPLORATORY_DIR, "census_data_by_state_means.csv"))
}

plot_state_ranges <- function() {
  (
    CENSUS_DATA |>
    dplyr::filter(race %in% RACES_MODEL) |>
    dplyr::group_by(race, state) |>
    dplyr::summarize(
      hom.own_mean = 100 * matrixStats::weightedMean(hom.own, hom.tot, na.rm = TRUE),
      hom.own_sd = 100 * matrixStats::weightedSd(hom.own, hom.tot, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      state = (
        state |>
        forcats::fct_relabel(\(x) STATE_ABBREVS[x]) |>
        forcats::fct_rev()
      ),
      race = factor(race, RACES_MODEL)
    ) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        y = state,
        x = hom.own_mean,
        xmin = hom.own_mean - hom.own_sd,
        xmax = hom.own_mean + hom.own_sd,
        color = race,
        fill = race
      )
    ) +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge2(1)) +
    ggplot2::scale_color_manual(values = RACE_COLORS) +
    ggplot2::scale_fill_manual(values = RACE_COLORS) +
    ggplot2::geom_hline(
      data = tidyr::expand_grid(
        y = STATE_ABBREVS,
        race = factor(RACES_MODEL)
      ),
      mapping = ggplot2::aes(yintercept = y),
      linetype = "dotted",
      alpha = 0.3
    ) +
    ggplot2::facet_wrap(vars(race), nrow = 2, scales = "free") +
    ggplot2::xlim(c(0, 100)) +
    ggplot2::ylab("State") +
    ggplot2::xlab("Home ownership rate (%)") +
    ggplot2::theme_classic(GGPLOT_BASE_SIZE_BIG) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_text(size = 8)
    )
  ) |>
  save_ggplot(
    file.path(OUTPUT_EXPLORATORY_DIR, "state_ranges.pdf"),
    width = GGPLOT_WIDTH,
    height = 1.5 * GGPLOT_HEIGHT
  )
}

make_binomial_model <- function() {
  model <- glm(
    formula = MODEL_INFO[["0"]][["formula"]],
    data = MODEL_DATA,
    family = binomial
  )

  save_rds(
    model,
    file = file.path(
      OUTPUT_EXPLORATORY_DIR,
      "model_rds",
      stringr::str_c("model_0.rds")
    )
  )

  save_output(
    print(summary(model)),
    file_name = file.path(
      OUTPUT_EXPLORATORY_DIR,
      "model_summary",
      stringr::str_c("summary_0.txt")
    )
  )
}

do_binomial_model_analysis <- function() {
  model_data <- get_model_data()
  glm_out <- load_model(model_name = "0")

  # Coefficient summary
  save_output(
    {
      cat("Coefficient Estimates\n")
      print(summary(glm_out))
      cat(stringr::str_c(rep("-", 100), collapse = ""))
      cat("\n")
      cat("Coefficient Summary\n")
      print(quantile(glm_out[["coefficients"]], seq(0, 1, by = 0.1)))
      cat(stringr::str_c(rep("-", 100), collapse = ""))
      cat("\n")
      cat("Linear residual Summary\n")
      print(
        quantile(
          (
            (qlogis(model_data[["hom.own"]]) - predict(glm_out, type = "link")) |>
            purrr::keep(is.finite)
          ),
          seq(0, 1, by = 0.1)
        )
      )
    },
    file_name = file.path(OUTPUT_EXPLORATORY_DIR, "binomial_analysis", "binomial_summary.txt")
  )

  # Plot simulated residual histogram vs. true residuals
  resid <- residuals(glm_out, type = "pearson")
  prob_hat <- predict(glm_out, type = "response")
  sd_hat <- sqrt(prob_hat * (1 - prob_hat) / model_data[["size"]])
  num_reps <- 100
  num_obs <- length(resid)
  resid_sim <- sapply(
    seq_len(num_reps),
    function(...) {
      x <- rbinom(length(resid), size = model_data[["size"]], prob = prob_hat)
      x <- x / model_data[["size"]]
      (x - prob_hat) / sd_hat
    }
  )
  resid_data <- tibble::tibble(
    resid = resid,
    size = model_data[["size"]],
    type = "Observed"
  )
  resid_sim_data <- tibble::tibble(
    resid = as.vector(resid_sim),
    size = rep(model_data[["size"]], num_reps),
    type = "Simulated",
    iter = rep(seq_len(num_reps), each = num_obs)
  )
  (
    ggplot2::ggplot() +
    ggplot2::geom_freqpoly(
      data = resid_sim_data,
      mapping = ggplot2::aes(
        x = resid,
        group = iter,
        weight = size
      ),
      alpha = 0.1,
      bins = 30
    ) +
    ggplot2::geom_freqpoly(
      data = resid_data,
      mapping = ggplot2::aes(
        x = resid,
        weight = size
      ),
      bins = 30
    ) +
    ggplot2::facet_wrap(vars(type), ncol = 2, scales = "free") +
    ggplot2::theme_classic(base_size = GGPLOT_BASE_SIZE_SMALL) +
    ggplot2::xlab("Standardized residual") +
    ggplot2::ylab("Count")
  ) |>
  save_ggplot(
    file.path(OUTPUT_EXPLORATORY_DIR, "binomial_analysis", "overdispersion.pdf"),
    width = GGPLOT_WIDTH,
    height = GGPLOT_HEIGHT / 3
  )

  # Phi estimation
  phi_data <- tibble::tibble(
    resid_sq = resid^2,
    sample_size = model_data[["size"]],
    county_size = model_data[["hom.tot"]],
    phi = (sample_size - 1) / (resid_sq - 1) - 1
  ) |>
  dplyr::filter(phi > 0)

  phi_lm_out_1 <- lm(log(phi) ~ 1, data = phi_data)
  phi_lm_out_2 <- lm(log(phi) ~ log(county_size), data = phi_data)

  file_out <- file.path(OUTPUT_EXPLORATORY_DIR, "binomial_analysis", "phi_coeff.txt")
  create_parent_dir(file_out)
  save_output(
    {
      cat("Coefficient Estimates (log(phi) ~ 1)\n")
      print(summary(phi_lm_out_1))
      cat(stringr::str_c(rep("-", 100), collapse = ""))
      cat("\n")

      cat("Residual Summary (log(phi) ~ 1)\n")
      print(quantile(residuals(phi_lm_out_2), seq(0, 1, by = 0.1)))
      cat(stringr::str_c(rep("-", 100), collapse = ""))
      cat("\n")

      cat("Coefficient Estimates (log(phi) ~ log(county_size))\n")
      print(summary(phi_lm_out_2))
      cat(stringr::str_c(rep("-", 100), collapse = ""))
      cat("\n")

      cat("Residual Summary (log(phi) ~ 1)\n")
      print(quantile(residuals(phi_lm_out_2), seq(0, 1, by = 0.1)))
    },
    file_name = file_out
  )

  # log(phi) vs. log(population) plot
  (
    ggplot2::ggplot(phi_data, ggplot2::aes(log(county_size), log(phi))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      formula = y ~ x,
      method = "lm",
      se = FALSE,
      color = "red",
      linewidth = 2,
      linetype = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = phi_lm_out_1[["coefficients"]][[1]],
      color = "blue",
      linewidth = 2,
      linetype = "dashed"
    ) +
    ggplot2::xlab("Log population") +
    ggplot2::ylab("Log precision estimate") +
    ggplot2::theme_classic(base_size = GGPLOT_BASE_SIZE_SMALL)
  ) |>
  save_ggplot(
    file.path(OUTPUT_EXPLORATORY_DIR, "binomial_analysis", "phi_scatter.pdf"),
    width = GGPLOT_WIDTH / 2,
    height = GGPLOT_HEIGHT / 2
  )
}

do_missing_value_analysis <- function() {
  data <- (
    CENSUS_DATA |>
    dplyr::select(
      race,
      size,
      edu.hs,
      emp.ue,
      inc.inc.trans = inc.inc, # To correspond with model variables
      hom.own
    ) |>
    dplyr::filter(race %in% RACES_MODEL) |>
    dplyr::mutate(
      edu.hs = is.na(edu.hs),
      emp.ue = is.na(emp.ue),
      inc.inc.trans = is.na(inc.inc.trans),
      hom.own = is.na(hom.own)
    ) |>
    dplyr::mutate(All = edu.hs | emp.ue | inc.inc.trans | hom.own) |>
    tidyr::pivot_longer(
      !c(race, size),
      names_to = "variable",
      values_to = "missing"
    ) |>
    dplyr::mutate(missing = ifelse(missing, 1, 0)) |>
    # Make the "All" race data
    x => dplyr::bind_rows(
      x,
      x |> dplyr::mutate(race = "All")
    ) |>
    dplyr::reframe(
      data = list(
        list(weight_type = "size", missing_type = "Present", count = sum((1 - missing) * size)),
        list(weight_type = "size", missing_type = "Missing", count = sum(missing * size)),
        list(weight_type = "obs", missing_type = "Present", count = sum(1 - missing)),
        list(weight_type = "obs", missing_type = "Missing", count = sum(missing))
      ),
      .by = c("race", "variable")
    ) |>
    tidyr::unnest_wider(data) |>
    dplyr::mutate(
      variable = replace_all_pairs(
        variable,
        COLUMNS_PAPER_OLD,
        COLUMNS_PAPER_NEW
      ),
      missing_type = factor(missing_type, c("Present", "Missing")),
      race = factor(race, c(RACES_MODEL, "All"))
    ) |>
    dplyr::arrange(variable, race, weight_type, missing_type)
  )

  purrr::walk(
    c("size", "obs"),
    function(weight_type) {
      # barplot
      (
        data |>
        dplyr::filter(weight_type == !!weight_type) |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = missing_type,
            y = count,
            label = count,
            fill = race
          )
        ) +
        ggplot2::geom_col() +
        ggplot2::geom_text(vjust = -.5) +
        ggplot2::scale_fill_manual(values = c(All = "#808080", RACE_COLORS)) +
        ggplot2::ylab("Count") +
        ggplot2::facet_grid(
          rows = ggplot2::vars(variable),
          cols = ggplot2::vars(race),
          scales = "free"
        ) +
        ggplot2::theme_classic(GGPLOT_BASE_SIZE_SMALL) +
        ggplot2::theme(
          legend.position = "none",
          panel.border = ggplot2::element_rect(fill = NA, linewidth = 1),
          axis.title.x = ggplot2::element_blank()
        )
      ) |>
      save_ggplot(
        file.path(
          OUTPUT_EXPLORATORY_DIR,
          "missing_values",
          stringr::str_c("missing_barplot_", weight_type, ".pdf")
        )
      )
    }
  )

  # Missing table
  data |>
  dplyr::nest_by(race, variable, weight_type) |>
  purrr::pmap_dfr(
    function(race, variable, weight_type, data) {
      tibble::tibble(
        race = !!race,
        variable = !!variable,
        weight_type = !!weight_type,
        missing_pct = if (!!weight_type == "size") {
          data |>
          dplyr::summarize(pct = 100 * count[missing_type == "Missing"] / sum(count)) |>
          dplyr::pull(pct) |>
          x => sprintf("%04.2f", x)
        } else if (!!weight_type == "obs") {
          data |>
          dplyr::summarize(pct = 100 * count[missing_type == "Missing"] / sum(count)) |>
          dplyr::pull(pct) |>
          x => sprintf("%05.2f", x)
        }
      )
    }
  ) |>
  tidyr::pivot_wider(
    id_cols = c(variable, race),
    names_from = weight_type,
    values_from = missing_pct
  ) |>
  dplyr::mutate(
    missing_pct = stringr::str_c(size, "/", obs),
    variable = factor(variable, c("All", "ownersp", "income", "hsedu", "unemp"))
  ) |>
  dplyr::mutate(
    variable = ifelse(
      variable == "All",
      "All",
      stringr::str_c("$\\mathit{", variable, "}$")
    )
  ) |>
  tidyr::pivot_wider(
    id_cols = race,
    names_from = variable,
    values_from = missing_pct
  ) |>
  dplyr::select(
    race,
    `$\\mathit{ownersp}$`,
    `$\\mathit{income}$`,
    `$\\mathit{hsedu}$`,
    `$\\mathit{unemp}$`,
    All
  ) |>
  dplyr::rename_with(function(x) " ", race) |>
  xtable::xtable(align = "rl|llll|l") |>
  save_tex_formatted(
    file.path(
      OUTPUT_EXPLORATORY_DIR,
      "missing_values",
      stringr::str_c("missing_table.tex")
    ),
    sanitize.text.function = base::I,
    hline_after_extra = 4
  )
}

make_model_formula_table <- function() {
  get_formula <- function(formula_obj, response) {
    strs <- as.character(formula_obj)
    if ((length(strs) != 3) || (strs[[1]] != "~")) {
      stop(stringr::str_interp("Invalid formula: ${strs}"))
    }

    strs[[3]] |>
    stringr::str_split(" ") |>
    purrr::pluck(1) |>
    x => (
      if (is.null(response)) {
        c(" & ", x)
      } else {
        c(response, " & \\sim ", x)
      }
    ) |>
    replace_all_pairs(COLUMNS_PAPER_OLD, COLUMNS_PAPER_NEW) |>
    stringr::str_replace("^([a-z]+)$", "\\\\mathit{\\1}") |>
    stringr::str_c(collapse = " ") |>
    stringr::str_split(" \\+ ") |>
    purrr::pluck(1) |>
    x => {
      groups <- rep(1, length(x))
      if (
        stringr::str_detect(x[[length(x)]], "popshareratio") &&
        stringr::str_detect(x[[length(x)]], "group") &&
        (groups[[length(x) - 1]] == groups[[length(x)]])
      ) {
        groups[[length(x)]] <- groups[[length(x)]] + 1
      }
      split(x, groups)
    } |>
    purrr::map(stringr::str_c, collapse = " + ") |>
    purrr::flatten_chr() |>
    stringr::str_c(collapse = " + \\\\ & ")
  }

  data <- purrr::imap_dfr(
    MODEL_INFO,
    function(model_info, model_name) {
      formula_obj <- model_info[["formula"]]
      reponse_name <- COLUMNS_PAPER_NEW[[
        which(COLUMNS_PAPER_OLD == "hom.own.count")
      ]]
      if ("brmsformula" %in% class(formula_obj)) {
        main_formula <- (
          formula_obj[["formula"]] |>
          get_formula(NULL)
        )
        phi_formula <- (
          formula_obj[["pforms"]][["phi"]] |>
          get_formula(NULL)
        )
      } else if ("formula" %in% class(formula_obj)) {
        main_formula <- (
          formula_obj |>
          get_formula(NULL)
        )
        phi_formula <- NA
      }

      tibble::tibble(
        model_name = model_name,
        main_formula = main_formula,
        phi_formula = phi_formula
      )
    }
  )

  data |>
  purrr::pmap_dfr(
    function(model_name, main_formula, phi_formula, N_formula) {
      list(
        `Proportion ($\\theta$)` = main_formula,
        `Precision ($\\phi$)` = phi_formula
      ) |>
      purrr::map(function(x) stringr::str_c("$\\begin{aligned} ", x, " \\end{aligned}$")) |>
      tibble::as_tibble() |>
      x => dplyr::bind_cols(tibble::tibble(Model = model_name), x)
    }
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      function(x) tidyr::replace_na(x, "N/A")
    )
  ) |>
  xtable::xtable() |>
  save_tex(
    file.path(OUTPUT_EXPLORATORY_DIR, "model_formula_table", "model_formula_table.tex"),
    sanitize.text.function = I
  )
}

make_size_table <- function() {
  columns <- c(
    "S1501" = "edu.tot",
    "S2301" = "emp.tot",
    "S2503" = "fin.tot",
    "S2502" = "hom.tot",
    "S1903" = "inc.tot",
    "S2501" = "occ.tot",
    "S2506+ S2507*" = "val.tot",
    "B98001" = "size"
  )
  desc <- c(
    "S1501" = "Education",
    "S2301" = "Employment",
    "S2503" = "Financial characteristics",
    "S2502" = "Housing demographics",
    "S1903" = "Income",
    "S2501" = "Housing occupancy characteristics",
    "S2506+ S2507*" = "Housing financial characteristics",
    "B98001" = "Survey sample size"
  )
  samp = c(
    "S1501" = "Population aged 18+",
    "S2301" = "Population aged 16+",
    "S2503" = "Occupied housing units",
    "S2502" = "Occupied housing units",
    "S1903" = "Occupied housing units",
    "S2501" = "Occupied housing units",
    "S2506+ S2507*" = "Owned housing units",
    "B98001" = "Interviewed households"
  )

  label <- function(x) {
    format(signif(x / 1000, 3), big.mark = ",")
  }

  CENSUS_DATA |>
  dplyr::filter(race == "Total") |>
  dplyr::select(dplyr::all_of(columns)) |>
  dplyr::summarize(
    dplyr::across(
      dplyr::everything(),
      function(x) {
        low <- quantile(x, 0.1)
        high <- quantile(x, 0.9)
        paste0(label(low), " - ", label(high))
      }
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "Table",
    values_to = "Size"
  ) |>
  dplyr::mutate(
    Description = desc[`Table`],
    Sample = samp[`Table`]
  ) |>
  dplyr::select(`Table`, Description, Sample, Size) |>
  dplyr::rename(`Size (thousands)` = Size) |>
  xtable::xtable() |>
  save_tex(
    file.path(OUTPUT_EXPLORATORY_DIR, "size_table", "size_table.tex")
  )
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

do_all_exploratory_analyses <- function() {
  plot_pair_all(
    CENSUS_DATA,
    file.path(OUTPUT_EXPLORATORY_DIR, "pairs"),
    facet_num_cols = 2
  )
  plot_histogram_all(
    CENSUS_DATA,
    file.path(OUTPUT_EXPLORATORY_DIR, "histogram")
  )
  plot_choropleth_all(
    CENSUS_DATA,
    file.path(OUTPUT_EXPLORATORY_DIR, "choropleth"),
    facet_num_cols = 2
  )
  make_var_ranges_table()
  make_data_summary()
  plot_state_ranges()
  make_binomial_model()
  do_binomial_model_analysis()
  make_outlier_tables()
  do_missing_value_analysis()
  make_model_formula_table()
  make_size_table()
}
