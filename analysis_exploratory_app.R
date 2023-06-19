do_all_exploratory_analyses_app <- function() {
  #Exporting plots to Article (County data)

  #Scatterplot of NY (not broken by race)
  (
    plot_app(
      area = "County",
      Stated = "New York",
      Group1 = "Total",
      Group2 = "None",
      Group3 = "None",
      Group4 = "None",
      Vary = "Own",
      Varx = "LInc",
      Plot = "SC",
      SC.Smoother = "lm"
    ) +
    ggplot2::xlim(4.4, 5.2) +
    ggplot2::ylim(10, 80) +
    ggplot2::scale_color_manual(values = "black")
  ) |>
  save_ggplot(
    file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "sc_imd_ny.pdf"),
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
  )

  #Scatterplot of NY (broken by race)
  (
    plot_app(
      area = "County",
      Stated = "New York",
      Group1 = "WhiteNH",
      Group2 = "Hispanic", 
      Group3 = "Black",
      Group4 = "Asian",
      Vary = "Own",
      Varx = "LInc",
      Plot = "SC",
      SC.Smoother = "lm"
    ) +
    ggplot2::xlim(4.4, 5.2) +
    ggplot2::ylim(10, 80)
  ) |>
  save_ggplot(
    file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "sc_imd_raceny.pdf"),
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
  )

  #Boxplots of homeownership that shows differences between races
  plot_app(
    area = "County",
    Group1 = "WhiteNH", 
    Group2 = "Hispanic",
    Group3 = "Black",
    Group4 = "Asian",
    Plot = "BP"
  ) |>
  save_ggplot(
    file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "bp_imd.pdf"),
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
  )

  #Choropleth of homeownership (greens)
  plot_choropleth_app(
    col_pal = "Greens",
    Group1 = "Total",
    free_scales = TRUE
  ) |>
  save_tmap(
    file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "choro_greens.pdf"),
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
  )

  #Choropleth of homeownership (grays)
  plot_choropleth_app(
    col_pal = "Greys",
    Group1 = "Total",
    free_scales = TRUE
  ) |>
  save_tmap(
    file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "choro_greys.pdf"),
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
  )
}