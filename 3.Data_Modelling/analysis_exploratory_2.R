

#-----------------------------------------------------------------------------

#Exporting plots to Article (County data)

#Figure 1a: Scatterplot of NY broken (Not broken by race)
#6x8
sc_imd_ny <-
  plot_app(area = "County", Stated = "New York",
        Group1 = "Total", Group2 = "None", Group3 = "None", Group4 = "None",
        Vary = "Own", Varx = "LInc",
        Plot = "SC", SC.Smoother = "lm") + theme(legend.position = "none")+
  xlim(4.4,5.2) + ylim(10,80) + scale_color_manual(values = "black")
ggsave(
  file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "sc_imd_ny.pdf"),
  plot = sc_imd_ny, width = 8, height = 6, units = "in", dpi = 300)

#Figure 1b: Scatterplot of NY (Broken by race)
#6x8
sc_imd_raceny <-
  plot_app(area = "County", Stated = "New York",
        Group1 = "WhiteNH", Group2 = "Hispanic", 
        Group3 = "Black", Group4 = "Asian",
        Vary = "Own", Varx = "LInc",
        Plot = "SC", SC.Smoother = "lm")+
  xlim(4.4,5.2) + ylim(10,80) 
ggsave(
  file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "sc_imd_raceny.pdf"),
  plot = sc_imd_raceny, width = 8, height = 6, units = "in", dpi = 300)

#Figure 2: Boxplots of homeownership that shows differences between races
#6x8
bp_imd <-
  plot_app(area = "County", Group1 = "WhiteNH", 
        Group2 = "Hispanic", Group3 = "Black", Group4 = "Asian",
        Plot = "BP")
ggsave(
  file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "bp_imd.pdf"),
  plot = bp_imd, width = 8, height = 6, units = "in", dpi = 300)

#Figure 3: Choropleth Map
chloro_greens <- plot_chloropleth_app(col_pal = "Greens", group1 = "Total", free_scales = TRUE)
tmap_save(chloro_greens, file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "choro_greens.pdf"),
  width = 8, height = 6, units = "in", dpi = 300)

chloro_greys <- plot_chloropleth_app(col_pal = "Greys", group1 = "Total", free_scales = TRUE)
tmap_save(chloro_greys, file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "choro_greys.pdf"),
  width = 8, height = 6, units = "in", dpi = 300)
