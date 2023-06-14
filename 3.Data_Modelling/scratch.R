source("3.Data_Modelling/initialize.R")
initialize()

CENSUS_DATA |>
  dplyr::group_by(race) |>
  dplyr::summarize(
    edu.hs = weighted.mean(edu.hs, edu.tot, na.rm = TRUE),
    emp.ue = weighted.mean(emp.ue, emp.tot, na.rm = TRUE),
    hom.own = weighted.mean(hom.own, hom.tot, na.rm = TRUE),
    inc.inc = weighted.mean(inc.inc, inc.tot, na.rm = TRUE)
  )

CENSUS_DATA |>
  dplyr::group_by(race) |>
  dplyr::summarize(
    edu.hs = sum(edu.hs * edu.tot, na.rm = TRUE),
    emp.ue = sum(emp.ue * emp.tot, na.rm = TRUE),
    hom.own = sum(hom.own * hom.tot, na.rm = TRUE),
    inc.inc = sum(inc.inc * inc.tot, na.rm = TRUE)
  )

CENSUS_DATA |>
  dplyr::group_by(race) |>
  dplyr::summarize(
    edu.tot = sum(edu.tot, na.rm = TRUE),
    emp.tot = sum(emp.tot, na.rm = TRUE),
    hom.tot = sum(hom.tot, na.rm = TRUE),
    inc.tot = sum(inc.tot, na.rm = TRUE)
  )

# do_all_compute_models()
# do_all_model_analyses()

# Some old prior analysis
# initialize("3", TRUE)
# compute_model("1")
# {
#   x <- load_model("1")
#   y <- brms::posterior_predict(x, ndraws = 100)
#   y <- sweep(y, 2, MODEL_DATA$size, FUN = "/")
#   g <- ggplot2::ggplot()
#   for (i in 1:100) {
#     g <- g + ggplot2::geom_histogram(
#       mapping = ggplot2::aes(x, weight = w),
#       data = data.frame(x = y[i,], f = i, w = MODEL_DATA$size),
#       # alpha = 0.1,
#       linewidth = 0.1
#     )
#   }
#   g <- g + ggplot2::geom_histogram(
#     mapping = ggplot2::aes(x, weight = w),
#     data = data.frame(x = MODEL_DATA$hom.own.count / MODEL_DATA$size,  w = MODEL_DATA$size, f = 101),
#     fill = "red"
#   )
#   g <- g + facet_wrap(~ f, ncol = 10)
#   ggsave(paste0("test_", PRIOR_TYPE, ".png"), plot = g, width = 20, height = 20)
# }
# qlogis(weighted.mean(MODEL_DATA$hom.own, MODEL_DATA$size, na.rm = TRUE))
# plot_post_pred_stat_all()
# plot_post_pred_density_all()


# initialize("2", FALSE)
# do_all_compute_models()

initialize("1", FALSE)
# compute_model("4", 8000, overwrite = TRUE)
do_all_model_analyses()
initialize("1", TRUE)
do_all_model_analyses()
