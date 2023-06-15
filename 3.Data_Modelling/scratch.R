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

initialize("1", FALSE)
xxx <- load_model("4") |> brms::posterior_epred() |> colMeans()
xxx <- xxx / MODEL_DATA$size

initialize("3", FALSE)
yyy <- load_model("4") |> brms::posterior_epred() |> colMeans()
yyy <- yyy / MODEL_DATA$size
sqrt(mean((xxx - yyy)^2))

cbind(xxx, yyy)[1:10,]

plot(xxx, yyy)

cor(xxx, yyy)


sss$fixed

for (prior_type in c("1", "2", "3", "4")) {
  initialize(prior_type, FALSE)
  # compute_model("4")
  # do_all_model_analyses()
  make_model_coef_table()
  make_mean_vs_model_table()
  make_model_effect_county("New York/New York County")
  make_model_effect_county("Florida/Hillsborough County")
  initialize(prior_type, TRUE)
  make_model_coef_table()
  make_mean_vs_model_table()
  make_model_effect_county("New York/New York County")
  make_model_effect_county("Florida/Hillsborough County")
  # compute_model("4")
  # do_all_model_analyses()
}
