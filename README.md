# JSM2022_HomeOwnership

Code for the data processing the model fitting for the article titled "Exploring Racial and Ethnic Differences in US Home Ownership with Bayesian Beta-Binomial Regression" (Jhonathan Medri, Tejasvi Channagiri, and Lu Lu, 2023) submitted to the [Journal of Data Science](https://jds-online.org/journal/JDS) (JDS). This is a follow-up to a presentation given  at the Joint Statistical Meeting (JSM) 2022 Data Expo Challenge administered by the Sections on Statistical Computing, Statistical Graphics, and Government Statistics of the American Statistical Association (ASA).

## US Census Bureau ACS Data

The sociodemographic data was obtained from [data.census.gov](https://data.census.gov/). The following tables were used:

* S1501: Educational attainment.
* S1903: Median income in the past 12 months.
* S2301: Employment status.
* S2501: Occupancy characteristics.
* S2502: Demographic characteristics for occupied housing units.
* S2503: Financial characteristics.
* S2504: Physical housing characteristics for occupied housing units.
* S2506: Financial characteristics for housing units with a mortgage.
* S2507: Financial characteristics for housing units without a mortgage.
* DP05: ACS demographic and housing estimates.
* B98001: Unweighted housing unit sample.

The data was aggregated at the county level and comes from the American Community Survey (ACS) 5-year estimate tables for the years 2015-2020.

## US Census Bureau Cartographic Boundary Files

The shape file for plotting choropleths of US counties and states was downloaded from the US Census website [here](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html) and [here](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html).

## Dependencies

This code had been tested with R version 4.2.2. The following packages are also required. These packages require explicit installation but may themselves have further dependencies that will also be installed.

* tidyverse 2.0.0
* ggdist 3.2.1
* sf 1.0.12
* tmap 3.3.3
* xtable 1.8.4
* rstan 2.26.13
* brms 2.19.0
* posterior 1.4.1
* matrixStats 0.63.0
* extraDistr 1.9.1

The Shiny app requires the following additional packages.

* shiny 1.7.4
* shinythemes 1.2.0
* shinyWidgets 0.7.6

## Reproducing Analyses

The following raw data files are already included in the repository in the "data/input" directory. These files have been downloaded directly from US Census Bureau websites [data.census.gov](https://data.census.gov) and [www.census.gov](https://www.census.gov).

* DP05.zip
* B98001.zip
* S1501.zip
* S1903.zip
* S2301.zip
* S2501.zip
* S2502.zip
* S2503.zip
* S2506.zip
* S2507.zip
* cb_2015_us_county_20m.zip
* cb_2015_us_state_20m.zip
* cb_2016_us_county_20m.zip
* cb_2016_us_state_20m.zip
* cb_2017_us_county_20m.zip
* cb_2017_us_state_20m.zip
* cb_2018_us_county_20m.zip
* cb_2018_us_state_20m.zip
* cb_2019_us_county_20m.zip
* cb_2019_us_state_20m.zip
* cb_2020_us_county_20m.zip
* cb_2020_us_state_20m.zip

To reproduce all data processing and analyses, run the scripts "do_process_data.R" and "do_analysis.R" in that order. The data processing is expected to take less than 10 minutes on a standard PC, while the analysis make take between 2 to 5 hours, since it fits several models using MCMC.

## Shiny App

We developed a Shiny app to allow users the ability to further explore the data and model presented here. The app has the following visualization options:

* Box Plots: Visualize the distribution of different variables in our data.
* Scatter Plots: Visualize the relationship between two differerent variables in our data.
* Time Series Plots: Visualize the temporal trend of different variables in our data.
* Choropleth Maps: Visualize the spatial distribution of different variables across the mainland United States.
* Predictive Models: Visualize the posterior predictive home ownership rates from our fitted models. This version has been revised since JSM 2022.
* Predictive Models (JSM 2022): Visualize the posterior predictive home ownership rates from our fitted model. This version was presented as JSM 2022.

While the publication focused on data for the year 2020, the Shiny app allows users to explore data from years 2015 to 2020. Moreover, many variables not considered in the modeling are available for visualization. The app also allows visualizing data at the state level (over the whole US) or at the county level (for individual states).

To run the app the data must have already been processed using the "do_process_data.R" script. Then the "run_app.R" script must be run. The app should start up in less than a minute after the "run_app.R" is run.

## Figure List

The following figures/tables are used in the publication should be produced by running the "do_analysis.R" script (see [Reproducing Analyses](#reproducing-analyses)). When reproducing the analysis, some figures/tables may look slightly different due to randomness in the MCMC initialization, though we expect posterior mean estimates to remain nearly identical. They are listed in order of appearance in the publication.

* Table 1: "data/output_exploratory/census_data_by_race_means.csv" (only part of this data is shown in the publication).
* Figure 1a: "data/output_exploratory/data_exploration/sc_imd_ny.pdf".
* Figure 1b: "data/output_exploratory/data_exploration/sc_imd_raceny.pdf".
* Figure 2a: "data/output_exploratory/data_exploration/bp_imd.pdf".
* Figure 2b: "data/output_exploratory/data_exploration/choro_greys.pdf".
* Table 2: "data/output_exploratory/model_formula_table/model_formula_table.tex".
* Table 3a: "data/output_1_posterior/model_coef/model_coef_4.csv".
* Table 3b: "data/output_1_posterior/model_coef/model_coef_4_exp.csv".
* Table 4a: "data/output_1_posterior/model_effect_cases/FloridaHillsboroughCounty_4.tex".
* Table 4b: "data/output_1_posterior/model_effect_cases/NewYorkNewYorkCounty_4.tex".
* Table S1: "data/output_exploratory/size_table/size_table.tex".
* Table S2: "data/output_exploratory/county_ranges.tex".
* Figure S1: "data/output_exploratory/census_data_by_race_dot_chart.pdf".
* Table S3: Made by hand with information from [data.census.gov](https://data.census.gov/).
* Table S4: "data/output_exploratory/missing_values/missing_table.tex".
* Figure S2: "data/output_exploratory/state_ranges.pdf".
* Figure S3a: "data/output_exploratory/binomial_analysis/overdispersion.pdf".
* Figure S3b: "data/output_exploratory/binomial_analysis/phi_scatter.pdf".
* Figure S4: "data/output_1_posterior/loo_analysis/loo_figure.pdf".
* Figure S5a:
  * Density: "data/output_1_posterior/post_pred_density/post_pred_density_4.pdf".
  * Mean: "data/output_1_posterior/post_pred_stat_mean/post_pred_stat_mean_4.pdf".
  * Variance: "data/output_1_posterior/post_pred_stat_variance/post_pred_stat_variance_4.pdf".
* Figure S5b:
  * Density: "data/output_1_prior/post_pred_density/post_pred_density_4.pdf".
  * Mean: "data/output_1_prior/post_pred_stat_mean/post_pred_stat_mean_4.pdf".
  * Variance: "data/output_1_prior/post_pred_stat_variance/post_pred_stat_variance_4.pdf".
* Table S5a: "data/output_1_posterior/model_coef/model_coef_4_state.csv".
* Table S5b: "data/output_1_posterior/model_coef/model_coef_4_state_exp.csv".
* Figures S6, S7: Made by taking screenshots after running "run_app.R" (see [Shiny App](#shiny-app)).

## Contact

The maintainers of this repository are:

* Tejasvi Channagiri (tchannagiri@gmail.com)
* Jhonathan Medri (jm192@usf.edu)