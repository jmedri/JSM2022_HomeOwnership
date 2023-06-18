# JSM2022_HomeOwnership

Code for the data processing the model fitting for the article titled "Exploring Racial and Ethnic Differences in US Home Ownership with
Bayesian Beta-Binomial Regression" (Jhonathan Medri, Tejasvi Channagiri, and Lu Lu, 2023) to the [Journal of Data Science](https://jds-online.org/journal/JDS) (JDS). This is a follow-up to a presentation at the Joint Statistical Meeting (JSM) 2022 Data Expo Challenge administered by the Sections on Statistical Computing, Statistical Graphics, and Government Statistics of the American Statistical Association (ASA).

## US Census Bureau ACS Data

The sociodemographic data was obtained from [data.census.gov](https://data.census.gov/cedsci/). The following tables were used:

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

The following files must be downloaded from the supplemental data and placed into the directory "input". These files have been downloaded directly from US Census Bureau websites [data.census.gov](https://data.census.gov).

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

Then run the scripts "do_process_data.R" and "do_analysis.R" in that order to reproduce all data processing and analyses. The data processing is expected to take less than 10 minutes on a standard PC, while the analysis make take between 2 to 5 hours, since it fits several models using MCMC.

## Shiny App

We developed a Shiny app to allow users the ability to further explore the data and model presented here. The app has the following visualization options:

* Box Plots: visualize the distribution of different variables in our data.
* Scatter Plots: visualize the relationship between two differerent variables in our data.
* Time Series Plots: visualize the trend of different variables in our data.
* Choropleth Maps: visualize the distribution of different variables across the United States main land.
* Predictive Models: visualize the posterior predictive home ownership rates from our final selected model.

While the publication focused on data for the year 2020, the Shiny app allows users to explore data from years 2015 to 2020. Moreover, many variables not considered in the modeling are available for visualization. The app also allows visualizing data at the state level (over the whole US) or at the county level (for individual states).

To run the app the data must have already been processed using the "do_process_data.R" script. Then the "run_app.R" script must be run. The app should start up in less than a minute after the "run_app.R" is run.

## Contact

The maintainers of this repository are:
* Tejasvi Channagiri (tchannagiri@gmail.com)
* Jhonathan Medri (jm192@usf.edu)