# JSM2022_HomeOwnership

# R Shiny

Code for the data processing the model fitting for the article titled "Exploring Racial and Ethnic Trends in US Home Ownership with
Bayesian Beta-Binomial Regression" (Jhonathan Medri, Tejasvi Channagiri, and Lu Lu, 2023) to the [Journal of Data Science](https://jds-online.org/journal/JDS) (JDS). This is a follow-up to a presentation at the Joint Statistical Meeting (JSM) 2022 Data Expo Challenge administered by the Sections on Statistical Computing, Statistical Graphics, and Government Statistics of the American Statistical Association (ASA).

## US Census Bureau ACS Data

The sociodemographic data was obtained from [data.census.gov](https://data.census.gov/cedsci/). The following tables were used:

* S1501: Educational attainment.
* S1903: Median income in the past 12 months (in 2020 inflation-adjusted dollars).
* S2301: Employment status.
* S2501: Occupancy characteristics.
* S2502: Demographic characteristics for occupied housing units.
* S2503: Financial characteristics.
* S2506: Financial characteristics for housing units with a mortgage.
* S2507: Financial characteristics for housing units without a mortgage.
* DP05: ACS demographic and housing estimates.
* B98001: Unweighted housing unit sample.

The data was aggregated at the county level and comes from the American Community Survey (ACS) 5-year estimate tables for the year 2020.

## US Census Bureau Cartographic Boundary Files

The shape file for plotting chloropleths of US counties and states was downloaded from the US Census website [here](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html). The files used were:

* [cb_2020_us_county_20m.zip](https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_20m.zip)
* [cb_2020_us_state_20m.zip](https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_20m.zip)

## Dependencies
This code had been tested with R version 4.2.2. The following packages are also required. These packages require explicit installation but may themselves have further dependencies that will also be installed.

* tidyverse 1.3.2
* ggdist 3.2.0
* sf 1.0.9
* tmap 3.3.3
* xtable 1.8.4
* rstan 2.26.13
* brms 2.18.0
* posterior 1.3.1
* matrixStats 0.62.0
* extraDistr 1.9.1

## Reproducing Analyses

The following files must be downloaded from the supplemental data and placed into the directory "input". These files have been downloaded directly from US Census Bureau websites (see above) and are not further modified in any way.

* ACSDP5Y2020.DP05_2022-11-09T180054.zip
* ACSDT5Y2020.B98001_2022-11-09T180622.zip
* ACSST5Y2020.S1501_2022-11-09T180209.zip
* ACSST5Y2020.S1903_2022-11-09T180317.zip
* ACSST5Y2020.S2301_2022-11-09T180421.zip
* ACSST5Y2020.S2501_2022-11-21T140942.zip
* ACSST5Y2020.S2502_2022-11-09T180449.zip
* ACSST5Y2020.S2503_2022-11-21T072023.zip
* ACSST5Y2020.S2506_2022-11-21T071729.zip
* ACSST5Y2020.S2507_2022-11-21T071846.zip
* cb_2020_us_county_20m.zip
* cb_2020_us_state_20m.zip

Then run the scripts "do_process_data.R" and "do_analysis.R" in that order to reproduce all data processing and analyses.