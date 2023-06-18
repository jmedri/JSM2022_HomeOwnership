# depends on library(tidyverse)
# depends on library(sf)
# depends on constants.R

process_shape_data_unzip <- function() {
  unzip(
    file.path(INPUT_DIR, "cb_2020_us_county_20m.zip"),
    exdir = file.path(
      INPUT_PROCESSED_DIR,
      "census_shape",
      "cb_2020_us_county_20m"
    )
  )
  unzip(
    file.path(INPUT_DIR, "cb_2020_us_state_20m.zip"),
    exdir = file.path(
      INPUT_PROCESSED_DIR,
      "census_shape",
      "cb_2020_us_state_20m"
    )
  )
}

process_shape_data_county <- function() {
  us_county <- sf::st_read(file.path(
    INPUT_PROCESSED_DIR,
    "census_shape",
    "cb_2020_us_county_20m",
    "cb_2020_us_county_20m.shx"
  ))
  us_state <- (
    sf::st_read(
      file.path(
        INPUT_PROCESSED_DIR,
        "census_shape",
        "cb_2020_us_state_20m",
        "cb_2020_us_state_20m.shx"
      )
    ) |>
      remove_spatial()
  )
  us_county |>
    dplyr::left_join(
      us_state |> dplyr::select(STATEFP, NAME),
      by = "STATEFP",
      suffix = c("", "_state")
    ) |>
    dplyr::rename(state = NAME_state, county = NAME, geoid = GEOID) |>
    dplyr::filter(state %in% STATES) |>
    dplyr::mutate(
      county = stringr::str_c(state, county, sep = "/"),
      geoid = as.integer(geoid)
    ) |>
    dplyr::select(state, county, geoid, geometry) |>
    dplyr::arrange(state, county, geoid) |>
    save_st(
      driver = "ESRI Shapefile",
      append = FALSE,
      file_name = file.path(INPUT_PROCESSED_DIR, "census_shape", "county_processed")
    )
}

process_shape_data_state <- function() {
  sf::st_read(
    file.path(
      INPUT_PROCESSED_DIR,
      "census_shape",
      "cb_2020_us_state_20m",
      "cb_2020_us_state_20m.shx"
    )
  ) |>
    dplyr::rename(state = NAME) |>
    dplyr::filter(state %in% STATES) |>
    dplyr::select(state, geometry) |>
    dplyr::arrange(state) |>
    save_st(
      file_name = file.path(INPUT_PROCESSED_DIR, "census_shape", "state_processed"),
      driver = "ESRI Shapefile",
      append = FALSE
    )
}

process_shape_data_all <- function() {
  process_shape_data_unzip()
  process_shape_data_county()
  process_shape_data_state()
}

load_shape_data <- function(recompute = FALSE) {
  STATE_SHAPE_DATA <<- sf::st_read(file.path(
    INPUT_PROCESSED_DIR,
    "census_shape",
    "state_processed"
  ))
  COUNTY_SHAPE_DATA <<- sf::st_read(file.path(
    INPUT_PROCESSED_DIR,
    "census_shape",
    "county_processed"
  ))
}

join_with_shape <- function(data, spatial_unit = "county") {
  #' @title join_with_shape()
  #' @description Joins the data frame with shape data.
  #' For this function to work the global variables COUNTY_SHAPE_DATA and
  #' STATE_SHAPE_DATA must have the corresponding shape data.
  #' @param data Data frame with data to plot. If spatial_unit == "county"
  #' data must contain a "geoid" data must contain a "geoid" column
  #' that corresponds with the geoid's in US_COUNTY[[x]]. If spatial_unit == "state",
  #' data must contain a "state" column with the names of states.
  #' @param spatial_unit Either "county" or "state" depending on whether county or state
  #' data should be joined with data.
  (
    if (spatial_unit == "county") {
      dplyr::full_join(
        COUNTY_SHAPE_DATA |> dplyr::select(geoid, geometry),
        data,
        by = "geoid"
      ) |>
      dplyr::arrange(state, county, geoid, race)
    } else if (spatial_unit == "state") {
      dplyr::full_join(STATE_SHAPE_DATA, data, by = "state") |>
      dplyr::arrange(state, race)
    } else {
      stop(stringr::str_c("Unknown spatial unit: ", spatial_unit))
    }
  ) |>
  sf::st_as_sf()
}
