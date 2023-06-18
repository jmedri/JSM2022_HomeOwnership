# depends on library(tidyverse)
# depends on library(sf)
# depends on constants.R

get_name_shape_data <- function(area, year) {
  paste0("cb_", year, "_us_", area, "_20m")
}

process_shape_data_unzip <- function() {
  for (year in YEARS) {
    county_name <- get_name_shape_data("county", year)
    state_name <- get_name_shape_data("state", year)
    unzip(
      file.path(INPUT_DIR, paste0(county_name, ".zip")),
      exdir = file.path(
        INPUT_PROCESSED_DIR,
        "census_shape",
        county_name
      )
    )
    unzip(
      file.path(INPUT_DIR, paste0(state_name, ".zip")),
      exdir = file.path(
        INPUT_PROCESSED_DIR,
        "census_shape",
        state_name
      )
    )
  }
}

process_shape_data_county <- function() {
  for (year in YEARS) {
    name_county <- get_name_shape_data("county", year)
    name_state <- get_name_shape_data("state", year)
    us_county <- sf::st_read(
      file.path(
        INPUT_PROCESSED_DIR,
        "census_shape",
        name_county,
        paste0(name_county, ".shx")
      )
    )
    us_state <- (
      sf::st_read(
        file.path(
          INPUT_PROCESSED_DIR,
          "census_shape",
          name_state,
          paste0(name_state, ".shx")
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
      county = paste(state, county, sep = "/"),
      geoid = as.integer(geoid)
    ) |>
    dplyr::select(state, county, geoid, geometry) |>
    dplyr::arrange(state, county, geoid) |>
    save_st(
      driver = "ESRI Shapefile",
      append = FALSE,
      file_name = file.path(
        INPUT_PROCESSED_DIR,
        "census_shape",
        paste0("county_processed_", as.character(year))
      )
    )
  }
}

process_shape_data_state <- function() {
  for (year in YEARS) {
    name_state <- get_name_shape_data("state", year)
    sf::st_read(
      file.path(
        INPUT_PROCESSED_DIR,
        "census_shape",
        name_state,
        paste0(name_state, ".shx")
      )
    ) |>
    dplyr::rename(state = NAME) |>
    dplyr::filter(state %in% STATES) |>
    dplyr::select(state, geometry) |>
    dplyr::arrange(state) |>
    save_st(
      file_name = file.path(
        INPUT_PROCESSED_DIR,
        "census_shape",
        paste0("state_processed_", as.character(year))
      ),
      driver = "ESRI Shapefile",
      append = FALSE
    )
  }
}

process_shape_data_all <- function() {
  process_shape_data_unzip()
  process_shape_data_county()
  process_shape_data_state()
}

load_processed_shape_data <- function(area, year) {
  sf::st_read(
    file.path(
      INPUT_PROCESSED_DIR,
      "census_shape",
      paste0(area, "_processed_", year)
    )
  )
}

load_shape_data <- function() {
  STATE_SHAPE_DATA <<- load_processed_shape_data("state", 2020)
  COUNTY_SHAPE_DATA <<- load_processed_shape_data("county", 2020)
}

join_with_shape <- function(data, area = "county", year = 2020) {
  #' @title join_with_shape()
  #' @description Joins the data frame with shape data.
  #' For this function to work the global variables COUNTY_SHAPE_DATA and
  #' STATE_SHAPE_DATA must have the corresponding shape data.
  #' @param data Data frame with data to plot. If area == "county"
  #' data must contain a "geoid" data must contain a "geoid" column
  #' that corresponds with the geoid's in US_COUNTY[[x]]. If area == "state",
  #' data must contain a "state" column with the names of states.
  #' @param area Either "county" or "state" depending on whether county or state
  #' data should be joined with data.
  (
    if (area == "county") {
      shape_data <- load_processed_shape_data("county", year)
      dplyr::full_join(
        shape_data |> dplyr::select(geoid, geometry),
        data,
        by = "geoid"
      ) |>
      dplyr::arrange(state, county, geoid, race)
    } else if (area == "state") {
      shape_data <- load_processed_shape_data("state", year)
      dplyr::full_join(shape_data, data, by = "state") |>
      dplyr::arrange(state, race)
    } else {
      stop(stringr::str_c("Unknown area: ", area))
    }
  ) |>
  sf::st_as_sf()
}
