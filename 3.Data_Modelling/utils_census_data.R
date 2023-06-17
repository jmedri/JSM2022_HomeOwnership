process_census_data_1_unzip <- function() {
  file_info_list <- list(
    list(table = "DP05", zip = "ACSDP5Y2020.DP05_2022-11-09T180054.zip", files = c("ACSDP5Y2020.DP05-Data.csv", "ACSDP5Y2020.DP05-Column-Metadata.csv")),
    list(table = "B98001", zip = "ACSDT5Y2020.B98001_2022-11-09T180622.zip", files = c("ACSDT5Y2020.B98001-Data.csv", "ACSDT5Y2020.B98001-Column-Metadata.csv")),
    list(table = "S1501", zip = "ACSST5Y2020.S1501_2022-11-09T180209.zip", files = c("ACSST5Y2020.S1501-Data.csv", "ACSST5Y2020.S1501-Column-Metadata.csv")),
    list(table = "S1903", zip = "ACSST5Y2020.S1903_2022-11-09T180317.zip", files = c("ACSST5Y2020.S1903-Data.csv", "ACSST5Y2020.S1903-Column-Metadata.csv")),
    list(table = "S2301", zip = "ACSST5Y2020.S2301_2022-11-09T180421.zip", files = c("ACSST5Y2020.S2301-Data.csv", "ACSST5Y2020.S2301-Column-Metadata.csv")),
    list(table = "S2501", zip = "ACSST5Y2020.S2501_2022-11-21T140942.zip", files = c("ACSST5Y2020.S2501-Data.csv", "ACSST5Y2020.S2501-Column-Metadata.csv")),
    list(table = "S2502", zip = "ACSST5Y2020.S2502_2022-11-09T180449.zip", files = c("ACSST5Y2020.S2502-Data.csv", "ACSST5Y2020.S2502-Column-Metadata.csv")),
    list(table = "S2503", zip = "ACSST5Y2020.S2503_2022-11-21T072023.zip", files = c("ACSST5Y2020.S2503-Data.csv", "ACSST5Y2020.S2503-Column-Metadata.csv")),
    list(table = "S2506", zip = "ACSST5Y2020.S2506_2022-11-21T071729.zip", files = c("ACSST5Y2020.S2506-Data.csv", "ACSST5Y2020.S2506-Column-Metadata.csv")),
    list(table = "S2507", zip = "ACSST5Y2020.S2507_2022-11-21T071846.zip", files = c("ACSST5Y2020.S2507-Data.csv", "ACSST5Y2020.S2507-Column-Metadata.csv"))
  )
  for (file_info in file_info_list) {
    unzip(file.path(INPUT_DIR, file_info[["zip"]]), files = file_info[["files"]], exdir = file.path(INPUT_PROCESSED_DIR, "census", "1_unzip"))
  }
}

# Double check
process_census_data_2_long <- function() {
  table_info_list <- list(
    list(
      file = "ACSDP5Y2020.DP05-Data.csv",
      columns = list(
        list(annotation = "Estimate!!RACE!!Total population", variable = "DP05.group.pop", race = "Total"),
        list(annotation = "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!White alone", variable = "DP05.group.pop", race = "WhiteNH"),
        list(annotation = "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)", variable = "DP05.group.pop", race = "Hispanic"),
        list(annotation = "Estimate!!RACE!!Total population!!One race!!Black or African American", variable = "DP05.group.pop", race = "Black"),
        list(annotation = "Estimate!!RACE!!Total population!!One race!!Asian", variable = "DP05.group.pop", race = "Asian")
      )
    ),
    list(
      file = "ACSDT5Y2020.B98001-Data.csv",
      columns = list(
        list(annotation = "Estimate!!Initial addresses selected", variable = "B98001.addresses", race = "Total"),
        list(annotation = "Estimate!!Final number of housing unit interviews", variable = "B98001.interviews", race = "Total")
      )
    ),
    list(
      file = "ACSST5Y2020.S1501-Data.csv",
      columns = list(
        # We will add the 18-24 and the 25+ age groups to get the total
        list(annotation = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 18 to 24 years", variable = "S1501.group.total.1", race = "Total"),
        list(annotation = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over", variable = "S1501.group.total.2", race = "Total"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!White alone, not Hispanic or Latino", variable = "S1501.group.total", race = "WhiteNH"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Hispanic or Latino Origin", variable = "S1501.group.total", race = "Hispanic"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Black alone", variable = "S1501.group.total", race = "Black"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Asian alone", variable = "S1501.group.total", race = "Asian"),
        list(annotation = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 18 to 24 years!!High school graduate (includes equivalency)", variable = "S1501.group.edu.1", race = "Total"),
        list(annotation = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 18 to 24 years!!Some college or associate's degree", variable = "S1501.group.edu.2", race = "Total"),
        list(annotation = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 18 to 24 years!!Bachelor's degree or higher", variable = "S1501.group.edu.3", race = "Total"),
        list(annotation = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate or higher", variable = "S1501.group.edu.4", race = "Total"),
        list(annotation = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher", variable = "S1501.group.edu.5", race = "Total"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!White alone, not Hispanic or Latino!!High school graduate or higher", variable = "S1501.group.HSt", race = "WhiteNH"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Hispanic or Latino Origin!!High school graduate or higher", variable = "S1501.group.HSt", race = "Hispanic"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Black alone!!High school graduate or higher", variable = "S1501.group.HSt", race = "Black"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Asian alone!!High school graduate or higher", variable = "S1501.group.HSt", race = "Asian"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!White alone, not Hispanic or Latino!!Bachelor's degree or higher", variable = "S1501.group.BSt", race = "WhiteNH"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Hispanic or Latino Origin!!Bachelor's degree or higher", variable = "S1501.group.BSt", race = "Hispanic"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Black alone!!Bachelor's degree or higher", variable = "S1501.group.BSt", race = "Black"),
        list(annotation = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Asian alone!!Bachelor's degree or higher", variable = "S1501.group.BSt", race = "Asian")
      )
    ),
    list(
      file = "ACSST5Y2020.S1903-Data.csv",
      columns = list(
        list(annotation = "Estimate!!Number!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households", variable = "S1903.group.total", race = "Total"),
        list(annotation = "Estimate!!Number!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households!!White alone, not Hispanic or Latino", variable = "S1903.group.total", race = "WhiteNH"),
        list(annotation = "Estimate!!Number!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households!!Hispanic or Latino origin (of any race)", variable = "S1903.group.total", race = "Hispanic"),
        list(annotation = "Estimate!!Number!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households!!One race--!!Black or African American", variable = "S1903.group.total", race = "Black"),
        list(annotation = "Estimate!!Number!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households!!One race--!!Asian", variable = "S1903.group.total", race = "Asian"),
        list(annotation = "Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households", variable = "S1903.group.medianincome", race = "Total"),
        list(annotation = "Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households!!White alone, not Hispanic or Latino", variable = "S1903.group.medianincome", race = "WhiteNH"),
        list(annotation = "Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households!!Hispanic or Latino origin (of any race)", variable = "S1903.group.medianincome", race = "Hispanic"),
        list(annotation = "Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households!!One race--!!Black or African American", variable = "S1903.group.medianincome", race = "Black"),
        list(annotation = "Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households!!One race--!!Asian", variable = "S1903.group.medianincome", race = "Asian")
      )
    ),
    list(
      file = "ACSST5Y2020.S2301-Data.csv",
      columns = list(
        list(annotation = "Estimate!!Total!!Population 16 years and over", variable = "S2301.group.total", race = "Total"),
        list(annotation = "Estimate!!Total!!Population 16 years and over!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone, not Hispanic or Latino", variable = "S2301.group.total", race = "WhiteNH"),
        list(annotation = "Estimate!!Total!!Population 16 years and over!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)", variable = "S2301.group.total", race = "Hispanic"),
        list(annotation = "Estimate!!Total!!Population 16 years and over!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone", variable = "S2301.group.total", race = "Black"),
        list(annotation = "Estimate!!Total!!Population 16 years and over!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone", variable = "S2301.group.total", race = "Asian"),
        list(annotation = "Estimate!!Unemployment rate!!Population 16 years and over", variable = "S2301.group.ue", race = "Total"),
        list(annotation = "Estimate!!Unemployment rate!!Population 16 years and over!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone, not Hispanic or Latino", variable = "S2301.group.ue", race = "WhiteNH"),
        list(annotation = "Estimate!!Unemployment rate!!Population 16 years and over!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)", variable = "S2301.group.ue", race = "Hispanic"),
        list(annotation = "Estimate!!Unemployment rate!!Population 16 years and over!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone", variable = "S2301.group.ue", race = "Black"),
        list(annotation = "Estimate!!Unemployment rate!!Population 16 years and over!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone", variable = "S2301.group.ue", race = "Asian")
      )
    ),
    list(
      file = "ACSST5Y2020.S2501-Data.csv",
      columns = list(
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units", variable = "S2501.total", race = "Total"),
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units!!HOUSEHOLD TYPE (INCLUDING LIVING ALONE) AND AGE OF HOUSEHOLDER!!Family households", variable = "S2501.householdtype.family", race = "Total"),
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units!!HOUSEHOLD TYPE (INCLUDING LIVING ALONE) AND AGE OF HOUSEHOLDER!!Nonfamily households", variable = "S2501.householdtype.nonfamily", race = "Total")
      )
    ),
    list(
      file = "ACSST5Y2020.S2502-Data.csv",
      columns = list(
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units", variable = "S2502.group.total", race = "Total"),
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!White alone, not Hispanic or Latino", variable = "S2502.group.total", race = "WhiteNH"),
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Hispanic or Latino origin", variable = "S2502.group.total", race = "Hispanic"),
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!One race --!!Black or African American", variable = "S2502.group.total", race = "Black"),
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!One race --!!Asian", variable = "S2502.group.total", race = "Asian"),
        list(annotation = "Estimate!!Owner-occupied housing units!!Occupied housing units", variable = "S2502.group.owned", race = "Total"),
        list(annotation = "Estimate!!Owner-occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!White alone, not Hispanic or Latino", variable = "S2502.group.owned", race = "WhiteNH"),
        list(annotation = "Estimate!!Owner-occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Hispanic or Latino origin", variable = "S2502.group.owned", race = "Hispanic"),
        list(annotation = "Estimate!!Owner-occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!One race --!!Black or African American", variable = "S2502.group.owned", race = "Black"),
        list(annotation = "Estimate!!Owner-occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!One race --!!Asian", variable = "S2502.group.owned", race = "Asian"),
        list(annotation = "Estimate!!Renter-occupied housing units!!Occupied housing units", variable = "S2502.group.rented", race = "Total"),
        list(annotation = "Estimate!!Renter-occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!White alone, not Hispanic or Latino", variable = "S2502.group.rented", race = "WhiteNH"),
        list(annotation = "Estimate!!Renter-occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Hispanic or Latino origin", variable = "S2502.group.rented", race = "Hispanic"),
        list(annotation = "Estimate!!Renter-occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!One race --!!Black or African American", variable = "S2502.group.rented", race = "Black"),
        list(annotation = "Estimate!!Renter-occupied housing units!!Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!One race --!!Asian", variable = "S2502.group.rented", race = "Asian")
      )
    ),
    list(
      file = "ACSST5Y2020.S2503-Data.csv",
      columns = list(
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units", variable = "S2503.total", race = "Total"),
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units!!HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS)!!Median household income (dollars)", variable = "S2503.income.median", race = "Total"),
        list(annotation = "Estimate!!Occupied housing units!!Occupied housing units!!MONTHLY HOUSING COSTS!!Median (dollars)", variable = "S2503.cost.median", race = "Total")
      )
    ),
    list(
      file = "ACSST5Y2020.S2506-Data.csv",
      columns = list(
        list(annotation = "Estimate!!Owner-occupied housing units with a mortgage!!Owner-occupied housing units with a mortgage", variable = "S2506.total", race = "Total"),
        list(annotation = "Estimate!!Owner-occupied housing units with a mortgage!!Owner-occupied housing units with a mortgage!!VALUE!!Median (dollars)", variable = "S2506.value.median", race = "Total"),
        list(annotation = "Estimate!!Owner-occupied housing units with a mortgage!!Owner-occupied housing units with a mortgage!!REAL ESTATE TAXES!!Median (dollars)", variable = "S2506.tax.median", race = "Total")
      )
    ),
    list(
      file = "ACSST5Y2020.S2507-Data.csv",
      columns = list(
        list(annotation = "Estimate!!Owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage", variable = "S2507.total", race = "Total"),
        list(annotation = "Estimate!!Owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!VALUE!!Median (dollars)", variable = "S2507.value.median", race = "Total"),
        list(annotation = "Estimate!!Owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!REAL ESTATE TAXES!!Median (dollars)", variable = "S2507.tax.median", race = "Total")
      )
    )
  )

  table_data <- (
    purrr::map(
      table_info_list,
      function(table_info) {
        data <- readr::read_csv(file.path(INPUT_PROCESSED_DIR, "census", "1_unzip", table_info[["file"]]))
        annotation_row <- data[1, ]
        data <- data[-1, ]
        purrr::map(
          table_info[["columns"]],
          function(col_info) {
            col_index <- which(annotation_row == col_info[["annotation"]])
            if (length(col_index) != 1) {
              stop(stringr::str_c(
                "Could not find column annotation: ",
                col_info[["annotation"]],
                " in file ",
                table_file
              ))
            }
            old_col_name <- names(data)[[col_index]]
            col_data <-
              list(
                data = (
                  data |>
                    dplyr::select(
                      NAME,
                      GEO_ID,
                      dplyr::all_of(c(value = old_col_name))
                    ) |>
                    dplyr::mutate(
                      variable = col_info[["variable"]],
                      Group = col_info[["race"]]
                    )
                ),
                column_rename = list(
                  tibble::tibble(
                    file = table_info[["file"]],
                    old_col = old_col_name,
                    group = col_info[["race"]],
                    new_col = col_info[["variable"]],
                    annotation = col_info[["annotation"]]
                  )
                )
              )
          }
        )
      }
    ) |>
      do.call(c, args = _)
  )

  (
    table_data |>
      purrr::map(purrr::pluck, "data") |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        NAME = (
          strsplit(NAME, ", ") |>
            purrr::map(rlang::set_names, c("County", "State"))
        )
      ) |>
      tidyr::unnest_wider(NAME) |>
      dplyr::select(State, County, GEO_ID, Group, variable, value) |>
      dplyr::mutate(Group = factor(Group, c("Total", "WhiteNH", "Hispanic", "Black", "Asian"))) |>
      dplyr::arrange(State, County, Group, variable) |>
      save_csv(file.path(INPUT_PROCESSED_DIR, "census", "2_long", "data.csv"))
  )

  (
    table_data |>
      purrr::map(purrr::pluck, "column_rename") |>
      dplyr::bind_rows() |>
      save_csv(file.path(INPUT_PROCESSED_DIR, "census", "2_long", "column_rename.csv"))
  )
}

process_census_data_3_wide <- function() {
  readr::read_csv(file.path(INPUT_PROCESSED_DIR, "census", "2_long", "data.csv")) |>
    tidyr::pivot_wider(
      id_cols = c(State, County, GEO_ID, Group),
      names_from = variable,
      values_from = value
    ) |>
    dplyr::mutate(dplyr::across(!c(State, County, GEO_ID, Group), as.numeric)) |>
    dplyr::nest_by(Group, .keep = TRUE) |>
    purrr::pmap_dfr(function(Group, data) { # For combining the split columns for Total race
      if (Group == "Total") {
        data <- (
          data |>
            dplyr::mutate(
              S1501.group.total = S1501.group.total.1 + S1501.group.total.2,
              S1501.group.HSt = (
                S1501.group.edu.1 +
                  S1501.group.edu.2 +
                  S1501.group.edu.3 +
                  S1501.group.edu.4
              ),
              S1501.group.BSt = S1501.group.edu.3 + S1501.group.edu.5
            )
        )
      }
      data
    }) |>
    dplyr::select(
      !c(
        S1501.group.total.1,
        S1501.group.total.2,
        S1501.group.edu.1,
        S1501.group.edu.2,
        S1501.group.edu.3,
        S1501.group.edu.4,
        S1501.group.edu.5
      )
    ) |>
    x => {
      x |>
        dplyr::select(dplyr::all_of(sort(colnames(x))))
    } |>
    dplyr::relocate(State, County, GEO_ID, Group) |>
    dplyr::arrange(State, County, Group) |>
    save_csv(file.path(INPUT_PROCESSED_DIR, "census", "3_wide", "data.csv"))
}

check_old_new_data <- function() {
  data_new <- readr::read_csv(file.path(INPUT_PROCESSED_DIR, "census", "3_wide", "data.csv"))
  data_old <- (
    readr::read_csv(file.path(INPUT_PROCESSED_DIR, "census", "dataexpo.county.2.csv")) |>
      dplyr::filter(Year == 2020) |>
      dplyr::filter(Group %in% c("Total", "WhiteNH", "Hispanic", "Black", "Asian")) |>
      dplyr::mutate(dplyr::across(!c(State, County, GEO_ID, Group), as.numeric))
  )

  common_columns <- intersect(colnames(data_new), colnames(data_old))
  make_longer <- function(u) {
    u |>
      dplyr::select(dplyr::all_of(common_columns)) |>
      tidyr::pivot_longer(!c(State, County, GEO_ID, Group)) |>
      dplyr::arrange(State, County, Group, name) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        function(v) {
          ifelse(is.na(v), "", as.character(v))
        }
      ))
  }
  data_new <- make_longer(data_new)
  data_old <- make_longer(data_old)

  if (nrow(data_new) != nrow(data_old)) {
    stop(stringr::str_interp("Unequals rows: {{nrow(x)}} {{nrow(y)}}"))
  }

  race_columns <- common_columns |> purrr::keep(stringr::str_detect, "DP05|S1501|S1903|S2301|S2502")
  non_race_columns <- common_columns |> purrr::keep(stringr::str_detect, "S2506|S2507")

  data_new_race <- data_new |> dplyr::filter(name %in% race_columns)
  data_old_race <- data_old |> dplyr::filter(name %in% race_columns)

  if (!all(data_new_race == data_old_race)) {
    stop("Unequal race elements")
  }

  data_new_non_race <- data_new |>
    dplyr::filter(name %in% non_race_columns) |>
    dplyr::filter(Group == "Total")
  data_old_non_race <- data_old |>
    dplyr::filter(name %in% non_race_columns) |>
    dplyr::filter(Group == "Total")

  if (!all(data_new_non_race == data_old_non_race)) {
    stop("Unequal non-race elements")
  }

  print("All good!")
}

fix_geoid <- function(geoid) {
  geoid |>
    stringr::str_replace(stringr::fixed("0500000US"), "") |>
    as.integer()
}

get_pop_size_data <- function(data) {
  data |>
    dplyr::group_by(race) |>
    dplyr::mutate(pop.tot.nation = sum(pop.tot)) |>
    dplyr::ungroup() |>
    dplyr::group_by(state, county, geoid) |>
    dplyr::mutate(
      pop.tot.nation.all = pop.tot.nation[race == "Total"],
      pop.share.nation = pop.tot.nation / pop.tot.nation.all,
      pop.tot.all = pop.tot[race == "Total"],
      pop.share = pop.tot / pop.tot.all,
      pop.share.ratio = pop.share / pop.share.nation,
      size = round(pop.share * size[race == "Total"])
    ) |>
    dplyr::ungroup()
}

process_census_data_4_model <- function() {
  readr::read_csv(file.path(INPUT_PROCESSED_DIR, "census", "3_wide", "data.csv")) |>
    dplyr::select(dplyr::all_of(COLUMNS_RENAME)) |>
    dplyr::filter(state %in% STATES) |>
    dplyr::mutate(dplyr::across(!c(state, county, geoid, race), as.numeric)) |>
    dplyr::mutate(
      geoid = (
        geoid |>
          stringr::str_replace(stringr::fixed("0500000US"), "") |>
          as.integer()
      ),
      county = stringr::str_c(state, "/", county),
      emp.ue = emp.ue / 100,
      edu.hs = edu.hs / edu.tot,
      edu.bs = edu.bs / edu.tot,
      fin.cost = 12 * fin.cost / fin.inc,
      occ.fam = occ.fam / occ.tot,
      hom.own = hom.own / hom.tot,
      val.tot = mort.tot + nmort.tot,
      val.hom = (mort.val * mort.tot + nmort.val * nmort.tot) / val.tot,
      val.tax = (mort.tax * mort.tot + nmort.tax * nmort.tot) / val.tot,
      val.mort = mort.tot / val.tot,
      race = factor(race, RACES)
    ) |>
    get_pop_size_data() |>
    dplyr::group_by(state, county, geoid) |>
    dplyr::mutate(
      fin.cost = fin.cost[race == "Total"],
      fin.inc = fin.inc[race == "Total"],
      fin.tot = fin.tot[race == "Total"],
      occ.fam = occ.fam[race == "Total"],
      occ.tot = occ.tot[race == "Total"],
      val.hom = val.hom[race == "Total"],
      val.mort = val.mort[race == "Total"],
      val.tax = val.tax[race == "Total"],
      val.tot = val.tot[race == "Total"]
    ) |>
    dplyr::select(dplyr::all_of(COLUMN_NAMES)) |>
    dplyr::arrange(state, county, race) |>
    save_csv(file.path(INPUT_PROCESSED_DIR, "census", "4_model", "data.csv"))
}

load_census_data <- function() {
  CENSUS_DATA <<- (
    readr::read_csv(file.path(INPUT_PROCESSED_DIR, "census", "4_model", "data.csv")) |>
      dplyr::mutate(race = factor(race, levels = RACES))
  )
}

process_census_data_all <- function() {
  process_census_data_1_unzip()
  process_census_data_2_long()
  process_census_data_3_wide()
  # This is a check to make sure the new data processing against the old.
  # The resulting data should be almost +/- machine precision errors in the arithmetic.
  # Only used for debugging.
  # check_old_new_data()
  process_census_data_4_model()
}
