CENSUS_TABLES_APP <- c(
  "DP05",
  "S1501",
  "S1903",
  "S2301",
  "S2501",
  "S2502",
  "S2503",
  "S2504",
  "S2506",
  "S2507"
)

HOUSING_TABLES_APP <- c(
  "B98001",
  "S2501",
  "S2502",
  "S2503",
  "S2504",
  "S2506",
  "S2507"
)

RACE_TABLES_APP <- c(
  "DP05",
  "S1501",
  "S1903",
  "S2301",
  "S2502"
)

CENSUS_TABLE_FILE_APP <- c(
  "B98001" = "ACSDT5YXXXX.B98001-Data.csv",
  "DP05" = "ACSDP5YXXXX.DP05-Data.csv",
  "S1501" = "ACSST5YXXXX.S1501-Data.csv",
  "S1903" = "ACSST5YXXXX.S1903-Data.csv",
  "S2301" = "ACSST5YXXXX.S2301-Data.csv",
  "S2501" = "ACSST5YXXXX.S2501-Data.csv",
  "S2502" = "ACSST5YXXXX.S2502-Data.csv",
  "S2503" = "ACSST5YXXXX.S2503-Data.csv",
  "S2504" = "ACSST5YXXXX.S2504-Data.csv",
  "S2506" = "ACSST5YXXXX.S2506-Data.csv",
  "S2507" = "ACSST5YXXXX.S2507-Data.csv"
)

RACES_APP <- c(
  "All Races and Ethnicities" = "Total",
  "White" = "White", 
  "White Non-Hispanic" = "WhiteNH",
  "Hispanic" = "Hispanic",
  "Black" = "Black",
  "Asian" = "Asian",
  "American Indian or Alaska Native" = "AIoAN", 
  "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
  "Other" = "Other",
  "Two or more" = "Two+",
  "None" = "None"
)


RACE_COLORS_APP = c(
  "Total" = "#808080",
  "White" = "#e60049",
  "WhiteNH" = "#dc0ab4",
  "Hispanic" = "#9b19f5",
  "Black" = "#0bb4ff",
  "Asian" = "#00bfa0",
  "AIoAN" =  "#ffa300",
  "NHoOPI" = "#e6d800",
  "Other" = "#b3d4ff",
  "Two+" = "#50e991",
  "None" = "#000000"
)

RACES_MODEL_APP <- RACES_APP[RACES_APP %in% RACES_MODEL]

VARS_APP <- c(
  "Home Ownership Rate (%)" = "Own",
  "High School Completion (%)" = "HS",
  "Bachelor Degree Completion (%)" = "BS",
  "Household Annual Income (Current US$)" = "Inc",
  "Log10 Annual Income (Current US$)" = "LInc",
  "Population (in Millions)" = "Pop",
  "Population Share (%)" = "Pop_Share",
  "Log10 Population" = "LPop",
  "Total Households (in Millions)" = "HTot",
  "Log10 Households" = "LHTot",
  "Unemployment (%)" = "UE"
)

COLOR_PALETTES_APP <- c(
  "Red" = "Reds",
  "Purple" = "Purples",
  "Orange" = "Oranges",
  "Green" = "Greens",
  "Blue" = "Blues",
  "Gray" = "Greys"
)

SMOOTHERS_APP <- c(
  "Linear Model" = "lm",
  "Generalized Linear Model" = "glm",
  "Generalized Additive Model" = "gam",
  "Locally Estimated Scatterplot" = "loess"
)

STATES_APP <- c("All", STATES)

MODEL_NAMES_APP <- c(
  "0 (binomial)" = "0",
  "1 (beta-binomial)" = "1",
  "2 (beta-binomial)" = "2",
  "3 (beta-binomial)" = "3",
  "4 (beta-binomial, final model)" = "4",
  "5 (beta-binomial)" = "5",
  "6 (beta-binomial)" = "6",
  "7 (beta-binomial)" = "7",
  "8 (beta-binomial)" = "8"
)

MODEL_VARS_JS_APP <- (
  "({
    '0': ['state', 'race', 'hs', 'ue', 'inc'],
    '1': ['state', 'race', 'hs', 'ue', 'inc', 'tot'],
    '2': ['state', 'race', 'hs', 'ue', 'inc', 'tot'],
    '3': ['state', 'race', 'hs', 'inc', 'tot'],
    '4': ['state', 'race', 'inc', 'tot'],
    '5': ['state', 'race', 'tot'],
    '6': ['race', 'inc', 'tot'],
    '7': ['state', 'race', 'inc', 'tot'],
    '8': ['state', 'race', 'inc']
  })" |>
  stringr::str_remove_all(stringr::fixed("\n"))
)

MODEL_FILES_APP <- c(
  "0" = "data/output_exploratory/model_rds/model_0.rds",
  "1" = "data/output_1_posterior/model_rds/model_1.rds",
  "2" = "data/output_1_posterior/model_rds/model_2.rds",
  "3" = "data/output_1_posterior/model_rds/model_3.rds",
  "4" = "data/output_1_posterior/model_rds/model_4.rds",
  "5" = "data/output_1_posterior/model_rds/model_5.rds",
  "6" = "data/output_1_posterior/model_rds/model_6.rds",
  "7" = "data/output_1_posterior/model_rds/model_7.rds",
  "8" = "data/output_1_posterior/model_rds/model_8.rds"
)
