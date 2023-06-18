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
  "White (non-Hispanic)" = "WhiteNH",
  "Black" = "Black",
  "Hispanic" = "Hispanic",
  "Asian" = "Asian",
  "None" = "None"
)

RACES_MODEL_APP <- RACES_APP[RACES_APP %in% RACES_MODEL]

VARS_APP <- c(
  "Home Ownership Rate (%)" = "Own",
  "High School Completion (%)" = "HS",
  "Bachelor Degree Completion (%)" = "BS",
  "Household Annual Income (Current US$)" = "Inc",
  "Log10 Annual Income (Current US$)" = "LInc",
  "Population Size" = "Pop",
  "Log10 Population Size" = "LPop",
  "Population Share (%)" = "Pop_Share",
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
