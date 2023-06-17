RACES <- c(
  "Total",
  "WhiteNH",
  "Hispanic",
  "Black",
  "Asian"
)

# 50 US states
STATES <- c(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "West Virginia",
  "Wisconsin",
  "Wyoming"
)

# 48 contiguous US states
CONTIGUOUS_STATES <- STATES[!(STATES %in% c("Alaska", "Hawaii"))]

COLUMNS_RENAME <- c(
  "geoid" = "GEO_ID",
  "state" = "State",
  "county" = "County",
  "race" = "Group",
  "size" = "B98001.interviews",
  "pop.tot" = "DP05.group.pop",
  "edu.tot" = "S1501.group.total",
  "edu.hs" = "S1501.group.HSt",
  "edu.bs" = "S1501.group.BSt",
  "inc.tot" = "S1903.group.total",
  "inc.inc" = "S1903.group.medianincome",
  "emp.tot" = "S2301.group.total",
  "emp.ue" = "S2301.group.ue",
  "occ.tot" = "S2501.total",
  "occ.fam" = "S2501.householdtype.family",
  "hom.own" = "S2502.group.owned",
  "hom.tot" = "S2502.group.total",
  "fin.tot" = "S2503.total",
  "fin.inc" = "S2503.income.median",
  "fin.cost" = "S2503.cost.median",
  "mort.tot" = "S2506.total",
  "mort.val" = "S2506.value.median",
  "mort.tax" = "S2506.tax.median",
  "nmort.tot" = "S2507.total",
  "nmort.val" = "S2507.value.median",
  "nmort.tax" = "S2507.tax.median"
)

ID_COLUMNS <- c("state", "county", "geoid", "race")

COLUMN_NAMES <- (
  names(COLUMNS_RENAME) |>
    setdiff(ID_COLUMNS) |>
    purrr::discard(stringr::str_detect, "mort|nmort") |>
    append(
      c(
        "val.tot",
        "val.hom",
        "val.tax",
        "val.mort",
        "pop.tot.all",
        "pop.tot",
        "pop.share",
        "pop.share.ratio"
      )
    ) |>
    sort() |>
    append(ID_COLUMNS, 0)
)

VALUE_COLUMNS <- setdiff(COLUMN_NAMES, ID_COLUMNS)

TOTAL_COLUMNS <- VALUE_COLUMNS |> stringr::str_subset("size|tot")

MEAN_COLUMNS <- setdiff(VALUE_COLUMNS, TOTAL_COLUMNS)

RACE_COLUMNS <- (
  COLUMNS_RENAME |>
    purrr::keep(stringr::str_detect, "DP05|S1501|S1903|S2301|S2502") |>
    names() |>
    append(c("pop.share", "pop.share.ratio", "size"))
)

NON_RACE_COLUMNS <- setdiff(VALUE_COLUMNS, RACE_COLUMNS)

MODEL_COLUMNS <- c(
  "state",
  "race",
  "edu.hs",
  "emp.ue",
  "hom.own",
  "hom.tot",
  "inc.inc",
  "size"
)

GGPLOT_WIDTH <- 10 # inches
GGPLOT_HEIGHT <- 10 # inches
TMAP_WIDTH <- 5000 # pixels
TMAP_HEIGHT <- 5000 # pixels

GGPLOT_BASE_SIZE_BIG <- 20
GGPLOT_BASE_SIZE_SMALL <- 16

RACE_COLORS <- c(
  WhiteNH = "#e41a1c",
  Hispanic = "#984ea3",
  Black = "#377eb8",
  Asian = "#4daf4a"
)

COLUMNS_RENAME_PAPER <- c(
  "group" = "race",
  "hsedu" = "edu.hs",
  "unemp" = "emp.ue",
  "owners" = "hom.own.count",
  "ownersp" = "hom.own",
  "income" = "inc.inc.trans",
  "sampsize" = "size",
  "logpop" = "hom.tot.log",
  "pop" = "hom.tot"
)

COLUMNS_PAPER_NEW <- names(COLUMNS_RENAME_PAPER)
COLUMNS_PAPER_OLD <- unname(COLUMNS_RENAME_PAPER)

STATE_ABBREVS <- c(
  "Alabama" = "AL",
  "Alaska" = "AK",
  "Arizona" = "AZ",
  "Arkansas" = "AR",
  "California" = "CA",
  "Colorado" = "CO",
  "Connecticut" = "CT",
  "Delaware" = "DE",
  "Florida" = "FL",
  "Georgia" = "GA",
  "Hawaii" = "HI",
  "Idaho" = "ID",
  "Illinois" = "IL",
  "Indiana" = "IN",
  "Iowa" = "IA",
  "Kansas" = "KS",
  "Kentucky" = "KY",
  "Louisiana" = "LA",
  "Maine" = "ME",
  "Maryland" = "MD",
  "Massachusetts" = "MA",
  "Michigan" = "MI",
  "Minnesota" = "MN",
  "Mississippi" = "MS",
  "Missouri" = "MO",
  "Montana" = "MT",
  "Nebraska" = "NE",
  "Nevada" = "NV",
  "New Hampshire" = "NH",
  "New Jersey" = "NJ",
  "New Mexico" = "NM",
  "New York" = "NY",
  "North Carolina" = "NC",
  "North Dakota" = "ND",
  "Ohio" = "OH",
  "Oklahoma" = "OK",
  "Oregon" = "OR",
  "Pennsylvania" = "PA",
  "Rhode Island" = "RI",
  "South Carolina" = "SC",
  "South Dakota" = "SD",
  "Tennessee" = "TN",
  "Texas" = "TX",
  "Utah" = "UT",
  "Vermont" = "VT",
  "Virginia" = "VA",
  "Washington" = "WA",
  "West Virginia" = "WV",
  "Wisconsin" = "WI",
  "Wyoming" = "WY"
)
