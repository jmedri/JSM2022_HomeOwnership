unzip_census_tables <- function() {
  for (table in names(CENSUS_TABLE_FILE_APP)) {
    for (year in YEARS) {
      log_msg("Unzipping", table, year)
      unzip(
        file.path(INPUT_DIR, paste0(table, ".zip")),
        files = stringr::str_replace(
          CENSUS_TABLE_FILE_APP[[table]],
          "XXXX",
          as.character(year)
        ),
        exdir = file.path(INPUT_PROCESSED_DIR, "census_app", "unzip")
      )
    }
  }
}

process_app_housing_data <- function(Table = "S2501", Year = 2020) {
  # Define Location of the File
  file_name <- file.path(
    INPUT_PROCESSED_DIR,
    "census_app",
    "unzip",
    stringr::str_replace(
      CENSUS_TABLE_FILE_APP[[Table]],
      "XXXX",
      as.character(Year)
    )
  )

  # Define and name working variables
  vars_old <- if (Table == "B98001") {
    c("GEO_ID", "NAME", "B98001_001E", "B98001_002E")
  } else if ((Table == "S2501") & (Year <= 2016)) {
    c(
      "GEO_ID", "NAME",
      "S2501_C02_001E", "S2501_C02_002E", "S2501_C02_003E",
      "S2501_C02_004E", "S2501_C02_005E",
      "S2501_C02_009E", "S2501_C02_023E"
    )
  } else if ((Table == "S2501") & (Year >= 2017)) {
    c(
      "GEO_ID", "NAME",
      "S2501_C04_001E", "S2501_C04_002E", "S2501_C04_003E",
      "S2501_C04_004E", "S2501_C04_005E",
      "S2501_C04_009E", "S2501_C04_023E"
    )
  } else if ((Table == "S2502") & (Year <= 2016)) {
    c(
      "GEO_ID", "NAME",
      "S2502_C02_001E", "S2502_C02_010E", "S2502_C02_003E",
      "S2502_C02_005E", "S2502_C02_009E", "S2502_C02_011E",
      "S2502_C02_012E", "S2502_C02_013E", "S2502_C02_014E",
      "S2502_C02_015E", "S2502_C02_016E", "S2502_C02_017E",
      "S2502_C02_018E", "S2502_C02_019E", "S2502_C02_020E",
      "S2502_C02_021E", "S2502_C02_022E", "S2502_C02_023E",
      "S2502_C02_024E", "S2502_C02_025E", "S2502_C02_026E",
      "S2502_C02_027E"
    )
  } else if ((Table == "S2502") & (Year >= 2017)) {
    c(
      "GEO_ID", "NAME",
      "S2502_C04_001E", "S2502_C04_010E", "S2502_C04_003E",
      "S2502_C04_005E", "S2502_C04_009E", "S2502_C04_011E",
      "S2502_C04_012E", "S2502_C04_013E", "S2502_C04_014E",
      "S2502_C04_015E", "S2502_C04_016E", "S2502_C04_017E",
      "S2502_C04_018E", "S2502_C04_019E", "S2502_C04_020E",
      "S2502_C04_021E", "S2502_C04_022E", "S2502_C04_023E",
      "S2502_C04_024E", "S2502_C04_025E", "S2502_C04_026E",
      "S2502_C04_027E"
    )
  } else if ((Table == "S2503") & (Year <= 2016)) {
    c(
      "GEO_ID", "NAME",
      "S2503_C02_001E",
      #Income,,
      "S2503_C02_002E", "S2503_C02_003E", "S2503_C02_004E",
      "S2503_C02_005E", "S2503_C02_006E", "S2503_C02_007E",
      "S2503_C02_008E", "S2503_C02_009E", "S2503_C02_010E",
      "S2503_C02_011E", "S2503_C02_012E", "S2503_C02_013E",
      #Housing Costs,
      "S2503_C02_014E", "S2503_C02_015E", "S2503_C02_016E",
      "S2503_C02_017E", "S2503_C02_018E", "S2503_C02_019E",
      "S2503_C02_020E", "S2503_C02_021E", "S2503_C02_022E",
      "S2503_C02_023E", "S2503_C02_024E",
      #Housing Costs Percentage,
      "S2503_C02_026E", "S2503_C02_027E", "S2503_C02_028E",
      "S2503_C02_030E", "S2503_C02_031E", "S2503_C02_032E",
      "S2503_C02_034E", "S2503_C02_035E", "S2503_C02_036E",
      "S2503_C02_038E", "S2503_C02_039E", "S2503_C02_040E",
      "S2503_C02_042E", "S2503_C02_043E", "S2503_C02_044E"
    )
  } else if ((Table == "S2503") & (Year >= 2017)) {
    c(
      "GEO_ID", "NAME",
      "S2503_C04_001E",
      #Income,,
      "S2503_C04_002E", "S2503_C04_003E", "S2503_C04_004E",
      "S2503_C04_005E", "S2503_C04_006E", "S2503_C04_007E",
      "S2503_C04_008E", "S2503_C04_009E", "S2503_C04_010E",
      "S2503_C04_011E", "S2503_C04_012E", "S2503_C04_013E",
      #Housing Costs,
      "S2503_C04_014E", "S2503_C04_015E", "S2503_C04_016E",
      "S2503_C04_017E", "S2503_C04_018E", "S2503_C04_019E",
      "S2503_C04_020E", "S2503_C04_021E", "S2503_C04_022E",
      "S2503_C04_023E", "S2503_C04_024E",
      #Housing Costs Percentage,
      "S2503_C04_026E", "S2503_C04_027E", "S2503_C04_028E",
      "S2503_C04_030E", "S2503_C04_031E", "S2503_C04_032E",
      "S2503_C04_034E", "S2503_C04_035E", "S2503_C04_036E",
      "S2503_C04_038E", "S2503_C04_039E", "S2503_C04_040E",
      "S2503_C04_042E", "S2503_C04_043E", "S2503_C04_044E"
    )
  } else if ((Table == "S2504") & (Year <= 2016)) {
    c(
      "GEO_ID", "NAME",
      "S2504_C02_001E",
      #Units
      "S2504_C02_002E", "S2504_C02_003E", "S2504_C02_004E", "S2504_C02_005E",
      "S2504_C02_006E", "S2504_C02_007E", "S2504_C02_008E",
      #Year Built
      "S2504_C02_009E", "S2504_C02_010E", "S2504_C02_011E", "S2504_C02_012E",
      "S2504_C02_013E", "S2504_C02_014E", "S2504_C02_015E",
      #Rooms
      "S2504_C02_016E", "S2504_C02_017E", "S2504_C02_018E", "S2504_C02_019E",
      "S2504_C02_020E",
      #Bedrooms
      "S2504_C02_021E", "S2504_C02_022E", "S2504_C02_023E", "S2504_C02_024E",
      #Facilities
      "S2504_C02_025E", "S2504_C02_026E",
      #Vehicle
      "S2504_C02_027E", "S2504_C02_028E", "S2504_C02_029E", "S2504_C02_030E",
      #Telephone
      "S2504_C02_031E",
      #Fuel
      "S2504_C02_032E", "S2504_C02_033E", "S2504_C02_034E", "S2504_C02_035E",
      "S2504_C02_036E", "S2504_C02_037E", "S2504_C02_038E"
    )
  } else if ((Table == "S2504") & (Year >= 2017)) {
    c(
      "GEO_ID", "NAME",
      "S2504_C04_001E",
      #Units
      "S2504_C04_002E", "S2504_C04_003E", "S2504_C04_004E", "S2504_C04_005E",
      "S2504_C04_006E", "S2504_C04_007E", "S2504_C04_008E",
      #Year Built
      "S2504_C04_009E", "S2504_C04_010E", "S2504_C04_011E", "S2504_C04_012E",
      "S2504_C04_013E", "S2504_C04_014E", "S2504_C04_015E",
      #Rooms
      "S2504_C04_016E", "S2504_C04_017E", "S2504_C04_018E", "S2504_C04_019E",
      "S2504_C04_020E",
      #Bedrooms
      "S2504_C04_021E", "S2504_C04_022E", "S2504_C04_023E", "S2504_C04_024E",
      #Facilities
      "S2504_C04_025E", "S2504_C04_026E",
      #Vehicle
      "S2504_C04_027E", "S2504_C04_028E", "S2504_C04_029E", "S2504_C04_030E",
      #Telephone
      "S2504_C04_031E",
      #Fuel
      "S2504_C04_032E", "S2504_C04_033E", "S2504_C04_034E", "S2504_C04_035E",
      "S2504_C04_036E", "S2504_C04_037E", "S2504_C04_038E"
    )
  } else if ((Table == "S2506") & (Year <= 2016)) {
    c(
      "GEO_ID", "NAME",
      "S2506_C01_001E",
      #Value
      "S2506_C01_002E", "S2506_C01_003E", "S2506_C01_004E",
      "S2506_C01_005E", "S2506_C01_006E", "S2506_C01_007E",
      "S2506_C01_008E", "S2506_C01_009E",
      #Status
      "S2506_C01_011E", "S2506_C01_012E",
      "S2506_C01_013E", "S2506_C01_014E",
      #Income (12 months)
      "S2506_C01_015E", "S2506_C01_016E", "S2506_C01_017E",
      "S2506_C01_018E", "S2506_C01_019E", "S2506_C01_020E",
      "S2506_C01_021E", "S2506_C01_022E", "S2506_C01_023E",
      #Ratio
      "S2506_C01_024E", "S2506_C01_025E",
      "S2506_C01_026E", "S2506_C01_027E",
      #Housing Costs
      "S2506_C01_029E", "S2506_C01_030E", "S2506_C01_031E", "S2506_C01_032E",
      "S2506_C01_033E", "S2506_C01_034E", "S2506_C01_035E", "S2506_C01_036E",
      "S2506_C01_037E", "S2506_C01_038E", "S2506_C01_039E",
      #Housing Costs percentage
      "S2506_C01_041E", "S2506_C01_042E", "S2506_C01_043E",
      "S2506_C01_045E", "S2506_C01_046E", "S2506_C01_047E",
      "S2506_C01_049E", "S2506_C01_050E", "S2506_C01_051E",
      "S2506_C01_053E", "S2506_C01_054E", "S2506_C01_055E",
      "S2506_C01_057E", "S2506_C01_058E", "S2506_C01_059E",
      #Real State Taxes
      "S2506_C01_061E", "S2506_C01_062E", "S2506_C01_063E",
      "S2506_C01_064E", "S2506_C01_065E"
    )
  } else if ((Table == "S2506") & (Year >= 2017) & (Year < 2020)) {
    c(
      "GEO_ID", "NAME",
      "S2506_C02_001E",
      #Value
      "S2506_C02_002E", "S2506_C02_003E", "S2506_C02_004E",
      "S2506_C02_005E", "S2506_C02_006E", "S2506_C02_007E",
      "S2506_C02_008E", "S2506_C02_009E",
      #Status
      "S2506_C02_011E", "S2506_C02_012E",
      "S2506_C02_013E", "S2506_C02_014E",
      #Income (12 months)
      "S2506_C02_015E", "S2506_C02_016E", "S2506_C02_017E",
      "S2506_C02_018E", "S2506_C02_019E", "S2506_C02_020E",
      "S2506_C02_021E", "S2506_C02_022E", "S2506_C02_023E",
      #Ratio
      "S2506_C02_024E", "S2506_C02_025E",
      "S2506_C02_026E", "S2506_C02_027E",
      #Housing Costs
      "S2506_C02_029E", "S2506_C02_030E", "S2506_C02_031E", "S2506_C02_032E",
      "S2506_C02_033E", "S2506_C02_034E", "S2506_C02_035E", "S2506_C02_036E",
      "S2506_C02_037E", "S2506_C02_038E", "S2506_C02_039E",
      #Housing Costs percentage
      "S2506_C02_041E", "S2506_C02_042E", "S2506_C02_043E",
      "S2506_C02_045E", "S2506_C02_046E", "S2506_C02_047E",
      "S2506_C02_049E", "S2506_C02_050E", "S2506_C02_051E",
      "S2506_C02_053E", "S2506_C02_054E", "S2506_C02_055E",
      "S2506_C02_057E", "S2506_C02_058E", "S2506_C02_059E",
      #Real State Taxes
      "S2506_C02_061E", "S2506_C02_062E", "S2506_C02_063E",
      "S2506_C02_064E", "S2506_C02_065E"
    )
  } else if ((Table == "S2506") & (Year == 2020)) {
    c(
      "GEO_ID", "NAME",
      "S2506_C02_001E",
      #Value
      "S2506_C02_002E", "S2506_C02_003E", "S2506_C02_004E",
      "S2506_C02_005E", "S2506_C02_006E", "S2506_C02_007E",
      "S2506_C02_008E", "S2506_C02_009E",
      #Status
      "S2506_C02_011E", "S2506_C02_012E",
      "S2506_C02_013E", "S2506_C02_014E",
      #Income (12 months)
      "S2506_C02_016E", "S2506_C02_017E", "S2506_C02_018E",
      "S2506_C02_019E", "S2506_C02_020E", "S2506_C02_021E",
      "S2506_C02_022E", "S2506_C02_023E", "S2506_C02_024E",
      #Ratio
      "S2506_C02_025E", "S2506_C02_026E",
      "S2506_C02_027E", "S2506_C02_028E",
      #Housing Costs
      "S2506_C02_030E", "S2506_C02_031E", "S2506_C02_032E", "S2506_C02_033E",
      "S2506_C02_034E", "S2506_C02_035E", "S2506_C02_036E", "S2506_C02_037E",
      "S2506_C02_038E", "S2506_C02_039E", "S2506_C02_040E",
      #Housing Costs percentage
      "S2506_C02_042E", "S2506_C02_043E", "S2506_C02_044E",
      "S2506_C02_046E", "S2506_C02_047E", "S2506_C02_048E",
      "S2506_C02_050E", "S2506_C02_051E", "S2506_C02_052E",
      "S2506_C02_054E", "S2506_C02_055E", "S2506_C02_056E",
      "S2506_C02_058E", "S2506_C02_059E", "S2506_C02_060E",
      #Real State Taxes
      "S2506_C02_062E", "S2506_C02_063E", "S2506_C02_064E",
      "S2506_C02_065E", "S2506_C02_066E"
    )
  } else if ((Table == "S2507") & (Year <= 2016)) {
    c(
      "GEO_ID", "NAME",
      "S2507_C01_001E",
      #Value
      "S2507_C01_002E", "S2507_C01_003E", "S2507_C01_004E",
      "S2507_C01_005E", "S2507_C01_006E", "S2507_C01_007E",
      "S2507_C01_008E", "S2507_C01_009E", "S2507_C01_010E",
      #Income (12 months)
      "S2507_C01_011E", "S2507_C01_012E", "S2507_C01_013E",
      "S2507_C01_014E", "S2507_C01_015E", "S2507_C01_016E",
      "S2507_C01_017E", "S2507_C01_018E", "S2507_C01_019E",
      #Ratio
      "S2507_C01_020E", "S2507_C01_021E",
      "S2507_C01_022E", "S2507_C01_023E",
      #Housing Costs
      "S2507_C01_025E", "S2507_C01_026E", "S2507_C01_027E", "S2507_C01_028E",
      "S2507_C01_029E", "S2507_C01_030E", "S2507_C01_031E", "S2507_C01_032E",
      #Housing Costs percentage
      "S2507_C01_034E", "S2507_C01_035E", "S2507_C01_036E",
      "S2507_C01_038E", "S2507_C01_039E", "S2507_C01_040E",
      "S2507_C01_042E", "S2507_C01_043E", "S2507_C01_044E",
      "S2507_C01_046E", "S2507_C01_047E", "S2507_C01_048E",
      "S2507_C01_050E", "S2507_C01_051E", "S2507_C01_052E",
      #Real State Taxes
      "S2507_C01_054E", "S2507_C01_055E", "S2507_C01_056E",
      "S2507_C01_057E", "S2507_C01_058E"
    )
  } else if ((Table == "S2507") & (Year >= 2017)) {
    c(
      "GEO_ID", "NAME",
      "S2507_C02_001E",
      #Value
      "S2507_C02_002E", "S2507_C02_003E", "S2507_C02_004E",
      "S2507_C02_005E", "S2507_C02_006E", "S2507_C02_007E",
      "S2507_C02_008E", "S2507_C02_009E", "S2507_C02_010E",
      #Income (12 months)
      "S2507_C02_011E", "S2507_C02_012E", "S2507_C02_013E",
      "S2507_C02_014E", "S2507_C02_015E", "S2507_C02_016E",
      "S2507_C02_017E", "S2507_C02_018E", "S2507_C02_019E",
      #Ratio
      "S2507_C02_020E", "S2507_C02_021E",
      "S2507_C02_022E", "S2507_C02_023E",
      #Housing Costs
      "S2507_C02_025E", "S2507_C02_026E", "S2507_C02_027E", "S2507_C02_028E",
      "S2507_C02_029E", "S2507_C02_030E", "S2507_C02_031E", "S2507_C02_032E",
      #Housing Costs percentage
      "S2507_C02_034E", "S2507_C02_035E", "S2507_C02_036E",
      "S2507_C02_038E", "S2507_C02_039E", "S2507_C02_040E",
      "S2507_C02_042E", "S2507_C02_043E", "S2507_C02_044E",
      "S2507_C02_046E", "S2507_C02_047E", "S2507_C02_048E",
      "S2507_C02_050E", "S2507_C02_051E", "S2507_C02_052E",
      #Real State Taxes
      "S2507_C02_054E", "S2507_C02_055E", "S2507_C02_056E",
      "S2507_C02_057E", "S2507_C02_058E"
    )
  } else {
    stop("Invalid Table or Year")
  }

  vars_new <- if (Table == "B98001") {
    c(
      "GEO_ID", "NAME",
      "B98001.group.isize", "B98001.group.fsize",
      "Year", "Table"
    )
  } else if (Table == "S2501") {
    c(
      "GEO_ID", "NAME",
      "S2501.totalowned", "S2501.householdsize.1p",
      "S2501.householdsize.2p", "S2501.householdsize.3p",
      "S2501.householdsize.4p+", "S2501.householdtype.family",
      "S2501.householdtype.nonfamily",
      "Year", "Table"
    )
  } else if (Table == "S2502") {
    c(
      "GEO_ID", "NAME",
      "S2502.totalowned",
      "S2502.race.whiteshare", "S2502.race.blackshare",
      "S2502.race.asianshare", "S2502.race.hispanicshare",
      "S2502.age.35-", "S2502.age.35-44", "S2502.age.45-54",
      "S2502.age.55-64", "S2502.age.65-74", "S2502.age.75-84",
      "S2502.age.85+",
      "S2502.edu.HS-", "S2502.edu.HS", "S2502.edu.Col-", "S2502.edu.Col+",
      "Year", "Table",
      "S2502.yearmoved.2010+", "S2502.yearmoved.2000-2009",
      "S2502.yearmoved.1990-1999", "S2502.yearmoved.1989-"
    )
  } else if (Table == "S2503") {
    c(
      "GEO_ID", "NAME",
      "S2503.totalowned",
      #Income,,
      "S2503.income.5k-", "S2503.income.5-10k", "S2503.income.10-15k",
      "S2503.income.15-20k", "S2503.income.20-25k", "S2503.income.25-35k",
      "S2503.income.35-50k", "S2503.income.50-75k", "S2503.income.75-100k",
      "S2503.income.100-150k", "S2503.income.150k+", "S2503.income.median",
      #Housing Costs,
      "S2503.cost.300-", "S2503.cost.300-500", "S2503.cost.500-800",
      "S2503.cost.800-1k", "S2503.cost.1-1.5k", "S2503.cost.1.5-2k",
      "S2503.cost.2-2.5k", "S2503.cost.2.5-3k", "S2503.cost.3k+",
      "S2503.cost.Nocashrent", "S2503.cost.median",
      "Year", "Table",
      #Housing Costs Percentage,
      "S2503.costp.20-", "S2503.costp.20-30", "S2503.costp.30+",
      "S2503.costp.median"
    )
  } else if (Table == "S2504") {
    c(
      "GEO_ID", "NAME",
      "S2504.totalowned",
      #Units
      "S2504.unit.1det", "S2504.unit.1att", "S2504.unit.2", "S2504.unit.3-4",
      "S2504.unit.5-9", "S2504.unit.10+", "S2504.unit.other",
      #Year Built
      "S2504.ybuilt.2014+", "S2504.ybuilt.2010-2013",
      "S2504.ybuilt.2000-2009", "S2504.ybuilt.1980-1999",
      "S2504.ybuilt.1960-1979", "S2504.ybuilt.1940-1959",
      "S2504.ybuilt.1939-",
      #Rooms
      "S2504.rooms.1", "S2504.rooms.2-3", "S2504.rooms.4-5",
      "S2504.rooms.6-7", "S2504.rooms.8+",
      #Bedrooms
      "S2504.bedrooms.0", "S2504.bedrooms.1",
      "S2504.bedrooms.2-3", "S2504.bedrooms.4+",
      #Facilities
      "S2504.facilities.plumbingok", "S2504.facilities.kitchenok",
      #Vehicle
      "S2504.vehicles.0", "S2504.vehicles.1",
      "S2504.vehicles.2", "S2504.vehicles.3+",
      #Telephone
      "S2504.telephone.service",
      #Fuel
      "S2504.fuel.utgas", "S2504.fuel.btLPgas", "S2504.fuel.elect",
      "S2504.fuel.oilkeros", "S2504.fuel.coalcoke", "S2504.fuel.other",
      "S2504.fuel.nofuel",
      "Year", "Table"
    )
  } else if (Table == "S2506") {
    c(
      "GEO_ID", "NAME",
      "S2506.totalmort",
      #Value
      "S2506.value.50k-", "S2506.value.50-100k", "S2506.value.100-300k",
      "S2506.value.300-500k", "S2506.value.500-750k", "S2506.value.750-1M",
      "S2506.value.1M+", "S2506.value.median",
      #Status
      "S2506.status.2ndmort", "S2506.status.homeequity",
      "S2506.status.both", "S2506.status.none",
      #Income (12 months)
      "S2506.income.10k-", "S2506.income.10-25k", "S2506.income.25-35k",
      "S2506.income.35-50k", "S2506.income.50-75k", "S2506.income.75-100k",
      "S2506.income.100-150k", "S2506.income.150k+", "S2506.income.median",
      #Ratio
      "S2506.ratio.2-", "S2506.ratio.2-3",
      "S2506.ratio.3-4", "S2506.ratio.4+",
      #Housing Costs
      "S2506.cost.200-", "S2506.cost.200-400", "S2506.cost.400-600",
      "S2506.cost.600-800", "S2506.cost.800-1k", "S2506.cost.1-1.5k",
      "S2506.cost.1.5-2k", "S2506.cost.2-2.5k", "S2506.cost.2.5-3k",
      "S2506.cost.3k+", "S2506.cost.median",
      #Real State Taxes
      "S2506.tax.800-", "S2506.tax.800-1.5k", "S2506.tax.1.5k+",
      "S2506.tax.No", "S2506.tax.median",
      "Year", "Table",
      #Housing Costs percentage
      "S2506.costp.20-", "S2506.costp.20-30", "S2506.costp.30+",
      "S2506.costp.median"
    )
  } else if (Table == "S2507") {
    c(
      "GEO_ID", "NAME",
      "S2507.totalmort",
      #Value
      "S2507.value.50k-", "S2507.value.50-100k", "S2507.value.100-199k",
      "S2507.value.200-299k", "S2507.value.300-500k", "S2507.value.500-750k",
      "S2507.value.750-1M", "S2507.value.1M+", "S2507.value.median",
      #Income (12 months)
      "S2507.income.10k-", "S2507.income.10-25k", "S2507.income.25-35k",
      "S2507.income.35-50k", "S2507.income.50-75k", "S2507.income.75-100k",
      "S2507.income.100-150k", "S2507.income.150k+", "S2507.income.median",
      #Ratio
      "S2507.ratio.2-", "S2507.ratio.2-3",
      "S2507.ratio.3-4", "S2507.ratio.4+",
      #Housing Costs
      "S2507.cost.200-", "S2507.cost.200-399", "S2507.cost.400-599",
      "S2507.cost.600-999", "S2507.cost.1-1.3k", "S2507.cost.1.3-1.5k",
      "S2507.cost.1.5k+", "S2507.cost.median",
      #Real State Taxes
      "S2507.tax.800-", "S2507.tax.800-1.5k", "S2507.tax.1.5k+",
      "S2507.tax.No", "S2507.tax.median",
      "Year", "Table",
      #Housing Costs percentage
      "S2507.costp.20-", "S2507.costp.20-30", "S2507.costp.30+",
      "S2507.costp.median"
    )
  } else {
    stop("Table not found")
  }

  # Call File and Create Year and Table Variables
  data <- readr::read_csv(file_name)[-1, vars_old, drop = FALSE]
  data$Year <- Year
  data$Table <- Table

  # Some Extra Processing for Table S2502
  data$S2502_C024_028E <- ifelse(
    (data$Table == "S2502") & (data$Year <= 2016),
    as.numeric(data$S2502_C02_022E) + as.numeric(data$S2502_C02_023E),
    ifelse(
      ((data$Table == "S2502") & (data$Year == 2017)),
      as.numeric(data$S2502_C04_022E) + as.numeric(data$S2502_C04_023E),
      ifelse(
        (data$Table == "S2502") & (data$Year > 2017),
        (
          as.numeric(data$S2502_C04_022E) +
          as.numeric(data$S2502_C04_023E) +
          as.numeric(data$S2502_C04_024E)
        ),
        "S2502_C024_028E"
      )
    )
  )

  data$S2502_C024_029E <- ifelse(
    (data$Table == "S2502") & (data$Year <= 2016),
    as.numeric(data$S2502_C02_024E),
    ifelse(
      (data$Table == "S2502") & (data$Year == 2017),
      as.numeric(data$S2502_C04_024E),
      ifelse(
        (data$Table == "S2502") & (data$Year > 2017),
        as.numeric(data$S2502_C04_025E),
        "S2502_C024_029E"
      )
    )
  )

  data$S2502_C024_030E <- ifelse(
    (data$Table == "S2502") & (data$Year <= 2016),
    as.numeric(data$S2502_C02_025E),
    ifelse(
      (data$Table == "S2502") & (data$Year == 2017),
      as.numeric(data$S2502_C04_025E),
      ifelse(
        (data$Table == "S2502") & (data$Year > 2017),
        as.numeric(data$S2502_C04_026E),
        "S2502_C024_030E"
      )
    )
  )

  data$S2502_C024_031E <- ifelse(
    (data$Table == "S2502") & (data$Year <= 2016),
    as.numeric(data$S2502_C02_026E) + as.numeric(data$S2502_C02_027E),
    ifelse(
      (data$Table == "S2502") & (data$Year == 2017),
      as.numeric(data$S2502_C04_026E) + as.numeric(data$S2502_C04_027E),
      ifelse(
        (data$Table == "S2502") & (data$Year > 2017),
        as.numeric(data$S2502_C04_027E),
        "S2502_C024_031E"
      )
    )
  )

  data <- if ((Table == "S2502") & (Year <= 2016)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2502_C02_022E",
          "S2502_C02_023E",
          "S2502_C02_024E",
          "S2502_C02_025E",
          "S2502_C02_026E",
          "S2502_C02_027E"
        )
      )
    ]
  } else if ((Table == "S2502") & (Year >= 2017)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2502_C04_022E",
          "S2502_C04_023E",
          "S2502_C04_024E",
          "S2502_C04_025E",
          "S2502_C04_026E",
          "S2502_C04_027E"
        )
      )
    ]
  } else if (Table != "S2502") {
    data[,
      !(
        names(data) %in%
        c(
          "S2502_C024_028E",
          "S2502_C024_029E",
          "S2502_C024_030E",
          "S2502_C024_031E"
        )
      )
    ]
  }

  # Some Extra Processing for Table S2503
  data$S2503_C024_045E <- ifelse(
    (data$Table == "S2503") & (data$Year <= 2016),
    (
      as.numeric(data$S2503_C02_026E) +
      as.numeric(data$S2503_C02_030E) +
      as.numeric(data$S2503_C02_034E) +
      as.numeric(data$S2503_C02_038E) +
      as.numeric(data$S2503_C02_042E)
    ),
    ifelse(
      (data$Table == "S2503") & (data$Year >= 2017),
      (
        as.numeric(data$S2503_C04_026E) +
        as.numeric(data$S2503_C04_030E) +
        as.numeric(data$S2503_C04_034E) +
        as.numeric(data$S2503_C04_038E) +
        as.numeric(data$S2503_C04_042E)
      ),
      "S2503_C024_044E"
    )
  )

  data$S2503_C024_046E <- ifelse(
    (data$Table == "S2503") & (data$Year <= 2016),
    (
      as.numeric(data$S2503_C02_027E) +
      as.numeric(data$S2503_C02_031E) +
      as.numeric(data$S2503_C02_035E) +
      as.numeric(data$S2503_C02_039E) +
      as.numeric(data$S2503_C02_043E)
    ),
    ifelse(
      (data$Table == "S2503") & (data$Year >= 2017),
      (
        as.numeric(data$S2503_C04_027E) +
        as.numeric(data$S2503_C04_031E) +
        as.numeric(data$S2503_C04_035E) +
        as.numeric(data$S2503_C04_039E) +
        as.numeric(data$S2503_C04_043E)
      ),
      "S2503_C024_045E"
    )
  )

  data$S2503_C024_047E <- ifelse(
    (data$Table == "S2503") & (data$Year <= 2016),
    (
      as.numeric(data$S2503_C02_028E) +
      as.numeric(data$S2503_C02_032E) +
      as.numeric(data$S2503_C02_036E) +
      as.numeric(data$S2503_C02_040E) +
      as.numeric(data$S2503_C02_044E)
    ),
    ifelse(
      (data$Table == "S2503") & (data$Year >= 2017),
      (
        as.numeric(data$S2503_C04_028E) +
        as.numeric(data$S2503_C04_032E) +
        as.numeric(data$S2503_C04_036E) +
        as.numeric(data$S2503_C04_040E) +
        as.numeric(data$S2503_C04_044E)
      ),
      "S2503_C024_046E"
    )
  )

  data$S2503_C024_048E <- ifelse(
    (data$Table == "S2503") & (data$Year <= 2016),
    (
      as.numeric(data$S2503_C02_024E) * 12 /
      as.numeric(data$S2503_C02_013E)
    ),
    ifelse(
      (data$Table == "S2503") & (data$Year >= 2017),
      (
        as.numeric(data$S2503_C04_024E) * 12 /
        as.numeric(data$S2503_C04_013E)
      ),
      "S2503_C024_047E"
    )
  )

  data <- if ((Table == "S2503") & (Year <= 2016)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2503_C02_026E", "S2503_C02_027E",
          "S2503_C02_028E", "S2503_C02_029E",
          "S2503_C02_030E", "S2503_C02_031E",
          "S2503_C02_032E", "S2503_C02_033E",
          "S2503_C02_034E", "S2503_C02_035E",
          "S2503_C02_036E", "S2503_C02_037E",
          "S2503_C02_038E", "S2503_C02_039E",
          "S2503_C02_040E", "S2503_C02_041E",
          "S2503_C02_042E", "S2503_C02_043E",
          "S2503_C02_044E"
        )
      )
    ]
  } else if ((Table == "S2503") & (Year >= 2017)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2503_C04_026E", "S2503_C04_027E",
          "S2503_C04_028E", "S2503_C04_029E",
          "S2503_C04_030E", "S2503_C04_031E",
          "S2503_C04_032E", "S2503_C04_033E",
          "S2503_C04_034E", "S2503_C04_035E",
          "S2503_C04_036E", "S2503_C04_037E",
          "S2503_C04_038E", "S2503_C04_039E",
          "S2503_C04_040E", "S2503_C04_041E",
          "S2503_C04_042E", "S2503_C04_043E",
          "S2503_C04_044E"
        )
      )
    ]
  } else if (Table != "S2503") {
    data[,
      !(
        names(data) %in%
        c(
          "S2503_C024_045E", "S2503_C024_046E",
          "S2503_C024_047E", "S2503_C024_048E"
        )
      )
    ]
  }


  # Some Extra Processing for Table S2506
  data$S2506_C012_067E <- ifelse(
    (data$Table == "S2506") & (data$Year <= 2016),
    (
      as.numeric(data$S2506_C01_041E) +
      as.numeric(data$S2506_C01_045E) +
      as.numeric(data$S2506_C01_049E) +
      as.numeric(data$S2506_C01_053E) +
      as.numeric(data$S2506_C01_057E)
    ),
    ifelse(
      (data$Table == "S2506") & (data$Year >= 2017) & (data$Year < 2020),
      (
        as.numeric(data$S2506_C02_041E) +
        as.numeric(data$S2506_C02_045E) +
        as.numeric(data$S2506_C02_049E) +
        as.numeric(data$S2506_C02_053E) +
        as.numeric(data$S2506_C02_057E)
      ),
      ifelse(
        (data$Table == "S2506") & (data$Year == 2020),
        (
          as.numeric(data$S2506_C02_042E) +
          as.numeric(data$S2506_C02_046E) +
          as.numeric(data$S2506_C02_050E) +
          as.numeric(data$S2506_C02_054E) +
          as.numeric(data$S2506_C02_058E)
        ),
        "S2506_C012_067E"
      )
    )
  )

  data$S2506_C012_068E <- ifelse(
    (data$Table == "S2506") & (data$Year <= 2016),
    (
      as.numeric(data$S2506_C01_042E) +
      as.numeric(data$S2506_C01_046E) +
      as.numeric(data$S2506_C01_050E) +
      as.numeric(data$S2506_C01_054E) +
      as.numeric(data$S2506_C01_058E)
    ),
    ifelse(
      (data$Table == "S2506") &
      (data$Year >= 2017) & (data$Year < 2020),
      (
        as.numeric(data$S2506_C02_042E) +
        as.numeric(data$S2506_C02_046E) +
        as.numeric(data$S2506_C02_050E) +
        as.numeric(data$S2506_C02_054E) +
        as.numeric(data$S2506_C02_058E)
      ),
      ifelse(
        (data$Table == "S2506") & (data$Year == 2020),
        (
          as.numeric(data$S2506_C02_043E) +
          as.numeric(data$S2506_C02_047E) +
          as.numeric(data$S2506_C02_051E) +
          as.numeric(data$S2506_C02_055E) +
          as.numeric(data$S2506_C02_059E)
        ),
        "S2506_C012_068E"
      )
    )
  )

  data$S2506_C012_069E <- ifelse(
    (data$Table == "S2506") & (data$Year <= 2016),
    (
      as.numeric(data$S2506_C01_043E) +
      as.numeric(data$S2506_C01_047E) +
      as.numeric(data$S2506_C01_051E) +
      as.numeric(data$S2506_C01_055E) +
      as.numeric(data$S2506_C01_059E)
    ),
    ifelse(
      (data$Table == "S2506") & (data$Year >= 2017) & (data$Year < 2020),
      (
        as.numeric(data$S2506_C02_043E) +
        as.numeric(data$S2506_C02_047E) +
        as.numeric(data$S2506_C02_051E) +
        as.numeric(data$S2506_C02_055E) +
        as.numeric(data$S2506_C02_059E)
      ),
      ifelse(
        (data$Table == "S2506") & (data$Year == 2020),
        (
          as.numeric(data$S2506_C02_044E) +
          as.numeric(data$S2506_C02_048E) +
          as.numeric(data$S2506_C02_052E) +
          as.numeric(data$S2506_C02_056E) +
          as.numeric(data$S2506_C02_060E)
        ),
        "S2506_C012_069E"
      )
    )
  )

  data$S2506_C012_070E <- ifelse(
    (data$Table == "S2506") & (data$Year <= 2016),
    (
      as.numeric(data$S2506_C01_039E) * 12 /
      as.numeric(data$S2506_C01_023E)
    ),
    ifelse(
      (data$Table == "S2506") & (data$Year >= 2017) & (data$Year < 2020),
      (
        as.numeric(data$S2506_C02_039E) * 12 /
        as.numeric(data$S2506_C02_023E)
      ),
      ifelse(
        (data$Table == "S2506") & (data$Year == 2020),
        (
          as.numeric(data$S2506_C02_040E) * 12 /
          as.numeric(data$S2506_C02_024E)
        ),
        "S2506_C012_069E"
      )
    )
  )

  data <- if ((Table == "S2506") & (Year <= 2016)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2506_C01_041E", "S2506_C01_042E",
          "S2506_C01_043E", "S2506_C01_044E",
          "S2506_C01_045E", "S2506_C01_046E",
          "S2506_C01_047E", "S2506_C01_048E",
          "S2506_C01_049E", "S2506_C01_050E",
          "S2506_C01_051E", "S2506_C01_052E",
          "S2506_C01_053E", "S2506_C01_054E",
          "S2506_C01_055E", "S2506_C01_056E",
          "S2506_C01_057E", "S2506_C01_058E",
          "S2506_C01_059E"
        )
      )
    ]
  } else if ((Table == "S2506") & (Year >= 2017) & (Year < 2020)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2506_C02_041E", "S2506_C02_042E",
          "S2506_C02_043E", "S2506_C02_044E",
          "S2506_C02_045E", "S2506_C02_046E",
          "S2506_C02_047E", "S2506_C02_048E",
          "S2506_C02_049E", "S2506_C02_050E",
          "S2506_C02_051E", "S2506_C02_052E",
          "S2506_C02_053E", "S2506_C02_054E",
          "S2506_C02_055E", "S2506_C02_056E",
          "S2506_C02_057E", "S2506_C02_058E",
          "S2506_C02_059E"
        )
      )
    ]
  } else if ((Table == "S2506") & (Year == 2020)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2506_C02_042E", "S2506_C02_043E",
          "S2506_C02_044E", "S2506_C02_045E",
          "S2506_C02_046E", "S2506_C02_047E",
          "S2506_C02_048E", "S2506_C02_049E",
          "S2506_C02_050E", "S2506_C02_051E",
          "S2506_C02_052E", "S2506_C02_053E",
          "S2506_C02_054E", "S2506_C02_055E",
          "S2506_C02_056E", "S2506_C02_057E",
          "S2506_C02_058E", "S2506_C02_059E",
          "S2506_C02_060E"
        )
      )
    ]
  } else if (Table != "S2506") {
    data[,
      !(
        names(data) %in%
        c(
          "S2506_C012_067E", "S2506_C012_068E",
          "S2506_C012_069E", "S2506_C012_070E"
        )
      )
    ]
  }

  # Some Extra Processing for Table S2507
  data$S2507_C012_059E <- ifelse(
    (data$Table == "S2507") & (data$Year <= 2016),
    (
      as.numeric(data$S2507_C01_034E) +
      as.numeric(data$S2507_C01_038E) +
      as.numeric(data$S2507_C01_042E) +
      as.numeric(data$S2507_C01_046E) +
      as.numeric(data$S2507_C01_050E)
    ),
    ifelse(
      (data$Table == "S2507") & (data$Year >= 2017),
      (
        as.numeric(data$S2507_C02_034E) +
        as.numeric(data$S2507_C02_038E) +
        as.numeric(data$S2507_C02_042E) +
        as.numeric(data$S2507_C02_046E) +
        as.numeric(data$S2507_C02_050E)
      ),
      "S2507_C012_059E"
    )
  )

  data$S2507_C012_060E <- ifelse(
    (data$Table == "S2507") & (data$Year <= 2016),
    (
      as.numeric(data$S2507_C01_035E) +
      as.numeric(data$S2507_C01_039E) +
      as.numeric(data$S2507_C01_043E) +
      as.numeric(data$S2507_C01_047E) +
      as.numeric(data$S2507_C01_051E)
    ),
    ifelse(
      (data$Table == "S2507") & (data$Year >= 2017),
      (
        as.numeric(data$S2507_C02_035E) +
        as.numeric(data$S2507_C02_039E) +
        as.numeric(data$S2507_C02_043E) +
        as.numeric(data$S2507_C02_047E) +
        as.numeric(data$S2507_C02_051E)
      ),
      "S2507_C012_060E"
    )
  )

  data$S2507_C012_061E <- ifelse(
    (data$Table == "S2507") & (data$Year <= 2016),
    as.numeric(data$S2507_C01_036E) +
    as.numeric(data$S2507_C01_040E) +
    as.numeric(data$S2507_C01_044E) +
    as.numeric(data$S2507_C01_048E) +
    as.numeric(data$S2507_C01_052E),
    ifelse(
      (data$Table == "S2507") & (data$Year >= 2017),
      (
        as.numeric(data$S2507_C02_036E)+
        as.numeric(data$S2507_C02_040E)+
        as.numeric(data$S2507_C02_044E)+
        as.numeric(data$S2507_C02_048E)+
        as.numeric(data$S2507_C02_052E)
      ),
      "S2507_C012_061E"
    )
  )

  data$S2507_C012_062E <- ifelse(
    (data$Table == "S2507") & (data$Year <= 2016),
    (
      as.numeric(data$S2507_C01_032E) * 12 /
      as.numeric(data$S2507_C01_019E)
    ),
    ifelse(
      (data$Table == "S2507") & (data$Year >= 2017),
      (
        as.numeric(data$S2507_C02_032E) * 12 /
        as.numeric(data$S2507_C02_019E)
      ),
      "S2507_C012_062E"
    )
  )

  data <- if ((Table == "S2507") & (Year <= 2016)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2507_C01_034E", "S2507_C01_035E",
          "S2507_C01_036E", "S2507_C01_037E",
          "S2507_C01_038E", "S2507_C01_039E",
          "S2507_C01_040E", "S2507_C01_041E",
          "S2507_C01_042E", "S2507_C01_043E",
          "S2507_C01_044E", "S2507_C01_045E",
          "S2507_C01_046E", "S2507_C01_047E",
          "S2507_C01_048E", "S2507_C01_049E",
          "S2507_C01_050E", "S2507_C01_051E",
          "S2507_C01_052E"
        )
      )
    ]
  } else if ((Table == "S2507") & (Year >= 2017)) {
    data[,
      !(
        names(data) %in%
        c(
          "S2507_C02_034E", "S2507_C02_035E",
          "S2507_C02_036E", "S2507_C02_037E",
          "S2507_C02_038E", "S2507_C02_039E",
          "S2507_C02_040E", "S2507_C02_041E",
          "S2507_C02_042E", "S2507_C02_043E",
          "S2507_C02_044E", "S2507_C02_045E",
          "S2507_C02_046E", "S2507_C02_047E",
          "S2507_C02_048E", "S2507_C02_049E",
          "S2507_C02_050E", "S2507_C02_051E",
          "S2507_C02_052E"
        )
      )
    ]
  } else if (Table != "S2507") {
      data[,
      !(
        names(data) %in%
        c(
          "S2507_C012_059E", "S2507_C012_060E",
          "S2507_C012_061E", "S2507_C012_062E"
        )
      )
    ]
  }

  # Final steps: Rename variables, and create County, State variables
  colnames(data) <- vars_new
  data$County <- gsub(",.*$", "", data$NAME)
  data$State <- gsub(".*, ", "", data$NAME)
  data$NAME <- NULL
  data$Table <- NULL

  data
}

process_app_race_data <- function(Table = "S2501", Year = 2020) {
  # Define Location of the File
  file_name <- file.path(
    INPUT_PROCESSED_DIR,
    "census_app",
    "unzip",
    stringr::str_replace(CENSUS_TABLE_FILE_APP[[Table]], "XXXX", as.character(Year))
  )

  # Define and name working variables
  vars_old <- if (Table == "S1501") {
    c(
      #GEOID and NAME
      "GEO_ID", "NAME",
      #Total in the survey
      "S1501_C01_001E", "S1501_C01_006E",
      #Total broken by race
      #Total by educational attainment (considering both age groups)
      "S1501_C01_003E", "S1501_C01_004E", "S1501_C01_005E", "S1501_C01_009E",
      "S1501_C01_010E", "S1501_C01_011E", "S1501_C01_012E", "S1501_C01_013E",
      #Total by educational attainment broken by race
      #White
      "S1501_C01_028E", "S1501_C01_029E", "S1501_C01_030E",
      #White alone, not Hispanic or Latino
      "S1501_C01_031E", "S1501_C01_032E", "S1501_C01_033E",
      #Black
      "S1501_C01_034E", "S1501_C01_035E", "S1501_C01_036E",
      #American Indian and Alaska Native,
      "S1501_C01_037E", "S1501_C01_038E", "S1501_C01_039E",
      #Asian
      "S1501_C01_040E", "S1501_C01_041E", "S1501_C01_042E",
      #Native Hawaiian and Other Pacific
      "S1501_C01_043E", "S1501_C01_044E", "S1501_C01_045E",
      #Other
      "S1501_C01_046E", "S1501_C01_047E", "S1501_C01_048E",
      #Two+
      "S1501_C01_049E", "S1501_C01_050E", "S1501_C01_051E",
      #Hispanic
      "S1501_C01_052E", "S1501_C01_053E", "S1501_C01_054E"
    )
  } else if ((Table == "S1903") & (Year <= 2016)) {
    c(
      #GEOID and NAME
      "GEO_ID", "NAME",
      #Total household and median income in the survey
      "S1903_C01_001E", "S1903_C02_001E",
      #Total households broken by race (percents...)
      "S1903_C01_002E", "S1903_C01_003E", "S1903_C01_004E", "S1903_C01_005E",
      "S1903_C01_006E", "S1903_C01_007E", "S1903_C01_008E", "S1903_C01_009E",
      "S1903_C01_010E",
      #Median Income broken by Race
      "S1903_C02_002E", "S1903_C02_003E", "S1903_C02_004E", "S1903_C02_005E",
      "S1903_C02_006E", "S1903_C02_007E", "S1903_C02_008E", "S1903_C02_009E",
      "S1903_C02_010E"
    )
  } else if ((Table == "S1903") & (Year >= 2017)) {
    c(
      #GEOID and NAME
      "GEO_ID", "NAME",
      #Total household and median income in the survey
      "S1903_C01_001E", "S1903_C03_001E",
      #Total households broken by race (totals)
      "S1903_C01_002E", "S1903_C01_003E", "S1903_C01_004E", "S1903_C01_005E",
      "S1903_C01_006E", "S1903_C01_007E", "S1903_C01_008E", "S1903_C01_009E",
      "S1903_C01_010E",
      #Median Income broken by Income
      "S1903_C03_002E", "S1903_C03_003E", "S1903_C03_004E", "S1903_C03_005E",
      "S1903_C03_006E", "S1903_C03_007E", "S1903_C03_008E", "S1903_C03_009E",
      "S1903_C03_010E"
    )
  } else if (Table == "S2301") {
    c(
      #GEOID and NAME
      "GEO_ID", "NAME",
      #Total households in the survey
      "S2301_C01_001E",
      #Total households broken by race (totals)
      "S2301_C01_012E", "S2301_C01_013E", "S2301_C01_014E", "S2301_C01_015E",
      "S2301_C01_016E", "S2301_C01_017E", "S2301_C01_018E", "S2301_C01_019E",
      "S2301_C01_020E",
      #Labor Force Part Rate, Employment / Pop Rate, Unemployment Rate Totals
      "S2301_C02_001E", "S2301_C03_001E", "S2301_C04_001E",
      #Same as above but broken by race / ethnicity
      "S2301_C02_012E", "S2301_C03_012E", "S2301_C04_012E",
      "S2301_C02_013E", "S2301_C03_013E", "S2301_C04_013E",
      "S2301_C02_014E", "S2301_C03_014E", "S2301_C04_014E",
      "S2301_C02_015E", "S2301_C03_015E", "S2301_C04_015E",
      "S2301_C02_016E", "S2301_C03_016E", "S2301_C04_016E",
      "S2301_C02_017E", "S2301_C03_017E", "S2301_C04_017E",
      "S2301_C02_018E", "S2301_C03_018E", "S2301_C04_018E",
      "S2301_C02_019E", "S2301_C03_019E", "S2301_C04_019E",
      "S2301_C02_020E", "S2301_C03_020E", "S2301_C04_020E"
    )
  } else if ((Table == "S2502") & (Year <= 2016)) {
    c(
      #GEOID and NAME
      "GEO_ID", "NAME",
      #Total and owned households in the survey
      "S2502_C01_001E", "S2502_C02_001E",
      #Total households broken by race (percentages)
      "S2502_C01_002E", "S2502_C01_003E", "S2502_C01_004E", "S2502_C01_005E",
      "S2502_C01_006E", "S2502_C01_007E", "S2502_C01_008E", "S2502_C01_009E",
      "S2502_C01_010E",
      #Total owner-occupied housing units broken by race (percentages)
      "S2502_C02_002E", "S2502_C02_003E", "S2502_C02_004E", "S2502_C02_005E",
      "S2502_C02_006E", "S2502_C02_007E", "S2502_C02_008E", "S2502_C02_009E",
      "S2502_C02_010E"
    )
  } else if ((Table == "S2502") & (Year >= 2017)) {
    c(
      #GEOID and NAME
      "GEO_ID", "NAME",
      #Total and owned households in the survey
      "S2502_C01_001E", "S2502_C03_001E",
      #Total households broken by race (totals)
      "S2502_C01_002E", "S2502_C01_003E", "S2502_C01_004E", "S2502_C01_005E",
      "S2502_C01_006E", "S2502_C01_007E", "S2502_C01_008E", "S2502_C01_009E",
      "S2502_C01_010E",
      #Total owner-occupied housing units broken by race (totals)
      "S2502_C03_002E", "S2502_C03_003E", "S2502_C03_004E", "S2502_C03_005E",
      "S2502_C03_006E", "S2502_C03_007E", "S2502_C03_008E", "S2502_C03_009E",
      "S2502_C03_010E"
    )
  } else if ((Table == "DP05") & (Year <= 2016)) {
    c(
      "GEO_ID", "NAME",
       "DP05_0028E",
       "DP05_0032E", "DP05_0033E", "DP05_0034E", "DP05_0039E",
       "DP05_0047E", "DP05_0052E", "DP05_0053E", "DP05_0066E",
       "DP05_0072E"
    )
  } else if ((Table == "DP05") & (Year >= 2017)) {
    c(
      "GEO_ID", "NAME",
      "DP05_0033E",
      "DP05_0037E", "DP05_0038E", "DP05_0039E", "DP05_0044E",
      "DP05_0052E", "DP05_0057E", "DP05_0058E", "DP05_0071E",
      "DP05_0077E"
    )
  } else {
    stop("Table or year not found")
  }

  # Call File and Create Year and Table Variables
  data <- readr::read_csv(file_name)[-1, vars_old, drop = FALSE]
  data$Year <- Year
  data$Table <- Table
  data$County <- gsub(",.*$", "", data$NAME)
  data$State <- gsub(".*, ", "", data$NAME)

  # Build data set
  data <- if (Table == "S1501") {
    data |>
    dplyr::summarise(
      GEO_ID = GEO_ID,
      Year = Year,
      County = County,
      State = State,
      Group = "Total",
      S1501.group.total = as.numeric(S1501_C01_001E) + as.numeric(S1501_C01_006E),
      S1501.group.HSt = (
        as.numeric(S1501_C01_003E) +
        as.numeric(S1501_C01_004E) + as.numeric(S1501_C01_005E) +
        as.numeric(S1501_C01_009E)+ as.numeric(S1501_C01_010E)+
        as.numeric(S1501_C01_011E)+ as.numeric(S1501_C01_012E)+
        as.numeric(S1501_C01_013E)
      ),
      S1501.group.HSp = S1501.group.HSt / S1501.group.total,
      S1501.group.BSt = (
        as.numeric(S1501_C01_005E) +
        as.numeric(S1501_C01_012E) +
        as.numeric(S1501_C01_013E)
      ),
      S1501.group.BSp = S1501.group.BSt / S1501.group.total
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "White",
        S1501.group.total = as.numeric(S1501_C01_028E),
        S1501.group.HSt = as.numeric(S1501_C01_029E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_030E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "WhiteNH",
        S1501.group.total = as.numeric(S1501_C01_031E),
        S1501.group.HSt = as.numeric(S1501_C01_032E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_033E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Black",
        S1501.group.total = as.numeric(S1501_C01_034E),
        S1501.group.HSt = as.numeric(S1501_C01_035E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_036E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "AIoAN",
        S1501.group.total = as.numeric(S1501_C01_037E),
        S1501.group.HSt = as.numeric(S1501_C01_038E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_039E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Asian",
        S1501.group.total = as.numeric(S1501_C01_040E),
        S1501.group.HSt = as.numeric(S1501_C01_041E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_042E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "NHoOPI",
        S1501.group.total = as.numeric(S1501_C01_043E),
        S1501.group.HSt = as.numeric(S1501_C01_044E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_045E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Other",
        S1501.group.total = as.numeric(S1501_C01_046E),
        S1501.group.HSt = as.numeric(S1501_C01_047E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_048E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Two+",
        S1501.group.total = as.numeric(S1501_C01_049E),
        S1501.group.HSt = as.numeric(S1501_C01_050E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_051E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Hispanic",
        S1501.group.total = as.numeric(S1501_C01_052E),
        S1501.group.HSt = as.numeric(S1501_C01_053E),
        S1501.group.HSp = S1501.group.HSt / S1501.group.total,
        S1501.group.BSt = as.numeric(S1501_C01_054E),
        S1501.group.BSp = S1501.group.BSt / S1501.group.total
      )
    )
  } else if (Table == "S1903") {
    data |>
    dplyr::summarise(
      GEO_ID = GEO_ID,
      Year = Year,
      County = County,
      State = State,
      Group = "Total",
      S1903.group.total = as.numeric(S1903_C01_001E),
      S1903.group.medianincome = ifelse(Year < 2017, S1903_C02_001E, S1903_C03_001E)
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "White",
        S1903.group.total =ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_002E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_002E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_002E,
          S1903_C03_002E
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Black",
        S1903.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_003E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_003E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_003E,
          S1903_C03_003E
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "AIoAN",
        S1903.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_004E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_004E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_004E,
          S1903_C03_004E
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Asian",
        S1903.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_005E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_005E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_005E,
          S1903_C03_005E
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "NHoOPI",
        S1903.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_006E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_006E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_006E,
          S1903_C03_006E
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Other",
        S1903.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_007E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_007E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_007E,
          S1903_C03_007E
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Two+",
        S1903.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_008E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_009E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_008E,
          S1903_C03_008E
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Hispanic",
        S1903.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_009E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_009E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_009E,
          S1903_C03_009E
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "WhiteNH",
        S1903.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S1903_C01_010E) * as.numeric(S1903_C01_001E) / 100),
          as.numeric(S1903_C01_010E)
        ),
        S1903.group.medianincome = ifelse(
          Year < 2017,
          S1903_C02_010E,
          S1903_C03_010E
        )
      )
    )
  } else if (Table == "S2301") {
    data |>
    dplyr::summarise(
      GEO_ID = GEO_ID,
      Year = Year,
      County = County,
      State = State,
      Group = "Total",
      S2301.group.total = as.numeric(S2301_C01_001E),
      S2301.group.lfpr = as.numeric(S2301_C02_001E),
      S2301.group.epr = as.numeric(S2301_C03_001E),
      S2301.group.ue = as.numeric(S2301_C04_001E)
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "White",
        S2301.group.total = as.numeric(S2301_C01_012E),
        S2301.group.lfpr = as.numeric(S2301_C02_012E),
        S2301.group.epr = as.numeric(S2301_C03_012E),
        S2301.group.ue = as.numeric(S2301_C04_012E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Black",
        S2301.group.total = as.numeric(S2301_C01_013E),
        S2301.group.lfpr = as.numeric(S2301_C02_013E),
        S2301.group.epr = as.numeric(S2301_C03_013E),
        S2301.group.ue = as.numeric(S2301_C04_013E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "AIoAN",
        S2301.group.total = as.numeric(S2301_C01_014E),
        S2301.group.lfpr = as.numeric(S2301_C02_014E),
        S2301.group.epr = as.numeric(S2301_C03_014E),
        S2301.group.ue = as.numeric(S2301_C04_014E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Asian",
        S2301.group.total = as.numeric(S2301_C01_015E),
        S2301.group.lfpr = as.numeric(S2301_C02_015E),
        S2301.group.epr = as.numeric(S2301_C03_015E),
        S2301.group.ue = as.numeric(S2301_C04_015E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "NHoOPI",
        S2301.group.total = as.numeric(S2301_C01_016E),
        S2301.group.lfpr = as.numeric(S2301_C02_016E),
        S2301.group.epr = as.numeric(S2301_C03_016E),
        S2301.group.ue = as.numeric(S2301_C04_016E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Other",
        S2301.group.total = as.numeric(S2301_C01_017E),
        S2301.group.lfpr = as.numeric(S2301_C02_017E),
        S2301.group.epr = as.numeric(S2301_C03_017E),
        S2301.group.ue = as.numeric(S2301_C04_017E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Two+",
        S2301.group.total = as.numeric(S2301_C01_018E),
        S2301.group.lfpr = as.numeric(S2301_C02_018E),
        S2301.group.epr = as.numeric(S2301_C03_018E),
        S2301.group.ue = as.numeric(S2301_C04_018E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Hispanic",
        S2301.group.total = as.numeric(S2301_C01_019E),
        S2301.group.lfpr = as.numeric(S2301_C02_019E),
        S2301.group.epr = as.numeric(S2301_C03_019E),
        S2301.group.ue = as.numeric(S2301_C04_019E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "WhiteNH",
        S2301.group.total = as.numeric(S2301_C01_020E),
        S2301.group.lfpr = as.numeric(S2301_C02_020E),
        S2301.group.epr = as.numeric(S2301_C03_020E),
        S2301.group.ue = as.numeric(S2301_C04_020E)
      )
    )
  } else if (Table == "S2502") {
    data |>
    dplyr::summarise(
      GEO_ID = GEO_ID,
      Year = Year,
      County = County,
      State = State,
      Group = "Total",
      S2502.group.total = as.numeric(S2502_C01_001E),
      S2502.group.owned = ifelse(
        Year < 2017,
        as.numeric(S2502_C02_001E),
        as.numeric(S2502_C03_001E)
      ),
      S2502.group.ownedp = S2502.group.owned / S2502.group.total
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "White",
        S2502.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C01_001E) * as.numeric(S2502_C01_002E) / 100),
          as.numeric(S2502_C01_002E)
        ),
        S2502.group.owned = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) * as.numeric(S2502_C02_002E) / 100),
          as.numeric(S2502_C03_002E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Black",
        S2502.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C01_001E) * as.numeric(S2502_C01_003E) / 100),
          as.numeric(S2502_C01_003E)
        ),
        S2502.group.owned = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) * as.numeric(S2502_C02_003E) / 100),
          as.numeric(S2502_C03_003E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "AIoAN",
        S2502.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C01_001E) * as.numeric(S2502_C01_004E) / 100),
          as.numeric(S2502_C01_004E)
        ),
        S2502.group.owned = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) * as.numeric(S2502_C02_004E) / 100),
          as.numeric(S2502_C03_004E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Asian",
        S2502.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C01_001E) * as.numeric(S2502_C01_005E) / 100),
          as.numeric(S2502_C01_005E)
        ),
        S2502.group.owned = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) * as.numeric(S2502_C02_005E) / 100),
          as.numeric(S2502_C03_005E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "NHoOPI",
        S2502.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C01_001E) * as.numeric(S2502_C01_006E) / 100),
          as.numeric(S2502_C01_006E)
        ),
        S2502.group.owned = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) * as.numeric(S2502_C02_006E) / 100),
          as.numeric(S2502_C03_006E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Other",
        S2502.group.total = ifelse(Year < 2017,
          round(as.numeric(S2502_C01_001E) * as.numeric(S2502_C01_007E) / 100),
          as.numeric(S2502_C01_007E)
        ),
        S2502.group.owned = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) *as.numeric(S2502_C02_007E) / 100),
          as.numeric(S2502_C03_007E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Two+",
        S2502.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C01_001E) *as.numeric(S2502_C01_008E) / 100),
          as.numeric(S2502_C01_008E)
        ),
        S2502.group.owned = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) * as.numeric(S2502_C02_008E) / 100),
          as.numeric(S2502_C03_008E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Hispanic",
        S2502.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C01_001E) * as.numeric(S2502_C01_009E) / 100),
          as.numeric(S2502_C01_009E)
        ),
        S2502.group.owned = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) *as.numeric(S2502_C02_009E) / 100),
          as.numeric(S2502_C03_009E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "WhiteNH",
        S2502.group.total = ifelse(
          Year < 2017,
          round(as.numeric(S2502_C01_001E) *as.numeric(S2502_C01_010E) / 100),
          as.numeric(S2502_C01_010E)
        ),
        S2502.group.owned =  ifelse(
          Year < 2017,
          round(as.numeric(S2502_C02_001E) * as.numeric(S2502_C02_010E) / 100),
          as.numeric(S2502_C03_010E)
        ),
        S2502.group.ownedp = S2502.group.owned / S2502.group.total
      )
    )
  } else if (Table == "DP05") {
    data |>
    dplyr::summarise(
      GEO_ID = GEO_ID,
      Year = Year,
      County = County,
      State = State,
      Group = "Total",
      DP05.group.pop = ifelse(Year < 2017, as.numeric(DP05_0028E), as.numeric(DP05_0033E)),
      DP05.group.popp = ifelse(
        Year < 2017,
        DP05.group.pop / as.numeric(DP05_0028E),
        DP05.group.pop / as.numeric(DP05_0033E)
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "White",
        DP05.group.pop = ifelse(
          Year < 2017,
          as.numeric(DP05_0032E),
          as.numeric(DP05_0037E)
        ),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Black",
        DP05.group.pop = ifelse(Year < 2017, as.numeric(DP05_0033E), as.numeric(DP05_0038E)),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "AIoAN",
        DP05.group.pop = ifelse(Year < 2017, as.numeric(DP05_0034E), as.numeric(DP05_0039E)),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Asian",
        DP05.group.pop = ifelse(Year < 2017, as.numeric(DP05_0039E), as.numeric(DP05_0044E)),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "NHoOPI",
        DP05.group.pop = ifelse(Year < 2017, as.numeric(DP05_0047E), as.numeric(DP05_0052E)),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Other",
        DP05.group.pop = ifelse(Year < 2017, as.numeric(DP05_0052E), as.numeric(DP05_0057E)),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Two+",
        DP05.group.pop = ifelse(
          Year < 2017,
          as.numeric(DP05_0053E),
          as.numeric(DP05_0058E)
        ),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "Hispanic",
        DP05.group.pop = ifelse(Year < 2017, as.numeric(DP05_0066E), as.numeric(DP05_0071E)),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    ) |>
    rbind(
      data |>
      dplyr::summarise(
        GEO_ID = GEO_ID,
        Year = Year,
        County = County,
        State = State,
        Group = "WhiteNH",
        DP05.group.pop = ifelse(Year < 2017, as.numeric(DP05_0072E), as.numeric(DP05_0077E)),
        DP05.group.popp = ifelse(
          Year < 2017,
          DP05.group.pop / as.numeric(DP05_0028E),
          DP05.group.pop / as.numeric(DP05_0033E)
        )
      )
    )
  }

  data$NAME <- NULL
  data$Table <- NULL

  data
}

process_app_data_all <- function() {
  unzip_census_tables()

  housing_data <- (
    purrr::map(
      HOUSING_TABLES_APP,
      function(Table) {
        purrr::map(
          YEARS,
          function(Year) {
            log_msg("Processing housing table", Table, "for year", Year)
            process_app_housing_data(Table, Year)
          }
        ) |>
        purrr::list_rbind()
      }
    ) |>
    purrr::reduce(
      dplyr::full_join,
      by = c("GEO_ID", "Year", "State", "County")
    )
  )
  race_data <- (
    purrr::map(
      RACE_TABLES_APP,
      function(Table) {
        purrr::map(
          YEARS,
          function(Year) {
            log_msg("Processing race table", Table, "for year", Year)
            process_app_race_data(Table, Year)
          }
        ) |>
        purrr::list_rbind()
      }
    ) |>
    purrr::reduce(
      dplyr::full_join,
      by = c("GEO_ID", "Year", "State", "County", "Group")
    )
  )
  data <- dplyr::full_join(
    race_data,
    housing_data,
    by = c("GEO_ID", "Year", "State", "County")
  )
  data$calc.mortp <- (
    as.numeric(data$S2506.totalmort) /
    as.numeric(data$S2502.totalowned)
  )
  data$calc.hvalue <- (
    (
      as.numeric(data$S2506.value.median) *
      as.numeric(data$S2506.totalmort)
    ) +
    (
      as.numeric(data$S2507.value.median) *
      as.numeric(data$S2507.totalmort)
    ) /
    (
      as.numeric(data$S2506.totalmort) +
      as.numeric(data$S2507.totalmort)
    )
  )
  data$B98001.group.isize <- round(
    as.numeric(data$B98001.group.isize) *
    as.numeric(data$DP05.group.popp)
  )
  data$B98001.group.fsize <- round(
    as.numeric(data$B98001.group.fsize) *
    as.numeric(data$DP05.group.popp)
  )

  save_csv(data, file.path(INPUT_PROCESSED_DIR, "census_app", "data.csv"))
}

make_blank_plot <- function() {
  ggplot2::ggplot() +
  ggplot2::theme_void()
}

get_data_plot_app <- function(data) {
  dplyr::bind_cols(
    dplyr::select(
      data,
      dplyr::any_of(c("Year", "Group", "State","County", "GEO_ID"))
    ),
    (
      data |>
      dplyr::transmute(
        Own = Owned_P * 100,
        HS = HS_P * 100,
        Col = Col_P * 100,
        Inc = Inc_Med,
        LInc = log10(Inc_Med),
        Pop = Pop_T / 1000000,
        Pop_Share = Pop_P * 100,
        LPop = log10(Pop_T),
        UE = UE_P
      )
    )
  )
}

get_var_name_app <- function(var) {
  if (var == "Own") {
    "Home Ownership Rate (%)"
  } else if (var == "HS") {
    "High School Completion (%)"
  } else if (var == "Col") {
    "Bachelor Degree Completion (%)"
  } else if (var == "Inc") {
    "Household Annual\nIncome (Current US$)"
  } else if (var == "LInc") {
    expression(paste(Log[10], " Annual Income (Current US$)"))
  } else if (var == "Pop") {
    "Population (Million Inhabitants)"
  } else if (var == "Pop_Share") {
    "Population Share (%)"
  } else if (var == "LPop") {
    expression(paste(Log[10], " Population (Million Inhabitants)"))
  } else if (var == "UE") {
    "Unemployment Rate (%)"
  }
}

# Plots Function (For Box Plots, Scatter Plots, and Time Series)
plot_app <- function(
  area = "State",
  Stated = "All",
  Yearp = 2020,
  Group1 = "White",
  Group2 = "Black",
  Group3 = "Asian",
  Group4 = "Hispanic",
  Vary = "Own",
  Varx = "Inc",
  Plot = "BP",
  BP.Violin = "F",
  SC.Smoother = "loess"
) {
  Groups_r <- c(Group1, Group2, Group3, Group4)

  if (all(Groups_r == "None")) {
    return(make_blank_plot())
  }

  data.plot <- if (area == "State") {
    DATA_STATE_APP
  } else if (area == "County") {
    DATA_COUNTY_APP
  } else {
    stop("Invalid area")
  }

  data.plot <- if ((Plot == "BP") || (Plot == "SC")) {
    data.plot |>
    dplyr::filter(
      (Group %in% Groups_r) &
      (Year == Yearp) &
      if (area == "State") {
        (State != "All")
      } else if (area == "County") {
        (State %in% Stated) | (Stated == "All")
      }
    )
  } else if (Plot == "TS") {
    data.plot |>
    dplyr::filter(
      (Group %in% Groups_r) &
      (
        if (area == "County") {
          (State %in% Stated)
        } else if (area == "State") {
          State == "All"
        }
      )
    )
  } else {
    stop("Invalid plot type")
  }

  # Variables Definitions
  data.plot <- get_data_plot_app(data.plot)

  # Variables Names
  y.name <- get_var_name_app(Vary)
  x.name <- get_var_name_app(Varx)

  Rename <- function(GroupN) {
    ifelse(GroupN == "Total", "All", GroupN)
  }
  data.plot$Group <- Rename(data.plot$Group)
  FGroups <- Rename(RACES)
  FGroups <- FGroups[FGroups %in% data.plot$Group]
  colors <- setNames(RACE_COLORS, Rename(names(RACE_COLORS)))
  colors <- colors[FGroups]

  sc.plot <- (
    ggplot2::ggplot(
      data = data.plot,
      ggplot2::aes(
        x = .data[[Varx]],
        y = .data[[Vary]],
        color = factor(Group, levels = FGroups)
      ),
      na.rm = TRUE
    ) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::scale_color_manual(
      name = "Group",
      values = colors,
      na.translate = F
    ) +
    ggplot2::labs(x = x.name, y = y.name) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 20),
      axis.title.y = element_text(
        margin = ggplot2::margin(
          t = 0,
          r = 5,
          b = 0,
          l = 0
        ),
        size = 20
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 5,
          r = 0,
          b = 0,
          l = 0
        ),
        size = 20
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 15),
      legend.position = "bottom"
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(
        group = Group,
        color = Group
      ),
      formula = y ~ x,
      method = SC.Smoother,
      se = FALSE,
      na.rm = TRUE
    )
  )

  box.plot <- (
    ggplot2::ggplot(
      data = data.plot,
      ggplot2::aes(
        x = factor(Group, levels = FGroups),
        y = .data[[Vary]],
        fill = factor(Group, levels = FGroups)
      )
    ) +
    (
      if (BP.Violin == "F") {
        ggplot2::geom_boxplot(linewidth = 0.7)
      } else if (BP.Violin == "T") {
        ggplot2::geom_violin(linewidth = 0.7)
      }
    ) +
    (
      if (BP.Violin == "F") {
        ggplot2::geom_blank()
      } else if (BP.Violin == "T") {
        ggplot2::geom_jitter()
      }
    ) +
    ggplot2::ylab(y.name) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.y = ggplot2::element_text(size = 22, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 16, face = "bold"),
      axis.text = ggplot2::element_text(size = 20)
    )
  )

  ts.plot <- (
    ggplot2::ggplot(
      data = data.plot,
      ggplot2::aes(
        x = Year,
        y = .data[[Vary]],
        color = factor(Group, levels = FGroups)
      )
    ) +
    (
      if (area == "State") {
        ggplot2::geom_line(linewidth = 1.5)
      } else if (area == "County") {
        ggplot2::stat_summary(geom = "line", fun = median, linewidth = 1.5)
      }
    ) +
    ggplot2::scale_color_manual(
      name = "Group",
      values = colors,
      na.translate = F
    ) +
    ggplot2::ylab(y.name) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 20),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0,
          r = 5,
          b = 0,
          l = 0
        ),
        size = 20
      ),
      axis.title.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 15),
      legend.position = "bottom"
    )
  )

  if (Plot == "BP") {
    box.plot
  } else if (Plot == "SC") {
    sc.plot
  } else if (Plot == "TS") {
    ts.plot
  } else {
    stop("Plot must be one of 'BP', 'SC', or 'TS'.")
  }
}

plot_prediction_app <- function(
  model,
  race = RACES_MODEL,
  state = "All",
  inc.inc = NULL,
  separate_y = FALSE,
  x_lim_full = FALSE,
  fill_alpha = 0.3,
  title = NULL,
  show_subtitle = TRUE,
  base_size = 20
) {
  #' @title plot_prediction_app()
  #' @description Helper function for app interface to plot predictions.
  #' @param model model to use for prediction (brms::brm() output).
  #' @param race races to predict and plot. If "All" plots all races,
  #' else must be a subset of RACES_MODEL.
  #' @param state which state to predict. If "All" will show the predicition for whole US.
  #' @param inc.inc covariate value. If NULL gets default value from data.
  #' @param separate_y refer to plot_prediction() documentation.
  #' @param x_lim_full refer to plot_prediction() documentation.
  #' @param fill_alpha refer to plot_prediction() documentation.
  #' @param title refer to plot_prediction() documentation.
  #' @param show_subtitle refer to plot_prediction() documentation.
  #' @param interval_size refer to plot_prediction() documentation.
  new_data <- (
    list(
      race = race,
      state = state,
      inc.inc = inc.inc
    ) |>
    purrr::discard(is.null)
  )

  plot_prediction(
    model = model,
    data = MODEL_DATA,
    new_data = new_data,
    separate_y = separate_y,
    x_lim_full = x_lim_full,
    fill_alpha = fill_alpha,
    title = title,
    show_subtitle = show_subtitle,
    base_size = base_size
  )
}

# Plots function (for choropleth maps)
plot_choropleth_app <- function(
  area = "State",
  Stated = "All",
  Yearp = 2020,
  fill_var = "Own",
  col_pal = "Reds",
  title = NULL,
  facet_var = "Group",
  Group1 = "WhiteNH",
  Group2 = "None",
  Group3 = "None",
  Group4 = "None",
  facet_ncol = NA,
  breaks = NULL,
  outline = TRUE,
  color_na = "black",
  free_scales = FALSE
) {
  Groups_r <- c(Group1, Group2, Group3, Group4)

  if (all(Groups_r == "None")) {
    return(make_blank_plot())
  }

  data <- if (area == "State") {
    DATA_STATE_SHAPE_APP |>
    dplyr::filter(
      (State %in% STATES_MAINLAND) &
      (Year == Yearp) &
      (Group %in% Groups_r)
    )
  } else if ((area == "County") & (Stated == "All")) {
    DATA_COUNTY_SHAPE_APP |>
    dplyr::filter(
      (State %in% STATES_MAINLAND) &
      (Year == Yearp) &
      (Group %in% Groups_r)
    )
  } else if ((area == "County") & !(Stated == "All")) {
    DATA_COUNTY_SHAPE_APP |> dplyr::filter(
      (Group %in% Groups_r) &
      (Year == Yearp) &
      (State == Stated)
    )
  }

  data <- (
    get_data_plot_app(data) |>
    join_with_shape_app(area, Yearp) |>
    # filter again because join adds Alaska and Hawaii
    dplyr::filter(State %in% STATES_MAINLAND)
  )
  var.name <- get_var_name_app(fill_var)

  tmap_out <-
    tmap::tm_shape(data) +
    tmap::tm_fill(
      title = var.name,
      col = fill_var,
      palette = col_pal,
      style = "cont",
      breaks = breaks,
      colorNA = color_na,
      legend.is.portrait = FALSE
    )
  if (outline) {
    tmap_out <- tmap_out + tmap::tm_borders()
  }
  if (!is.null(facet_var)) {
    tmap_out <- (
      tmap_out +
      tmap::tm_facets(
        facet_var,
        free.scales = free_scales,
        ncol = facet_ncol
      )
    )
  }
  tmap_out <- (
    tmap_out +
    tmap::tm_layout(
      panel.label.size = 5,
      legend.outside = !free_scales,
      legend.outside.position = "bottom",
      legend.position = c("left", "bottom"),
      legend.outside.size = 0.3,
      legend.text.size = 1,
      legend.title.size= 1.5,
      legend.width = 0.5
    )
  )
  tmap_out
}

# Load data an set options for running the app
load_app_data <- function() {
  # Read county data
  DATA_COUNTY_FULL_APP <<- (
    readr::read_csv("data/input_processed/census_app/data.csv") |>
    dplyr::filter(State %in% STATES) |>
    dplyr::mutate(
      S2502.group.ownedp = S2502.group.owned / S2502.group.total,
      S1501.group.HSp = S1501.group.HSt / S1501.group.total,
      S1501.group.BSp = S1501.group.BSt / S1501.group.total
    )
  )

  # Aggregate data by state
  DATA_STATE_APP <<- (
    DATA_COUNTY_FULL_APP |>
    dplyr::group_by(Year, Group, State) |>
    dplyr::summarise(
      Owned_T = sum(as.numeric(S2502.group.total)),
      Owned_P = sum(as.numeric(S2502.group.owned) / Owned_T),
      HS_T = sum(as.numeric(S1501.group.HSt)),
      HS_P = HS_T / sum(as.numeric(S1501.group.total)),
      Col_T = sum(as.numeric(S1501.group.BSt)),
      Col_P = Col_T / sum(as.numeric(S1501.group.total)),
      Inc_Med = weighted.mean(
        as.numeric(S1903.group.medianincome),
        as.numeric(S1903.group.total),
        na.rm = TRUE
      ),
      Pop_T = sum(as.numeric(DP05.group.pop)),
      UE_P = weighted.mean(
        as.numeric(S2301.group.ue),
        as.numeric(S2301.group.total),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    rbind(
      DATA_COUNTY_FULL_APP |>
      dplyr::group_by(Year, Group) |>
      dplyr::summarise(
        State = "All",
        Owned_T = sum(as.numeric(S2502.group.total)),
        Owned_P = sum(as.numeric(S2502.group.owned) / Owned_T),
        HS_T = sum(as.numeric(S1501.group.HSt)),
        HS_P = HS_T / sum(as.numeric(S1501.group.total)),
        Col_T = sum(as.numeric(S1501.group.BSt)),
        Col_P = Col_T / sum(as.numeric(S1501.group.total)),
        Inc_Med = weighted.mean(
          as.numeric(S1903.group.medianincome),
          as.numeric(S1903.group.total),
          na.rm = TRUE
        ),
        Pop_T = sum(as.numeric(DP05.group.pop)),
        UE_P = weighted.mean(
          as.numeric(S2301.group.ue),
          as.numeric(S2301.group.total),
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    ) |>
    dplyr::group_by(Year, State) |>
    dplyr::mutate(Pop_P = Pop_T / sum(Pop_T[Group == "Total"])) |>
    dplyr::ungroup()
  )
  # County data
  DATA_COUNTY_APP <<- (
    DATA_COUNTY_FULL_APP |>
    dplyr::transmute(
      GEO_ID = GEO_ID,
      Year = Year,
      Group = Group,
      State = State,
      County = County,
      Owned_T = as.numeric(S2502.group.total),
      Owned_P = as.numeric(S2502.group.owned) / Owned_T,
      HS_T = as.numeric(S1501.group.HSt),
      HS_P = HS_T / as.numeric(S1501.group.total),
      Col_T = as.numeric(S1501.group.BSt),
      Col_P = Col_T / as.numeric(S1501.group.total),
      Inc_Med = as.numeric(S1903.group.medianincome),
      Pop_T = as.numeric(DP05.group.pop),
      UE_P = as.numeric(S2301.group.ue)
    ) |>
    rbind(
      DATA_COUNTY_FULL_APP |>
      dplyr::group_by(Year, Group) |>
      dplyr::summarise(
        GEO_ID = NA,
        State = "All",
        County = "All",
        Owned_T = sum(as.numeric(S2502.group.total)),
        Owned_P = sum(as.numeric(S2502.group.owned) / Owned_T),
        HS_T = sum(as.numeric(S1501.group.HSt)),
        HS_P = HS_T / sum(as.numeric(S1501.group.total)),
        Col_T = sum(as.numeric(S1501.group.BSt)),
        Col_P = Col_T / sum(as.numeric(S1501.group.total)),
        Inc_Med = weighted.mean(
          as.numeric(S1903.group.medianincome),
          as.numeric(S1903.group.total),
          na.rm = TRUE
        ),
        Pop_T = sum(as.numeric(DP05.group.pop)),
        UE_P = weighted.mean(
          as.numeric(S2301.group.ue),
          as.numeric(S2301.group.total),
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    ) |>
    dplyr::group_by(Year, State, County, GEO_ID) |>
    dplyr::mutate(Pop_P = Pop_T / Pop_T[Group == "Total"]) |>
    dplyr::ungroup()
  )

  # Load the final model for the app's predictive plots
  MODEL_APP <<- readRDS("data/output_1_posterior/model_rds/model_4.rds")
}

join_with_shape_app <- function(data, area = "county", year = 2020) {
  area <- stringr::str_to_lower(area)

  data <- data |> dplyr::filter(State != "All")

  if (area == "county") {
    data <- (
      data |>
      dplyr::mutate(
        GEO_ID = stringr::str_remove(GEO_ID, "0500000US") |> as.integer()
      ) |>
      dplyr::rename(county = County, geoid = GEO_ID)
    )
  }

  data <- data |> dplyr::rename(state = State, race = Group)

  data <- (
    data |>
    join_with_shape(area, year) |>
    dplyr::rename(State = state, Group = race)
  )

  if (area == "county") {
    data <- (
      data |>
      dplyr::rename(County = county, GEO_ID = geoid) |>
      dplyr::mutate(GEO_ID = paste0("0500000US", sprintf("%05d", GEO_ID)))
    )
  }

  data
}
