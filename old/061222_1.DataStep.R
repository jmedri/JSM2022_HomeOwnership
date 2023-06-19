library(tidyverse)

#------1. Housing Data--------------------------------------------------


housing.agg <- function(Year = 2020, Route = 1) {

  # Define Location of the File

  route <-
    if (Route == 1) {
      paste("Housing Tables S250X/S2501 - Occupancy/Occ.",
        as.character(Year), ".csv",
        sep = ""
      )
    } else if (Route == 2) {
      paste("Housing Tables S250X/S2502 - Demographics/Demo.",
        as.character(Year), ".csv",
        sep = ""
      )
    } else if (Route == 3) {
      paste("Housing Tables S250X/S2503 - Financial/Fin.",
        as.character(Year), ".csv",
        sep = ""
      )
    } else if (Route == 4) {
      paste("Housing Tables S250X/S2504 - Physical/Phy.",
        as.character(Year), ".csv",
        sep = ""
      )
    } else if (Route == 5) {
      paste("Housing Tables S250X/S2506 - Mortgage/Mort.",
        as.character(Year), ".csv",
        sep = ""
      )
    } else if (Route == 6) {
      paste("Housing Tables S250X/S2507 - NoMortgage/NoMort.",
            as.character(Year), ".csv",
            sep = ""
      )} else {}

  # Define and name working variables

  variables <-
    if (Route == 1 & Year <= 2016) {
      c(
        "GEO_ID", "NAME",
        "S2501_C02_001E", "S2501_C02_002E", "S2501_C02_003E",
        "S2501_C02_004E", "S2501_C02_005E",
        "S2501_C02_009E", "S2501_C02_023E"
      )
    } else if (Route == 1 & Year >= 2017) {
      c(
        "GEO_ID", "NAME",
        "S2501_C04_001E", "S2501_C04_002E", "S2501_C04_003E",
        "S2501_C04_004E", "S2501_C04_005E",
        "S2501_C04_009E", "S2501_C04_023E"
      )
    } else if (Route == 2 & Year <= 2016) {
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
    } else if (Route == 2 & Year >= 2017) {
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
    } else if (Route == 3 & Year <= 2016) {
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
    } else if (Route == 3 & Year >= 2017) {
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
    } else if (Route == 4 & Year <= 2016) {
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
    } else if (Route == 4 & Year >= 2017) {
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
    } else if (Route ==5 & Year <= 2016)
    {
      c("GEO_ID", "NAME",
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
    } else if (Route ==5 & Year >= 2017 & Year < 2020)
    {
      c("GEO_ID", "NAME",
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
    } else if (Route ==5 & Year == 2020)
    {
      c("GEO_ID", "NAME",
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
    } else if (Route ==6 & Year <= 2016)
    {
      c("GEO_ID", "NAME",
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
    } else if (Route ==6 & Year >= 2017)
    {
      c("GEO_ID", "NAME",
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
    }
  
  
  varnames <-
    if (Route == 1) {
      c(
        "GEO_ID", "NAME",
        "S2501.totalowned", "S2501.householdsize.1p",
        "S2501.householdsize.2p", "S2501.householdsize.3p",
        "S2501.householdsize.4p+", "S2501.householdtype.family",
        "S2501.householdtype.nonfamily",
        "Year", "Route"
      )
    } else if (Route == 2) {
      c(
        "GEO_ID", "NAME",
        "S2502.totalowned",
        "S2502.race.whiteshare", "S2502.race.blackshare",
        "S2502.race.asianshare", "S2502.race.hispanicshare",
        "S2502.age.35-", "S2502.age.35-44", "S2502.age.45-54",
        "S2502.age.55-64", "S2502.age.65-74", "S2502.age.75-84",
        "S2502.age.85+",
        "S2502.edu.HS-", "S2502.edu.HS", "S2502.edu.Col-", "S2502.edu.Col+",
        "Year", "Route",
        "S2502.yearmoved.2010+", "S2502.yearmoved.2000-2009",
        "S2502.yearmoved.1990-1999", "S2502.yearmoved.1989-"
      )
    } else if (Route == 3) {
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
        "Year", "Route",
        #Housing Costs Percentage,
        "S2503.costp.20-", "S2503.costp.20-30", "S2503.costp.30+",
        "S2503.costp.median"
      )
    } else if (Route ==4){
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
        "Year", "Route"
      )
    } else if (Route ==5)
    {
      c("GEO_ID", "NAME",
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
        "Year", "Route",
        #Housing Costs percentage
        "S2506.costp.20-", "S2506.costp.20-30", "S2506.costp.30+",
        "S2506.costp.median"
      )
    } else if (Route ==6)
    {
      c("GEO_ID", "NAME",
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
        "Year", "Route",
        #Housing Costs percentage
        "S2507.costp.20-", "S2507.costp.20-30", "S2507.costp.30+",
        "S2507.costp.median"
      )
    }
      

  # Call File and Create Year and Route Variables

  data.housing <- read.csv(route, header = TRUE)[-1, variables]
  data.housing$Year <- Year
  data.housing$Route <- Route


  # Some Extra Processing for Route 2

  data.housing$S2502_C024_028E <-
    ifelse(data.housing$Route == 2 & data.housing$Year <= 2016,
      as.numeric(data.housing$S2502_C02_022E) +
        as.numeric(data.housing$S2502_C02_023E),
      ifelse(data.housing$Route == 2 & data.housing$Year == 2017,
             as.numeric(data.housing$S2502_C04_022E) +
               as.numeric(data.housing$S2502_C04_023E),
      ifelse(data.housing$Route == 2 & data.housing$Year > 2017,
        as.numeric(data.housing$S2502_C04_022E) +
          as.numeric(data.housing$S2502_C04_023E) +
          as.numeric(data.housing$S2502_C04_024E),
        "S2502_C024_028E"
      )
    ))

  data.housing$S2502_C024_029E <-
    ifelse(data.housing$Route == 2 & data.housing$Year <= 2016,
      as.numeric(data.housing$S2502_C02_024E),
      ifelse(data.housing$Route == 2 & data.housing$Year == 2017,
             as.numeric(data.housing$S2502_C04_024E),      
      ifelse(data.housing$Route == 2 & data.housing$Year > 2017,
        data.housing$S2502_C04_025E,
        "S2502_C024_029E"
      )
    ))

  data.housing$S2502_C024_030E <-
    ifelse(data.housing$Route == 2 & data.housing$Year <= 2016,
      as.numeric(data.housing$S2502_C02_025E),
      ifelse(data.housing$Route == 2 & data.housing$Year == 2017,
             as.numeric(data.housing$S2502_C04_025E),
      ifelse(data.housing$Route == 2 & data.housing$Year > 2017,
        data.housing$S2502_C04_026E,
        "S2502_C024_030E"
      )
    ))

  data.housing$S2502_C024_031E <-
    ifelse(data.housing$Route == 2 & data.housing$Year <= 2016,
      as.numeric(data.housing$S2502_C02_026E) +
        as.numeric(data.housing$S2502_C02_027E),
      ifelse(data.housing$Route == 2 & data.housing$Year == 2017,
             as.numeric(data.housing$S2502_C04_026E) +
               as.numeric(data.housing$S2502_C04_027E),
      ifelse(data.housing$Route == 2 & data.housing$Year > 2017,
        as.numeric(data.housing$S2502_C04_027E),
        "S2502_C024_031E"
      )
    ))

  data.housing <-
    if (Route == 2 & Year <= 2016) {
      data.housing[, -which(names(data.housing) %in%
        c(
          "S2502_C02_022E",
          "S2502_C02_023E",
          "S2502_C02_024E",
          "S2502_C02_025E",
          "S2502_C02_026E",
          "S2502_C02_027E"
        ))]
    } else if (Route == 2 & Year >= 2017) {
      data.housing[, -which(names(data.housing) %in%
        c(
          "S2502_C04_022E",
          "S2502_C04_023E",
          "S2502_C04_024E",
          "S2502_C04_025E",
          "S2502_C04_026E",
          "S2502_C04_027E"
        ))]
    } else if (Route != 2) {
      data.housing[, -which(names(data.housing) %in%
        c(
          "S2502_C024_028E",
          "S2502_C024_029E",
          "S2502_C024_030E",
          "S2502_C024_031E"
        ))]
    }

  # Some Extra Processing for Route 3
  
  data.housing$S2503_C024_045E <-
    
  ifelse(data.housing$Route ==3 & data.housing$Year <= 2016,
         as.numeric(data.housing$S2503_C02_026E)+
           as.numeric(data.housing$S2503_C02_030E)+
           as.numeric(data.housing$S2503_C02_034E)+
           as.numeric(data.housing$S2503_C02_038E)+
           as.numeric(data.housing$S2503_C02_042E),
         ifelse(data.housing$Route ==3 & data.housing$Year >= 2017,
                as.numeric(data.housing$S2503_C04_026E)+
                  as.numeric(data.housing$S2503_C04_030E)+
                  as.numeric(data.housing$S2503_C04_034E)+
                  as.numeric(data.housing$S2503_C04_038E)+
                  as.numeric(data.housing$S2503_C04_042E),
         "S2503_C024_044E")
         )
    
  data.housing$S2503_C024_046E <-
    ifelse(data.housing$Route ==3 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2503_C02_027E)+
             as.numeric(data.housing$S2503_C02_031E)+
             as.numeric(data.housing$S2503_C02_035E)+
             as.numeric(data.housing$S2503_C02_039E)+
             as.numeric(data.housing$S2503_C02_043E),
           ifelse(data.housing$Route ==3 & data.housing$Year >= 2017,
                  as.numeric(data.housing$S2503_C04_027E)+
                    as.numeric(data.housing$S2503_C04_031E)+
                    as.numeric(data.housing$S2503_C04_035E)+
                    as.numeric(data.housing$S2503_C04_039E)+
                    as.numeric(data.housing$S2503_C04_043E),
                  "S2503_C024_045E")
    )
    
  data.housing$S2503_C024_047E <-
    ifelse(data.housing$Route ==3 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2503_C02_028E)+
             as.numeric(data.housing$S2503_C02_032E)+
             as.numeric(data.housing$S2503_C02_036E)+
             as.numeric(data.housing$S2503_C02_040E)+
             as.numeric(data.housing$S2503_C02_044E),
           ifelse(data.housing$Route ==3 & data.housing$Year >= 2017,
                  as.numeric(data.housing$S2503_C04_028E)+
                    as.numeric(data.housing$S2503_C04_032E)+
                    as.numeric(data.housing$S2503_C04_036E)+
                    as.numeric(data.housing$S2503_C04_040E)+
                    as.numeric(data.housing$S2503_C04_044E),
                  "S2503_C024_046E")
    )
  
  data.housing$S2503_C024_048E <- 
    ifelse(data.housing$Route ==3 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2503_C02_024E)*12 /
             as.numeric(data.housing$S2503_C02_013E),
           ifelse(data.housing$Route ==3 & data.housing$Year >= 2017,
                  as.numeric(data.housing$S2503_C04_024E)*12 /
                    as.numeric(data.housing$S2503_C04_013E),
                  "S2503_C024_047E")
    )
  
  data.housing <-
    if (Route == 3 & Year <= 2016) {
      data.housing[, -which(names(data.housing) %in%
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
                              ))]
    } else if (Route == 3 & Year >= 2017) {
      data.housing[, -which(names(data.housing) %in%
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
                              ))]
    } else if (Route != 3) {
      data.housing[, -which(names(data.housing) %in%
                              c(
                                "S2503_C024_045E", "S2503_C024_046E",
                                "S2503_C024_047E", "S2503_C024_048E"
                              ))]
    }

  
  # Some Extra Processing for Route 5
  
  data.housing$S2506_C012_067E <- 
    ifelse(data.housing$Route ==5 & data.housing$Year <= 2016,
         as.numeric(data.housing$S2506_C01_041E)+
           as.numeric(data.housing$S2506_C01_045E)+
           as.numeric(data.housing$S2506_C01_049E)+
           as.numeric(data.housing$S2506_C01_053E)+
           as.numeric(data.housing$S2506_C01_057E),
         ifelse(data.housing$Route == 5 & 
                  data.housing$Year >= 2017 & data.housing$Year < 2020,
                as.numeric(data.housing$S2506_C02_041E)+
                  as.numeric(data.housing$S2506_C02_045E)+
                  as.numeric(data.housing$S2506_C02_049E)+
                  as.numeric(data.housing$S2506_C02_053E)+
                  as.numeric(data.housing$S2506_C02_057E),
                ifelse(data.housing$Route ==5 & data.housing$Year == 2020,
                       as.numeric(data.housing$S2506_C02_042E)+
                         as.numeric(data.housing$S2506_C02_046E)+
                         as.numeric(data.housing$S2506_C02_050E)+
                         as.numeric(data.housing$S2506_C02_054E)+
                         as.numeric(data.housing$S2506_C02_058E),
                       "S2506_C012_067E"))
  )
  
  data.housing$S2506_C012_068E <- 
    ifelse(data.housing$Route ==5 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2506_C01_042E)+
             as.numeric(data.housing$S2506_C01_046E)+
             as.numeric(data.housing$S2506_C01_050E)+
             as.numeric(data.housing$S2506_C01_054E)+
             as.numeric(data.housing$S2506_C01_058E),
           ifelse(data.housing$Route ==5 & 
                    data.housing$Year >= 2017 & data.housing$Year < 2020,
                  as.numeric(data.housing$S2506_C02_042E)+
                    as.numeric(data.housing$S2506_C02_046E)+
                    as.numeric(data.housing$S2506_C02_050E)+
                    as.numeric(data.housing$S2506_C02_054E)+
                    as.numeric(data.housing$S2506_C02_058E),
                  ifelse(data.housing$Route ==5 & data.housing$Year == 2020,
                         as.numeric(data.housing$S2506_C02_043E)+
                           as.numeric(data.housing$S2506_C02_047E)+
                           as.numeric(data.housing$S2506_C02_051E)+
                           as.numeric(data.housing$S2506_C02_055E)+
                           as.numeric(data.housing$S2506_C02_059E),
                         "S2506_C012_068E"))
    )
  
  data.housing$S2506_C012_069E <- 
    ifelse(data.housing$Route ==5 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2506_C01_043E)+
             as.numeric(data.housing$S2506_C01_047E)+
             as.numeric(data.housing$S2506_C01_051E)+
             as.numeric(data.housing$S2506_C01_055E)+
             as.numeric(data.housing$S2506_C01_059E),
           ifelse(data.housing$Route ==5 & 
                    data.housing$Year >= 2017 & data.housing$Year < 2020,
                  as.numeric(data.housing$S2506_C02_043E)+
                    as.numeric(data.housing$S2506_C02_047E)+
                    as.numeric(data.housing$S2506_C02_051E)+
                    as.numeric(data.housing$S2506_C02_055E)+
                    as.numeric(data.housing$S2506_C02_059E),
                  ifelse(data.housing$Route ==5 & data.housing$Year == 2020,
                         as.numeric(data.housing$S2506_C02_044E)+
                           as.numeric(data.housing$S2506_C02_048E)+
                           as.numeric(data.housing$S2506_C02_052E)+
                           as.numeric(data.housing$S2506_C02_056E)+
                           as.numeric(data.housing$S2506_C02_060E),
                         "S2506_C012_069E"))
    )
  
  data.housing$S2506_C012_070E <-
    ifelse(data.housing$Route ==5 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2506_C01_039E)*12/
           as.numeric(data.housing$S2506_C01_023E),
           ifelse(data.housing$Route ==5 & 
                    data.housing$Year >= 2017 & data.housing$Year <2020,
                  as.numeric(data.housing$S2506_C02_039E)*12/
                    as.numeric(data.housing$S2506_C02_023E),
                  ifelse(data.housing$Route ==5 & data.housing$Year == 2020,
                         as.numeric(data.housing$S2506_C02_040E)*12/
                           as.numeric(data.housing$S2506_C02_024E),
                         "S2506_C012_069E")
                  ))
  
  data.housing <-
    if (Route == 5 & Year <= 2016) {
      data.housing[, -which(names(data.housing) %in%
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
                              ))]
    } else if (Route == 5 & Year >= 2017 & Year < 2020) {
      data.housing[, -which(names(data.housing) %in%
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
                              ))]
    } else if (Route == 5 & Year == 2020) {
      data.housing[, -which(names(data.housing) %in%
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
                              ))]
    } else if (Route != 5) {
      data.housing[, -which(names(data.housing) %in%
                              c(
                                "S2506_C012_067E", "S2506_C012_068E",
                                "S2506_C012_069E", "S2506_C012_070E"
                              ))]
    }
  
  # Some Extra Processing for Route 6
  
  data.housing$S2507_C012_059E <- 
    ifelse(data.housing$Route ==6 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2507_C01_034E)+
             as.numeric(data.housing$S2507_C01_038E)+
             as.numeric(data.housing$S2507_C01_042E)+
             as.numeric(data.housing$S2507_C01_046E)+
             as.numeric(data.housing$S2507_C01_050E),
           ifelse(data.housing$Route == 6 & 
                    data.housing$Year >= 2017,
                  as.numeric(data.housing$S2507_C02_034E)+
                    as.numeric(data.housing$S2507_C02_038E)+
                    as.numeric(data.housing$S2507_C02_042E)+
                    as.numeric(data.housing$S2507_C02_046E)+
                    as.numeric(data.housing$S2507_C02_050E),
                         "S2507_C012_059E"))
  
  data.housing$S2507_C012_060E <- 
    ifelse(data.housing$Route ==6 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2507_C01_035E)+
             as.numeric(data.housing$S2507_C01_039E)+
             as.numeric(data.housing$S2507_C01_043E)+
             as.numeric(data.housing$S2507_C01_047E)+
             as.numeric(data.housing$S2507_C01_051E),
           ifelse(data.housing$Route == 6 & 
                    data.housing$Year >= 2017,
                  as.numeric(data.housing$S2507_C02_035E)+
                    as.numeric(data.housing$S2507_C02_039E)+
                    as.numeric(data.housing$S2507_C02_043E)+
                    as.numeric(data.housing$S2507_C02_047E)+
                    as.numeric(data.housing$S2507_C02_051E),
                  "S2507_C012_060E"))
  
  data.housing$S2507_C012_061E <- 
    ifelse(data.housing$Route ==6 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2507_C01_036E)+
             as.numeric(data.housing$S2507_C01_040E)+
             as.numeric(data.housing$S2507_C01_044E)+
             as.numeric(data.housing$S2507_C01_048E)+
             as.numeric(data.housing$S2507_C01_052E),
           ifelse(data.housing$Route == 6 & 
                    data.housing$Year >= 2017,
                  as.numeric(data.housing$S2507_C02_036E)+
                    as.numeric(data.housing$S2507_C02_040E)+
                    as.numeric(data.housing$S2507_C02_044E)+
                    as.numeric(data.housing$S2507_C02_048E)+
                    as.numeric(data.housing$S2507_C02_052E),
                  "S2507_C012_061E"))
  
  data.housing$S2507_C012_062E <-
    ifelse(data.housing$Route ==6 & data.housing$Year <= 2016,
           as.numeric(data.housing$S2507_C01_032E)*12/
             as.numeric(data.housing$S2507_C01_019E),
           ifelse(data.housing$Route ==6 & data.housing$Year >= 2017,
                  as.numeric(data.housing$S2507_C02_032E)*12/
                    as.numeric(data.housing$S2507_C02_019E),
                         "S2507_C012_062E")
           )
  
  data.housing <-
    if (Route == 6 & Year <= 2016) {
      data.housing[, -which(names(data.housing) %in%
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
                              ))]
    } else if (Route == 6 & Year >= 2017) {
      data.housing[, -which(names(data.housing) %in%
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
                              ))]
    } else if (Route != 6) {
      data.housing[, -which(names(data.housing) %in%
                              c(
                                "S2507_C012_059E", "S2507_C012_060E",
                                "S2507_C012_061E", "S2507_C012_062E"
                              ))]
    }
  
  # Final steps: Rename variables, and create County, State variables

  colnames(data.housing) <- varnames
  data.housing$County <- gsub(",.*$", "", data.housing$NAME)
  data.housing$State <- gsub(".*,", "", data.housing$NAME)
  
  return(data.housing)
}

housing.agg.test <- housing.agg(Route = 6, Year = 2020)

#------2. Race/Ethnic Data--------------------------------------------------

racethnic <- function(Year = 2020, Route = 1){
  
  # Define Location of the File
  
  route <-
    if (Route == 1) {
      paste("Race-Ethnic Variables/S1501 - Education Tables/Edu.",
            as.character(Year), ".csv",
            sep = ""
      )
    } else if (Route == 2) {
      paste("Race-Ethnic Variables/S1903 - Income Tables/Inc.",
            as.character(Year), ".csv",
            sep = ""
      )
    } else if (Route == 3) {
      paste("Race-Ethnic Variables/S2301 - Employment Tables/Emp.",
            as.character(Year), ".csv",
            sep = ""
      )
    } else if (Route == 4) {
      paste("Race-Ethnic Variables/S2502 - Demographics/Demo.",
            as.character(Year), ".csv",
            sep = ""
      )
    } else if (Route == 5) {
      paste("Race-Ethnic Variables/DP05 - Population Tables/Pop.",
            as.character(Year), ".csv",
            sep = ""
      )
    } else {}  
  
  # Define and name working variables
  
  variables <-
    if(Route == 1)
    {c(
      #GEOID and NAME
      "GEO_ID",	"NAME",
      #Total in the survey
      "S1501_C01_001E", "S1501_C01_006E",
      #Total broken by race
      "S1501_C01_010E", "S1501_C01_003E", "S1501_C01_005E", "S1501_C01_009E",
      #Total by educational attainment (considering both age groups)
      "S1501_C01_003E", "S1501_C01_004E", "S1501_C01_005E",
      "S1501_C01_009E", "S1501_C01_010E", "S1501_C01_011E",
      "S1501_C01_012E", "S1501_C01_013E",
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
    )} else if (Route == 2 & Year <= 2016)
    {c(
      #GEOID and NAME
      "GEO_ID",	"NAME",
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
    } else if (Route == 2 & Year >= 2017)
    {c(
      #GEOID and NAME
      "GEO_ID",	"NAME",
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
    } else if (Route == 3)
    {c(#GEOID and NAME
      "GEO_ID",	"NAME",
      #Total households in the survey
      "S2301_C01_001E",
      #Total households broken by race (totals)
      "S2301_C01_012E", "S2301_C01_013E", "S2301_C01_014E", "S2301_C01_015E",
      "S2301_C01_016E", "S2301_C01_017E", "S2301_C01_018E", "S2301_C01_019E",
      "S2301_C01_020E",
      #Labor Force Part Rate, Employment/Pop Rate, Unemployment Rate Totals
      "S2301_C02_001E", "S2301_C03_001E", "S2301_C04_001E",
      #Same as above but broken by race/ethnicity
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
    } else if (Route == 4 & Year <= 2016)
    {c(#GEOID and NAME
      "GEO_ID",	"NAME",
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
    } else if (Route == 4 & Year >= 2017)
    {c(#GEOID and NAME
      "GEO_ID",	"NAME",
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
    } else if (Route ==5 & Year <= 2016)
    {c("GEO_ID",	"NAME",
       "DP05_0028E", 
       "DP05_0032E", "DP05_0033E", "DP05_0034E", "DP05_0039E", 
       "DP05_0047E", "DP05_0052E", "DP05_0053E", "DP05_0066E",
       "DP05_0072E"
    )
    } else if (Route == 5 & Year >= 2017)
      {c("GEO_ID",	"NAME",
        "DP05_0033E",
        "DP05_0037E", "DP05_0038E", "DP05_0039E", "DP05_0044E",
        "DP05_0052E", "DP05_0057E", "DP05_0058E", "DP05_0071E",
        "DP05_0077E"
      )}
  
  # Call File and Create Year and Route Variables
  
  data.racethnic <- read.csv(route, header = TRUE)[-1, variables]
  data.racethnic$Year <- Year
  data.racethnic$Route <- Route
  data.racethnic$County <- gsub(",.*$", "", data.racethnic$NAME)
  data.racethnic$State <- gsub(".*,", "", data.racethnic$NAME)
  
  # Build data set
  
  data.racethnic <-
    if(Route ==1)
    {data.racethnic %>%
        summarise(
          GEO_ID	= GEO_ID,
          Year = Year,
          County = County,
          State = State,
          Group = "Total",
          S1501.group.total = as.numeric(S1501_C01_001E) +
            as.numeric(S1501_C01_006E),
          S1501.group.HSt = as.numeric(S1501_C01_003E) + 
            as.numeric(S1501_C01_004E) + as.numeric(S1501_C01_005E) +
            as.numeric(S1501_C01_009E)+ as.numeric(S1501_C01_010E)+
            as.numeric(S1501_C01_011E)+ as.numeric(S1501_C01_012E)+
            as.numeric(S1501_C01_013E),
          S1501.group.HSp = S1501.group.HSt/S1501.group.total,
          S1501.group.BSt = as.numeric(S1501_C01_005E) + as.numeric(S1501_C01_012E)+
            as.numeric(S1501_C01_013E),
          S1501.group.BSp = S1501.group.BSt/S1501.group.total
        ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "White",
                  S1501.group.total = as.numeric(S1501_C01_028E),
                  S1501.group.HSt = as.numeric(S1501_C01_029E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_030E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
                ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "WhiteNH",
                  S1501.group.total = as.numeric(S1501_C01_031E),
                  S1501.group.HSt = as.numeric(S1501_C01_032E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_033E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
        ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "Black",
                  S1501.group.total = as.numeric(S1501_C01_034E),
                  S1501.group.HSt = as.numeric(S1501_C01_035E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_036E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
        ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "AIoAN",
                  S1501.group.total = as.numeric(S1501_C01_037E),
                  S1501.group.HSt = as.numeric(S1501_C01_038E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_039E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
        ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "Asian",
                  S1501.group.total = as.numeric(S1501_C01_040E),
                  S1501.group.HSt = as.numeric(S1501_C01_041E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_042E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
        ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "NHoOPI",
                  S1501.group.total = as.numeric(S1501_C01_043E),
                  S1501.group.HSt = as.numeric(S1501_C01_044E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_045E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
        ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "Other",
                  S1501.group.total = as.numeric(S1501_C01_046E),
                  S1501.group.HSt = as.numeric(S1501_C01_047E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_048E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
        ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "Two+",
                  S1501.group.total = as.numeric(S1501_C01_049E),
                  S1501.group.HSt = as.numeric(S1501_C01_050E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_051E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
        ) %>%
        rbind(., data.racethnic %>%
                summarise(
                  GEO_ID	= GEO_ID,
                  Year = Year,
                  County = County,
                  State = State,
                  Group = "Hispanic",
                  S1501.group.total = as.numeric(S1501_C01_052E),
                  S1501.group.HSt = as.numeric(S1501_C01_053E),
                  S1501.group.HSp = S1501.group.HSt/S1501.group.total,
                  S1501.group.BSt = as.numeric(S1501_C01_054E),
                  S1501.group.BSp = S1501.group.BSt/S1501.group.total
                )
        )
      } else if(Route == 2){data.racethnic %>%
          summarise(
            GEO_ID	= GEO_ID,
            Year = Year,
            County = County,
            State = State,
            Group = "Total",
            S1903.group.total = as.numeric(S1903_C01_001E),
            S1903.group.medianincome = 
              ifelse(Year < 2017, S1903_C02_001E, S1903_C03_001E)
            ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "White",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_002E)*
                                as.numeric(S1903_C01_001E)/100),
                             S1903_C01_002E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_002E,
                        S1903_C03_002E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Black",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_003E)*
                                as.numeric(S1903_C01_001E)/100),
                        S1903_C01_003E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_003E,
                        S1903_C03_003E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "AIoAN",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_004E)*
                                as.numeric(S1903_C01_001E)/100),
                        S1903_C01_004E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_004E,
                        S1903_C03_004E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Asian",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_005E)*
                                as.numeric(S1903_C01_001E)/100),
                        S1903_C01_005E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_005E,
                        S1903_C03_005E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "NHoOPI",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_006E)*
                                as.numeric(S1903_C01_001E)/100),
                        S1903_C01_006E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_006E,
                        S1903_C03_006E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Other",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_007E)*
                                as.numeric(S1903_C01_001E)/100),
                        S1903_C01_007E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_007E,
                        S1903_C03_007E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Two+",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_008E)*
                                as.numeric(S1903_C01_001E)/100),
                        S1903_C01_009E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_008E,
                        S1903_C03_008E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Hispanic",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_009E)*
                                as.numeric(S1903_C01_001E)/100),
                        S1903_C01_009E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_009E,
                        S1903_C03_009E)
                  )
                ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "WhiteNH",
                    S1903.group.total = 
                      ifelse(
                        Year < 2017,
                        round(as.numeric(S1903_C01_010E)*
                                as.numeric(S1903_C01_001E)/100),
                        S1903_C01_010E),
                    S1903.group.medianincome =
                      ifelse(
                        Year < 2017,
                        S1903_C02_010E,
                        S1903_C03_010E)
                  )
          )
      } else if(Route == 3){data.racethnic %>%
          summarise(
            GEO_ID	= GEO_ID,
            Year = Year,
            County = County,
            State = State,
            Group = "Total",
            S2301.group.total = as.numeric(S2301_C01_001E),
            S2301.group.lfpr = as.numeric(S2301_C02_001E),
            S2301.group.epr = as.numeric(S2301_C03_001E),
            S2301.group.ue = as.numeric(S2301_C04_001E) 
             ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "White",
                    S2301.group.total = as.numeric(S2301_C01_012E),
                    S2301.group.lfpr = as.numeric(S2301_C02_012E),
                    S2301.group.epr = as.numeric(S2301_C03_012E),
                    S2301.group.ue = as.numeric(S2301_C04_012E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Black",
                    S2301.group.total = as.numeric(S2301_C01_013E),
                    S2301.group.lfpr = as.numeric(S2301_C02_013E),
                    S2301.group.epr = as.numeric(S2301_C03_013E),
                    S2301.group.ue = as.numeric(S2301_C04_013E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "AIoAN",
                    S2301.group.total = as.numeric(S2301_C01_014E),
                    S2301.group.lfpr = as.numeric(S2301_C02_014E),
                    S2301.group.epr = as.numeric(S2301_C03_014E),
                    S2301.group.ue = as.numeric(S2301_C04_014E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Asian",
                    S2301.group.total = as.numeric(S2301_C01_015E),
                    S2301.group.lfpr = as.numeric(S2301_C02_015E),
                    S2301.group.epr = as.numeric(S2301_C03_015E),
                    S2301.group.ue = as.numeric(S2301_C04_015E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "NHoOPI",
                    S2301.group.total = as.numeric(S2301_C01_016E),
                    S2301.group.lfpr = as.numeric(S2301_C02_016E),
                    S2301.group.epr = as.numeric(S2301_C03_016E),
                    S2301.group.ue = as.numeric(S2301_C04_016E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Other",
                    S2301.group.total = as.numeric(S2301_C01_017E),
                    S2301.group.lfpr = as.numeric(S2301_C02_017E),
                    S2301.group.epr = as.numeric(S2301_C03_017E),
                    S2301.group.ue = as.numeric(S2301_C04_017E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Two+",
                    S2301.group.total = as.numeric(S2301_C01_018E),
                    S2301.group.lfpr = as.numeric(S2301_C02_018E),
                    S2301.group.epr = as.numeric(S2301_C03_018E),
                    S2301.group.ue = as.numeric(S2301_C04_018E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Hispanic",
                    S2301.group.total = as.numeric(S2301_C01_019E),
                    S2301.group.lfpr = as.numeric(S2301_C02_019E),
                    S2301.group.epr = as.numeric(S2301_C03_019E),
                    S2301.group.ue = as.numeric(S2301_C04_019E)
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
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
      } else if(Route == 4){data.racethnic %>%
          summarise(
            GEO_ID	= GEO_ID,
            Year = Year,
            County = County,
            State = State,
            Group = "Total",
            S2502.group.total = as.numeric(S2502_C01_001E),
            S2502.group.owned = 
              ifelse(Year < 2017,
                     as.numeric(S2502_C02_001E),
                     as.numeric(S2502_C03_001E)),
            S2502.group.ownedp = S2502.group.owned/S2502.group.total   
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "White",
                    S2502.group.total = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C01_001E)*
                               as.numeric(S2502_C01_002E)/100),
                             as.numeric(S2502_C01_002E)
                      ),
                    S2502.group.owned = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C02_001E)*
                               as.numeric(S2502_C02_002E)/100),
                             as.numeric(S2502_C03_002E)
                      ),
                    S2502.group.ownedp = S2502.group.owned/S2502.group.total
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Black",
                    S2502.group.total = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C01_001E)*
                               as.numeric(S2502_C01_003E)/100),
                             as.numeric(S2502_C01_003E)
                      ),
                    S2502.group.owned = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C02_001E)*
                               as.numeric(S2502_C02_003E)/100),
                             as.numeric(S2502_C03_003E)
                      ),
                    S2502.group.ownedp = S2502.group.owned/S2502.group.total
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "AIoAN",
                    S2502.group.total = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C01_001E)*
                                     as.numeric(S2502_C01_004E)/100),
                             as.numeric(S2502_C01_004E)
                      ),
                    S2502.group.owned = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C02_001E)*
                                     as.numeric(S2502_C02_004E)/100),
                             as.numeric(S2502_C03_004E)
                      ),
                    S2502.group.ownedp = S2502.group.owned/S2502.group.total
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Asian",
                    S2502.group.total = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C01_001E)*
                               as.numeric(S2502_C01_005E)/100),
                             as.numeric(S2502_C01_005E)
                      ),
                    S2502.group.owned = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C02_001E)*
                               as.numeric(S2502_C02_005E)/100),
                             as.numeric(S2502_C03_005E)
                      ),
                    S2502.group.ownedp = S2502.group.owned/S2502.group.total
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "NHoOPI",
                    S2502.group.total = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C01_001E)*
                                     as.numeric(S2502_C01_006E)/100),
                             as.numeric(S2502_C01_006E)
                      ),
                    S2502.group.owned = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C02_001E)*
                                     as.numeric(S2502_C02_006E)/100),
                             as.numeric(S2502_C03_006E)
                      ),
                    S2502.group.ownedp = S2502.group.owned/S2502.group.total
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Other",
                    S2502.group.total = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C01_001E)*
                                     as.numeric(S2502_C01_007E)/100),
                             as.numeric(S2502_C01_007E)
                      ),
                    S2502.group.owned = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C02_001E)*
                                     as.numeric(S2502_C02_007E)/100),
                             as.numeric(S2502_C03_007E)
                      ),
                    S2502.group.ownedp = S2502.group.owned/S2502.group.total
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Two+",
                    S2502.group.total = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C01_001E)*
                                     as.numeric(S2502_C01_008E)/100),
                             as.numeric(S2502_C01_008E)
                      ),
                    S2502.group.owned = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C02_001E)*
                                     as.numeric(S2502_C02_008E)/100),
                             as.numeric(S2502_C03_008E)
                      ),
                    S2502.group.ownedp = S2502.group.owned/S2502.group.total
                  )
          ) %>%
          rbind(., data.racethnic %>%
                  summarise(
                    GEO_ID	= GEO_ID,
                    Year = Year,
                    County = County,
                    State = State,
                    Group = "Hispanic",
                    S2502.group.total = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C01_001E)*
                               as.numeric(S2502_C01_009E)/100),
                             as.numeric(S2502_C01_009E)
                      ),
                    S2502.group.owned = 
                      ifelse(Year < 2017,
                             round(as.numeric(S2502_C02_001E)*
                               as.numeric(S2502_C02_009E)/100),
                             as.numeric(S2502_C03_009E)
                      ),
                    S2502.group.ownedp = S2502.group.owned/S2502.group.total
                    )
          ) %>% 
          rbind(., data.racethnic %>%
                   summarise(
                     GEO_ID	= GEO_ID,
                     Year = Year,
                     County = County,
                     State = State,
                     Group = "WhiteNH",
                     S2502.group.total = 
                       ifelse(Year < 2017,
                              round(as.numeric(S2502_C01_001E)*
                                      as.numeric(S2502_C01_010E)/100),
                              as.numeric(S2502_C01_010E)
                       ),
                     S2502.group.owned = 
                       ifelse(Year < 2017,
                              round(as.numeric(S2502_C02_001E)*
                                      as.numeric(S2502_C02_010E)/100),
                              as.numeric(S2502_C03_010E)
                       ),
                     S2502.group.ownedp = S2502.group.owned/S2502.group.total
                   )
          )
      } else if(Route == 5){data.racethnic %>%
          summarise(
            GEO_ID	= GEO_ID,
            Year = Year,
            County = County,
            State = State,
            Group = "Total",
            DP05.group.pop = 
              ifelse(Year < 2017, 
                     as.numeric(DP05_0028E),
                     as.numeric(DP05_0033E)),
            DP05.group.popp = 
              ifelse(Year < 2017,
                     DP05.group.pop/as.numeric(DP05_0028E),
                     DP05.group.pop/as.numeric(DP05_0033E))
            ) %>% 
          rbind(., data.racethnic %>%
            summarise(
              GEO_ID	= GEO_ID,
              Year = Year,
              County = County,
              State = State,
              Group = "White",
              DP05.group.pop = 
                ifelse(Year < 2017, 
                       as.numeric(DP05_0032E),
                       as.numeric(DP05_0037E)),
              DP05.group.popp = 
                ifelse(Year < 2017,
                       DP05.group.pop/as.numeric(DP05_0028E),
                       DP05.group.pop/as.numeric(DP05_0033E)))
          ) %>%
    rbind(., data.racethnic %>%
          summarise(
            GEO_ID	= GEO_ID,
            Year = Year,
            County = County,
            State = State,
            Group = "Black",
            DP05.group.pop = 
              ifelse(Year < 2017, 
                     as.numeric(DP05_0033E),
                     as.numeric(DP05_0038E)),
            DP05.group.popp = 
              ifelse(Year < 2017,
                     DP05.group.pop/as.numeric(DP05_0028E),
                     DP05.group.pop/as.numeric(DP05_0033E)))
          ) %>%
    rbind(., data.racethnic %>%
            summarise(
              GEO_ID	= GEO_ID,
              Year = Year,
              County = County,
              State = State,
              Group = "AIoAN",
              DP05.group.pop = 
                ifelse(Year < 2017, 
                       as.numeric(DP05_0034E),
                       as.numeric(DP05_0039E)),
              DP05.group.popp = 
                ifelse(Year < 2017,
                       DP05.group.pop/as.numeric(DP05_0028E),
                       DP05.group.pop/as.numeric(DP05_0033E)))
          ) %>%
    rbind(., data.racethnic %>%
            summarise(
              GEO_ID	= GEO_ID,
              Year = Year,
              County = County,
              State = State,
              Group = "Asian",
              DP05.group.pop = 
                ifelse(Year < 2017, 
                       as.numeric(DP05_0039E),
                       as.numeric(DP05_0044E)),
              DP05.group.popp = 
                ifelse(Year < 2017,
                       DP05.group.pop/as.numeric(DP05_0028E),
                       DP05.group.pop/as.numeric(DP05_0033E)))
          ) %>%
    rbind(., data.racethnic %>%
            summarise(
              GEO_ID	= GEO_ID,
              Year = Year,
              County = County,
              State = State,
              Group = "NHoOPI",
              DP05.group.pop = 
                ifelse(Year < 2017, 
                       as.numeric(DP05_0047E),
                       as.numeric(DP05_0052E)),
              DP05.group.popp = 
                ifelse(Year < 2017,
                       DP05.group.pop/as.numeric(DP05_0028E),
                       DP05.group.pop/as.numeric(DP05_0033E)))
          ) %>%
    rbind(., data.racethnic %>%
            summarise(
              GEO_ID	= GEO_ID,
              Year = Year,
              County = County,
              State = State,
              Group = "Other",
              DP05.group.pop = 
                ifelse(Year < 2017, 
                       as.numeric(DP05_0052E),
                       as.numeric(DP05_0057E)),
              DP05.group.popp = 
                ifelse(Year < 2017,
                       DP05.group.pop/as.numeric(DP05_0028E),
                       DP05.group.pop/as.numeric(DP05_0033E)))
          ) %>%
    rbind(., data.racethnic %>%
            summarise(
              GEO_ID	= GEO_ID,
              Year = Year,
              County = County,
              State = State,
              Group = "Two+",
              DP05.group.pop = 
                ifelse(Year < 2017, 
                       as.numeric(DP05_0053E),
                       as.numeric(DP05_0058E)),
              DP05.group.popp = 
                ifelse(Year < 2017,
                       DP05.group.pop/as.numeric(DP05_0028E),
                       DP05.group.pop/as.numeric(DP05_0033E)))
          ) %>%
    rbind(., data.racethnic %>%
            summarise(
              GEO_ID	= GEO_ID,
              Year = Year,
              County = County,
              State = State,
              Group = "Hispanic",
              DP05.group.pop = 
                ifelse(Year < 2017, 
                       as.numeric(DP05_0066E),
                       as.numeric(DP05_0071E)),
              DP05.group.popp = 
                ifelse(Year < 2017,
                       DP05.group.pop/as.numeric(DP05_0028E),
                       DP05.group.pop/as.numeric(DP05_0033E)))
          ) %>%
    rbind(., data.racethnic %>%
            summarise(
              GEO_ID	= GEO_ID,
              Year = Year,
              County = County,
              State = State,
              Group = "WhiteNH",
              DP05.group.pop = 
                ifelse(Year < 2017, 
                       as.numeric(DP05_0072E),
                       as.numeric(DP05_0077E)),
              DP05.group.popp = 
                ifelse(Year < 2017,
                       DP05.group.pop/as.numeric(DP05_0028E),
                       DP05.group.pop/as.numeric(DP05_0033E)))
    )
  }
      
      return(data.racethnic)
      
    
  
}

racethnic.test <- racethnic(Year = 2015, Route = 5)

#-----------------CREATE FILES, MERGE THEM, AND EXPORT CSV--------------------#

agg.list.f <- function(Routef = 1, Varf = 1){
var.list <- list()

if(Varf == 1){
for (i in 2015:2020){
   housing.agg.i <- housing.agg(Year = i, Route = Routef) 
    var.list <- rbind(var.list,
                      housing.agg.i)
}}
else if(Varf == 2){
  for (i in 2015:2020){
    racethnic.i <- racethnic(Year = i, Route = Routef) 
    var.list <- rbind(var.list,
                      racethnic.i)
  }}

return(var.list)

}

housing.agg.list <- 
  list(agg.list.f(1, 1), agg.list.f(2, 1),
       agg.list.f(3, 1), agg.list.f(4, 1),
       agg.list.f(5, 1), agg.list.f(6, 1)) %>%
  reduce(full_join, by = c('GEO_ID', 'Year'))

racethnic.agg.list <- 
  list(agg.list.f(1, 2), agg.list.f(2, 2),
       agg.list.f(3, 2), agg.list.f(4, 2),
       agg.list.f(5, 2)) %>%
  reduce(full_join, by = c('GEO_ID', 'Year', 'Group'))

dataset.expo.final <- 
  list(racethnic.agg.list, housing.agg.list) %>%
  reduce(full_join, by = c('GEO_ID', 'Year')) %>%
  .[, -which(names(.) %in%
               c(
                 "County.x.x.x", "State.x.x.x", "County.y.x", "State.y.x",                    
                 "County.x.x.x.x", "State.x.x.x.x", "County.y.y.x", 
                 "State.y.y.x", "NAME.x", "Route.x", "County.x.y", "State.x.y",                    
                 "NAME.y", "Route.y", "County.y.y.y", "State.y.y.y", "NAME.x.x",
                 "Route.x.x", "County.x.x.y", "State.x.x.y", "NAME.y.y", 
                 "Route.y.y", "County.y.y.y.y", "State.y.y.y.y", 
                 "County.x.x.x.x.x", "State.x.x.x.x.x", "Route.y.y.y",
                 "County.y.y.y.y.y", "State.y.y.y.y.y", "NAME.x.x.x",
                 "Route.x.x.x", "NAME.y.y.y" 
                          ))] %>%
  unique(.)
 
names(dataset.expo.final)

#Some final manual calculations
  
dataset.expo.final$calc.mortp <-
  as.numeric(dataset.expo.final$S2506.totalmort)/
  as.numeric(dataset.expo.final$S2502.totalowned)

dataset.expo.final$calc.hvalue <-
  (as.numeric(dataset.expo.final$S2506.value.median)*
  as.numeric(dataset.expo.final$S2506.totalmort) +
  as.numeric(dataset.expo.final$S2507.value.median)*
  as.numeric(dataset.expo.final$S2507.totalmort))/
  (as.numeric(dataset.expo.final$S2506.totalmort) +
     as.numeric(dataset.expo.final$S2507.totalmort))

data.expo.final.temp <- 
  dataset.expo.final[,c(1:33,61,72,76,123,170,201,202)]

  
write.csv(dataset.expo.final,
          file = '072722_dataexpo.county.csv')

write.csv(data.expo.final.temp,
          file = 'dataexpo.county.csv')