#-----------------------------------------------------------------------------
# Define Libraries and Set Working Directory

library(tidyverse)
library(tmap)

#-----------------------------------------------------------------------------

#Read County Data

data.county <- read.csv("3.Data_Modelling_Output/input_processed/census/3_wide/data.csv") %>%
  filter(!State %in% c("Puerto Rico", "District of Columbia")) %>%
  mutate(
    S2502.group.ownedp = S2502.group.owned / S2502.group.total,
    S1501.group.HSp = S1501.group.HSt / S1501.group.total,
    S1501.group.BSp = S1501.group.BSt / S1501.group.total
  )

data_state_ch <- CENSUS_DATA |>
  dplyr::group_by(race, state) |>
  dplyr::summarise(
    edu.bs = weighted.mean(edu.bs, edu.tot, na.rm = TRUE),
    edu.hs = weighted.mean(edu.hs, edu.tot, na.rm = TRUE),
    emp.ue = weighted.mean(emp.ue, emp.tot, na.rm = TRUE),
    fin.cost = weighted.mean(fin.cost, fin.tot, na.rm = TRUE),
    fin.inc = weighted.mean(fin.inc, fin.tot, na.rm = TRUE),
    hom.own = weighted.mean(hom.own, hom.tot, na.rm = TRUE),
    inc.inc = weighted.mean(inc.inc, inc.tot, na.rm = TRUE),
    occ.fam = weighted.mean(occ.fam, occ.tot, na.rm = TRUE),
    val.hom = weighted.mean(val.hom, val.tot, na.rm = TRUE),
    val.mort = weighted.mean(val.mort, val.tot, na.rm = TRUE),
    val.tax = weighted.mean(val.tax, val.tot, na.rm = TRUE),
    edu.tot = sum(edu.tot, na.rm = TRUE),
    emp.tot = sum(emp.tot, na.rm = TRUE),
    fin.tot = sum(fin.tot, na.rm = TRUE),
    hom.tot = sum(hom.tot, na.rm = TRUE),
    inc.tot = sum(inc.tot, na.rm = TRUE),
    occ.tot = sum(occ.tot, na.rm = TRUE),
    pop.tot = sum(pop.tot, na.rm = TRUE),
    size = sum(size, na.rm = TRUE),
    val.tot = sum(val.tot, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(race) |>
  dplyr::mutate(pop.tot.nation = sum(pop.tot)) |>
  dplyr::ungroup() |>
  dplyr::group_by(state) |>
  dplyr::mutate(
    pop.tot.nation.all = pop.tot.nation[race == "Total"],
    pop.share.nation = pop.tot.nation / pop.tot.nation.all,
    pop.tot.all = pop.tot[race == "Total"],
    pop.share = pop.tot / pop.tot.all,
    pop.share.ratio = pop.share / pop.share.nation,
    size = round(pop.share * size[race == "Total"])
  ) |>
  dplyr::ungroup() |>
  join_with_shape("state")

data_county_ch <- join_with_shape(CENSUS_DATA, "county")

#-----------------------------------------------------------------------------

#Aggregate Data

options(scipen=999)

#Aggregated by State
data.state <- data.county %>%
  group_by(Group, State) %>%
  summarise(
    Owned_T = sum(as.numeric(S2502.group.total)),
    Owned_P = sum(as.numeric(S2502.group.owned) / Owned_T),
    HS_T = sum(as.numeric(S1501.group.HSt)),
    HS_P = HS_T / sum(as.numeric(S1501.group.total)),
    Col_T = sum(as.numeric(S1501.group.BSt)),
    Col_P = Col_T / sum(as.numeric(S1501.group.total)),
    Inc_Med = weighted.mean(as.numeric(S1903.group.medianincome),
      as.numeric(S1903.group.total),
      na.rm = TRUE
    ),
    Pop_T = sum(as.numeric(DP05.group.pop)),
    UE_P = weighted.mean(as.numeric(S2301.group.ue),
      as.numeric(S2301.group.total),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>% rbind(.,
              data.county %>%
                group_by(Group) %>%
                summarise(
                  State = "All",
                  Owned_T = sum(as.numeric(S2502.group.total)),
                  Owned_P = sum(as.numeric(S2502.group.owned) / Owned_T),
                  HS_T = sum(as.numeric(S1501.group.HSt)),
                  HS_P = HS_T / sum(as.numeric(S1501.group.total)),
                  Col_T = sum(as.numeric(S1501.group.BSt)),
                  Col_P = Col_T / sum(as.numeric(S1501.group.total)),
                  Inc_Med = weighted.mean(as.numeric(S1903.group.medianincome),
                                          as.numeric(S1903.group.total),
                                          na.rm = TRUE
                  ),
                  Pop_T = sum(as.numeric(DP05.group.pop)),
                  UE_P = weighted.mean(as.numeric(S2301.group.ue),
                                       as.numeric(S2301.group.total),
                                       na.rm = TRUE
                  ),
                  .groups = "drop"
                ))

# Might not need since only one year is used
#Aggregated by County
data.countyag <- data.county %>%
  group_by(Group, State, County) %>%
  summarise(
    Owned_T = sum(as.numeric(S2502.group.total)),
    Owned_P = sum(as.numeric(S2502.group.owned) / Owned_T),
    HS_T = sum(as.numeric(S1501.group.HSt)),
    HS_P = HS_T / sum(as.numeric(S1501.group.total)),
    Col_T = sum(as.numeric(S1501.group.BSt)),
    Col_P = Col_T / sum(as.numeric(S1501.group.total)),
    Inc_Med = weighted.mean(as.numeric(S1903.group.medianincome),
                            as.numeric(S1903.group.total),
                            na.rm = TRUE
    ),
    Pop_T = sum(as.numeric(DP05.group.pop)),
    UE_P = weighted.mean(as.numeric(S2301.group.ue),
                         as.numeric(S2301.group.total),
                         na.rm = TRUE
    ),
    .groups = "drop"
  ) %>% rbind(.,
              data.county %>%
                group_by(Group) %>%
                summarise(
                  State = "All",
                  County = "All",
                  Owned_T = sum(as.numeric(S2502.group.total)),
                  Owned_P = sum(as.numeric(S2502.group.owned) / Owned_T),
                  HS_T = sum(as.numeric(S1501.group.HSt)),
                  HS_P = HS_T / sum(as.numeric(S1501.group.total)),
                  Col_T = sum(as.numeric(S1501.group.BSt)),
                  Col_P = Col_T / sum(as.numeric(S1501.group.total)),
                  Inc_Med = weighted.mean(as.numeric(S1903.group.medianincome),
                                          as.numeric(S1903.group.total),
                                          na.rm = TRUE
                  ),
                  Pop_T = sum(as.numeric(DP05.group.pop)),
                  UE_P = weighted.mean(as.numeric(S2301.group.ue),
                                       as.numeric(S2301.group.total),
                                       na.rm = TRUE
                  ),
                  .groups = "drop"
                )) 
  
# #-----------------------------------------------------------------------------  

#Define Helpful Functions

#Plots Function (For Box Plots, Scatter Plots, and Time Series)

plots <- function(area = "State",
                  Stated = "All",
                  Group1 = "White",
                  Group2 = "Black",
                  Group3 = "Asian",
                  Group4 = "Hispanic", 
                  Vary = "Own",
                  Varx = "Inc",
                  Plot = "BP",
                  BP.Violin = "F",
                  SC.Smoother = "loess"){
  
  data.plot <-
  if(area == "State") {data.state}
  else if(area == "State.tj") {data.state.tj}
  else if(area == "County") {data.countyag}
  else if(area == "Nation") {data.nation.tj}
  
  data.plot$State <- 
  if(area == "Nation") {""}
  else {str_trim(data.plot$State)}
  
  data.plot <- 
    if(Plot == "BP" | Plot == "SC")
    {data.plot %>%
    filter(., Group %in% c(Group1, Group2, Group3, Group4) &
        {if(area == "State" | area == "State.tj") {!State %in% c("All")}
          else if (area == "County") {(State %in% Stated) | (Stated == "All")}
          else if (area == "Nation") {State == ""}
          }
        )}
  
  else if(Plot == "TS")
  {data.plot %>%
    filter(., Group %in% c(Group1, Group2, Group3, Group4) &
             {if(area == "County" | area == "State.tj"){State %in% Stated}
               else if(area == "State" | area == "State.tj"){State %in% "All"}
               else if(area == "Nation"){State == ""}
               })}
  
  # Variables Definitions

  y.var <-
    if (Vary == "Own") {
      data.plot$Owned_P*100
    } else if (Vary == "HS") {
      data.plot$HS_P*100
    } else if (Vary == "Col") {
      data.plot$Col_P*100
    } else if (Vary == "Inc") {
      data.plot$Inc_Med
    } else if (Vary == "LInc") {
      log10(data.plot$Inc_Med)
    } else if (Vary == "Pop") {
      data.plot$Pop_T/1000000
    } else if (Vary == "LPop") {
      log10(data.plot$Pop_T)
    } else if (Vary == "UE") {
      data.plot$UE_P
    }

  x.var <-
    if (Varx == "Own") {
      data.plot$Owned_P*100
    } else if (Varx == "HS") {
      data.plot$HS_P*100
    } else if (Varx == "Col") {
      data.plot$Col_P*100
    } else if (Varx == "Inc") {
      data.plot$Inc_Med
    } else if (Varx == "LInc") {
      log10(data.plot$Inc_Med)
    } else if (Varx == "Pop") {
      data.plot$Pop_T/1000000
    } else if (Varx == "LPop") {
      log10(data.plot$Pop_T)
    } else if (Varx == "UE") {
      data.plot$UE_P
    }

  # Variables Names

  y.name <-
    if (Vary == "Own") {
      "Home Ownership Rate (%)"
    } else if (Vary == "HS") {
      "High School Completion Rate (%)"
    } else if (Vary == "Col") {
      "Bachelor Degree Completion Rate (%)"
    } else if (Vary == "Inc") {
      "Household Annual\nIncome (Current US$)"
    } else if (Vary == "LInc") {
      expression(paste(Log[10], 
                       "  Monthly Income (Current US$)"))
    } else if (Vary == "Pop") {
      "Population (Million Inhabitants)"
    } else if (Vary == "LPop") {
      expression(paste(Log[10], 
                       "  Population (Million Inhabitants)"))
    } else if (Vary == "UE") {
      "Unemployment Rate (%)"
    }

  x.name <-
    if (Varx == "Own") {
      "Home Ownership Rate (%)"
    } else if (Varx == "HS") {
      "High School Completion Rate (%)"
    } else if (Varx == "Col") {
      "Bachelor Degree\nCompletion Rate (%)"
    } else if (Varx == "Inc") {
      "Household Annual Income (Current US$)"
    } else if (Varx == "LInc") {
      expression(paste(Log[10], 
                       "  Monthly Income (Current US$)"))
    } else if (Varx == "Pop") {
      "Population (Million Inhabitants)"
    } else if (Varx == "LPop") {
      expression(paste(Log[10], 
                       "  Population (Million Inhabitants)"))
    } else if (Varx == "UE") {
      "Unemployment Rate (%)"
    }


  # Aesthetics Definitions

  sumgroup <- sum(c("Group1", "Group2", "Group3", "Group4")
                  == "None")
  colors <-
    if (sumgroup == 1) {
      c("#e41a1c", "#984ea3", "#377eb8")
    } else if (sumgroup == 2) {
      c("#e41a1c", "#984ea3")
    } else if (sumgroup == 3) {
      c("#e41a1c")
    } else {
      c("#e41a1c", "#984ea3", "#377eb8", "#4daf4a")
    }
  
  data.plot$Group <-
    ifelse(data.plot$Group == "AIoAN", 
           "American Indian\nor Alaska Native",
           ifelse(data.plot$Group == "NHoOPI",
                  "Native Hawaiian\nor Other Pacific\nIslander",
                  ifelse(data.plot$Group == "WhiteNH",
                         #"White Not\nHispanic",
                         "WhiteNH",
                         ifelse(data.plot$Group == "Two+",
                                "Two or\nmore races",
                                ifelse(data.plot$Group == "Total",
                                       "All",
                                       data.plot$Group)))))
  
  Rename <- function(GroupN) {
  
  GroupNName <-
    if(GroupN == "AIoAN") {"American Indian\nor Alaska Native"}
    else if(GroupN == "NHoOPI"){"Native Hawaiian\nor Other Pacific\nIslander"}
    else if(GroupN == "WhiteNH"){"WhiteNH"}
    else if(GroupN == "Two+"){"Two or\nmore races"}
    else if(GroupN == "Total"){"All"}
    else {GroupN}

  return(GroupNName)
  
  }
  
  FGroups <- unique(c(Rename(Group1),
               Rename(Group2),
               Rename(Group3),
               Rename(Group4)))

  sc.plot <-
    ggplot(
      data = data.plot,
      aes(
        x = x.var, y = y.var,
        color = factor(Group, levels = FGroups)
      ),
      na.rm = TRUE
    ) +
    geom_point(alpha = 0.2) +
    scale_color_manual(
      name = "Group",
      values = colors,
      na.translate = F
    ) +
    labs(x = x.name, y = y.name) +
    theme(
      axis.text = element_text(size = 20),
      axis.title.y = element_text(margin = margin(
        t = 0,
        r = 5,
        b = 0,
        l = 0
      ), size = 20),
      axis.title.x = element_text(margin = margin(
        t = 5,
        r = 0,
        b = 0,
        l = 0
      ), size = 20),
      legend.title = element_blank(),
      legend.text = element_text(size = 15),
      legend.position = "bottom",
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.background = element_blank()
    ) +
    geom_smooth(aes(
      group = Group,
      color = Group
    ),
    method = SC.Smoother,
    se = FALSE, na.rm = TRUE
    )
  
  box.plot <-
    ggplot(
      data = data.plot,
      aes(
        x = factor(Group, levels = FGroups),
        y = y.var,
        fill = factor(Group, levels = FGroups)
        )
      ) +
    
    {if(BP.Violin == "F") {geom_boxplot(lwd = 0.7)}
      else if(BP.Violin == "T") {geom_violin(lwd = 0.7)}}+
    
    {if(BP.Violin == "F") {}
      else if(BP.Violin == "T") {geom_jitter()}}+
        
    ylab(y.name) +
    scale_fill_manual(values = colors) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 20),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.background = element_blank()
    )
  
  ts.plot <-
    ggplot(
      data = data.plot,
      aes(
        x = Year, 
        y = y.var,
        color = factor(Group, levels = FGroups))
    )   +
    {if(area == "State" | area == "State.tj" | area == "Nation")
      {geom_line(size = 1.5)}
      else if(area == "County")
        {stat_summary(geom = "line",
                     fun = median, size = 1.5)
        }}+
    
    scale_color_manual(
      name = "Group",
      values = colors,
      na.translate = F
    )+
    
    ylab(y.name) +
    
    theme(
      axis.text = element_text(size = 20),
      axis.title.y = element_text(margin = margin(
        t = 0,
        r = 5,
        b = 0,
        l = 0
      ), size = 20),
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 15),
      legend.position = "bottom",
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.background = element_blank()
    )

  return(
    if(Plot == 'BP'){box.plot}
    else if(Plot == 'SC'){sc.plot}
    else if(Plot == 'TS'){ts.plot}
)
}

# Plots Function (For Choropleth Maps)

plot_chloropleth_2 <- function(
  area = "State",
  State = "All",
  fill_var = "hom.own",
  col_pal = "Reds",
  title = NULL,
  facet_var = "race",
  group1 = "WhiteNH",
  group2 = "None",
  group3 = "None",
  group4 = "None",
  facet_ncol = NA,
  breaks = NULL,
  outline = TRUE,
  color_na = "black",
  free_scales = FALSE
) {
  groups_r <- c(group1, group2, group3, group4)
  
  data <-  if(area == "State") {
    data_state_ch %>%
    filter(
      !state %in% c("Alaska", "Hawaii") & 
      race %in% groups_r
    )
  } else if((area == "County") & (State == "All")) {
    data_county_ch %>%
    filter(
      !(state %in% c("Alaska", "Hawaii")) &
      (race %in% groups_r)
    )
  } else if((area == "County") & !(State == "All")) {
    data_county_ch %>% filter(
      (race %in% groups_r) &
      (state %in% State)
    )
  }

  data$linc <- log10(data$inc.inc)
  data$lpop <- log10(data$pop.tot)
  data$edu.hs <- data$edu.hs * 100
  data$edu.bs <- data$edu.bs * 100
  data$emp.ue <- data$emp.ue * 100
  data$pop.share <- data$pop.share * 100
  data$hom.own <- data$hom.own * 100

  var.name <- c(
    "hom.own" = "Home Ownership (%)",
    "edu.hs" = "High School Completion (%)",
    "edu.bs" = "Bachelor's Degree Completion (%)",
    "emp.ue" = "Unemployment (%)",
    "pop.tot" = "Total Population",
    "pop.share" = "Racial Population Share (%)",
    "inc.inc" = "Household Annual Income (US$)",
    "linc" = "Log10 Annual Income (US$)",
    "lpop" = "Log10 Total Population"
  )[[fill_var]]
  
  the_plot <-
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
    the_plot <- the_plot + tmap::tm_borders()
  }
  if (!is.null(facet_var)) {
    the_plot <- (
      the_plot +
      tmap::tm_facets(
        facet_var,
        free.scales = free_scales,
        ncol = facet_ncol
      )
    )
  }
  the_plot <- (
    the_plot +
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
  the_plot
}

#-----------------------------------------------------------------------------

#Exporting plots to Article (County data)

#Figure 1a: Scatterplot of NY broken (Not broken by race)
#6x8
sc_imd_ny <-
  plots(area = "County", Stated = "New York",
        Group1 = "Total", Group2 = "None", Group3 = "None", Group4 = "None",
        Vary = "Own", Varx = "LInc",
        Plot = "SC", SC.Smoother = "lm") + theme(legend.position = "none")+
  xlim(4.4,5.2) + ylim(10,80) + scale_color_manual(values = "black")
ggsave(
  file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "sc_imd_ny.pdf"),
  plot = sc_imd_ny, width = 8, height = 6, units = "in", dpi = 300)

#Figure 1b: Scatterplot of NY (Broken by race)
#6x8
sc_imd_raceny <-
  plots(area = "County", Stated = "New York",
        Group1 = "WhiteNH", Group2 = "Hispanic", 
        Group3 = "Black", Group4 = "Asian",
        Vary = "Own", Varx = "LInc",
        Plot = "SC", SC.Smoother = "lm")+
  xlim(4.4,5.2) + ylim(10,80) 
ggsave(
  file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "sc_imd_raceny.pdf"),
  plot = sc_imd_raceny, width = 8, height = 6, units = "in", dpi = 300)

#Figure 2: Boxplots of homeownership that shows differences between races
#6x8
bp_imd <-
  plots(area = "County", Group1 = "WhiteNH", 
        Group2 = "Hispanic", Group3 = "Black", Group4 = "Asian",
        Plot = "BP")
ggsave(
  file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "bp_imd.pdf"),
  plot = bp_imd, width = 8, height = 6, units = "in", dpi = 300)

#Figure 3: Choropleth Map
chloro_greens <- plot_chloropleth_2(col_pal = "Greens", group1 = "Total", free_scales = TRUE)
tmap_save(chloro_greens, file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "choro_greens.pdf"),
  width = 8, height = 6, units = "in", dpi = 300)

chloro_greys <- plot_chloropleth_2(col_pal = "Greys", group1 = "Total", free_scales = TRUE)
tmap_save(chloro_greys, file.path(OUTPUT_EXPLORATORY_DIR, "data_exploration", "choro_greys.pdf"),
  width = 8, height = 6, units = "in", dpi = 300)


#-----------------------------------------------------------------------------

#Section 2.2 Values

############## I have put this code for producing the tables in another file ##############

#Values obtained for Paragraph 1 of Section 2.2
#Housing variables
# weighted.mean(data.county[data.county$Group == "Total",]$S2502.group.ownedp,
#               as.numeric(data.county[data.county$Group == "Total",]$S2502.group.total),
#               na.rm = TRUE)

############## We are no longer displaying these variables ##############


# weighted.mean(c(
#   data.county[data.county$Group == "Total",]$S2507.costp.median,
#   data.county[data.county$Group == "Total",]$S2506.costp.median), c(
#   data.county[data.county$Group == "Total",]$S2507.totalmort,
#   data.county[data.county$Group == "Total",]$S2506.totalmort),
#               na.rm = TRUE)

# weighted.mean(as.numeric(c(
#   data.county[data.county$Group == "Total" & 
#                 data.county$Year == 2020,]$S2507.value.median,
#   data.county[data.county$Group == "Total" &
#                 data.county$Year == 2020,]$S2506.value.median)),
#   as.numeric(c(
#                   data.county[data.county$Group == "Total" &
#                                 data.county$Year == 2020,]$S2507.totalmort,
#                   data.county[data.county$Group == "Total" &
#                                 data.county$Year == 2020,]$S2506.totalmort)),
#   na.rm = TRUE)

############## We are no longer displaying these variables ##############

# weighted.mean(as.numeric(c(
#   data.county[data.county$Group == "Total" & 
#                 data.county$Year == 2020,]$S2507.tax.median,
#   data.county[data.county$Group == "Total" &
#                 data.county$Year == 2020,]$S2506.tax.median)),
#   as.numeric(c(
#     data.county[data.county$Group == "Total" &
#                   data.county$Year == 2020,]$S2507.totalmort,
#     data.county[data.county$Group == "Total" &
#                   data.county$Year == 2020,]$S2506.totalmort)),
#   na.rm = TRUE)

# sum(data.county[data.county$Group == "Total" &
#               data.county$Year == 2020,]$S2506.totalmort)/
#   (sum(data.county[data.county$Group == "Total" &
#                      data.county$Year == 2020,]$S2506.totalmort)+
#      sum(data.county[data.county$Group == "Total" &
#                        data.county$Year == 2020,]$S2507.totalmort))

#Housing variables and sociodemographics broken by race ()
#and County Ranges
#This code reproduces the results of Table 2


#Home Ownership

# weighted.mean(data.county[data.county$Group == "Total",]$S2502.group.ownedp,
#               data.county[data.county$Group == "Total",]$S2502.group.total)
# quantile(data.county[data.county$Group == "Total",]$S2502.group.ownedp,
#          probs = c(0.1,0.9))

# weighted.mean(data.county[data.county$Group == "WhiteNH",]$S2502.group.ownedp,
#               data.county[data.county$Group == "WhiteNH",]$S2502.group.total)
# weighted.mean(data.county[data.county$Group == "Black",]$S2502.group.ownedp,
#               data.county[data.county$Group == "Black",]$S2502.group.total)
# weighted.mean(data.county[data.county$Group == "Asian",]$S2502.group.ownedp,
#               data.county[data.county$Group == "Asian",]$S2502.group.total)
# weighted.mean(data.county[data.county$Group == "Hispanic",]$S2502.group.ownedp,
#               data.county[data.county$Group == "Hispanic",]$S2502.group.total)

# #High School Attainment
# weighted.mean(data.county[data.county$Group == "Total",]$S1501.group.HSp,
#               data.county[data.county$Group == "Total",]$S1501.group.total)
# quantile(data.county[data.county$Group == "Total",]$S1501.group.HSp,
#          probs = c(0.1,0.9))

# weighted.mean(data.county[data.county$Group == "WhiteNH",]$S1501.group.HSp,
#               data.county[data.county$Group == "WhiteNH",]$S1501.group.total)
# weighted.mean(data.county[data.county$Group == "Black",]$S1501.group.HSp,
#               data.county[data.county$Group == "Black",]$S1501.group.total)
# weighted.mean(data.county[data.county$Group == "Asian",]$S1501.group.HSp,
#               data.county[data.county$Group == "Asian",]$S1501.group.total)
# weighted.mean(data.county[data.county$Group == "Hispanic",]$S1501.group.HSp,
#               data.county[data.county$Group == "Hispanic",]$S1501.group.total)

# #Annual Income
# weighted.mean(
#   as.numeric(
#   data.county[data.county$Group == "Total",]$S1903.group.medianincome),
#   as.numeric(
#     data.county[data.county$Group == "Total",]$S1903.group.total),
#   na.rm = TRUE)
# quantile(  as.numeric(
#   data.county[data.county$Group == "Total",]$S1903.group.medianincome),
#   probs = c(0.1,0.9), na.rm = TRUE)

# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "WhiteNH",]$S1903.group.medianincome),
#   as.numeric(
#     data.county[data.county$Group == "WhiteNH",]$S1903.group.total),
#   na.rm = TRUE)

# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "Black",]$S1903.group.medianincome),
#   as.numeric(
#     data.county[data.county$Group == "Black",]$S1903.group.total),
#   na.rm = TRUE)

# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "Asian",]$S1903.group.medianincome),
#   as.numeric(
#     data.county[data.county$Group == "Asian",]$S1903.group.total),
#   na.rm = TRUE)

# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "Hispanic",]$S1903.group.medianincome),
#   as.numeric(
#     data.county[data.county$Group == "Hispanic",]$S1903.group.total),
#   na.rm = TRUE)

# #Unemployment
# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "Total" &
#                   data.county$Year == 2020,]$S2301.group.ue),
#   as.numeric(
#     data.county[data.county$Group == "Total" &
#                   data.county$Year == 2020,]$S2301.group.total),
#   na.rm = TRUE)
# quantile(data.county[data.county$Group == "Total" &
#                        data.county$Year == 2020,]$S2301.group.ue,
#          probs = c(0.1,0.9))


# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "WhiteNH",]$S2301.group.ue),
#   as.numeric(
#     data.county[data.county$Group == "WhiteNH",]$S2301.group.total),
#   na.rm = TRUE)

# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "Black",]$S2301.group.ue),
#   as.numeric(
#     data.county[data.county$Group == "Black",]$S2301.group.total),
#   na.rm = TRUE)

# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "Asian",]$S2301.group.ue),
#   as.numeric(
#     data.county[data.county$Group == "Asian",]$S2301.group.total),
#   na.rm = TRUE)

# weighted.mean(
#   as.numeric(
#     data.county[data.county$Group == "Hispanic",]$S2301.group.ue),
#   as.numeric(
#     data.county[data.county$Group == "Hispanic",]$S2301.group.total),
#   na.rm = TRUE)

# #Population Share
# sum(as.numeric(
#   data.county[data.county$Group == "Total",]$DP05.group.pop))/
#   sum(as.numeric(
#     data.county[data.county$Group == "Total",]$DP05.group.pop))
# quantile(as.numeric(
#   data.county[data.county$Group == "Total",]$DP05.group.pop),
#   probs = c(0.1,0.9))

# sum(as.numeric(
#   data.county[data.county$Group == "WhiteNH",]$DP05.group.pop))/
#   sum(as.numeric(
#     data.county[data.county$Group == "Total",]$DP05.group.pop))
  
# sum(as.numeric(
#   data.county[data.county$Group == "Black",]$DP05.group.pop))/
#   sum(as.numeric(
#     data.county[data.county$Group == "Total",]$DP05.group.pop))
  
# sum(as.numeric(
#   data.county[data.county$Group == "Asian",]$DP05.group.pop))/
#   sum(as.numeric(
#     data.county[data.county$Group == "Total",]$DP05.group.pop))

# sum(as.numeric(
#     data.county[data.county$Group == "Hispanic",]$DP05.group.pop))/
#     sum(as.numeric(
#       data.county[data.county$Group == "Total",]$DP05.group.pop))
