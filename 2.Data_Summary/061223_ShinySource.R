#-----------------------------------------------------------------------------
# Define Libraries and Set Working Directory

library(tidyverse)
library(tmap)
setwd(paste("C:/Users/jhona/OneDrive/Documents/Classes/USF",
        "/ASA Data Challenge 2022/04. R Scripts", sep = ""))

#-----------------------------------------------------------------------------

#Read County Data

data.county <- read.csv("072722_dataexpo.county.csv") %>%
  filter(.,
         !State %in% c(" Puerto Rico", " District of Columbia"))

data_state_ch <- readRDS("census_data_state.RDS")
data_county_ch <- readRDS("census_data_county.RDS")

#-----------------------------------------------------------------------------

#Aggregate Data

options(scipen=999)

#Aggregated by State
data.state <- data.county %>%
  group_by(Year, Group, State) %>%
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
    )
  ) %>% rbind(.,
              data.county %>%
                group_by(Year, Group) %>%
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
                  )
                ))

#Aggregated by County
data.countyag <- data.county %>%
  group_by(Year, Group, State, County) %>%
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
    )
  ) %>% rbind(.,
              data.county %>%
                group_by(Year, Group) %>%
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
                  )
                )) 
  
#-----------------------------------------------------------------------------  

#Define Helpful Functions

#Plots Function (For Box Plots, Scatter Plots, and Time Series)

plots <- function(area = "State",
                  Yearp = 2020,
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
      Year == Yearp & 
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

plot_chloropleth <- function(
    area = "State",
    State = "All",
    yearch = 2020,
    fill_var = "hom.own",
    col_pal = "Reds",
    title = NULL,
    facet_var = "race",
    group1 = "White",
    group2 = "None",
    group3 = "None",
    group4 = "None",
    facet_ncol = NA,
    breaks = NULL,
    outline = TRUE,
    color_na = "black",
    free_scales = FALSE,
    log_scale = FALSE
) {
  # keep only contiguous states for plotting
  
  groups_r <- c(group1, group2, group3, group4)
  
  data <- 
    if(area == "State"){data_state_ch %>%
        filter(.,!state %in% c("Alaska", "Hawaii") & 
        race %in% groups_r &
        year %in% yearch)}
  else if(area == "County" & State == "All"){data_county_ch %>%
      filter(., !state %in% c("Alaska", "Hawaii") &
               race %in% groups_r &
               year %in% yearch)}
    else if(area == "County" & !State == "All"){data_county_ch %>%
        filter(., race %in% groups_r &
                 year %in% yearch &
                 state %in% State)}
      
  data$linc <- log10(data$inc.inc)
  data$lpop <- log10(data$pop.tot)
  data$edu.hs <- data$edu.hs*100
  data$edu.bs <- data$edu.bs*100
  data$emp.ue <- data$emp.ue*100
  data$pop.share <- data$pop.share*100
  data$hom.own <- data$hom.own*100

  var.name <-
    if(fill_var == "hom.own") "Home Ownership (%)"
  else if(fill_var == "edu.hs") "High School Completion (%)"
  else if(fill_var == "edu.bs") "Bachelor's Degree Completion (%)"
  else if(fill_var == "emp.ue") "Unemployment (%)"
  else if(fill_var == "pop.tot") "Total Population"
  else if(fill_var == "pop.share") "Racial Population Share (%)"
  else if(fill_var == "inc.inc") "Household Annual Income (US$)"
  else if(fill_var == "linc") "Log10 Annual Income (US$)"
  else if(fill_var == "lpop") "Log10 Total Population"
  

  #if (log_scale) {
  #  data[[fill_var]] <- log10(
  #    replace(data[[fill_var]], data[[fill_var]] <= 0, NA)
  #  )
  #}
  
  the_plot <-
    tmap::tm_shape(data) +
    tmap::tm_fill(
      title = var.name,
      col = 
        fill_var,
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
    the_plot <- the_plot +
      tmap::tm_facets(
        facet_var,
        free.scales = free_scales,
        ncol = facet_ncol
      )
  }
  the_plot <- the_plot +
    #tmap::tm_layout(
      # Note these parameters are chosen to look good at
      # 5000 x 5000 pixel resolution
      #main.title = if (is.null(title)) fill_var else title,
      #main.title = NULL,
      #main.title.size = 3,
      #panel.label.size = 3,
      #legend.outside = !free_scales,
      #legend.outside.position = "bottom",
      #legend.outside.position = c("bottom","center"),
      #legend.outside.size = 0.2,
      #legend.position = c("left", "bottom"),
      #legend.title.size = 5,
      #legend.text.size = 3
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
    
  
  return(the_plot)
  
}

#-----------------------------------------------------------------------------

#Exporting plots to Article (County data)

#Figure 1a: Scatterplot of NY broken (Not broken by race)
#6x8
sc_imd_ny <-
  plots(area = "County", Yearp = 2020, Stated = "New York",
        Group1 = "Total", Group2 = "None", Group3 = "None", Group4 = "None",
        Vary = "Own", Varx = "LInc",
        Plot = "SC", SC.Smoother = "lm") + theme(legend.position = "none")+
  xlim(4.4,5.2) + ylim(10,80) + scale_color_manual(values = "black")

#Figure 1b: Scatterplot of NY (Broken by race)
#6x8
sc_imd_raceny <-
  plots(area = "County", Yearp = 2020, Stated = "New York",
        Group1 = "WhiteNH", Group2 = "Hispanic", 
        Group3 = "Black", Group4 = "Asian",
        Vary = "Own", Varx = "LInc",
        Plot = "SC", SC.Smoother = "lm")+
  xlim(4.4,5.2) + ylim(10,80) 

#Figure 2: Boxplots of homeownership that shows differences between races
#6x8
bp_imd <-
  plots(area = "County", Yearp = 2020, Group1 = "WhiteNH", 
        Group2 = "Hispanic", Group3 = "Black", Group4 = "Asian",
        Plot = "BP")

#Figure 3: Choropleth Map
plot_chloropleth(col_pal = "Greens", free_scales = TRUE)


#-----------------------------------------------------------------------------

#Section 2.2 Values

#Values obtained for Paragraph 1 of Section 2.2
#Housing variables
weighted.mean(data.county[data.county$Group == "Total" &
                            data.county$Year == 2020,]$S2502.group.ownedp,
              as.numeric(data.county[data.county$Group == "Total" &
                            data.county$Year == 2020,]$S2502.group.total),
              na.rm = TRUE)

weighted.mean(c(
  data.county[data.county$Group == "Total" & 
                              data.county$Year == 2020,]$S2507.costp.median,
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$S2506.costp.median), c(
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$S2507.totalmort,
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$S2506.totalmort),
              na.rm = TRUE)

weighted.mean(as.numeric(c(
  data.county[data.county$Group == "Total" & 
                data.county$Year == 2020,]$S2507.value.median,
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$S2506.value.median)),
  as.numeric(c(
                  data.county[data.county$Group == "Total" &
                                data.county$Year == 2020,]$S2507.totalmort,
                  data.county[data.county$Group == "Total" &
                                data.county$Year == 2020,]$S2506.totalmort)),
  na.rm = TRUE)



weighted.mean(as.numeric(c(
  data.county[data.county$Group == "Total" & 
                data.county$Year == 2020,]$S2507.tax.median,
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$S2506.tax.median)),
  as.numeric(c(
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$S2507.totalmort,
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$S2506.totalmort)),
  na.rm = TRUE)

sum(data.county[data.county$Group == "Total" &
              data.county$Year == 2020,]$S2506.totalmort)/
  (sum(data.county[data.county$Group == "Total" &
                     data.county$Year == 2020,]$S2506.totalmort)+
     sum(data.county[data.county$Group == "Total" &
                       data.county$Year == 2020,]$S2507.totalmort))

#Housing variables and sociodemographics broken by race ()
#and County Ranges
#This code reproduces the results of Table 2

#Home Ownership


weighted.mean(data.county[data.county$Group == "Total" &
                            data.county$Year == 2020,]$S2502.group.ownedp,
              data.county[data.county$Group == "Total" &
                            data.county$Year == 2020,]$S2502.group.total)
quantile(data.county[data.county$Group == "Total" &
                       data.county$Year == 2020,]$S2502.group.ownedp,
         probs = c(0.1,0.9))

weighted.mean(data.county[data.county$Group == "WhiteNH" &
                            data.county$Year == 2020,]$S2502.group.ownedp,
              data.county[data.county$Group == "WhiteNH" &
                            data.county$Year == 2020,]$S2502.group.total)
weighted.mean(data.county[data.county$Group == "Black" &
                            data.county$Year == 2020,]$S2502.group.ownedp,
              data.county[data.county$Group == "Black" &
                            data.county$Year == 2020,]$S2502.group.total)
weighted.mean(data.county[data.county$Group == "Asian" &
                            data.county$Year == 2020,]$S2502.group.ownedp,
              data.county[data.county$Group == "Asian" &
                            data.county$Year == 2020,]$S2502.group.total)
weighted.mean(data.county[data.county$Group == "Hispanic" &
                            data.county$Year == 2020,]$S2502.group.ownedp,
              data.county[data.county$Group == "Hispanic" &
                            data.county$Year == 2020,]$S2502.group.total)

#High School Attainment
weighted.mean(data.county[data.county$Group == "Total" &
                            data.county$Year == 2020,]$S1501.group.HSp,
              data.county[data.county$Group == "Total" &
                            data.county$Year == 2020,]$S1501.group.total)
quantile(data.county[data.county$Group == "Total" &
                       data.county$Year == 2020,]$S1501.group.HSp,
         probs = c(0.1,0.9))

weighted.mean(data.county[data.county$Group == "WhiteNH" &
                            data.county$Year == 2020,]$S1501.group.HSp,
              data.county[data.county$Group == "WhiteNH" &
                            data.county$Year == 2020,]$S1501.group.total)
weighted.mean(data.county[data.county$Group == "Black" &
                            data.county$Year == 2020,]$S1501.group.HSp,
              data.county[data.county$Group == "Black" &
                            data.county$Year == 2020,]$S1501.group.total)
weighted.mean(data.county[data.county$Group == "Asian" &
                            data.county$Year == 2020,]$S1501.group.HSp,
              data.county[data.county$Group == "Asian" &
                            data.county$Year == 2020,]$S1501.group.total)
weighted.mean(data.county[data.county$Group == "Hispanic" &
                            data.county$Year == 2020,]$S1501.group.HSp,
              data.county[data.county$Group == "Hispanic" &
                            data.county$Year == 2020,]$S1501.group.total)

#Annual Income
weighted.mean(
  as.numeric(
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$S1903.group.medianincome),
  as.numeric(
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$S1903.group.total),
  na.rm = TRUE)
quantile(  as.numeric(
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$S1903.group.medianincome),
  probs = c(0.1,0.9), na.rm = TRUE)

weighted.mean(
  as.numeric(
    data.county[data.county$Group == "WhiteNH" &
                  data.county$Year == 2020,]$S1903.group.medianincome),
  as.numeric(
    data.county[data.county$Group == "WhiteNH" &
                  data.county$Year == 2020,]$S1903.group.total),
  na.rm = TRUE)

weighted.mean(
  as.numeric(
    data.county[data.county$Group == "Black" &
                  data.county$Year == 2020,]$S1903.group.medianincome),
  as.numeric(
    data.county[data.county$Group == "Black" &
                  data.county$Year == 2020,]$S1903.group.total),
  na.rm = TRUE)

weighted.mean(
  as.numeric(
    data.county[data.county$Group == "Asian" &
                  data.county$Year == 2020,]$S1903.group.medianincome),
  as.numeric(
    data.county[data.county$Group == "Asian" &
                  data.county$Year == 2020,]$S1903.group.total),
  na.rm = TRUE)

weighted.mean(
  as.numeric(
    data.county[data.county$Group == "Hispanic" &
                  data.county$Year == 2020,]$S1903.group.medianincome),
  as.numeric(
    data.county[data.county$Group == "Hispanic" &
                  data.county$Year == 2020,]$S1903.group.total),
  na.rm = TRUE)

#Unemployment
weighted.mean(
  as.numeric(
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$S2301.group.ue),
  as.numeric(
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$S2301.group.total),
  na.rm = TRUE)
quantile(data.county[data.county$Group == "Total" &
                       data.county$Year == 2020,]$S2301.group.ue,
         probs = c(0.1,0.9))


weighted.mean(
  as.numeric(
    data.county[data.county$Group == "WhiteNH" &
                  data.county$Year == 2020,]$S2301.group.ue),
  as.numeric(
    data.county[data.county$Group == "WhiteNH" &
                  data.county$Year == 2020,]$S2301.group.total),
  na.rm = TRUE)

weighted.mean(
  as.numeric(
    data.county[data.county$Group == "Black" &
                  data.county$Year == 2020,]$S2301.group.ue),
  as.numeric(
    data.county[data.county$Group == "Black" &
                  data.county$Year == 2020,]$S2301.group.total),
  na.rm = TRUE)

weighted.mean(
  as.numeric(
    data.county[data.county$Group == "Asian" &
                  data.county$Year == 2020,]$S2301.group.ue),
  as.numeric(
    data.county[data.county$Group == "Asian" &
                  data.county$Year == 2020,]$S2301.group.total),
  na.rm = TRUE)

weighted.mean(
  as.numeric(
    data.county[data.county$Group == "Hispanic" &
                  data.county$Year == 2020,]$S2301.group.ue),
  as.numeric(
    data.county[data.county$Group == "Hispanic" &
                  data.county$Year == 2020,]$S2301.group.total),
  na.rm = TRUE)


#Population Share
sum(as.numeric(
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$DP05.group.pop))/
  sum(as.numeric(
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$DP05.group.pop))
quantile(as.numeric(
  data.county[data.county$Group == "Total" &
                data.county$Year == 2020,]$DP05.group.pop),
  probs = c(0.1,0.9))

sum(as.numeric(
  data.county[data.county$Group == "WhiteNH" &
                data.county$Year == 2020,]$DP05.group.pop))/
  sum(as.numeric(
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$DP05.group.pop))
  
sum(as.numeric(
  data.county[data.county$Group == "Black" &
                data.county$Year == 2020,]$DP05.group.pop))/
  sum(as.numeric(
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$DP05.group.pop))
  
sum(as.numeric(
  data.county[data.county$Group == "Asian" &
                data.county$Year == 2020,]$DP05.group.pop))/
  sum(as.numeric(
    data.county[data.county$Group == "Total" &
                  data.county$Year == 2020,]$DP05.group.pop))

sum(as.numeric(
    data.county[data.county$Group == "Hispanic" &
                  data.county$Year == 2020,]$DP05.group.pop))/
    sum(as.numeric(
      data.county[data.county$Group == "Total" &
                    data.county$Year == 2020,]$DP05.group.pop))
