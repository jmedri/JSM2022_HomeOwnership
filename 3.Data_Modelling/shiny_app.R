#Define Libraries and Set Working Directory
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(styler)

# Initialize data and models
source("3.Data_Modelling/initialize.R")
initialize("1", FALSE)
model_g <- load_model("4") # model global variable

data.county <- (
  read.csv("3.Data_Modelling_Output/input_processed/census/3_wide/data.csv") %>%
  filter(!State %in% c("Puerto Rico", "District of Columbia")) %>%
  mutate(
    S2502.group.ownedp = S2502.group.owned / S2502.group.total,
    S1501.group.HSp = S1501.group.HSt / S1501.group.total,
    S1501.group.BSp = S1501.group.BSt / S1501.group.total
  )
)

data_state_ch <- (
  CENSUS_DATA |>
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
)

data_county_ch <- join_with_shape(CENSUS_DATA, "county")

# plot_prediction_app: helper function for app interface to plot predictions
# Parameters:
#     model: model to use for prediction (brms::brm() output).
#     race: races to predict and plot. If "All" plots all races, else must be a subset
#           of RACES_MODEL.
#     state: which state to predict. If "All" will show the predicition for whole US.
#     inc.inc: covariate value. If NULL gets default value from data.
#     separate_y: refer to plot_prediction() documentation.
#     x_lim_full: refer to plot_prediction() documentation.
#     fill_alpha: refer to plot_prediction() documentation.
#     title: refer to plot_prediction() documentation.
#     show_subtitle: refer to plot_prediction() documentation.
#     interval_size: refer to plot_prediction() documentation.
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

#------------------------------------------------------------------------------

#Define Helpful Variables

shiny.raceth <- c(
  "All Races and Ethnicities",
  "WhiteNH",
  "Black",
  "Hispanic",
  "Asian",
  "None"
)
shiny.var <- c(
  "Home Ownership (%)",
  "High School Completion (%)",
  "Bachelor Degree Completion (%)",
  "Household Annual Income (Current US$)",
  "Log10 Annual Income (Current US$)",
  "Population Size",
  "Log10 Population Inhabitants",
  "Unemployment (%)"
)

#------------------------------------------------------------------------------

#Define UI

ui <- bootstrapPage(
  #Define Theme
  navbarPage(
    theme = shinytheme("flatly"),
    "U.S. Home Ownership Visualizations",
    id = "nav",

    # 1. Define Box Plot Interface
    tabPanel(
      "Box Plots",
      sidebarLayout(
        sidebarPanel(
          # 1.1.1 Specify whether State or County Approach
          radioButtons("bp_area", "Data Type:",
                      choices = c("State", "County"),
                      inline = TRUE,
                      selected = "State"
          ),
          conditionalPanel(
            condition = "input.bp_area == 'County'",
            textInput(
              "bp_st",
              "Input State Name (i.e. Florida):"
            )
          ),
          # 1.1.3 Specify type of Box Plot
          radioButtons(
            "bp_v",
            "Type of Plot",
            choices = c("Box Plot", "Violin Plot"),
            selected = "Box Plot",
            inline = TRUE
          ),
          #1.1.4 Specify Variable y
          pickerInput(
            "bp_var",
            "Variable:",
            choices = shiny.var
          ),
          pickerInput(
            "bp.gr_1",
            "Racial/Ethnic Group 1:",
            choices = shiny.raceth,
            selected = "All Races and Ethnicities"
          ),
          pickerInput(
            "bp.gr_2",
            "Racial/Ethnic Group 2:",
            choices = shiny.raceth,
            selected = "None"
          ),
          pickerInput(
            "bp.gr_3",
            "Racial/Ethnic Group 3:",
            choices = shiny.raceth,
            selected = "None"
          ),
          pickerInput(
            "bp.gr_4",
            "Racial/Ethnic Group 4:",
            choices = shiny.raceth,
            selected = "None"
          ),
          #1.1.6 Include Action Button
          actionButton("bpgo", "Click here to Plot",
                      style = "background-color: #2d3e50"
          ),
        ),
        # 1.2 Plot Panel
        mainPanel(
          fluidRow(
            plotOutput(
              outputId = "boxplot",
              height = 600
            )
          )
        )
      )
    ),
    # 2. Define ScatterPlot Interface
    tabPanel(
      "Scatter Plots",
      sidebarLayout(
        sidebarPanel(
          # 2.1.1 Specify whether State or County Approach
          radioButtons(
            "sc_area",
            "Data Type:",
            choices = c("State", "County"),
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.sc_area == 'County'",
            textInput(
              "sc_st",
              "Input State Name (i.e. Florida):"
            )
          ),
          # 2.1.3 Specify Smoother
          radioButtons(
            "sc_sm",
            "Smoother",
            choices = c(
              "Linear Model",
              "Generalized Linear Model",
              "Generalized Additive Model",
              "Locally Estimated Scatterplot"
            ),
            selected = "Locally Estimated Scatterplot",
            inline = TRUE
          ),
          #2.1.4 Specify Variable y
          pickerInput(
            "sc_vary",
            "Variable y:",
            choices = shiny.var,
            selected = "Home Ownership Rate (%)"
          ),
          #2.1.5 Specify Variable x
          pickerInput(
            "sc_varx",
            "Variable x:",
            choices = shiny.var,
            selected = "Household Annual Income (Current US$)"
          ),
          #2.1.6.1 Conditional Groups
          pickerInput(
            "sc.gr_1",
            "Racial/Ethnic Group 1:",
            choices = shiny.raceth,
            selected = "All Races and Ethnicities"
          ),
          pickerInput(
            "sc.gr_2",
            "Racial/Ethnic Group 2:",
            choices = shiny.raceth,
            selected = "None"
          ),
          pickerInput(
            "sc.gr_3",
            "Racial/Ethnic Group 3:",
            choices = shiny.raceth,
            selected = "None"
          ),
          pickerInput(
            "sc.gr_4",
            "Racial/Ethnic Group 4:",
            choices = shiny.raceth,
            selected = "None"
          ),
          #2.1.7 Include Action Button
          actionButton(
            "scgo",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          ),
        ),
        # 2.2 Plot Panel
        mainPanel(
          fluidRow(
            plotOutput(
              outputId = "scatterplot",
              height = 600
            )
          )
        )
      )
    ),
    # 4. Define Choropleth Map Interface
    tabPanel(
      "Choropleth Maps",
      sidebarLayout(
        sidebarPanel(
          # 4.1.1 Specify whether State or County Approach
          radioButtons(
            "ch_area",
            "Data Type:",
            choices = c("State", "County"),
            inline = TRUE,
            selected = "State"
          ),
          conditionalPanel(
            condition = "input.ch_area == 'County'",
            textInput(
              "ch_st",
              "Input State Name (i.e. Florida):"
            )
          ),
          #4.1.3 Specify Variable
          pickerInput(
            "ch_var",
            "Variable:",
            choices = c(
              "Home Ownership (%)",
              "High School Completion (%)",
              "Bachelor Degree Completion (%)",
              "Household Annual Income (Current US$)",
              "Log10 Annual Income (Current US$)",
              "Population Size",
              "Log10 Population Size",
              "Population Share (%)",
              "Unemployment (%)"
            )
          ),
          # 4.1.5 Specify color of choropleth
          radioButtons(
            "ch_col",
            "Color",
            choices = c(
              "Blue",
              "Gray",
              "Green",
              "Orange",
              "Purple",
              "Red"
            ),
            selected = "Blue",
            inline = TRUE
          ),
          pickerInput(
            "ch.gr_1",
            "Racial/Ethnic Group 1:",
            choices = shiny.raceth,
            selected = "All Races and Ethnicities"
          ),
          pickerInput(
            "ch.gr_2",
            "Racial/Ethnic Group 2:",
            choices = shiny.raceth,
            selected = "None"
          ),
          pickerInput(
            "ch.gr_3",
            "Racial/Ethnic Group 3:",
            choices = shiny.raceth,
            selected = "None"
          ),
          pickerInput(
            "ch.gr_4",
            "Racial/Ethnic Group 4:",
            choices = shiny.raceth,
            selected = "None"
          ),
          #1.1.6 Include Action Button
          actionButton(
            "chgo",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          ),
        ),
        # 1.2 Plot Panel
        mainPanel(
          fluidRow(
            plotOutput(
              outputId = "choropleths",
              height = 600
            )
          )
        )
      )
    ),
    # 5. Define Model Interface
    tabPanel(
      "Predictive Models",
      sidebarLayout(
        sidebarPanel(
          #5.1.2 Select Race
          checkboxGroupInput(
            "pm_races",
            "Select race(s):",
            choices = c(
              "White",
              "Black",
              "Asian",
              "Other"
            ),
            selected = c("White", "Black", "Asian"),
            inline = TRUE
          ),
          #5.1.3 Input State
          tags$div(
            textInput(
              "pm_st",
              "State:",
              value = "Florida"
            ),
            style="display:inline-block"
          ),
          #5.1.5 Input High School
          numericInput(
            "pm_hs",
            "Input High School Completion % in State:",
            value = 90
          ),
          #5.1.6 Input Unemployment
          numericInput(
            "pm_ue",
            "Input Unemployment % in State:",
            value = 5
          ),
          #5.1.7 Input Income
          numericInput(
            "pm_inc",
            "Input Household Annual Income US$:",
            value = 60000
          ),
          #5.1.8 Input Home Value
          numericInput(
            "pm_val",
            "Input Home Value US$:",
            value = 200000
          ),
          #5.1.10 Not Overlaid
          radioButtons(
            "pm_sp",
            "Separate Plots?",
            choices = c("Yes", "No"),
            selected = "No",
            inline = TRUE
          ),
          #5.1.10 Include Action Button
          actionButton(
            "pmgo",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          )
        ),
        #5.2 Plot
        mainPanel(
          fluidRow(
            plotOutput(
              outputId = "pmplot",
              height = 600
            )
          )
        )
      )
    )
  )
)
#Close UI


#------------------------------------------------------------------------------

#Define Server
server <- function(input, output) {
  #2.1 Box Plot Output
  output$boxplot <- renderPlot({
    if (input$bpgo == 0) return ("")
    isolate(
      plots(
        Plot = 'BP',
        area = input$bp_area,
        Stated = input$bp_st,
        BP.Violin = switch (
          input$bp_v,
          "Box Plot" = "F",
          "Violin Plot" = "T"
        ),
        Vary = switch(
          input$bp_var,
          "Home Ownership (%)" = "Own",
          "High School Completion (%)" = "HS",
          "Bachelor Degree Completion (%)" = "Col",
          "Household Annual Income (Current US$)" = "Inc",
          "Log10 Annual Income (Current US$)" = "LInc",
          "Population Size" = "Pop",
          "Log10 Population Inhabitants" = "LPop",
          "Unemployment (%)" = "UE"
        ),
        Group1 = switch(
          input$bp.gr_1,
          "All Races and Ethnicities" = "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic",
        ),
        Group2 = switch(
          input$bp.gr_2,
          "All Races and Ethnicities" = "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        Group3 = switch(
          input$bp.gr_3,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic",
        ),
        Group4 = switch(
          input$bp.gr_4,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        )
      )
    )
  })

  #2.2 Scatter Plot Output
  output$scatterplot <- renderPlot({
    if (input$scgo == 0) return("")
    isolate(
      plots(
        Plot = 'SC',
        area = input$sc_area,
        Stated = input$sc_st,
        SC.Smoother = switch(
          input$sc_sm,
          "Linear Model" = "lm",
          "Generalized Linear Model" = "glm",
          "Generalized Additive Model" = "gam",
          "Locally Estimated Scatterplot" = "loess"
        ),
        Vary = switch(
          input$sc_vary,
          "Home Ownership (%)" = "Own",
          "High School Completion (%)" = "HS",
          "Bachelor Degree Completion (%)" = "Col",
          "Household Annual Income (Current US$)" = "Inc",
          "Log10 Annual Income (Current US$)" = "LInc",
          "Population Size" = "Pop",
          "Log10 Population Inhabitants" = "LPop",
          "Unemployment (%)" = "UE"
        ),
        Varx = switch(
          input$sc_varx,
          "Home Ownership (%)" = "Own",
          "High School Completion (%)" = "HS",
          "Bachelor Degree Completion (%)" = "Col",
          "Household Annual Income (Current US$)" = "Inc",
          "Log10 Annual Income (Current US$)" = "LInc",
          "Population Size" = "Pop",
          "Log10 Population Inhabitants" = "LPop",
          "Unemployment (%)" = "UE"
        ),
        Group1 = switch(
          input$sc.gr_1,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        Group2 = switch(
          input$sc.gr_2,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        Group3 = switch(
          input$sc.gr_3,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        Group4 = switch(
          input$sc.gr_4,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        )
      )
    )
  })
  #2.3 Time Series Plot Output
  output$tseries <- renderPlot({
    if (input$tsgo == 0) return("")
    isolate(
      plots(
        Plot = 'TS',
        Stated = input$ts_st,
        area = input$ts_area,
        Vary = switch(
          input$ts_var,
          "Home Ownership (%)" = "Own",
          "High School Completion (%)" = "HS",
          "Bachelor Degree Completion (%)" = "Col",
          "Household Annual Income (Current US$)" = "Inc",
          "Log10 Annual Income (Current US$)" = "LInc",
          "Population Size" = "Pop",
          "Log10 Population Inhabitants" = "LPop",
          "Unemployment (%)" = "UE"
        ),
        Group1 =  switch(
          input$ts.gr_1,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        Group2 = switch(
          input$ts.gr_2,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        Group3 = switch(
          input$ts.gr_3,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        Group4 = switch(
          input$ts.gr_4,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        )
      )
    )
  })
  #2.4 Choropleth Maps Output
  output$choropleths <- renderPlot({
    if (input$chgo == 0) return("")
    isolate(
      plot_chloropleth_2(
        area = input$ch_area,
        State = input$ch_st,
        free_scales = FALSE,
        fill_var = switch(
          input$ch_var,
          "Home Ownership (%)" = "hom.own",
          "High School Completion (%)" = "edu.hs",
          "Bachelor Degree Completion (%)" = "edu.bs",
          "Household Annual Income (Current US$)" = "inc.inc",
          "Log10 Annual Income (Current US$)" = "linc",
          "Population Size" = "pop.tot",
          "Log10 Population Size" = "lpop",
          "Population Share (%)" = "pop.share",
          "Unemployment (%)" = "emp.ue"
        ),
        col_pal = switch(
          input$ch_col,
          "Red" = "Reds",
          "Purple" = "Purples",
          "Orange" = "Oranges",
          "Green" = "Greens",
          "Blue" = "Blues",
          "Gray" = "Greys"
        ),
        group1 = switch(
          input$ch.gr_1,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        group2 = switch(
          input$ch.gr_2,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        group3 = switch(
          input$ch.gr_3,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        ),
        group4 = switch(
          input$ch.gr_4,
          "All Races and Ethnicities"= "Total",
          "White (non-Hispanic)" = "WhiteNH",
          "Black" = "Black",
          "Asian" = "Asian",
          "None" = "None",
          "Hispanic" = "Hispanic"
        )
      )
    )
  })
  #2.5 Predictive Models Output
  output$pmplot <- renderPlot({
    if (input$pmgo == 0) return("")
    isolate(
      plot_prediction_app(
        model = model,
        race = input$pm_races,
        state = input$pm_st,
        edu.hs = input$pm_hs,
        emp.ue = input$pm_ue,
        inc.inc = input$pm_inc,
        val.hom = input$pm_val,
        separate_y = switch(
          input$pm_sp,
          "Yes" = TRUE,
          "No" = FALSE
        ),
        title = "Predicted values for selected model"
      )
    )
  })
  #Close Server Bracket
}

#------------------------------------------------------------------------------

#Launch App
shinyApp(ui, server)
