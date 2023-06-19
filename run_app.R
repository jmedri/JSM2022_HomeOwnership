#Define Libraries and Set Working Directory
library(shiny)
library(shinythemes)
library(shinyWidgets)

packageVersion("shiny")
packageVersion("shinythemes")
packageVersion("shinyWidgets")

source("initialize.R")
initialize()

#Define UI
APP_UI <- shiny::bootstrapPage(
  #Define Theme
  shiny::navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    "U.S. Home Ownership Visualizations",
    id = "nav",
    # 1. Define Box Plot Interface
    shiny::tabPanel(
      "Box Plots",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # 1.1.1 Specify whether State or County Approach
          shiny::radioButtons(
            "bp_area",
            "Data Type:",
            choices = c("State", "County"),
            inline = TRUE,
            selected = "State"
          ),
          shiny::conditionalPanel(
            condition = "input.bp_area == 'County'",
            shinyWidgets::pickerInput(
              "bp_st",
              "Input State Name:",
              selected = "All",
              choices = STATES_APP
            )
          ),
          # 1.1.2 Specify year            
          shiny::radioButtons(
            "bp_y",
            "Years",
            choices = YEARS,
            inline = TRUE,
            selected = 2020
          ),
          # 1.1.3 Specify type of Box Plot
          shiny::radioButtons(
            "bp_v",
            "Type of Plot",
            choices = c("Box Plot" = FALSE, "Violin Plot" = TRUE),
            selected = FALSE,
            inline = TRUE
          ),
          #1.1.4 Specify Variable y
          shinyWidgets::pickerInput(
            "bp_var",
            "Variable:",
            choices = VARS_APP,
            selected = "Own"
          ),
          shinyWidgets::pickerInput(
            "bp.gr_1",
            "Racial/Ethnic Group 1:",
            choices = RACES_APP,
            selected = "Total"
          ),
          shinyWidgets::pickerInput(
            "bp.gr_2",
            "Racial/Ethnic Group 2:",
            choices = RACES_APP,
            selected = "None"
          ),
          shinyWidgets::pickerInput(
            "bp.gr_3",
            "Racial/Ethnic Group 3:",
            choices = RACES_APP,
            selected = "None"
          ),
          shinyWidgets::pickerInput(
            "bp.gr_4",
            "Racial/Ethnic Group 4:",
            choices = RACES_APP,
            selected = "None"
          ),
          #1.1.6 Include Action Button
          shiny::actionButton(
            "bpgo",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          ),
        ),
        # 1.2 Plot Panel
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::plotOutput(
              outputId = "boxplot",
              height = 600
            )
          )
        )
      )
    ),
    # 2. Define ScatterPlot Interface
    shiny::tabPanel(
      "Scatter Plots",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # 2.1.1 Specify whether State or County Approach
          shiny::radioButtons(
            "sc_area",
            "Data Type:",
            choices = c("State", "County"),
            selected = "State",
            inline = TRUE
          ),
          shiny::conditionalPanel(
            condition = "input.sc_area == 'County'",
            shinyWidgets::pickerInput(
              "sc_st",
              "Input State Name:",
              selected = "All",
              choices = STATES_APP
            )
          ),
          # 2.1.2 Specify year            
          shiny::radioButtons(
            "sc_y",
            "Years",
            choices = YEARS,
            inline = TRUE,
            selected = 2020
          ),
          # 2.1.3 Specify Smoother
          shiny::radioButtons(
            "sc_sm",
            "Smoother",
            choices = SMOOTHERS_APP,
            selected = "loess"
          ),
          #2.1.4 Specify Variable y
          shinyWidgets::pickerInput(
            "sc_vary",
            "Variable y:",
            choices = VARS_APP,
            selected = "Own"
          ),
          #2.1.5 Specify Variable x
          shinyWidgets::pickerInput(
            "sc_varx",
            "Variable x:",
            choices = VARS_APP,
            selected = "Inc"
          ),
          #2.1.6.1 Conditional Groups
          shinyWidgets::pickerInput(
            "sc.gr_1",
            "Racial/Ethnic Group 1:",
            choices = RACES_APP,
            selected = "Total"
          ),
          shinyWidgets::pickerInput(
            "sc.gr_2",
            "Racial/Ethnic Group 2:",
            choices = RACES_APP,
            selected = "None"
          ),
          shinyWidgets::pickerInput(
            "sc.gr_3",
            "Racial/Ethnic Group 3:",
            choices = RACES_APP,
            selected = "None"
          ),
          shinyWidgets::pickerInput(
            "sc.gr_4",
            "Racial/Ethnic Group 4:",
            choices = RACES_APP,
            selected = "None"
          ),
          #2.1.7 Include Action Button
          shiny::actionButton(
            "scgo",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          ),
        ),
        # 2.2 Plot Panel
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::plotOutput(
              outputId = "scatterplot",
              height = 600
            )
          )
        )
      )
    ),
    # 3. Define Time Series Interface
    shiny::tabPanel(
      "Time Series",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # 3.1.1 Specify whether State or County Approach
          shiny::radioButtons(
            "ts_area",
            "Data Type:",
            choices = c("State", "County"),
            inline = TRUE,
            selected = "State"
          ),
          shiny::conditionalPanel(
            condition = "input.ts_area == 'County'",
            shinyWidgets::pickerInput(
              "ts_st",
              "Input State Name:",
              selected = "All",
              choices = STATES_APP
            )
          ),      
          #3.1.2 Specify Variable
          shinyWidgets::pickerInput(
            "ts_var",
            "Variable:",
            choices = VARS_APP,
            selected = "Own"
          ),
          #3.1.3.1 Conditional Groups
          shinyWidgets::pickerInput(
            "ts.gr_1",
            "Racial/Ethnic Group 1:",
            choices = RACES_APP,
            selected = "Total"
          ),
          shinyWidgets::pickerInput(
            "ts.gr_2",
            "Racial/Ethnic Group 2:",
            choices = RACES_APP,
            selected = "None"
          ),
          shinyWidgets::pickerInput(
            "ts.gr_3",
            "Racial/Ethnic Group 3:",
            choices = RACES_APP,
            selected = "None"
          ),
          shinyWidgets::pickerInput(
            "ts.gr_4",
            "Racial/Ethnic Group 4:",
            choices = RACES_APP,
            selected = "None"
          ),
          #3.1.4 Include Action Button
          shiny::actionButton(
            "tsgo",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          ),
        ),
        # 3.2 Plot Panel
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::plotOutput(
              outputId = "tseries",
              height = 600
            )
          )
        )
      )
    ),
    # 4. Define Choropleth Map Interface
    shiny::tabPanel(
      "Choropleth Maps",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # 4.1.1 Specify whether State or County Approach
          shiny::radioButtons(
            "ch_area",
            "Data Type:",
            choices = c("State", "County"),
            inline = TRUE,
            selected = "State"
          ),
          shiny::conditionalPanel(
            condition = "input.ch_area == 'County'",
            shinyWidgets::pickerInput(
              "ch_st",
              "Input State Name:",
              selected = "All",
              choices = STATES_APP
            )
          ),
          # 4.1.2 Specify year            
          shiny::radioButtons(
            "ch_y",
            "Years",
            choices = YEARS,
            inline = TRUE
          ),
          #4.1.3 Specify Variable
          shinyWidgets::pickerInput(
            "ch_var",
            "Variable:",
            choices = VARS_APP,
            selected = "Own"
          ),
          # 4.1.5 Specify color of choropleth
          shiny::radioButtons(
            "ch_col",
            "Color",
            choices = COLOR_PALETTES_APP,
            selected = "Blue",
            inline = TRUE
          ),
          shinyWidgets::pickerInput(
            "ch.gr_1",
            "Racial/Ethnic Group 1:",
            choices = RACES_APP,
            selected = "Total"
          ),
          shinyWidgets::pickerInput(
            "ch.gr_2",
            "Racial/Ethnic Group 2:",
            choices = RACES_APP,
            selected = "None"
          ),
          shinyWidgets::pickerInput(
            "ch.gr_3",
            "Racial/Ethnic Group 3:",
            choices = RACES_APP,
            selected = "None"
          ),
          shinyWidgets::pickerInput(
            "ch.gr_4",
            "Racial/Ethnic Group 4:",
            choices = RACES_APP,
            selected = "None"
          ),
          #1.1.6 Include Action Button
          shiny::actionButton(
            "chgo",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          ),
        ),
        # 1.2 Plot Panel
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::plotOutput(
              outputId = "choropleths",
              height = 600
            )
          )
        )
      )
    ),
    # 5. Define Model Interface
    shiny::tabPanel(
      "Predictive Models",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          #5.1.1 Model Type
          shiny::tags$div(
            shinyWidgets::pickerInput(
              "pm_model",
              "Input Model Type:",
              selected = FINAL_MODEL,
              choices = MODEL_NAMES_APP
            ),
            style = "display:inline-block"
          ),
          #5.1.2 Select Race
          shiny::checkboxGroupInput(
            "pm_groups",
            "Select Racial/Ethnic Group(s):",
            choices = RACES_MODEL_APP,
            selected = RACES_MODEL_APP
          ),
          #5.1.3 Input State
          shiny::tags$div(
            shinyWidgets::pickerInput(
              "pm_st",
              "Input State Name:",
              selected = "All",
              choices = STATES_APP
            ),
            style = "display:inline-block"
          ),
          #5.1.5 Input High School
          shiny::conditionalPanel(
            condition = paste0(MODEL_VARS_JS_APP, "[input.pm_model].includes('hs')"),
            shiny::numericInput(
              "pm_hs",
              "Input High School Completion % in State:",
              value = 90
            )
          ),
          #5.1.6 Input Unemployment
          shiny::conditionalPanel(
            condition = paste0(MODEL_VARS_JS_APP, "[input.pm_model].includes('ue')"),
            shiny::numericInput(
              "pm_ue",
              "Input Unemployment % in State:",
              value = 5
            )
          ),
          #5.1.7 Input Income
          shiny::conditionalPanel(
            condition = paste0(MODEL_VARS_JS_APP, "[input.pm_model].includes('inc')"),
            shiny::numericInput(
              "pm_inc",
              "Input Household Annual Income US$:",
              value = 60000
            )
          ),
          #5.1.8 Input Population
          shiny::conditionalPanel(
            condition = paste0(MODEL_VARS_JS_APP, "[input.pm_model].includes('tot')"),
            shiny::numericInput(
              "pm_htot",
              "Input County Total Households:",
              value = 10000
            )
          ),
          #5.1.10 Not Overlaid
          shiny::radioButtons(
            "pm_sp",
            "Separate Plots?",
            choices = c("Yes" = TRUE, "No" = FALSE),
            selected = FALSE,
            inline = TRUE
          ),
          #5.1.10 Include Action Button
          shiny::actionButton(
            "pmgo",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          )
        ),
        #5.2 Plot
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::plotOutput(
              outputId = "pmplot",
              height = 600
            )
          )
        )
      )
    ),
    shiny::tabPanel(
      "Predictive Models 2",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          #5.1.1 Model Type
          shiny::radioButtons(
            "pm2_model",
            "Model Type:",
            choices = c(
              "MLE Gaussian" = "mle_gaus",
              "MLE Binomial" = "mle_bin",
              "Bayesian Gaussian" = "bay_gaus2",
              "Bayesian Beta Binomial" = "bay_betabin"
            )
          ),
          #5.1.2 Select Race
          shiny::checkboxGroupInput(
            "pm2_races",
            "Select Racial/Ethic Group(s):",
            choices = c("White", "Black", "Asian", "Other"),
            selected = c("White", "Black", "Asian"),
            inline = TRUE
          ),
          #5.1.3 Input State
          shiny::tags$div(
            shinyWidgets::pickerInput(
              "pm2_st",
              "Input State:",
              selected = "All",
              choices = STATES_APP
            ),
            style = "display:inline-block"
          ),
          #5.1.4 Input Year
          shiny::tags$div(
            shiny::numericInput(
              "pm2_y",
              "Input Year:",
              value = 2021
            ),
            style = "display:inline-block"
          ),
          #5.1.5 Input High School
          shiny::numericInput(
            "pm2_hs",
            "Input High School Completion % in State:",
            value = 90
          ),
          #5.1.6 Input Unemployment
          shiny::numericInput(
            "pm2_ue",
            "Input Unemployment % in State:",
            value = 5
          ),
          #5.1.7 Input Income
          shiny::numericInput(
            "pm2_inc",
            "Input Household Annual Income US$:",
            value = 60000
          ),
          #5.1.8 Input Home Value
          shiny::numericInput(
            "pm2_val",
            "Input Home Value US$:",
            value = 200000
          ),
          #5.1.9 Input Population Share %
          shiny::numericInput(
            "pm2_shr",
            "Input Race Population Share %:",
            value = 25
          ),
          #5.1.10 Not Overlaid
          shiny::radioButtons(
            "pm2_sp",
            "Separate Plots?",
            choices = c("Yes", "No"),
            selected = "No",
            inline = TRUE
          ),
          #5.1.10 Include Action Button
          shiny::actionButton(
            "pm2go",
            "Click here to Plot",
            style = "background-color: #2d3e50"
          )
        ),
        #5.2 Plot
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::plotOutput(
              outputId = "pmplot2",
              height = 600
            )
          )
        )
      )
    )
  )
)
#Close UI

#Define Server
APP_SERVER <- function(input, output) {
  #2.1 Box Plot Output
  output$boxplot <- renderPlot({
    if (input$bpgo == 0) return ("")
    isolate(
      plot_app(
        Plot = "BP",
        area = input$bp_area,
        Stated = input$bp_st,
        BP.Violin = as.logical(input$bp_v),
        Yearp = as.integer(input$bp_y),
        Vary = input$bp_var,
        Group1 = input$bp.gr_1,
        Group2 = input$bp.gr_2,
        Group3 = input$bp.gr_3,
        Group4 = input$bp.gr_4
      )
    )
  })
  #2.2 Scatter Plot Output
  output$scatterplot <- renderPlot({
    if (input$scgo == 0) return("")
    isolate(
      plot_app(
        Plot = "SC",
        area = input$sc_area,
        Stated = input$sc_st,
        SC.Smoother = input$sc_sm,
        Yearp = as.integer(input$sc_y),
        Vary = input$sc_vary,
        Varx = input$sc_varx,
        Group1 = input$sc.gr_1,
        Group2 = input$sc.gr_2,
        Group3 = input$sc.gr_3,
        Group4 = input$sc.gr_4
      )
    )
  })
  #2.3 Time Series Plot Output
  output$tseries <- renderPlot({
    if (input$tsgo == 0) return("")
    isolate(
      plot_app(
        Plot = "TS",
        Stated = input$ts_st,
        area = input$ts_area,
        Vary = input$ts_var,
        Group1 = input$ts.gr_1,
        Group2 = input$ts.gr_2,
        Group3 = input$ts.gr_3,
        Group4 = input$ts.gr_4
      )
    )
  })
  #2.4 Choropleth Maps Output
  output$choropleths <- renderPlot({
    if (input$chgo == 0) return("")
    isolate(
      plot_choropleth_app(
        area = input$ch_area,
        Stated = input$ch_st,
        Yearp = as.integer(input$ch_y),
        free_scales = FALSE,
        fill_var = input$ch_var,
        col_pal = input$ch_col,
        Group1 = input$ch.gr_1,
        Group2 = input$ch.gr_2,
        Group3 = input$ch.gr_3,
        Group4 = input$ch.gr_4
      )
    )
  })
  #2.5 Predictive Models Output
  output$pmplot <- renderPlot({
    if (input$pmgo == 0) return("")
    isolate(
      plot_prediction_app(
        model_name = input$pm_model,
        race = input$pm_groups,
        state = input$pm_st,
        HS = as.numeric(input$pm_hs),
        UE = as.numeric(input$pm_ue),
        Inc = as.numeric(input$pm_inc),
        HTot = as.numeric(input$pm_htot),
        separate_y = as.logical(input$pm_sp),
        title = "Predicted values for selected model"
      )
    )
  })
  #Close Server Bracket
}

#Launch App
shiny::shinyApp(APP_UI, APP_SERVER)
