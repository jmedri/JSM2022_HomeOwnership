#Define Libraries and Set Working Directory
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(styler)

source("3.Data_Modelling/initialize.R")
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
            # shiny::textInput(
            #   "bp_st",
            #   "Input State Name (i.e. Florida):",
            #   value = "All"
            # )
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
            choices = c("Box Plot", "Violin Plot"),
            selected = "Box Plot",
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
            # shiny::textInput(
            #   "sc_st",
            #   "Input State Name (i.e. Florida):",
            #   value = "All"
            # )
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
            # shiny::textInput(
            #   "ts_st",
            #   "Input State Name (i.e. Florida):",
            #   value = "All"
            # )
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
            # shiny::textInput(
            #   "ch_st",
            #   "Input State Name (i.e. Florida):",
            #   value = "All"
            # )
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
          #5.1.2 Select Race
          shiny::checkboxGroupInput(
            "pm_groups",
            "Select Racial/Ethnic Group(s):",
            choices = RACES_MODEL_APP,
            selected = RACES_MODEL_APP
          ),
          #5.1.3 Input State
          shiny::tags$div(
            # shiny::textInput(
            #   "pm_st",
            #   "Input State:",
            #   value = "All"
            # ),
            shinyWidgets::pickerInput(
              "pm_st",
              "Input State Name:",
              selected = "All",
              choices = STATES_APP
            ),
            style = "display:inline-block"
          ),
          # #5.1.5 Input High School
          # shiny::numericInput(
          #   "pm_hs",
          #   "Input High School Completion % in State:",
          #   value = 90
          # ),
          # #5.1.6 Input Unemployment
          # shiny::numericInput(
          #   "pm_ue",
          #   "Input Unemployment % in State:",
          #   value = 5
          # ),
          #5.1.7 Input Income
          shiny::numericInput(
            "pm_inc",
            "Input Household Annual Income US$:",
            value = 60000
          ),
          # #5.1.8 Input Home Value
          # shiny::numericInput(
          #   "pm_val",
          #   "Input Home Value US$:",
          #   value = 200000
          # ),
          #5.1.10 Not Overlaid
          shiny::radioButtons(
            "pm_sp",
            "Separate Plots?",
            choices = c("Yes", "No"),
            selected = "No",
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
        BP.Violin = switch(
          input$bp_v,
          "Box Plot" = "F",
          "Violin Plot" = "T"
        ),
        Yearp = input$bp_y,
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
        Yearp = input$sc_y,
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
        Yearp = input$ch_y,
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
        model = MODEL_APP,
        race = input$pm_groups,
        state = input$pm_st,
        # edu.hs = input$pm_hs,
        # emp.ue = input$pm_ue,
        inc.inc = input$pm_inc,
        # val.hom = input$pm_val,
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

#Launch App
shiny::shinyApp(APP_UI, APP_SERVER)
