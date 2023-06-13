#Define Libraries and Set Working Directory

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(styler)

setwd(paste("C:/Users/jhona/OneDrive/Documents/Classes/USF",
"/ASA Data Challenge 2022/04. R Scripts", sep=""))
source("061223_ShinySource.r")
source("plot_prediction_app.r")

#------------------------------------------------------------------------------

#Define Helpful Variables

shiny.years <- c(2015, 2016, 2017, 2018, 2019, 2020)
shiny.raceth <- c("All Races and Ethnicities", "White", "Black", 
                  "American Indian or Alaska Native",
                  "Asian", "Native Hawaiian or Other Pacific Islander",
                  "Other", "Two or more", "Hispanic", "White Non Hispanic",
                  "None")
shiny.var <- c("Home Ownership (%)",
               "High School Completion (%)", 
               "Bachelor Degree Completion (%)",
               "Household Annual Income (Current US$)",
               "Log10 Annual Income (Current US$)",
               "Population Size",
               "Log10 Population Inhabitants",
               "Unemployment (%)")

#------------------------------------------------------------------------------

#Define UI

ui <- 
  bootstrapPage(
    
    #Define Theme
    
    navbarPage(
      theme = shinytheme("flatly"),
      "U.S. Home Ownership Visualizations",
      id = "nav",
      
      # 1. Define Box Plot Interface
      
      tabPanel("Box Plots",
               sidebarLayout(
                 sidebarPanel(
                   
      # 1.1.1 Specify whether State or County Approach
                   radioButtons("bp_area", "Data Type:",
                               choices = c("State", "County"),
                               inline = T,
                               selected = "State"
                   ),
      
                   conditionalPanel(
                     condition = "input.bp_area == 'County'",
                     textInput(
                       "bp_st",
                       "Input State Name (i.e. Florida):"
                     )
                   ),             
                   
      # 1.1.2 Specify year            
      radioButtons("bp_y", "Years",
                   choices = shiny.years,
                   inline = T
      ),
      
      # 1.1.3 Specify type of Box Plot            
      radioButtons("bp_v", "Type of Plot",
                   choices = c("Box Plot", "Violin Plot"),
                   selected = "Box Plot",
                   inline = T
      ),
      
      #1.1.4 Specify Variable y
      pickerInput("bp_var", "Variable:",
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
      
      # 1.2 Plot Panel
                 ),
      
      mainPanel(fluidRow(
        plotOutput(
          outputId = "boxplot",
          height = 600
      )
      )
                 ))),
      
      # 2. Define ScatterPlot Interface
      
      tabPanel("Scatter Plots",
               sidebarLayout(
                 sidebarPanel(
                   
                   # 2.1.1 Specify whether State or County Approach
                   radioButtons("sc_area", "Data Type:",
                                choices = c("State", "County"),
                                inline = T
                   ),
                   
                   conditionalPanel(
                     condition = "input.sc_area == 'County'",
                     textInput(
                       "sc_st",
                       "Input State Name (i.e. Florida):"
                     )
                   ),      
                   
                   # 2.1.2 Specify year            
                   radioButtons("sc_y", "Years",
                                choices = shiny.years,
                                inline = T
                   ),
                   
                   # 2.1.3 Specify Smoother            
                   radioButtons("sc_sm", "Smoother",
                                choices = c("Linear Model", 
                                            "Generalized Linear Model",
                                            "Generalized Additive Model",
                                            "Locally Estimated Scatterplot"),
                                selected = "Locally Estimated Scatterplot",
                                inline = T
                   ),
                   
                   #2.1.4 Specify Variable y
                   pickerInput("sc_vary", "Variable y:",
                               choices = shiny.var,
                               selected = "Home Ownership Rate (%)"
                   ),
                   
                   #2.1.5 Specify Variable x
                   pickerInput("sc_varx", "Variable x:",
                               choices = shiny.var,
                               selected = "Household Annual Income (Current US$)"
                   ),
                   
                   #2.1.6 Specify number of groups
                   #radioButtons("sc_gr", "Number of Racial/Ethnic Groups",
                    #            choices = c(1,2,3,4),
                    #            inline = T
                   #),
                   
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
                   
                   actionButton("scgo", "Click here to Plot",
                                style = "background-color: #2d3e50"
                   ),
                   
                   # 2.2 Plot Panel
                 ),
                 
                 mainPanel(fluidRow(
                   plotOutput(
                     outputId = "scatterplot",
                     height = 600
                   )
                 )
                 ))),
      
      
      # 3. Define Time Series Interface
      
      tabPanel("Time Series",
               sidebarLayout(
                 sidebarPanel(
                   
                   # 3.1.1 Specify whether State or County Approach
                   radioButtons("ts_area", "Data Type:",
                                choices = c("State", "County"),
                                inline = T,
                                selected = "State"
                   ),
                   
                   conditionalPanel(
                     condition = "input.ts_area == 'County'",
                     textInput(
                       "ts_st",
                       "Input State Name (i.e. Florida):"
                     )
                   ),      
                   

                   #3.1.2 Specify Variable
                   pickerInput("ts_var", "Variable:",
                               choices = shiny.var,
                               selected = "Home Ownership (%)"
                   ),
                   
                   #3.1.3 Specify number of groups
                   #radioButtons("ts_gr", "Number of Racial/Ethnic Groups",
                  #            choices = c(1,2,3,4),
                   #             inline = T
                   #),
                   
                   #3.1.3.1 Conditional Groups
                     pickerInput(
                       "ts.gr_1",
                       "Racial/Ethnic Group 1:",
                       choices = shiny.raceth,
                       selected = "All Races and Ethnicities"
                     ),
                   
                     pickerInput(
                       "ts.gr_2",
                       "Racial/Ethnic Group 2:",
                       choices = shiny.raceth,
                       selected = "None"
                     ),
                   
                     pickerInput(
                       "ts.gr_3",
                       "Racial/Ethnic Group 3:",
                       choices = shiny.raceth,
                       selected = "None"
                     ),
                   
                     pickerInput(
                       "ts.gr_4",
                       "Racial/Ethnic Group 4:",
                       choices = shiny.raceth,
                       selected = "None"
                     ),
                   
                   #3.1.4 Include Action Button
                   
                   actionButton("tsgo", "Click here to Plot",
                                style = "background-color: #2d3e50"
                   ),
                   
                   # 3.2 Plot Panel
                 ),
                 
                 mainPanel(fluidRow(
                   plotOutput(
                      outputId = "tseries",
                     height = 600
                   )
                 )
                 ))),
      
      # 4. Define Choropleth Map Interface
      
      tabPanel("Choropleth Maps",
               sidebarLayout(
                 sidebarPanel(
                   
                   # 4.1.1 Specify whether State or County Approach
                   radioButtons("ch_area", "Data Type:",
                                choices = c("State", "County"),
                                inline = T,
                                selected = "State"
                   ),
                   
                   conditionalPanel(
                     condition = "input.ch_area == 'County'",
                     textInput(
                       "ch_st",
                       "Input State Name (i.e. Florida):"
                     )
                   ),
                   
                   # 4.1.2 Specify year            
                   radioButtons("ch_y", "Years",
                                choices = shiny.years,
                                inline = T
                   ),
                   
                   #4.1.3 Specify Variable
                   pickerInput("ch_var", "Variable:",
                               choices = 
                                 c("Home Ownership (%)",
                                 "High School Completion (%)",
                                 "Bachelor Degree Completion (%)",
                                 "Household Annual Income (Current US$)",
                                 "Log10 Annual Income (Current US$)",
                                 "Population Size",
                                 "Log10 Population Size",
                                 "Population Share (%)",
                                 "Unemployment (%)")
                   ),
                   
                   # 4.1.4 Specify number of breaks            
                   #radioButtons("ch_br", "Number of breaks",
                  #              choices = c(3,4,5,6,7),
                  #              selected = 5,
                  #              inline = T
                  # ),
                   
                   #4.1.5 Specify break scale
                   #radioButtons("ch_brt", "Type of Scales:",
                  #             choices = c("Same", "Different*"),
                  #             inline = T,
                  #             selected = "Different"
                  # ),
                  
#helpText("* This features works best when comparing more than one race."),
                   
                   # 4.1.5 Specify color of choropleth            
                   radioButtons("ch_col", "Color",
                                choices = c("Blue", "Gray", 
                                            "Green", "Orange", 
                                            "Purple", "Red"),
                                selected = "Blue",
                                inline = T
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
                   
                   actionButton("chgo", "Click here to Plot",
                                style = "background-color: #2d3e50"
                   ),
                   
                   # 1.2 Plot Panel
                 ),
                 
                 mainPanel(fluidRow(
                   plotOutput(
                     outputId = "choropleths",
                     height = 600
                   )
                 )
                 ))),
      
      # 5. Define Model Interface
      
      tabPanel("Predictive Models",
               
               sidebarLayout(
                 sidebarPanel(
                   
                   #5.1.1 Model Type
                   
                   radioButtons("pm_model", "Model type:",
                     choices = c("MLE Gaussian",
                                 "MLE Binomial",
                                 "Bayesian Gaussian",
                                 "Bayesian Beta Binomial"),
                     inline = T),
                 
                 
                 #5.1.2 Select Race
                 
                 checkboxGroupInput("pm_races", "Select race(s):",
                               choices = c("White", "Black",
                                           "Asian", "Other"),
                               selected = c("White", "Black", "Asian"),
                               inline = T),
                 
                 #5.1.3 Input State
                 
                 tags$div(
                 textInput(
                   "pm_st",
                   "State:",
                   value = "Florida"),
                   style="display:inline-block"
                 
                 ),
                 
                 #5.1.4 Input Year
                 
                # tags$div(
                # numericInput(
                #   "pm_y",
                #   "Input Year:",
                #   value = 2021),
                #   style="display:inline-block"),
                 
                 #5.1.5 Input High School
                 
                 numericInput(
                   "pm_hs",
                   "Input High School Completion % in State:",
                   value = 90),
                 
                 #5.1.6 Input Unemployment
                 
                 numericInput(
                   "pm_ue",
                   "Input Unemployment % in State:",
                   value = 5),
                 
                 #5.1.7 Input Income
                 
                 numericInput(
                   "pm_inc",
                   "Input Household Annual Income US$:",
                   value = 60000),
                 
                 #5.1.8 Input Home Value
                 
                 numericInput(
                   "pm_val",
                   "Input Home Value US$:",
                   value = 200000),
                 
                 #5.1.9 Input Population Share %
                 
                # numericInput(
                #   "pm_shr",
                #   "Input Race Population Share %:",
                #   value = 25),
                 
                 #5.1.10 Not Overlaid
                 
                 radioButtons(
                   "pm_sp", "Separate Plots?",
                   choices = c("Yes", "No"),
                   selected = "No", inline = T),
                 
                 #5.1.10 Include Action Button
                 
                 actionButton("pmgo", "Click here to Plot",
                              style = "background-color: #2d3e50"
                 )
                 
                 #5.2 Plot
                 ),
                 mainPanel(
                   fluidRow(
                     plotOutput(
                       outputId = "pmplot",
                       height = 600)
                   )
                 )
                   
                 
                 )
                 
               )
      
      #Close UI
      
      ))
      
    
#------------------------------------------------------------------------------
  
#Define Server

server <- function(input, output) {
  
  #2.1 Box Plot Output
  output$boxplot <-
    renderPlot({
      if(input$bpgo ==0) {return ("")}
      
      isolate(
        plots(
          Plot = 'BP',
          area = input$bp_area,
          Stated = input$bp_st,
          BP.Violin = switch (input$bp_v,
            "Box Plot" = "F",
            "Violin Plot" = "T"
          ),
          Yearp = input$bp_y,
          Vary = 
            switch(input$bp_var,
                   "Home Ownership (%)" = "Own",
                   "High School Completion (%)" = "HS", 
                   "Bachelor Degree Completion (%)" = "Col",
                   "Household Annual Income (Current US$)" = "Inc",
                   "Log10 Annual Income (Current US$)" = "LInc",
                   "Population Size" = "Pop",
                   "Log10 Population Inhabitants" = "LPop",
                   "Unemployment (%)" = "UE"
          ),
          
          Group1 = 
            switch(input$bp.gr_1,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                    "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                    "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group2 = 
            switch(input$bp.gr_2,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group3 = 
            switch(input$bp.gr_3,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group4 = 
            switch(input$bp.gr_4,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH")
          )
          )
        
      })
  
  #2.2 Scatter Plot Output
  output$scatterplot <-
    renderPlot({
      if(input$scgo ==0) {return ("")}
      
      isolate(
        plots(
          Plot = 'SC',
          area = input$sc_area,
          Stated = input$sc_st,
          SC.Smoother = 
            switch(input$sc_sm,
            "Linear Model" = "lm", 
            "Generalized Linear Model" = "glm",
            "Generalized Additive Model" = "gam",
            "Locally Estimated Scatterplot" = "loess"
          ),
          Yearp = input$sc_y,
          Vary = 
            switch(input$sc_vary,
                   "Home Ownership (%)" = "Own",
                   "High School Completion (%)" = "HS", 
                   "Bachelor Degree Completion (%)" = "Col",
                   "Household Annual Income (Current US$)" = "Inc",
                   "Log10 Annual Income (Current US$)" = "LInc",
                   "Population Size" = "Pop",
                   "Log10 Population Inhabitants" = "LPop",
                   "Unemployment (%)" = "UE"
            ),
          Varx = 
            switch(input$sc_varx,
                   "Home Ownership (%)" = "Own",
                   "High School Completion (%)" = "HS", 
                   "Bachelor Degree Completion (%)" = "Col",
                   "Household Annual Income (Current US$)" = "Inc",
                   "Log10 Annual Income (Current US$)" = "LInc",
                   "Population Size" = "Pop",
                   "Log10 Population Inhabitants" = "LPop",
                   "Unemployment (%)" = "UE"
            ),
          
          Group1 = 
            switch(input$sc.gr_1,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group2 = 
            switch(input$sc.gr_2,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group3 = 
            switch(input$sc.gr_3,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group4 = 
            switch(input$sc.gr_4,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH")
        )
      )
      
    })
  
  #2.3 Time Series Plot Output
  output$tseries <-
    renderPlot({
      if(input$tsgo ==0) {return ("")}
      
      isolate(
        plots(
          Plot = 'TS',
          Stated = input$ts_st,
          area = input$ts_area,
          Vary = 
            switch(input$ts_var,
                   "Home Ownership (%)" = "Own",
                   "High School Completion (%)" = "HS", 
                   "Bachelor Degree Completion (%)" = "Col",
                   "Household Annual Income (Current US$)" = "Inc",
                   "Log10 Annual Income (Current US$)" = "LInc",
                   "Population Size" = "Pop",
                   "Log10 Population Inhabitants" = "LPop",
                   "Unemployment (%)" = "UE"
            ),
          Group1 = 
            switch(input$ts.gr_1,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group2 = 
            switch(input$ts.gr_2,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group3 = 
            switch(input$ts.gr_3,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          Group4 = 
            switch(input$ts.gr_4,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH")
        )
      )
      
    })
  
  
  #2.4 Choropleth Maps Output
  output$choropleths <-
    renderPlot({
      if(input$chgo ==0) {return ("")}
      
      isolate(
        plot_chloropleth(
          area = input$ch_area,
          State = input$ch_st,
          yearch = input$ch_y,
          free_scales = FALSE,
          fill_var = switch(input$ch_var,
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
          col_pal = switch(input$ch_col,
            "Red" = "Reds",
            "Purple" = "Purples",
            "Orange" = "Oranges",
            "Green" = "Greens",
            "Blue" = "Blues",
            "Gray" = "Greys"
          ),
          group1 = 
            switch(input$ch.gr_1,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          group2 = 
            switch(input$ch.gr_2,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          group3 = 
            switch(input$ch.gr_3,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH"),
          group4 = 
            switch(input$ch.gr_4,
                   "All Races and Ethnicities"= "Total", "White" = "White", 
                   "Black" = "Black",
                   "American Indian or Alaska Native" = "AIoAN", 
                   "Asian" = "Asian", "None" = "None",
                   "Native Hawaiian or Other Pacific Islander" = "NHoOPI",
                   "Other" = "Other", "Two or more" = "Two+",
                   "Hispanic" = "Hispanic", "White Non Hispanic" = "WhiteNH")
          #breaks = input$ch_br
        )
      )
      
    })

  
  #2.5 Predictive Models Output
  output$pmplot <-
    renderPlot({
      if(input$pmgo ==0) {return ("")}
      
      isolate(
      plot_prediction_app(
        model_which = 
          switch(input$pm_model,
                 "MLE Gaussian" = "mle_gaus",
                 "MLE Binomial" = "mle_bin",
                 "Bayesian Gaussian" = "bay_gaus",
                 "Bayesian Beta Binomial" = "bay_betabin"),
        race = input$pm_races,
        state = input$pm_st,
        edu.hs = input$pm_hs,
        emp.ue = input$pm_ue,
        inc.inc = input$pm_inc,
        val.hom = input$pm_val,
        separate_y = switch(input$pm_sp,
                            "Yes" = TRUE, "No" = FALSE),
        title = "Predicted values for selected model"
      )
      )
      
    })
        
  #Close Server Bracket      
        }
  
#------------------------------------------------------------------------------
  
#Launch App

shinyApp(ui, server)
