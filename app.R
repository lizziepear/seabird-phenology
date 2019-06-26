## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Shiny app to explore the methods in the density maps paper
## Copyright (C) 2019 Lizzie Pearmain & Ana Carneiro
## This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
## This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
## You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list=ls())

library(shiny)
library(lubridate)

## set working directory
setwd("C:\\Users\\eliza\\OneDrive\\Documents\\PROJECTS\\seabird-phenology")
# setwd("C:\\Users\\Lizzie\\OneDrive\\Documents\\PROJECTS\\seabird-phenology")

# rm(list=ls())

## load source functions
source("functions_phenology.R")
source("functions_demography.R")

## Define app UI ----
ui <- navbarPage("Seabird phenology", selected = "Introduction",
  
  ## Link stylesheet and other HTML tags we need ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")#,
    # tags$link(href="https://fonts.googleapis.com/css?family=Roboto+Slab&display=swap", rel="stylesheet")
    # tags$div(class = "footer", "Disclaimer: this app is not finished")
  ),
  
  
  ########################################################################################
  ################## TAB 0 - Introduction to the methods ---- ############################
  ########################################################################################
  tabPanel("Introduction",
    titlePanel(h1("Introduction to the seabird density maps method")),
    sidebarLayout(
      sidebarPanel(
      ),
      mainPanel(
        h3("Mapping the global distribution of seabird populations: a framework for integrating biologging, demographic and phenological datasets."),
        tags$div("Steps:", tags$br(), tags$ol(tags$li("1. Download data from the", tags$a(href="www.seabirdtracking.org", "Seabird Tracking Database")),
                tags$li("2. Create one csv file per species"), tags$li("3. etc."))
        )
      )
    )
  ),
  
  ########################################################################################
  ################## TAB 1 - PHENOLOGY TABLES ---- #######################################
  ########################################################################################
  tabPanel("Phenology tables",
    titlePanel(h1("Seabird phenology")),
    ## Sidebar layout with input and output definitions ----
    sidebarLayout(
      ## Sidebar panel for inputs ----
      sidebarPanel(tags$p("This tab uses mean laying date and mean lengths of the incubation, brood-guard and post-brood stages to estimate average start dates of each section of the breeding cycle."), tags$br(),    
                   ## Input: Selector for choosing species type (this does nothing yet) ----
                   # selectInput(inputId = "species_type", label = "Choose whether your species is annual or biennial", choices = c("annual", "biennial")),
                   ## Input mean length of pre-laying
                   numericInput(inputId = "prelay_length_days", label = "Insert mean pre-laying length in days", value = 0),
                   ## Input mean laying date as day-month
                   textInput(inputId = "laydate_string", label = "enter mean laying date in format dd-mm", placeholder = "dd-mm"), #value = "01-01",
                   # Input: numeric input for length of incubation
                   numericInput(inputId = "inc_length_days", label = "Insert mean incubation length in days", value = 25),
                   # Input: numeric input for length of brood-guard
                   numericInput(inputId = "brood_length_days", label = "Insert mean brood-guard length in days", value = 25),
                   # Input: numeric input for length of post-brood
                   numericInput(inputId = "post_length_days", label = "Insert mean post-guard length in days", value = 25),
                   
                   ## Action button to trigger calculation
                   h3("To calculate monthly phenology weightings, click a button below:"),
                   p("Successful breeders:"),
                   # p("Adult sucessful breeders:") ## TODO: add options to calculate and display the timings for each pool of birds - as tabset isn't working.
                   actionButton("goSB", "Go"),
                   tags$br(),
                   p("Fail breeders:"),
                   # p("Adult sucessful breeders:") ## TODO: add options to calculate and display the timings for each pool of birds - as tabset isn't working.
                   actionButton("goFB", "Go")
                   
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(#"Average phenology timings",
          h2("Average phenology timings for successful breeders:"),
          tableOutput("phenTab1Succ"),
          tags$br(),
          h3("Average fail date for breeders"),
          p("(= halfway through breeding season from start of incubation to end of post-brood):"),
          textOutput("failDate"),
          tags$br(),
          h2("Average phenology timings for fail breeders:"),
          tags$br(),
          tableOutput("phenTab1Fail"),
          tags$br(),
          h2("Monthly average phenology for successful breeders:"),
          tableOutput("phenTabBeta"),
          tags$br(),
          h2("Monthly average phenology for fail breeders:"),
          tableOutput("phenTabGamma"),
          tags$br(), tags$br()
        # tabsetPanel(
        #   tabPanel("Successful breeders",
        #            h2("Monthly average phenology for adult successful breeders (group beta):"),
        #            tableOutput("phenTabBeta")
        #   ),
        #   tabPanel("Fail breeders",
        #            h2("Average phenology timings for your population:"), tableOutput("phenTab1"), tags$br(),
        #            h2("Monthly average phenology for adult fail breeders (group gamma):")#,
        #            # tableOutput("phenTabDelta")
        #   ),
        #   tabPanel("Non-breeders",
        #            h2("Average phenology timings for your population:"), tableOutput("phenTab1"), tags$br(),
        #            h2("Monthly average phenology for adult non-breeders (sabbaticals) (group gamma):")#,
        #            # tableOutput("phenTabGamma")
        #   ),
        #   tabPanel("Juveniles",
        #            h2("Average phenology timings for your population:"), tableOutput("phenTab1"), tags$br(),
        #            h2("Monthly average phenology for juveniles (group zeta):")#,
        #            # tableOutput("phenTabTheta")
        #   ),
        #   tabPanel("Immatures",
        #            h2("Average phenology timings for your population:"), tableOutput("phenTab1"), tags$br(),
        #            h2("Monthly average phenology for immatures (group theta):")#,
        #            # tableOutput("phenTabTheta")
        #   )
        # ) ## end tabsetPanel()
      ) ## end mainPanel()
    ) ## end sidebarLayout()
  ), ## end tabPanel("Phenology")
  
  ########################################################################################
  ################### TAB 2 - DEMOGRAPHY MODEL ###########################################
  ########################################################################################
  
  tabPanel("Demography model",
           titlePanel(h1("Seabird demography")),
    sidebarLayout(
      sidebarPanel(tags$p("This tab uses demographic parameters to estimate the stable stage distribution of individuals in the populatio. Output age categories are juveniles, immatures, breeding adults (split into successful and failed breeders) and adults which are not breeding this season (sabbaticals)."), tags$br(),    
                   ## Input: Selector for age of first breeding ----
                   selectInput(inputId = "afb", label = "Average age at first breeding (rounded to whole number of years):", choices = c(1:15)),
                   ## Input: numeric input (0 - 1) for average annual adult survival
                   numericInput(inputId = "Sa", min = 0, max = 1, step = 0.01, label = "Average annual survival of adults (birds of breeding age):", value = 0.9),
                   ## Input: numeric input (0 - 1) for average annual juvenile / immature survival
                   numericInput(inputId = "Sj", min = 0, max = 1, step = 0.01, label = "Average annual survival of juveniles / immatures (average from fledging until recruitment into the breeding population", value = 0.8),
                   # Input: numeric input (0 - 1) for breeding SUCCESS
                   numericInput(inputId = "BS", min = 0, max = 1, step = 0.01, label = "Average breeding success (probability of a breeding attempt resulting in a fledged chick)", value = 0.6),
                   # Input: numeric input (0 - 1) for breeding FREQUENCY
                   numericInput(inputId = "BF", min = 0, max = 1, step = 0.01, label = "Average breeding frequency (proportion of the adult population breeding in any given year", value = 0.7),
                   ## Action button to trigger running the model
                   p("To run the demography model, click the button below"),
                   actionButton("go", "Go")
      ),
      mainPanel(
        p("Output from demography model will go here - a) table showing population stratification in proportions; b) figure 1 showing populatino breakdown as bar chart (or maybe pie chart?); c) numbers of birds in each stratum of the population"),
        h2("Estimated stable stage distribution of your population:"),
        tableOutput("demTab1")
      )
    )
  )  
)

##### Define server logic ----
server <- function(input, output) {
  
  ########################################################################################
  ################## TAB 1 - PHENOLOGY TABLES ---- #######################################
  ########################################################################################
  
  #### Make tables for SUCCESSFUL BREEDERS ----
  ## Make the first phenTable1 table
  makePhenTable1Succ <- reactive({
    phenTable1Succ(input$laydate_string, input$prelay_length_days, input$inc_length_days, 
               input$brood_length_days, input$post_length_days)
  })
  ## Make the pretty version of phenTable1
  makePrettyPhenTable1Succ <- reactive({
    prettyPhenTable1(makePhenTable1Succ())
  })
  
  #### Make output showing fail date as a text output ----
  makeFailDate <- reactive({
    findFailDate(makePhenTable1Succ())
  })
  ## Render it:
  output$failDate <- renderText({
    makeFailDate()
  })
  
  #### Make tables for FAIL BREEDERS ----
  ## Make the first phenTable1 table
  makePhenTable1Fail <- reactive({
    phenTable1Fail(input$laydate_string, input$prelay_length_days, input$inc_length_days, 
                   input$brood_length_days, input$post_length_days)
  })
  ## Make the pretty version of phenTable1
  makePrettyPhenTable1Fail <- reactive({
    prettyPhenTable1(makePhenTable1Fail())
  })
  
  ## Make the monthly table for SUCC - phenTableBeta table ----
  makePhenTableBeta <- eventReactive(input$goSB, {
    
    ## create a progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing table", value = 0)
    ## close the progress when the reactive exits, even if there's an error:
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    phenTableBeta(makePhenTable1Succ(), updateProgress)
  })
  
  ## Make the monthly table for SUCC - phenTableBeta table ----
  makePhenTableGamma <- eventReactive(input$goFB, {
    
    ## create a progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing table", value = 0)
    ## close the progress when the reactive exits, even if there's an error:
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    phenTableBeta(makePhenTable1Fail(), updateProgress)
  })
  
  ## Render the first phenTable1 table for SUCC ----
  output$phenTab1Succ <- renderTable({
    makePrettyPhenTable1Succ()
  })
  ## Render the first phenTable1 table for FAIL ----
  output$phenTab1Fail <- renderTable({
    makePrettyPhenTable1Fail()
  })
  
  ## Render the second phenTableBeta table FOR SUCC ----
  output$phenTabBeta <- renderTable({
    makePhenTableBeta()
  })
  ## Render the second phenTableBeta table FOR FAIL ----
  output$phenTabGamma <- renderTable({
    makePhenTableGamma()
  })
  
  ########################################################################################
  ################### TAB 2 - DEMOGRAPHY MODEL ###########################################
  ########################################################################################
  
  ## Make the overall demography table ----
  makeDemTable1 <- reactive({
    popStrat(input$afb, input$Sa, input$Sj, input$BS, input$BF)
  })
  
  ########################################################################################
  ######### change output options
  ########################################################################################
  
  # outputOptions(output, "phenTab1", suspendWhenHidden = FALSE)
  
}

# Create Shiny app ----
shinyApp(ui, server)