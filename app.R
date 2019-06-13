## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Shiny app to explore the methods in the density maps paper
## Lizzie Pearmain & Ana Carneiro
## June 2019
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

library(shiny)
library(lubridate)

## set working directory
setwd("C:\\Users\\eliza\\OneDrive\\Documents\\BirdLife\\Ana_bycatch\\App_v2")

# rm(list=ls())

## load source functions
source("functions.R")

## Define app UI ----
ui <- navbarPage("Seabird phenology", selected = "Introduction",
  
  ## Link stylesheet and other HTML tags we need ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    # tags$link(href="https://fonts.googleapis.com/css?family=Roboto+Slab&display=swap", rel="stylesheet")
    tags$div(class = "footer", "Disclaimer: this app is not finished")
  ),
  
  
  
  ## TAB 0 - Introduction to the methods ----
  tabPanel("Introduction",
    titlePanel(h1("Introduction to the seabird density maps method")),
    h3("Mapping the global distribution of seabird populations: a framework for integrating biologging, demographic and phenological datasets."),
    tags$div(
      "Steps:", tags$br(),
      tags$ol(tags$li("1. Download data from the", tags$a(href="www.seabirdtracking.org", "Seabird Tracking Database")),
              tags$li("2. Create one csv file per species"),
              tags$li("3. etc."))
    )
  ),
  
  
  ## TAB 1 - PHENOLOGY TABLE 1 ----
  tabPanel("Phenology table 1",
  
    titlePanel(h1("Seabird phenology")),
  ## Sidebar layout with input and output definitions ----
  sidebarLayout(
    ## Sidebar panel for inputs ----
    sidebarPanel(tags$p("This tab uses mean laying date and mean lengths of the incubation, brood-guard and post-brood stages to estimate average start dates of each section of the breeding cycle."), tags$br(),    
      ## Input: Selector for choosing species type (this does nothing yet) ----
      selectInput(inputId = "species_type", label = "Choose whether your species is annual or biennial", choices = c("annual", "biennial")),
      ## Input mean length of pre-laying
      numericInput(inputId = "prelay_length_days", label = "Insert mean pre-laying length in days", value = 0),
      ## Input mean laying date as day-month
      textInput(inputId = "laydate_string", label = "enter mean laying date in format dd-mm", value = "01-01"),
      # Input: numeric input for length of incubation
      numericInput(inputId = "inc_length_days", label = "Insert mean incubation length in days", value = 25),
      # Input: numeric input for length of brood-guard
      numericInput(inputId = "brood_length_days", label = "Insert mean brood-guard length in days", value = 25),
      # Input: numeric input for length of post-brood
      numericInput(inputId = "post_length_days", label = "Insert mean post-guard length in days", value = 25)
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # # Output: Formatted text for caption ----
      # h3(textOutput("caption", container = span)),
      h2("Average phenology timings for your population:"),
      # Output: HTML table with requested number of observations ----
      tableOutput("phenTab1"),
      tags$br(),
      h2("Monthly average phenology for successful breeders:"),
      tableOutput("phenTabBeta")
      )
    )
  ),
  
  tabPanel("Demography model",     
    sidebarLayout(
      sidebarPanel(
        h2("Input demography parameters will go here.")
      ),
      mainPanel(
        h2("Output from demography model will go here - both numbers of birds and figure 1 showing population stratification")
      )
    )
  )  
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  ## Make the first phenTable1 table----
  makePhenTable1 <- reactive({
    phenTable1(input$laydate_string, input$prelay_length_days, input$inc_length_days, 
               input$brood_length_days, input$post_length_days)
  })
  
  ## Make the second phenTableBeta table ----
  makePhenTableBeta <- reactive({
    phenTableBeta(makePhenTable1()) ## need to make this a function??
  })
  
  ## Render the first phenTable1 table ----
  output$phenTab1 <- renderTable({
    makePhenTable1()
  })
  
  ## Render the second phenTableBeta table ----
  output$phenTabBeta <- renderTable({
    makePhenTableBeta()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)