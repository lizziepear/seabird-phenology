## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Shiny app to explore the methods in the density maps paper
## Copyright (C) 2019 Lizzie Pearmain & Ana Carneiro
## This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
## This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
## You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

library(shiny)
library(shinydashboard)
library(lubridate)

# setwd("C:/Users/Lizzie/OneDrive/Documents/PROJECTS/shiny-dashboard")
setwd("C:/Users/eliza/OneDrive/Documents/PROJECTS/seabird-phenology")

## load source functions
source("functions_phenology.R")
source("functions_demography.R")


#### writing inputs ----

html_to_insert_flowchart <- paste0('
          <img src="flowchart_v2.png" alt="method framework" height="600" width="800">
          <br>
          <p>References:</p>
          <ul>
          <li>[a] Lascelles et al. (2016)</li>
          <li>[b] Oppel et al. (2018)</li>
          </ul>
        ')

html_framework <- paste0('
  <h2>Mapping the global distribution of seabird populations: a framework for integrating tracking, demographic and phenological datasets</h2>
  <p>This app is designed to help users understand the methods for incoporating phenology into seabird density maps, and create their own phenology metadata tables for including in the framework presented.
  </p>
  <p>[MORE INFORMATION ON PHENOLOGY METHODS HERE
	<ul><li>Example of an annual species breeding cycle, plus diagram</li><li>Example of a biennial species plus diagram</li></ul>
  </p>
  <br>
  <p>Steps in the framework:
	  <ol>
		  <li>Download data from the Seabird Tracking Database</li>
		  <li>Create one .csv file per species</li>
		  <li>Follow the instructions and run the R files (in the corresponding order):</li>
		    <ul>
			    <li>01_demography</li>
			    <li>02_cleaning_data</li>
			    <li>03_kernels</li>
		    </ul>
		  <li>Do a bootstrap analysis - following instructions in [1] and [2]</li>
		  <li>Follow the instructions and run the R files (in the corresponding order):</li>
		    <ul>
			    <li>04_combining_selecting_renaming</li>
			    <li>05_land_mask</li>
		    </ul>
		  <li>Create metadata based on phenology</li>
	  </ol>
  </p>
  ')

#### construct app ----

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("map-signs")
    ), # 
    menuItem("Framework", tabName = "framework", icon = icon("compass")
    ),
    menuItem("Explore phenology tables", tabName = "phenology", icon = icon("calendar-alt")#,
      # menuSubItem("Explore phenology", tabName = "phen_inputs", icon = icon("calendar-alt"))
    ), # 
    
    menuItem("R Scripts for framework", icon = icon("external-link-alt"), href = "https://github.com/anacarneiro/DensityMaps"),
    menuItem("Seabird Tracking Database", icon = icon("external-link-alt"), href = "http://seabirdtracking.org")#,
    
  )
)


body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(href="https://fonts.googleapis.com/css?family=Bree+Serif&display=swap", rel="stylesheet")
  ),
  
  tags$script(HTML("$('body').addClass('fixed');")),
  # tags$head(tags$style(".sidebar-menu li { margin-bottom: 100px; }")),
  # tags$img(align="right", src="logo.png", height=30),
  
  tabItems(
    tabItem(tabName = "introduction",
      h2("Introduction"),
      HTML(html_framework)
    ),
    
    tabItem(tabName = "framework",
      h2("Framework"),
      HTML(html_to_insert_flowchart)
    ),
    
    # tabItem(tabName = "phenology",
    #   h2("phenology info here")
    # ),
    
    tabItem(tabName = "phenology", 
      h2("Phenology timings for your population"),
      box(title = "Inputs", color = "light-blue", width = 4, solidHeader = TRUE, collapsible = TRUE,
        ## Input: Selector for choosing species type ----
        radioButtons(inputId = "species_type", label = "Species breeding cycle type", choices = c("annual", "biennial"), selected="annual"),
        ## Input mean length of pre-laying
        numericInput(inputId = "prelay_length_days", label = "Mean pre-laying length (days)", value = 0, min = 0, max = 50),
        ## Input mean laying date as day-month
        textInput(inputId = "laydate_string", label = "Mean laying date in format dd-mm", placeholder = "dd-mm"), #value = "01-01",
        # Input: numeric input for length of incubation
        numericInput(inputId = "inc_length_days", label = "Mean incubation length (days)", value = 50, min = 0, max = 100),
        # Input: numeric input for length of brood-guard
        numericInput(inputId = "brood_length_days", label = "Mean brood-guard length (days)", value = 25, min = 0, max = 100),
        # Input: numeric input for length of post-brood
        numericInput(inputId = "post_length_days", label = "Mean post-guard length (days)", value = 80, min = 0, max = 300)
      ),
      tabBox(
        title = "Average phenology timings", id = "tabset1", selected = "Successful breeders", width = 6, side = "right",
        tabPanel("Fail breeders", tableOutput("phenTab1Fail"),
                 span(style="display:inline;", "Average fail date: "), span(style="display:inline;", textOutput("failDate"))),
        tabPanel("Successful breeders", tableOutput("phenTab1Succ"))
        
      ),
      
    box(title = "Breeding cycle", plotOutput("breedCycle", width = "600px", height = "250px")),
      # box(title = tagList(shiny::icon("gear"), "Calculate monthly phenology tables"), 
      #     status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,
      #     ## Action button to trigger calculation
      #     p("Successful breeders:"),
      #     # p("Adult sucessful breeders:") ## TODO: add options to calculate and display the timings for each pool of birds - as tabset isn't working.
      #     
      #     tags$br(),
      #     p("Fail breeders:"),
      #     # p("Adult sucessful breeders:") ## TODO: add options to calculate and display the timings for each pool of birds - as tabset isn't working.
      #     
      #     tags$br(),
      #     p("Adult sabbaticals, immatures and juveniles:"),
      #     actionButton("goAll", "Go")
      # ),
      box(title = "Monthly phenology for successful breeders", 
          status = "warning", width = 12, solidHeader = T, collapsible = TRUE,
          actionButton("goSB", tagList(shiny::icon("gear"), "Calculate")),
          tableOutput("phenTabBeta")
      ),
      box(title = "Monthly phenology for fail breeders",
          status = "warning", width = 12, solidHeader = T, collapsible = TRUE,
          actionButton("goFB", tagList(shiny::icon("gear"), "Calculate")),
          tableOutput("phenTabGamma")
      ),
      box(title = "Monthly phenology for sabbaticals (non-breeders)",
          status = "danger", width = 12, solidHeader = T, collapsible = TRUE,
          actionButton("goSab", tagList(shiny::icon("gear"), "Calculate")),
          tableOutput("phenTabDelta")
      ),
      box(title = "Monthly phenology for Immatures",
          status = "success", width = 6, solidHeader = T, collapsible = TRUE,
          actionButton("goImm", tagList(shiny::icon("gear"), "Calculate")),
          tableOutput("phenTabTheta")
      ),
      box(title = "Monthly phenology for Juveniles",
          status = "info", width = 6, solidHeader = T, collapsible = TRUE,
          actionButton("goJuv", tagList(shiny::icon("gear"), "Calculate")),
          tableOutput("phenTabZeta")
      )
    )#,
  )
)

## make the dashboard page
ui <- dashboardPage(
  dashboardHeader(title = "Seabird phenology", titleWidth = 250),
  sidebar,
  body
  
)

server <- function(input, output) {
  
  ################## Summary phenology table ---- #######################################
  
  ############ success ################
  
  ## Make the first phenTable1 table
  makePhenTable1Succ <- reactive({
    req(input$laydate_string, input$prelay_length_days, input$inc_length_days, input$brood_length_days, input$post_length_days, input$species_type)
    phenTable1Succ(input$laydate_string, input$prelay_length_days, input$inc_length_days, input$brood_length_days, input$post_length_days, input$species_type)
  })
  ## Make the pretty version of phenTable1
  makePrettyPhenTable1Succ <- reactive({
    prettyPhenTable1(makePhenTable1Succ())
  })
  ## Render the first phenTable1 table for SUCC ----
  output$phenTab1Succ <- renderTable({
    makePrettyPhenTable1Succ()
  })
  
  #### make plot of cycle ####
  makePlotCycle <- reactive({
    plotCycle(makePhenTable1Succ(), input$species_type)
  })
  ### render plot ####
  output$breedCycle <- renderPlot({
    makePlotCycle()
  })
  
  ############## fail #################
  #### Make output showing fail date as a text output ----
  makeFailDate <- reactive({
    req(input$laydate_string, input$inc_length_days, input$brood_length_days, input$post_length_days)
    findFailDate(makePhenTable1Succ())
  })
  ## Render Fail Date as text:
  output$failDate <- renderText({
    makeFailDate()
  })
  
  #### Make tables for FAIL BREEDERS ----
  ## Make the first phenTable1 table
  makePhenTable1Fail <- reactive({
    req(input$laydate_string, input$prelay_length_days, input$inc_length_days, input$brood_length_days, input$post_length_days)
    phenTable1Fail(input$laydate_string, input$prelay_length_days, input$inc_length_days, input$brood_length_days, input$post_length_days)
  })
  ## Make the pretty version of phenTable1
  makePrettyPhenTable1Fail <- reactive({
    prettyPhenTable1(makePhenTable1Fail())
  })
  ## Render the first phenTable1 table for FAIL ----
  output$phenTab1Fail <- renderTable({
    makePrettyPhenTable1Fail()
  })
  
  ################## Monthly phenology table for Successful breeders ---- #######################################
  ## Make phenTable1SuccANNUAL - for feeding into phenTableBreed function (breeding cycle cut to one year) ----
  makePhenTable1SuccAnnual <- reactive({
    phenTable1Succ(input$laydate_string, input$prelay_length_days, input$inc_length_days, input$brood_length_days, input$post_length_days, "annual")
  })
  ## Make the monthly table for SUCC - phenTableBeta table ----
  ## TODO: Change this so that it uses the ANNUAL version of PhenTable1Succ ONLY - not the biennial version.
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
    
    phenTableBreed(makePhenTable1SuccAnnual(), updateProgress) ## changed to call makePhenTable1SuccANNUAL instead of the normal version
  })
  ## Render the second phenTableBeta table FOR SUCC ----
  output$phenTabBeta <- renderTable({
    makePhenTableBeta()
  })
  
  ################## Monthly phenology table for Fail breeders ---- ############################################
  ## Make the monthly table for FAIL - phenTableGamma table ----
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
    
    phenTableBreed(makePhenTable1Fail(), updateProgress)
  })
  ## Render the second phenTableBeta table FOR FAIL ----
  output$phenTabGamma <- renderTable({
    makePhenTableGamma()
  })
  
  ################## Monthly phenology table for Sabbaticals, Immatures and Juveniles ---- ########################
  ## make the full monthly table for SABBS
  makePhenTableDelta <- eventReactive(input$goSab, {
    phenTableDelta()
  })
  ## render the table for SABBS
  output$phenTabDelta <- renderTable({
    makePhenTableDelta()
  })
  
  ## make the full monthly table for IMM
  makePhenTableTheta <- eventReactive(input$goImm, {
    phenTableTheta()
  })
  ## render the table for IMM
  output$phenTabTheta <- renderTable({
    makePhenTableTheta()
  })
  #### Adult non-breeders, immatures, and juveniles ----
  ## make the full monthly table for JUV
  makePhenTableZeta <- eventReactive(input$goJuv, {
    phenTableZeta()
  })
  ## render the table for JUV
  output$phenTabZeta <- renderTable({
    makePhenTableZeta()
  })
  
}

shinyApp(ui,server)
