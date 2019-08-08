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

setwd("C:/Users/eliza/OneDrive/Documents/PROJECTS/seabird-phenology") ## CHANGE TO YOUR DIRECTORY

## load source functions
source("functions_phenology.R")

#### writing inputs ----

html_to_insert_flowchart <- paste0('
  <h3>Mapping the global distribution of seabird populations:</h3>
  <h4>a framework for integrating tracking, demographic and phenological datasets</h4>
  <br>
  <h4>More details and R scripts can be found at the <a href="https://github.com/anacarneiro/DensityMaps">GitHub page</a>.</h4>
  <br>  
  <img src="flowchart_v2.png" alt="method framework" height="600" width="800">
  <br>
  <p>References:</p>
  <ul>
    <li>[a] <a href="https://doi.org/10.1111/ddi.12411">Lascelles <em>et al.</em> (2016)</a> Applying global criteria to tracking data to define important areas for marine conservation. <em>Diversity and Distributions.</em></li>
    <li>[b] <a href="https://doi.org/10.1016/j.marpol.2018.08.024">Oppel <em>et al.</em> (2018)</a> Spatial scales of marine conservation management for breeding seabirds. <em>Marine Policy.</em></li>
  </ul>
        ')

html_intro_1 <- paste0('
  <h3>How to use this app</h3>
  <p>
    This app has been developed to help users create monthly phenology tables for each life-history stage in a population from known average phenology timings. These tables are then used to create equations to combine distribution rasters of each breeding stage, in the correct weightings, to produce an average monthly distribution raster for each pool of individuals in the population.
  </p>

  <ol>
    <li>Click <strong>Explore phenology tables</strong> on the left</li>
    <li>Enter the average phenology timings for your population (see <em>inputs</em> below) and explore the resulting breeding cycle timings and monthly tables for each life-history stage</li>
    <li>If you are using the framework, click <strong>Download files</strong> and follow the instructions to download a .csv file of monthly phenology metadata for your population, to use in the R scripts provided (see <strong>R scripts for framework</strong>)</li>
  </ol>
  
                         
  <h3>Inputs:</h3>
  <ul>
    <li>Average egg laying date for your population</li>
    <li>Average duration in days of the different stages of the breeding cycle:
      <ul>
        <li>pre-laying exodus</li>
        <li>incubation</li>
        <li>brood-guard</li>
        <li>post-brood</li>
      </ul>
    </li>
    <li>Breeding cycle type (see box for examples breeding cycles of annual and biennial species).</li>
  </ul>

  <h3>Outputs:</h3>
  <ul>
    <li>Summary phenology table for your population, showing dates and durations of each stage of the breeding cycle</li>
    <li>Monthly phenology tables for each life-history stage of the population. These tables show the average number of days in each month spent in each stage of the breeding cycle, and are used for weighting distribution rasters when creating monthly distributions for each life-history stage.</li>
    <li>Downloadable csv file of full phenology metadata, in the correct format for input into the rest of the R scripts for the analysis framework.</li>
  </ul>
  ')

html_intro_2 <- paste0('
  <p>* In the absence of more detailed data, we assume that fail breeders fail halfway between the start of incubation (egg-laying date) and the end of the post-brood stage (chick-fledging).</p>
  <p>
    <strong>Biennial species: </strong><br>
    If the full breeding cycle lasts longer than one full year, we truncate the end of post-brood before calculating the monthly tables, so that the cycle lasts 365 days only. This simplifies the analysis, enabling the same methods to be used for both annual and biennial species.
  </p>
                       ')

#### construct app ----

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("map-signs")
    ), 
    menuItem("Framework", tabName = "framework", icon = icon("compass")
    ),
    menuItem("Explore phenology tables", tabName = "phenology", icon = icon("calendar-alt")
    ),
    
    menuItem("Download files", tabName = "download", icon = icon("file-download")
    ),
    
    menuItem("Source code for this app", icon = icon("external-link-alt"), href = "https://github.com/lizziepear/seabird-phenology"),
    menuItem("R Scripts for framework", icon = icon("external-link-alt"), href = "https://github.com/anacarneiro/DensityMaps"),
    menuItem("Seabird Tracking Database", icon = icon("external-link-alt"), href = "http://seabirdtracking.org"),
    
    HTML('<a href="http://www.birdlife.org/">
      <img src="BLI_logo.png" alt="BirdLife International" style="width:220px;position:fixed;bottom:0;padding:10px;">
      </a>')
    
  )
)


body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(href="https://fonts.googleapis.com/css?family=Bree+Serif&display=swap", rel="stylesheet")
  ),
  
  tags$script(HTML("$('body').addClass('fixed');")), ## make the sidebar fixed.
  
  tabItems(
    tabItem(tabName = "introduction",
            h2("Mapping the global distribution of seabird populations:"),
            h3("A framework for integrating tracking, demographic and phenological datasets"), tags$br(),
            h4("This app is designed to help users understand the methods for incoporating phenology into seabird density maps, and create their own phenology metadata tables for use in the framework."), tags$br(),
      box(title = "Introduction", status = "primary", width = 6, solidHeader = TRUE, 
          HTML(html_intro_1),
          tableOutput("lifeStagesTable"),
          HTML(html_intro_2)
      ),
      box(title = "Example of annual species breeding cycle", status = "success", width = 6, solidHeader = TRUE, collapsible = TRUE,
          p(strong("Species / population: "), "Black-browed Albatross, Falkland Islands"),
          tableOutput("phenTabBBA"),
          p(strong("Cycle spans one year:")),
          plotOutput("cycleBBA", width = "600px", height = "250px")
      ),
      box(title = "Example of biennial species breeding cycle", status = "success", width = 6, solidHeader = TRUE, collapsible = TRUE,
          p(strong("Species / population: "), "Tristan Albatross, Gough Island"),
          tableOutput("phenTabTRA"),
          p(strong("Cycle spans two years:")),
          plotOutput("cycleTRA", width = "600px", height = "250px")
      )
    ),
    
    tabItem(tabName = "framework",
      h2("Framework"),
      HTML(html_to_insert_flowchart)
    ),
    
    tabItem(tabName = "download",
      h2("Prepare phenology metadata to download"),
      box(title = "Create downloadable data file", status = "primary", width = 6, solidHeader = TRUE, collapsible = FALSE,
          p(tags$strong("Optional: "), "Input species and population information:"),
          textInput(inputId = "Species", label = "Species name"),
          textInput(inputId = "IslandGroup", label = "Population name"),
          tags$br(),
          actionButton(inputId = "goFullMeta", label = "Step 1. Calculate full phenology metadata table", icon = icon("cogs")),
          tags$br(), tags$br(),
          downloadButton(outputId = "phenDownload", label = "Step 2. Download full phenology metadata table as a .csv file", icon = icon("file-download")),
          tags$br(), tags$br(),
          p(tags$strong("Please note:"), " you will need to fill in some fields manually:", tags$ul(tags$li("Device type"), tags$li("Replacement distributions to use (in the case of unavailable tracking data) for both Age and Breed Stage)")))
          ),
      
      box(title = "Full metadata table", status = "success", width = 12, solidHeader = TRUE, collapsible = TRUE,
          tableOutput("fullMeta"))
    ),
    
    tabItem(tabName = "phenology", 
      h2("Phenology timings for your population"),
      box(title = "Inputs", status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,
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
      
      box(title = "Breeding cycle", status = "primary", 
        plotOutput("breedCycle", width = "600px", height = "250px")),
      
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
  
  ################# Life-history stages table #############################
  output$lifeStagesTable <- renderTable({
    makeLifeStagesTable()
  })
  
  ################# Example breeding cycles for first page #############################
  ### Biennial example: Tristan Albatross, Gough island
  output$phenTabTRA <- renderTable({
    prettyPhenTable1(phenTable1Succ("16-12", 25, 75, 31, 259, "biennial"))
  })
  output$cycleTRA <- renderPlot({
    plotCycle(phenTable1Succ("16-12", 25, 75, 31, 259, "biennial"), "biennial")
  })
  # Annual example: Black-browed Albatross, Falkland Islands
  output$phenTabBBA <- renderTable({
    prettyPhenTable1(phenTable1Succ("10-10", 22, 68, 20, 94, "annual"))
  })
  output$cycleBBA <- renderPlot({
    plotCycle(phenTable1Succ("10-10", 22, 68, 20, 94, "annual"), "annual")
  })
  
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
    # findFailDate(makePhenTable1Succ()) ## this does change based on species_type
    findFailDate(phenTable1Succ(input$laydate_string, input$prelay_length_days, input$inc_length_days, input$brood_length_days, input$post_length_days, "biennial")) ## find fail date always based on biennial
  })
  ## Render Fail Date as text:
  output$failDate <- renderText({
    makeFailDate()
  })
  
  #### Make tables for FAIL BREEDERS ----
  ## Make the first phenTable1 table
  makePhenTable1Fail <- reactive({
    req(input$laydate_string, input$prelay_length_days, input$inc_length_days, input$brood_length_days, input$post_length_days)
    phenTable1Fail(input$laydate_string, input$prelay_length_days, input$inc_length_days, input$brood_length_days, input$post_length_days) ## Fail table does not change based on species_type
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
  
  
  ################## Make full metadata table ---- ########################
  makeFullMeta <- eventReactive(input$goFullMeta, {
    
    ## create a progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing metadata", value = 0)
    ## close the progress when the reactive exits, even if there's an error:
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    phenMetaFull(input$laydate_string, input$prelay_length_days, 
                 input$inc_length_days, input$brood_length_days, input$post_length_days, 
                 input$Species, input$IslandGroup, updateProgress)
  })
  ## Render the full metadata phenology table ----
  output$fullMeta <- renderTable({
    makeFullMeta()
  })
  
  ################## Download full metadata table ---- ####################
  output$phenDownload <- downloadHandler(
    filename = function() {
      paste("metadata_monthly_equations", ".csv", sep="")
    },
    content = function(file) {
      write.csv(makeFullMeta(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui,server)
