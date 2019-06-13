library(shiny)


setwd("C:\\Users\\eliza\\OneDrive\\Documents\\BirdLife\\Ana_bycatch\\App_v1")

# rm(list=ls())

source("functions.R")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(href="https://fonts.googleapis.com/css?family=Roboto+Slab&display=swap", rel="stylesheet")
  ),
  
  # App title ----
  titlePanel(h1("Seabird phenology")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # # Input: Text for providing a caption ----
      # # Note: Changes made to the caption in the textInput control
      # # are updated in the output area immediately as you type
      # textInput(inputId = "caption",
      #           label = "Caption:",
      #           value = "Data Summary"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "species_type",
                  label = "Choose whether your species is annual or biennial",
                  choices = c("annual", "biennial")),
      
      # Input: calendar entry for mena laying date ----
      # numericInput(inputId = "laydate",
      #              label = "Insert mean laying date",
      #              value = 10),
      # dateInput(inputId = "laydate",
      #           label = "insert mean laying date (month and day only)",
      #           format = "mm-dd",
      #           value = "2000-01-01",
      #           min = "2000-01-01",
      #           max = "2000-12-31"),
      textInput(inputId = "laydate_string", 
                label = "enter mean laying date in format dd-mm", 
                value = "01-01"
                ),
      
      # Input: numeric input for length of incubation
      numericInput(inputId = "inc_length_days",
                   label = "Insert mean incubation length in days",
                   value = 25
                   ),
      
      # Input: numeric input for length of brood-guard
      numericInput(inputId = "brood_length_days",
                   label = "Insert mean brood-guard length in days",
                   value = 25
                   ),
      
      # Input: numeric input for length of post-brood
      numericInput(inputId = "post_length_days",
                   label = "Insert mean post-guard length in days",
                   value = 25
                   )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # # Output: Verbatim text for data summary ----
      # verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  # datasetInput <- reactive({
  #   switch(input$dataset,
  #          "rock" = rock,
  #          "pressure" = pressure,
  #          "cars" = cars)
  # })
  
  phenologyTable <- reactive({
    phenTable1(input$laydate_string, input$inc_length_days, input$brood_length_days,
               input$post_length_days)
  })
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  # output$caption <- renderText({
  #   paste("average laying date is", input$laydate_string)
  # })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  
  output$view <- renderTable({
    phenologyTable()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)