# app.R
# Load Libraries
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)
library(dplyr)
library(readxl)
library(AMR)
library(tools)

# Source Helper Files
source("utils.R")
source("ui_components.R")

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML("
    .shiny-file-input-progress { display: none !important; }
    .shiny-input-container { margin-bottom: 15px; }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      file_upload_ui(),
      data_config_ui()
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Overview & Configure", icon = icon("table"),
                 materialSwitch("view_type", "View Individual Patients", FALSE, width = "100%"),
                 DTOutput("overview_data")),
        tabPanel("AST Interpretation", icon = icon("microscope"), DTOutput("ast_table")),
        tabPanel("AMR Patterns", icon = icon("chart-bar"), plotOutput("amr_plot")),
        tabPanel("Multidrug Resistance", icon = icon("prescription-bottle"), textOutput("mdr_summary"))
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Reactive Values
  rv <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    file_info = list(ext = NULL, sheets = NULL, header = NULL)
  )
  
  # Reactive Conditions
  output$show_sheet_selector <- reactive({ rv$file_info$ext == "xlsx" })
  output$data_uploaded <- reactive({ !is.null(rv$raw_data) })
  
  # Set output options individually
  outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)
  
  # File Upload Handler
  observeEvent(input$file_browse, {
    req(input$file_browse)
    tryCatch({
      ext <- file_ext(input$file_browse$name)
      if (!ext %in% c("csv", "xlsx")) stop("Please select CSV or Excel file.")
      rv$file_info$ext <- ext
      if (ext == "xlsx") {
        sheets <- excel_sheets(input$file_browse$datapath)
        rv$file_info$sheets <- sheets
        updateSelectInput(session, "sheet_name", choices = sheets, selected = sheets[1])
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      reset("file_browse")
    })
  })
  
  # Data Loading Handler
  observeEvent(input$btn_upload, {
    req(input$file_browse, rv$file_info$ext)
    tryCatch({
      df <- read_file(input$file_browse$datapath, rv$file_info$ext, input$sheet_name)
      rv$raw_data <- df
      rv$header <- colnames(df)
      
      update_selectors <- function(id, patterns) {
        updateSelectInput(session, id, choices = rv$header,
                          selected = detect_column(rv$header, patterns))
      }
      update_selectors("PID", c("PID", "Patient ID"))
      update_selectors("SID", c("SID", "Sample ID"))
      update_selectors("Sample_Data", c("Sample Date", "Sampling Date"))
      update_selectors("Sample_Type", c("Sample Type", "Type Sample"))
      update_selectors("Pathogen", c("Pathogen", "Organism", "Bacteria"))
      update_selectors("AB_type_0", c(as.character(antibiotics$ab), as.character(antibiotics$name)))
      update_selectors("AB_type_1", c("Antibiotic", "AB"))
      update_selectors("interpretation", c("Interpretation", "SIR", "SIR Result"))
      update_selectors("MIC", c("MIC", "Minimum Inhibitory Concentration"))
      update_selectors("Zone_Size", c("Zone Size", "Inhibition Zone"))
      update_selectors("Methods", c("Method", "Test Method"))
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 10)
    })
  })
  
  # Data Processing Handler
  observeEvent(input$btn_process, {
    req(rv$raw_data, input$data_structure)
    tryCatch({
      inputs <- reactiveValuesToList(input)
      rv$processed_data <- process_ast_data(rv$raw_data, input$data_structure, inputs)
      showNotification("Data processed successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
    })
  })
  
  # Output Renderers
  output$overview_data <- renderDT({
    req(rv$raw_data)
    df <- if (input$view_type && !is.null(input$PID)) {
      rv$raw_data %>% filter(!is.na(.data[[input$PID]])) %>% select(input$PID, everything())
    } else {
      rv$raw_data
    }
    datatable(df, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE), filter = "top")
  })
  
  output$ast_table <- renderDT({
    req(rv$processed_data)
    datatable(rv$processed_data, options = list(scrollX = TRUE))
  })
  
  output$amr_plot <- renderPlot({
    req(rv$processed_data, input$AB_type_1)
    ggplot(rv$processed_data, aes(x = .data[[input$AB_type_1]], 
                                  fill = if (!is.null(input$interpretation)) .data[[input$interpretation]])) +
      geom_bar() +
      theme_minimal() +
      labs(title = "Antimicrobial Resistance Patterns", x = "Antibiotic", y = "Count")
  })
  
  output$mdr_summary <- renderText({
    req(rv$processed_data)
    "Multidrug resistance analysis coming soon!"
  })
}

# Run the App
shinyApp(ui, server)