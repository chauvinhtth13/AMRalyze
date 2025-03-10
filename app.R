library(shiny)
library(shinyWidgets)
library(shinyjs)
library(gridlayout)
library(bslib)
library(DT)
source("ui_function/ui_function.R")


library(ggplot2)
library(dplyr)
library(readxl)
library(AMR)

ui <- fluidPage(
  useShinyjs(),  # Sử dụng shinyjs để quản lý UI tốt hơn
  # Tắt thông báo upload
  tags$style(HTML("
    .shiny-file-input-progress {
      display: none !important;
    }
    .shiny-input-container {
      margin-bottom: 15px;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      div(
        fileInput(
          inputId = "file_browse",
          # Đã sửa tên inputId
          label = "Upload CSV/XLSX File",
          accept = c(".csv", ".xlsx"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        conditionalPanel(
          condition = "output.show_sheet_selector",
          selectInput(
            "sheet_name",
            "Select Sheet",
            choices = NULL,
            multiple = FALSE
          )
        ),
        actionButton(# Sử dụng button đẹp hơn từ shinyWidgets
          "btn_upload", "Upload Data", ),
        tags$hr()
      ), 
      div(
        conditionalPanel(
          condition = "output.data_uploaded",
          selectInput("PID", "Patient ID Collumn",choices = NULL),
          selectInput("SID", "Sample ID Collumn",choices = NULL),
          selectInput("Sample_Data", "Sampling Date Collumn",choices = NULL),
          selectInput("Sample_Type", "Sample Type Collumn",choices = NULL),
          selectInput("Pathogen", "Pathogen Collumn",choices = NULL),
          radioButtons("data_structure", "What structure does the AST data belong to?", 
                       choiceNames = list("The data has multiple antibiotic columns and each column contains MIC and Interpretation values",
                                          "The data has separate antibiotic name columns and MIC/Zone Size data columns (Recommended)"),
                       choiceValues = list(0,1),
                       selected = 1),
          conditionalPanel(
            condition = "input.data_structure == 0",
            selectInput("AB_type_0", "Antibiotics Collumn",choices = NULL, multiple = T)
          ),
          conditionalPanel(
            condition = "input.data_structure == 1",
            selectInput("AB_type_1", "Antibiotics Collumn",choices = NULL),
            selectInput("interpretation", "Interpretation Collumn (optional)",choices = NULL),
            selectInput("MIC", "MIC Collumn",choices = NULL),
            selectInput("Zone_Size", "Zone Size Collumn",choices = NULL),
            selectInput("Methods", "Test",choices = NULL),
          ),
          actionButton(
            "btn_process", "Process Data", ),
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          "Overview & Configure",
          icon = icon("table"),
          
          materialSwitch(inputId = "view_type", value = FALSE, 
                      label = "View Invidiual Patients",
                      width = "100%"),
          
          
          DTOutput("overview_data") 
        ),
        tabPanel(
          "AST Interpretation",
          icon = icon("microscope"),
          "Content for AST Interpretation"
        ),
        tabPanel(
          "Summary Antimicrobial Resistance Patterns",
          icon = icon("chart-bar"),
          "Content for AMR Patterns"
        ),
        tabPanel(
          "Summary Multidrug Resistance",
          icon = icon("prescription-bottle"),
          "Content for Multidrug Resistance"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Sử dụng reactiveValues để quản lý state
  rv <- reactiveValues(
    raw_data = NULL,
    file_info = list(
      ext = NULL,
      sheets = NULL,
      header = NULL
    )
  )
  
  # Hiển thị sheet selector cho file Excel
  output$show_sheet_selector <- reactive({
    req(rv$file_info$ext == "xlsx")
  })
  outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)
  
  # Xử lý khi upload file
  observeEvent(input$file_browse, {
    tryCatch({
      req(input$file_browse)
      
      # Validate file type
      ext <- tools::file_ext(input$file_browse$name)
      if (!ext %in% c("csv", "xlsx")) {
        stop("Invalid file type. Please select CSV or Excel file.")
      }
      
      # Cập nhật thông tin file
      rv$file_info$ext <- ext
      
      # Xử lý file Excel
      if (ext == "xlsx") {
        sheets <- excel_sheets(input$file_browse$datapath)
        rv$file_info$sheets <- sheets
        updateSelectInput(
          session, 
          "sheet_name",
          choices = sheets,
          selected = sheets[1]
        )
      }
    }, error = function(e) {
      showNotification(
        paste("Error:", e$message),
        type = "error",
        duration = 10
      )
      reset("file_browse")
    })
  })
  
  # Xử lý khi nhấn nút Upload
  observeEvent(input$btn_upload, {
    tryCatch({
      req(input$file_browse, rv$file_info$ext)
      
      # Đọc dữ liệu
      df <- if (rv$file_info$ext == "csv") {
        read.csv(
          input$file_browse$datapath,
          stringsAsFactors = FALSE,
          encoding = "UTF-8"
        )
      } else {
        req(input$sheet_name)
        read_excel(
          input$file_browse$datapath,
          sheet = input$sheet_name
        )
      }
      
      rv$raw_data <- df
      rv$header <- colnames(df)
      updateSelectInput(session,
                        "PID",
                        choices = rv$header,
                        selected = detect_column(rv$header, c("PID", "Patient ID"))[1])
      updateSelectInput(session,
                        "SID",
                        choices = rv$header,
                        selected = detect_column(rv$header, c("SID", "Sample ID"))[1])
      updateSelectInput(
        session,
        "Sample_Data",
        choices = rv$header,
        selected = detect_column(rv$header, c("Sample Date", "Sampling Date"))[1]
      )
      updateSelectInput(
        session,
        "Sample_Type",
        choices = rv$header,
        selected = detect_column(rv$header, c("Sample Type", "Type Sample"))[1]
      )
      updateSelectInput(
        session,
        "Pathogen",
        choices = rv$header,
        selected = detect_column(rv$header, c("Pathogen", "Organism", "Bacteria"))[1]
      )
      updateSelectInput(
        session,
        "AB_type_0",
        choices = rv$header,
        selected = detect_column(rv$header, c(
          as.character(antibiotics$ab),
          as.character(antibiotics$name)
        ))
      )
      updateSelectInput(session,
                        "AB_type_1",
                        choices = colnames(df),
                        selected = detect_column(colnames(df), c("Antibiotic", "AB"))[1])
      updateSelectInput(
        session,
        "interpretation",
        choices = colnames(df),
        selected = detect_column(colnames(df), c("Interpretation", "SIR", "SIR Result"))[1]
      )
      updateSelectInput(session,
                        "MIC",
                        choices = colnames(df),
                        selected = detect_column(colnames(df), c("MIC", "Minimum Inhibitory Concentration"))[1])
      updateSelectInput(session,
                        "Zone_Size",
                        choices = colnames(df),
                        selected = detect_column(colnames(df), c("Zone Size", "Inhibition Zone"))[1])
      updateSelectInput(session,
                        "Methods",
                        choices = colnames(df),
                        selected = detect_column(colnames(df), c("Method", "Test Method"))[1])
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  
  output$data_uploaded <- reactive({
    !is.null(rv$raw_data) # TRUE khi đã có dữ liệu
  })
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)
  
  # Hiển thị bảng dữ liệu
  output$overview_data <- renderDT({
    req(rv$raw_data)
  })
}

shinyApp(ui, server)