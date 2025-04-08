source("server/file_handler.R")
source("server/data_handler.R")
source("server/interpreter.R")

server <- function(input, output, session) {
  rv <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    patient_class = NULL,
    interpreted_data = NULL,
    file_info = list(
      ext = NULL,
      sheets = NULL,
      header = NULL
    )
  )
  
  # Reactive conditions
  output$show_sheet_selector <- reactive({
    rv$file_info$ext == "xlsx"
  })
  output$data_path <- reactive({
    !is.null(input$file_browse$datapath)
  })
  output$data_uploaded <- reactive({
    !is.null(rv$raw_data)
  })
  outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "data_path", suspendWhenHidden = FALSE)
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)
  
  # Call modular server functions
  handle_file_upload(input, output, session, rv)
  handle_data_processing(input, output, session, rv)
  handle_interpretation(input, output, session, rv)
  
  # Summary outputs
  output$summary_records <- renderText({
    req(rv$processed_data)
    paste("Total Records:", nrow(rv$processed_data))
  })
  output$summary_patients <- renderText({
    req(rv$processed_data, input$PID)
    paste("Total Patients:", length(unique(rv$processed_data[[input$PID]])))
  })
  output$summary_samples <- renderText({
    req(rv$processed_data, input$SID)
    paste("Total Samples:", length(unique(rv$processed_data[[input$SID]])))
  })
  output$summary_period <- renderText({
    req(rv$processed_data, input$Sample_Data)
    dates <- as.Date(rv$processed_data[[input$Sample_Data]])
    if (all(is.na(dates)))
      "Period: Not available (invalid date format)"
    else
      paste("Collection Period:",
            min(dates, na.rm = TRUE),
            "to",
            max(dates, na.rm = TRUE))
  })
  
  # Render tables and plots
  output$processed_table <- renderDT({
    req(rv$processed_data)
    datatable(
      rv$processed_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      filter = "top"
    )
  })
  
}