handle_data_processing <- function(input, output, session, rv) {
  observeEvent(input$btn_process, {
    req(rv$raw_data, input$data_structure)
    tryCatch({
      inputs <- reactiveValuesToList(input)
      required_cols <- if (input$data_structure == "0") {
        c("PID", "SID", "Sample_Data", "Sample_Type", "Pathogen", "AB_type_0")
      } else {
        c("PID", "SID", "Sample_Data", "Sample_Type", "Pathogen", "AB_type_1", "MIC")
      }
      
      missing_cols <- setdiff(required_cols, names(inputs))
      if (length(missing_cols) > 0) {
        stop(paste("Missing required selections:", paste(missing_cols, collapse = ", ")))
      }
      
      rv$processed_data <- select_collumn_data(rv$raw_data, input$data_structure, inputs)
      updateSelectInput(session, "patient_id", 
                        choices = unique(rv$processed_data[[input$PID]] %||% "No data"))
      showNotification("Data processed successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
    })
  })
}