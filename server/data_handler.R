handle_data_processing <- function(input, output, session, rv) {
  observeEvent(input$btn_process, {
    req(rv$raw_data, input$data_structure)
    tryCatch({
      inputs <- reactiveValuesToList(input)

      rv$processed_data <- select_collumn_data(rv$raw_data, input$data_structure, inputs)
      updateSelectInput(session, "patient_id", 
                        choices = unique(rv$processed_data[[input$PID]] %||% "No data"))
      showNotification("Data processed successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
    })
  })
}