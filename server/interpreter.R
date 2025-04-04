handle_interpretation <- function(input, output, session, rv) {
  observeEvent(input$btn_interpret, {
    req(rv$processed_data)
    tryCatch({
      data_to_interpret <- rv$processed_data
      if (input$data_structure == "1") {
        rv$interpreted_data <- data_to_interpret %>%
          mutate(Interpretation = suppressWarnings(
            as.sir(
              .data[[input$MIC]],
              mo = .data[[input$Pathogen]],
              ab = .data[[input$AB_type_1]],
              guideline = input$interpretation_mode
            )
          ))
      } else {
        rv$interpreted_data <- data_to_interpret %>%
          mutate(across(all_of(input$AB_type_0), ~ suppressWarnings(
            as.sir(
              .,
              mo = .data[[input$Pathogen]],
              ab = antibiotics$ab[match(., antibiotics$name)],
              guideline = input$interpretation_mode
            )
          )))
      }
      showNotification("Interpretation completed!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error during interpretation:", e$message), type = "error")
    })
  })
}