handle_file_upload <- function(input, output, session, rv) {
  # Define column mappings for auto-detection
  column_mappings <- list(
    PID = c("pid", "patient id"),
    SID = c("sid", "sample id"),
    Sample_Data = c("sample date", "sampling date"),
    Sample_Type = c("sample type", "type sample"),
    Pathogen = c("pathogen", "organism", "bacteria"),
    AB_type_0 = c(as.character(antibiotics$ab), as.character(antibiotics$name)),
    AB_type_1 = c("antibiotic", "ab"),
    interpretation = c("interpretation", "sir", "sir result"),
    MIC = c("mic", "minimum inhibitory concentration"),
    Zone_Size = c("zone size", "inhibition zone"),
    Methods = c("method", "test method")
  )
  
  # Helper function to update picker inputs
  update_picker <- function(id, headers) {
    updatePickerInput(
      session,
      id,
      choices = c("None",headers),
      selected = detect_column(headers, column_mappings[[id]])
    )
  }
  
  # Handle file selection (Browse...)
  observeEvent(input$file_browse, {
    req(input$file_browse)
    tryCatch({
      ext <- tolower(file_ext(input$file_browse$name))
      if (!ext %in% c("csv", "xlsx")) stop("Please select a CSV or Excel file.")
      if (file.size(input$file_browse$datapath) > 10e6) stop("File size exceeds 10MB limit.")
      
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
  
  # Handle file upload (Upload Data button)
  observeEvent(input$btn_upload, {
    req(input$file_browse, rv$file_info$ext)
    tryCatch({
      df <- read_file(input$file_browse$datapath, rv$file_info$ext, input$sheet_name)
      if (nrow(df) == 0) stop("Uploaded file is empty.")
      
      rv$raw_data <- df
      rv$header <- colnames(df)
      
      # Update all picker inputs using the helper function
      lapply(names(column_mappings), update_picker, headers = rv$header)
      
      showNotification("Data uploaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 10)
    })
  })
}