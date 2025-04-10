server <- function(input, output, session) {
  
  # Initialize reactiveValues to store states
  rv <- reactiveValues(
    file_info = list(name = NULL, ext = NULL, sheets = NULL, datapath = NULL, ready_for_upload = FALSE),
    uploaded_data = NULL,
    column_names = NULL,
    show_mapping = FALSE,
    processed_data = NULL # Stores data AFTER processing
  )
  
  # --- A. File Input Handling ---
  observeEvent(input$file_browse, {
    req(input$file_browse)
    # Reset states when a new file is selected
    rv$uploaded_data <- NULL
    rv$column_names <- NULL
    rv$show_mapping <- FALSE
    rv$processed_data <- NULL
    rv$file_info <- list(name = NULL, ext = NULL, sheets = NULL, datapath = NULL, ready_for_upload = FALSE) # Full reset
    fileinfo <- input$file_browse
    path <- fileinfo$datapath
    ext <- tolower(tools::file_ext(fileinfo$name))
    validation_error <- tryCatch({
      if (!ext %in% c("csv", "xlsx")) stop("Invalid file type. Please upload CSV or XLSX.")
      if (file.size(path) > 10 * 1024 * 1024) stop("File size exceeds 10MB limit.")
      rv$file_info$name <- fileinfo$name
      rv$file_info$ext <- ext
      rv$file_info$datapath <- path
      if (ext == "xlsx") {
        sheets <- readxl::excel_sheets(path)
        if (is.null(sheets) || length(sheets) == 0) stop("Could not read sheets from Excel file.")
        rv$file_info$sheets <- sheets
        updateSelectInput(session, "sheet_name", choices = sheets, selected = sheets[1])
        rv$file_info$ready_for_upload <- TRUE # Ready once sheet selected
      } else {
        rv$file_info$sheets <- NULL
        updateSelectInput(session, "sheet_name", choices = character(0), selected = character(0))
        rv$file_info$ready_for_upload <- TRUE # CSV ready immediately
      }
      NULL # Success
    }, error = function(e) { e$message }) # Failure
    
    if (!is.null(validation_error)) {
      showNotification(paste("File Validation Error:", validation_error), type = "error", duration = 10)
      shinyjs::reset("file_browse") # Reset file input using shinyjs
      rv$file_info <- list(name = NULL, ext = NULL, sheets = NULL, datapath = NULL, ready_for_upload = FALSE) # Ensure reset
    }
  }) # End observeEvent file_browse
  
  # --- B. Conditional UI Logic ---
  output$show_sheet_selector <- reactive({ !is.null(rv$file_info$ext) && rv$file_info$ext == "xlsx" })
  outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)
  output$show_upload_button <- reactive({ rv$file_info$ready_for_upload })
  outputOptions(output, "show_upload_button", suspendWhenHidden = FALSE)
  output$show_mapping_ui <- reactive({ rv$show_mapping })
  outputOptions(output, "show_mapping_ui", suspendWhenHidden = FALSE)
  
  # --- C. Data Uploading ---
  observeEvent(input$btn_upload, {
    req(rv$file_info$datapath)
    ext <- rv$file_info$ext
    path <- rv$file_info$datapath
    sheet <- if(ext == "xlsx") input$sheet_name else NULL
    rv$uploaded_data <- NULL # Reset before reading
    rv$column_names <- NULL
    rv$show_mapping <- FALSE
    rv$processed_data <- NULL
    progress <- Progress$new(session, min=0, max=1); progress$set(message = "Reading data...", value = 0.5); on.exit(progress$close())
    tryCatch({
      if (ext == "xlsx") { req(sheet); df_read <- readxl::read_excel(path, sheet = sheet) }
      else { df_read <- utils::read.csv(path, stringsAsFactors = FALSE, header = TRUE, check.names = FALSE) }
      if (is.null(df_read) || ncol(df_read) == 0) stop("Failed to read data or data is empty.")
      rv$uploaded_data <- dplyr::mutate_all(df_read, as.character) # Read all as character initially
      rv$column_names <- colnames(rv$uploaded_data)
      rv$show_mapping <- TRUE
      showNotification(paste0("Successfully uploaded '", rv$file_info$name, if(!is.null(sheet)) paste(" (Sheet:", sheet, ")") else "", "'."), type = "message")
    }, error = function(e) {
      showNotification(paste("Error Reading File:", e$message), type = "error", duration = 10)
      rv$uploaded_data <- NULL; rv$column_names <- NULL; rv$show_mapping <- FALSE # Ensure reset on error
    })
  }) # End observeEvent btn_upload
  
  # --- D. Update Picker Inputs when Data is Uploaded ---
  observeEvent(rv$column_names, {
    cols <- rv$column_names; req(cols)
    # Define potential matches (adjust these lists as needed)
    pid_matches <- c("PID", "Patient ID", "Study ID", "patient_id", "subject_id")
    sid_matches <- c("SID", "Sample ID", "sample_id", "specimen_id")
    date_matches <- c("Sample Date", "Date Sample", "Sampling Date", "Date Sampling", "sample_date", "collection_date")
    type_matches <- c("Sample Type", "Type Sample", "sample_type", "specimen_type", "source")
    pathogen_matches <- c("Pathogen", "Bacteria", "Bacteria Name", "Name Pathogen", "organism")
    ab_name_matches <- c("AB", "Antibiotics", "Name Antibiotics", "antibiotic", "drug", "agent")
    mic_matches <- c("MIC", "Minimum Inhibitory Concentration", "mic_value")
    zone_matches <- c("Zone Size", "Disk", "disk_diameter", "zone_diameter")
    interp_matches <- c("Interpretation", "SIR", "RIS", "breakpoint_result")
    method_matches <- c("Methods", "Test Methods", "method", "ast_method")
    ab_cols_matches <-  unique(c(antimicrobials$name, antimicrobials$ab))
    
    # Update pickers, using detect_column if available, else NULL
    updatePickerInput(session, "PID", choices = cols, selected = detect_column(cols, pid_matches))
    updatePickerInput(session, "SID", choices = cols, selected = detect_column(cols, sid_matches))
    updatePickerInput(session, "Sample_Date", choices = cols, selected = detect_column(cols, date_matches))
    updatePickerInput(session, "Sample_Type", choices = cols, selected = detect_column(cols, type_matches))
    updatePickerInput(session, "Pathogen", choices = cols, selected = detect_column(cols, pathogen_matches))
    updateVirtualSelect(session, "AB_cols", choices = cols, selected = detect_column(cols, ab_cols_matches))
    updatePickerInput(session, "AB_name", choices = cols, selected = detect_column(cols, ab_name_matches))
    updatePickerInput(session, "MIC", choices = cols, selected = detect_column(cols, mic_matches))
    updatePickerInput(session, "Zone_Size", choices = cols, selected = detect_column(cols, zone_matches))
    updatePickerInput(session, "Interpretation", choices = cols, selected = detect_column(cols, interp_matches))
    updatePickerInput(session, "Methods", choices = cols, selected = detect_column(cols, method_matches))
  })
  
  # --- E. Data Preview Table ---
  output$data_preview_table <- DT::renderDataTable({
    validate(need(rv$uploaded_data, "Upload data using the sidebar to see preview."))
    datatable( rv$uploaded_data, options = list(pageLength = 5, scrollX = TRUE, searching = FALSE, lengthChange = FALSE, autoWidth = TRUE), rownames = FALSE, filter = 'top')
  })
  
  # --- F. Dashboard Metrics (React to Processed Data) ---
  output$total_records <- renderText({
    # Show total records from raw upload immediately
    df <- rv$uploaded_data; if (is.null(df)) "0" else scales::comma(nrow(df)) })
  
  output$total_patients <- renderText({
    df <- rv$processed_data; validate(need(df, "Data not processed yet.")) # Require processed data
    validate(need("PatientID" %in% names(df), "Patient ID column not found in processed data."))
    scales::comma(n_distinct(df$PatientID, na.rm = TRUE)) })
  
  output$total_samples <- renderText({
    df <- rv$processed_data; validate(need(df, "Data not processed yet.")) # Require processed data
    validate(need("SampleID" %in% names(df), "Sample ID column not found in processed data."))
    scales::comma(n_distinct(df$SampleID, na.rm = TRUE)) })
  
  
  # --- G. Process Data Button ---
  observeEvent(input$btn_process, {
    validate(need(rv$uploaded_data, "Please upload data first."))
    # --- Input Validation ---
    validate(
      need(input$PID != "", "Mapping Error: Please select the Patient ID column."),
      need(input$SID != "", "Mapping Error: Please select the Sample ID column."),
      need(input$Sample_Date != "", "Mapping Error: Please select the Sampling Date column."),
      need(input$Pathogen != "", "Mapping Error: Please select the Pathogen column.")
      # Add more essential validations here...
    )
    if(input$data_structure == 'wide'){
      validate(need(length(input$AB_cols) > 0, "Mapping Error: Please select at least one Antibiotic Result column for wide format."))
    } else { # long format
      validate(need(input$AB_name != "", "Mapping Error: Please select the Antibiotic Name column for long format."))
      # Validate that at least one result column is selected if required
      validate(need(input$MIC != "" || input$Zone_Size != "" || input$Interpretation != "",
                    "Mapping Error: Please select at least MIC, Zone Size, or Interpretation column for long format."))
    }
    
    # --- Processing ---
    showNotification("Processing data based on mappings...", id="processing_msg", type="message", duration = NULL) # Keep until removed
    progress <- Progress$new(session, min=0, max=4); progress$set(message = "Starting...", value = 0); on.exit(progress$close())
    rv$processed_data <- NULL # Reset processed data before trying
    
    tryCatch({
      progress$set(value = 1, message = "Mapping columns...")
      
      # --- 1. Select & Rename Core Columns ---
      # Create a list of selected input columns to map to standard names
      # Handle optional columns by checking if input$X is non-empty
      core_map <- list( PatientID = input$PID, SampleID = input$SID, SampleDate = input$Sample_Date, SampleType = input$Sample_Type, Pathogen = input$Pathogen )
      core_map <- core_map[sapply(core_map, function(x) !is.null(x) && x != "")] # Remove empty selections
      
      if(length(core_map) == 0) stop("Core column mapping resulted in empty selection.")
      
      df_processed <- rv$uploaded_data %>% select(all_of(unname(unlist(core_map))))
      colnames(df_processed) <- names(core_map)[match(colnames(df_processed), unname(unlist(core_map)))]
      
      
      progress$set(value = 2, message = "Processing AST data...")
      
      # --- 2. Handle Wide vs Long AST Data ---
      if(input$data_structure == 'wide'){
        # WIDE to LONG transformation
        validate(need(length(input$AB_cols) > 0, "No antibiotic columns selected for wide format."))
        # You will likely need pivot_longer here from tidyr (add library(tidyr))
        # Example assumes result columns contain interpretation (S/I/R)
        # You might need more complex logic if they contain MICs or Zones
        df_processed <- df_processed %>%
          tidyr::pivot_longer(
            cols = all_of(input$AB_cols),
            names_to = "AntibioticName", # Placeholder standard name
            values_to = "Interpretation", # Placeholder standard name
            values_drop_na = TRUE # Optional: drop rows where AB result is NA
          )
        # Further steps: Standardize AntibioticName using AMR package's as.rsi(), etc.
        
      } else { # LONG format
        long_map <- list( AntibioticName = input$AB_name, MIC = input$MIC, Zone = input$Zone_Size, Interpretation = input$Interpretation, Method = input$Methods )
        long_map <- long_map[sapply(long_map, function(x) !is.null(x) && x != "")] # Remove empty optional selections
        
        if(length(long_map) == 0) stop("No AST result columns (Antibiotic, MIC, Zone, Interpretation) selected for long format.")
        
        # Select the chosen AST columns from original data
        df_ast_part <- rv$uploaded_data %>% select(all_of(unname(unlist(long_map))))
        colnames(df_ast_part) <- names(long_map)[match(colnames(df_ast_part), unname(unlist(long_map)))]
        
        # Combine with core columns (ensure row counts match - use cbind cautiously if row order is guaranteed, otherwise merge/join)
        # Assuming row order matches between core and ast parts selected from rv$uploaded_data
        df_processed <- cbind(df_processed, df_ast_part)
        # Further steps: Standardize AntibioticName, potentially calculate Interpretation from MIC/Zone using selected guideline (input$guideline)
      }
      
      
      progress$set(value = 3, message = "Cleaning data...")
      # --- 3. Data Type Conversion & Cleaning ---
      # Example: Convert date column (needs specific format guessing or user input)
      # Use tryFormats or specific format strings
      date_formats_to_try <- c("%Y-%m-%d", "%d/%m/%Y", "%d-%b-%y", "%Y/%m/%d") # Add expected formats
      df_processed <- df_processed %>%
        mutate(SampleDate = as.Date(lubridate::parse_date_time(SampleDate, orders = date_formats_to_try))) # Use lubridate for robust parsing
      
      # Example: Convert MIC to numeric (handle non-numeric like ">64", "<=0.5")
      # if("MIC" %in% names(df_processed)) {
      #    df_processed <- df_processed %>% mutate(MIC = AMR::as.mic_value(MIC))
      # }
      
      # Add other cleaning steps... (e.g., standardize pathogen names, sample types)
      
      
      # --- 4. Store Processed Data ---
      rv$processed_data <- df_processed
      progress$set(value = 4, message = "Processing Complete!")
      removeNotification("processing_msg") # Remove processing message
      showNotification("Data processed successfully! Dashboard updated.", type="message")
      
    }, error = function(e){
      removeNotification("processing_msg")
      showNotification(paste("Error during processing:", e$message), type = "error", duration = 10)
      rv$processed_data <- NULL # Ensure reset on error
    })
  }) # End observeEvent btn_process
  
  # --- H. Dashboard Plots (Placeholder - React to Processed Data) ---
  # output$dashboard_plot1 <- renderPlot({ req(rv$processed_data); ggplot(...) })
  # output$dashboard_plot2 <- renderPlot({ req(rv$processed_data); ggplot(...) })
  
} # End Server