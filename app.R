##### 0. Load Required Libraries and Scripts #####

# Core Shiny framework for building interactive applications
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)

# Theming and icons support
library(bslib)
library(bsicons)

# Data manipulation and utilities (tidyverse, data.table, etc.)
library(tidyverse)    # dplyr, tidyr, stringr, purrr, readr, etc.
library(magrittr)     # Pipe operators
library(data.table)   # High-performance data manipulation
library(reshape2)     # Data reshaping
library(scales)       # Formatting scales and axes

# Antimicrobial resistance (AMR) analysis tools
library(AMR)

# File input/output utilities
library(readxl)       # Read Excel files
library(openxlsx)     # Write Excel files
library(tools)        # File extension helpers

# Data presentation and summary tables
library(DT)           # Interactive data tables
library(gt)           # HTML table generation
library(gtsummary)    # Publication-quality summary tables

# Interactive plotting
library(plotly)       # Plotly for interactive charts

# Source user-defined utility functions and UI modules
source("utils/ui_utils.R",     local = TRUE)
source("utils/amr_utils.R",    local = TRUE)
source("ui/tab_dashboard.R",   local = TRUE)
source("ui/tab_data_input.R",  local = TRUE)
source("ui/tab_about.R",       local = TRUE)

##### 1. UI/UX Definition #####
ui <- page_navbar(
  ###### 1.1 Setting UI/UX #####
  title = "AMRalyze Dashboard",
  theme = bs_theme(bootswatch = "cerulean"),
  header = tags$head(
    useShinyjs(),
    includeCSS("www/styles.css"),
    tags$script(src = "www/features.js"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
  ),
  
  ##### 1.2. Tab Dashboard #####
  tab_dashboard(),
  
  ##### 1.3. Tab Data Input & Preview #####
  tab_data_input(),
  
  ##### 1.4. Tab About #####
  tab_about()
)


##### 2. Back-end Definition #####
server <- function(input, output, session) {
  
  ##### 2.1.1. Initialize reactiveValues #####
  rv <- reactiveValues(
    file_info = list(
      name = NULL,
      ext = NULL,
      sheets = NULL,
      datapath = NULL,
      ready_for_upload = FALSE
    ),
    uploaded_data = NULL,
    column_names = NULL,
    show_mapping = FALSE,
    processed_data = NULL,
    mdr_data = NULL
  )
  
  ##### 2.1.2. File Input Handling #####
  observeEvent(input$file_browse, {
    req(input$file_browse)
    rv$uploaded_data <- NULL
    rv$column_names <- NULL
    rv$show_mapping <- FALSE
    rv$processed_data <- NULL
    rv$file_info <- list(
      name = NULL,
      ext = NULL,
      sheets = NULL,
      datapath = NULL,
      ready_for_upload = FALSE
    )
    fileinfo <- input$file_browse
    path <- fileinfo$datapath
    ext <- tolower(tools::file_ext(fileinfo$name))
    validation_error <- tryCatch({
      if (!ext %in% c("csv", "xlsx"))
        stop("Invalid file type. Please upload CSV or XLSX.")
      if (file.size(path) > 10 * 1024 * 1024)
        stop("File size exceeds 10MB limit.")
      rv$file_info$name <- fileinfo$name
      rv$file_info$ext <- ext
      rv$file_info$datapath <- path
      if (ext == "xlsx") {
        sheets <- readxl::excel_sheets(path)
        if (is.null(sheets) ||
            length(sheets) == 0)
          stop("Could not read sheets from Excel file.")
        rv$file_info$sheets <- sheets
        updateSelectInput(session,
                          "sheet_name",
                          choices = sheets,
                          selected = sheets[1])
        rv$file_info$ready_for_upload <- TRUE
      } else {
        rv$file_info$sheets <- NULL
        updateSelectInput(session, "sheet_name", choices = character(0))
        rv$file_info$ready_for_upload <- TRUE
      }
      NULL
    }, error = function(e) {
      e$message
    })
    if (!is.null(validation_error)) {
      showNotification(
        paste("File Validation Error:", validation_error),
        type = "error",
        duration = 10
      )
      shinyjs::reset("file_browse")
      rv$file_info <- list(
        name = NULL,
        ext = NULL,
        sheets = NULL,
        datapath = NULL,
        ready_for_upload = FALSE
      )
    }
  })
  
  ##### 2.1.3. Conditional UI Logic #####
  output$show_sheet_selector <- reactive({
    !is.null(rv$file_info$ext) && rv$file_info$ext == "xlsx"
  })
  outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)
  
  output$show_upload_button <- reactive({
    rv$file_info$ready_for_upload
  })
  outputOptions(output, "show_upload_button", suspendWhenHidden = FALSE)
  
  output$show_mapping_ui <- reactive({
    rv$show_mapping
  })
  outputOptions(output, "show_mapping_ui", suspendWhenHidden = FALSE)
  
  output$show_patient_ui <- reactive({
    !is.null(rv$processed_data) && nrow(rv$processed_data) > 0
  })
  outputOptions(output, "show_patient_ui", suspendWhenHidden = FALSE)
  
  ##### 2.1.4. Data Uploading ######
  observeEvent(input$btn_upload, {
    req(rv$file_info$datapath)
    ext <- rv$file_info$ext
    path <- rv$file_info$datapath
    sheet <- if (ext == "xlsx")
      input$sheet_name
    else
      NULL
    rv$uploaded_data <- NULL
    rv$column_names <- NULL
    rv$show_mapping <- FALSE
    rv$processed_data <- NULL
    tryCatch({
      if (ext == "xlsx") {
        req(sheet)
        df_read <- readxl::read_excel(path, sheet = sheet)
      }
      else {
        df_read <- data.table::fread(
          path,
          stringsAsFactors = FALSE,
          header = TRUE,
          check.names = FALSE
        )
        df_read <- as.data.frame(df_read)
      }
      if (is.null(df_read) ||
          ncol(df_read) == 0)
        stop("Failed to read data or data is empty.")
      
      rv$uploaded_data <- dplyr::mutate_all(df_read, as.character)
      rv$column_names <- colnames(rv$uploaded_data)
      rv$show_mapping <- TRUE
      showNotification(
        paste0(
          "Successfully uploaded '",
          rv$file_info$name,
          if (!is.null(sheet))
            paste(" (Sheet:", sheet, ")")
          else
            "",
          "'."
        ),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error Reading File:", e$message),
                       type = "error",
                       duration = 10)
      rv$uploaded_data <- NULL
      rv$column_names <- NULL
      rv$show_mapping <- FALSE
    })
  })
  
  ##### 2.1.5. Update Picker Inputs ######
  observeEvent(rv$show_mapping, {
    # Trigger when mapping UI is shown
    cols <- rv$column_names
    req(cols)
    
    # Define potential matches (customize these)
    pid_matches <- c("PID", "Patient ID", "Study ID", "patient_id", "subject_id")
    sid_matches <- c("SID", "Sample ID", "sample_id", "specimen_id")
    date_matches <- c(
      "Sample Date",
      "Date Sample",
      "Sampling Date",
      "Date Sampling",
      "sample_date",
      "collection_date",
      "sampling_date",
      "date_sampling"
    )
    type_matches <- c("Sample Type",
                      "Type Sample",
                      "sample_type",
                      "specimen_type",
                      "source")
    pathogen_matches <- c("Pathogen",
                          "Bacteria",
                          "Bacteria Name",
                          "Name Pathogen",
                          "organism")
    ab_name_matches <- c("AB",
                         "Antibiotics",
                         "Name Antibiotics",
                         "antibiotic",
                         "drug",
                         "agent")
    mic_matches <- c("MIC", "Minimum Inhibitory Concentration", "mic_value")
    zone_matches <- c("Zone Size", "Disk", "disk_diameter", "zone_diameter")
    interp_matches <- c("Interpretation", "SIR", "RIS", "breakpoint_result")
    method_matches <- c("Methods", "Test Methods", "method", "ast_method")
    ab_cols_matches <- unique(c(antimicrobials$name, antimicrobials$ab))
    
    #Update inputs using detect_column if available, otherwise NULL selected
    updateVirtualSelect(
      session = session,
      inputId = "PID",
      choices = cols,
      selected = detect_column(cols, pid_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "SID",
      choices = cols,
      selected = detect_column(cols, sid_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "Sample_Date",
      choices = cols,
      selected = detect_column(cols, date_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "Sample_Type",
      choices = cols,
      selected = detect_column(cols, type_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "Pathogen",
      choices = cols,
      selected = detect_column(cols, pathogen_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "AB_cols",
      choices = cols,
      selected = detect_column(cols, ab_cols_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "AB_name",
      choices = cols,
      selected = detect_column(cols, ab_name_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "MIC",
      choices = cols,
      selected = detect_column(cols, mic_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "Zone_Size",
      choices = cols,
      selected = detect_column(cols, zone_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "Interpretation",
      choices = cols,
      selected = detect_column(cols, interp_matches)
    )
    updateVirtualSelect(
      session = session,
      inputId = "Methods",
      choices = cols,
      selected = detect_column(cols, method_matches)
    )
  })
  
  ##### 2.1.6 Process Data Button #####
  observeEvent(input$btn_process, {
    rv$processed_data <- NULL
    rv$mdr_data <- NULL
    
    # Use shinybusy for progress indication
    shiny::withProgress(message = 'Processing Data',
                        value = 0,
                        style = "notification",
                        {
                          tryCatch({
                            incProgress(0.1, detail = "Validating inputs...")
                            validate(
                              need(input$PID != "", "Mapping Error: Select Patient ID."),
                              need(input$SID != "", "Mapping Error: Select Sample ID."),
                              need(input$Pathogen != "", "Mapping Error: Select Pathogen.")
                            )
                            if (input$data_structure == 'wide') {
                              validate(need(
                                length(input$AB_cols) > 0,
                                "Mapping Error: Select Antibiotic columns for wide format."
                              ))
                            }
                            else {
                              validate(
                                need(
                                  input$AB_name != "",
                                  "Mapping Error: Select Antibiotic Name column."
                                ),
                                need(
                                  input$MIC != "" ||
                                    input$Zone_Size != "",
                                  "Mapping Error: Select MIC or Zone Size column."
                                )
                              )
                            }
                            
                            incProgress(0.1, detail = "Preparing data columns...")
                            
                            core_map <- list(
                              PatientID = input$PID,
                              SampleID = input$SID,
                              SampleDate = input$Sample_Date,
                              SampleType = input$Sample_Type,
                              Pathogen = input$Pathogen
                            )
                            core_map <- core_map[sapply(core_map, function(x)
                              ! is.null(x) && x != "")]
                            
                            if (input$data_structure == 'wide') {
                              df_processed <- rv$uploaded_data %>% select(all_of(unname(unlist(core_map))), all_of(input$AB_cols)) %>%
                                mutate_at(all_of(input$AB_cols), as.character) %>%
                                tidyr::pivot_longer(
                                  cols = all_of(input$AB_cols),
                                  names_to = "Antibiotic_Name",
                                  values_to = "Result",
                                  values_drop_na = TRUE
                                )
                              
                              colnames(df_processed) <- c(na.omit(names(core_map)[match(colnames(df_processed), unname(unlist(core_map)))]), "Antibiotic_Name", "Result")
                              
                              check_whonet_format <- df_processed %>% mutate(
                                Method_init = str_split_i(Antibiotic_Name, "_|-", 2),
                                Method = case_when(
                                  grepl("\\bND", Method_init) ~ "Disk",
                                  grepl("\\bNE", Method_init) ~ "E-Test",
                                  grepl("\\bNM", Method_init) ~ "MIC"
                                )
                              )
                              if (sum(grepl(
                                "\\bDisk\\b|\\bE-Test|\\bMIC",
                                unique(check_whonet_format$Method)
                              )) == length(unique(check_whonet_format$Method)))
                              {
                                df_processed <- check_whonet_format %>%  mutate(
                                  Antibiotic_Name_Temp = str_split_i(Antibiotic_Name, "_|-", 1),
                                  Antibiotic_Name = Antibiotic_Name_Temp,
                                  TempInterpretation = as.sir(str_extract(Result, "R|S|I|SSD|NI")),
                                  Value = str_extract(Result, "(<=|>=|<|>)?\\s*[0-9.]+"),
                                  MIC = if_else(Method %in% c("E-Test", "MIC"), Value, ""),
                                  
                                  Zone = if_else(Method == "Disk", Value, "")
                                ) %>% select(-c(Result, Antibiotic_Name_Temp, Value, Method_init))
                              } else {
                                df_processed <- check_whonet_format %>%
                                  mutate(
                                    MIC = str_extract(Result, "(<=|>=|<|>)?\\s*[0-9.]+"),
                                    Zone = "",
                                    TempInterpretation = as.sir(str_extract(Result, "R|S|I|SSD|NI")),
                                    Method = ""
                                  ) %>%
                                  select(-c(Result, Method_init))
                              }
                              
                            } else {
                              # long format
                              long_map <- list(
                                Antibiotic_Name = input$AB_name,
                                MIC = input$MIC,
                                Zone = input$Zone_Size,
                                TempInterpretation = input$Interpretation,
                                Method = input$Methods
                              )
                              long_map <- long_map[sapply(long_map, function(x)
                                ! is.null(x) && x != "")]
                              
                              df_processed <- rv$uploaded_data %>% select(all_of(unname(unlist(core_map))), all_of(unname(unlist(long_map))))
                              colnames(df_processed) <- c(na.omit(names(core_map)[match(colnames(df_processed), unname(unlist(core_map)))]), na.omit(names(long_map)[match(colnames(df_ast_part), unname(unlist(long_map)))]))
                            }
                            
                            incProgress(0.1, detail = "Standardizing (AST)...")
                            
                            if (!"TempInterpretation" %in% names(df_processed)) {
                              df_processed$TempInterpretation <- as.sir("")
                            }
                            if (!"Zone" %in% names(df_processed)) {
                              df_processed$Zone <- as.disk("")
                            }
                            if (!"MIC" %in% names(df_processed)) {
                              df_processed$MIC <- as.mic("")
                            }
                            
                            df_processed <- df_processed %>%
                              mutate(
                                mo_code = as.mo(Pathogen),
                                kingdom = mo_kingdom(mo_code),
                                gram_stain = mo_gramstain(mo_code),
                                ab_code = as.ab(Antibiotic_Name),
                                AntibioticName = ab_name(ab_code),
                                MIC = as.mic(MIC),
                                Zone = as.disk(Zone),
                                Interpretation = case_when(
                                  !is.na(MIC) & is.na(TempInterpretation) ~ as.sir(
                                    MIC,
                                    mo = mo_code,
                                    ab = ab_code,
                                    guideline = input$guideline
                                  ),!is.na(Zone) &
                                    is.na(TempInterpretation) ~ as.sir(
                                      Zone,
                                      mo = mo_code,
                                      ab = ab_code,
                                      guideline = input$guideline
                                    ),
                                  !is.na(TempInterpretation) ~ as.sir(TempInterpretation)
                                )
                              ) %>% select(-c(TempInterpretation, Antibiotic_Name))
                            
                            # --- Data Type Conversion & Cleaning ---
                            if ("SampleDate" %in% names(df_processed)) {
                              datetime_formats_to_try <- c(
                                "%Y-%m-%d %H:%M:%S",
                                "%Y-%m-%d %H:%M",
                                "Ymd HMS",
                                "Ymd HM",
                                "%d/%m/%Y %H:%M:%S",
                                "%d/%m/%Y %H:%M",
                                "%d-%m-%Y %H:%M:%S",
                                "%d-%m-%Y %H:%M",
                                "dmY HMS",
                                "dmY HM",
                                "%m/%d/%Y %I:%M:%S %p",
                                "%m/%d/%Y %I:%M %p",
                                "mdY IMSp",
                                "mdY IMp",
                                "%d-%b-%Y %H:%M:%S",
                                "%d-%b-%Y %H:%M",
                                "%b %d %Y %H:%M:%S",
                                "%b %d %Y %H:%M",
                                "%Y-%m-%d",
                                "%d/%m/%Y",
                                "%m/%d/%Y",
                                "%d-%b-%y",
                                "%Y/%m/%d",
                                "%b %d %Y",
                                "%d-%b-%Y",
                                "Ymd",
                                "dmY",
                                "mdY",
                                "dbY",
                                "Ybd",
                                "%Y%m%d%H%M%S"
                              )
                              
                              df_processed <- df_processed %>% mutate(
                                SampleDate_chr = as.character(SampleDate),
                                Sample_Date_Parsed = lubridate::parse_date_time(
                                  SampleDate_chr,
                                  orders = datetime_formats_to_try,
                                  quiet = TRUE
                                ),
                                SampleDate = if_else(
                                  is.na(Sample_Date_Parsed) &
                                    grepl("^[0-9]+(\\.[0-9]+)?$", SampleDate_chr),
                                  as.POSIXct(as.numeric(SampleDate_chr), origin = "1900-01-01"),
                                  Sample_Date_Parsed
                                )
                              ) %>%
                                select(-c(SampleDate_chr, Sample_Date_Parsed))
                            }
                            
                            incProgress(0.3, detail = "Calculating MDR status...")
                            mdr_mic_data <- df_processed %>% filter(!is.na(MIC)) %>%
                              tidyr::pivot_wider(
                                id_cols = c(
                                  PatientID,
                                  SampleID,
                                  Pathogen,
                                  mo_code,
                                  kingdom,
                                  gram_stain
                                ),
                                names_from = ab_code,
                                values_from = Interpretation
                              )
                            
                            mdr_zone_data <- df_processed %>% filter(!is.na(Zone) |
                                                                       (is.na(Zone) & is.na(MIC))) %>%
                              tidyr::pivot_wider(
                                id_cols = c(
                                  PatientID,
                                  SampleID,
                                  Pathogen,
                                  mo_code,
                                  kingdom,
                                  gram_stain
                                ),
                                names_from = ab_code,
                                values_from = Interpretation
                              )
                            
                            mic_only_cols <- setdiff(names(mdr_mic_data), names(mdr_zone_data))
                            zone_only_cols <- setdiff(names(mdr_zone_data), names(mdr_mic_data))
                            
                            
                            common_cols <- intersect(names(mdr_mic_data), names(mdr_zone_data))
                            ab_common_cols <- setdiff(
                              common_cols,
                              c(
                                "PatientID",
                                "SampleID",
                                "Pathogen",
                                "mo_code",
                                "kingdom",
                                "gram_stain"
                              )
                            )
                            
                            mdr_ab_common_data <- full_join(
                              mdr_mic_data,
                              mdr_zone_data,
                              by = c(
                                "PatientID",
                                "SampleID",
                                "Pathogen",
                                "mo_code",
                                "kingdom",
                                "gram_stain"
                              ),
                              suffix = c(".mic", ".zone")
                            )
                            
                            for (col in ab_common_cols)
                            {
                              col_mic <- paste0(col, ".mic")
                              col_zone <- paste0(col, ".zone")
                              mdr_ab_common_data <- mdr_ab_common_data %>%
                                mutate(!!col := coalesce(!!sym(col_mic), !!sym(col_zone)))
                            }
                            
                            rv$mdr_data <- mdr_ab_common_data %>%
                              select(c(
                                all_of(common_cols),
                                all_of(mic_only_cols),
                                all_of(zone_only_cols),
                              )) %>%
                              mutate(
                                MDR = mdro(
                                  guideline = "CMI2012",
                                  pct_required_classes = 0.5
                                ),
                                MDR = if_else(is.na(MDR), "Not Determined", MDR)
                              )
                            
                            # --- Store Processed Data ---
                            incProgress(0.4, detail = "Finalizing...")
                            rv$processed_data <- df_processed
                            showNotification("Data processed successfully! Dashboard updated.",
                                             type = "message")
                            
                          }, error = function(e) {
                            removeNotification("processing_msg")
                            showNotification(
                              paste("Error during processing:", e$message),
                              type = "error",
                              duration = 10
                            )
                            stop(e)
                            rv$processed_data <- NULL
                          })
                        })
  })
  
  
  ##### 2.1.7 Show Patient Data #####
  # Reactive expression for available Patient IDs after processing
  patient_ids <- reactive({
    req(rv$processed_data)
    sort(unique(rv$processed_data$PatientID))
  })
  # Reactive expression for available Sample IDs based on selected Patient ID
  sample_ids <- reactive({
    req(rv$processed_data, input$PID_show)
    rv$processed_data %>%
      filter(PatientID == input$PID_show) %>%
      pull(SampleID) %>%
      unique() %>%
      sort()
  })
  # Reactive expression for available Pathogens based on selected Patient and Sample ID
  pathogen_names <- reactive({
    req(rv$processed_data, input$PID_show, input$SID_show)
    rv$processed_data %>%
      filter(PatientID == input$PID_show, SampleID == input$SID_show) %>%
      pull(Pathogen) %>% # Use the standardized Pathogen name
      unique() %>%
      sort()
  })
  
  # Update filter choices reactively
  observe({
    updateVirtualSelect(
      session = session,
      inputId = "PID_show",
      choices = patient_ids(),
      selected = patient_ids()[1]
    )
  })
  observe({
    updateVirtualSelect(
      session = session,
      inputId = "SID_show",
      choices = sample_ids(),
      selected = sample_ids()[1]
    )
  })
  observe({
    updateVirtualSelect(
      session = session,
      inputId = "Pathogen_show",
      choices = pathogen_names(),
      selected = pathogen_names()[1]
    )
  })
  
  filtered_review_data <- reactive({
    req(rv$processed_data,
        input$PID_show,
        input$SID_show,
        input$Pathogen_show)
    rv$processed_data %>%
      filter(
        PatientID == input$PID_show &
          SampleID == input$SID_show &
          Pathogen == input$Pathogen_show
      )
  })
  
  mdr_review_data <- reactive({
    req(rv$mdr_data,
        input$PID_show,
        input$SID_show,
        input$Pathogen_show)
    rv$mdr_data %>%
      filter(
        PatientID == input$PID_show &
          SampleID == input$SID_show &
          Pathogen == input$Pathogen_show
      )
  })
  
  output$SampleDate_text <- renderText({
    data_sub <- filtered_review_data()
    validate(need(nrow(data_sub) > 0, "No data for current filter selection."))
    
    if ("SampleDate" %in% names(data_sub)) {
      unique_dates <- unique(data_sub$SampleDate)
      paste("Date Sample Collected:", unique_dates)
    } else {
      "Date Sample Collected: Not Available"
    }
  })
  
  output$SampleType_text <- renderText({
    data_sub <- filtered_review_data()
    validate(need(nrow(data_sub) > 0, "")) # Don't show message if date shown
    # Check if SampleType column exists after processing
    if ("SampleType" %in% names(data_sub)) {
      unique_types <- unique(data_sub$SampleType)
      paste("Sample Type:", unique_types)
    } else {
      "Sample Type: Not Available"
    }
  })
  
  output$Kingdom_text <- renderText({
    data_sub <- filtered_review_data()
    validate(need(nrow(data_sub) > 0, ""))
    unique_types <- unique(data_sub$kingdom)
    paste("Kingdom:", unique_types)
  })
  
  output$GramStain_text <- renderText({
    data_sub <- filtered_review_data()
    validate(need(nrow(data_sub) > 0, ""))
    unique_types <- unique(data_sub$gram_stain)
    paste("Gram stain:", unique_types)
  })
  
  output$MDR_text <- renderText({
    data_sub <- mdr_review_data()
    validate(need(nrow(data_sub) > 0, ""))
    unique_types <- unique(data_sub$MDR)
    paste("MDR Status (CMI2012):", unique_types)
  })
  
  output$AST_data_view_table <- DT::renderDataTable({
    data_sub <- filtered_review_data()
    data_sub <- data_sub %>% select(c(ab_code, AntibioticName, MIC, Zone, Interpretation, Method)) %>%
      mutate(ab_code = as.character(ab_code)) %>% filter(!(is.na(MIC) &
                                                             is.na(Zone) & is.na(Interpretation)))
    names(data_sub) <- c(
      "Antibiotic Code",
      "Antibiotic Name",
      "MIC (mg/L)",
      "Zone Size (mm)",
      "Interpretation",
      "Method"
    )
    datatable(
      data_sub,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        searching = TRUE,
        lengthChange = FALSE,
        autoWidth = FALSE
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  
  
  ##### 2.1.8 Download Processed Data #####
  output$download_processed_data <- downloadHandler(
    filename = function() {
      paste0("processed__all_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rv$processed_data, rv$mdr_data) # Need processed data
      
      data_processed_to_download <- rv$processed_data
      data_mdr_to_download <- rv$mdr_data
      
      wb <- createWorkbook()
      addWorksheet(wb, "Processed Data")
      addWorksheet(wb, "MDR Data")
      
      writeData(wb, 1, data_processed_to_download)
      writeData(wb, 2, data_mdr_to_download)
      saveWorkbook(wb, file)
    }
  )
  
  ##### 2.2. Server Logic For Dashboard #####
  
  ###### 2.2.1 Dashboard Metrics  ######
  output$total_records <- renderText({
    df <- rv$uploaded_data
    if (is.null(df))
      "0"
    else
      scales::comma(nrow(df))
  })
  output$total_patients <- renderText({
    df <- rv$processed_data
    if (is.null(df))
      "0"
    else
      scales::comma(n_distinct(df$PatientID, na.rm = TRUE))
  })
  output$total_samples <- renderText({
    df <- rv$processed_data
    if (is.null(df))
      "0"
    else
      scales::comma(n_distinct(df$SampleID, na.rm = TRUE))
  })
  
  ###### 2.1.7. Main Content Area Preview ######
  
  
  output$pathogen_summary_plot <- renderPlotly({
    # Require processed data
    validate(
      need(
        rv$processed_data,
        "Please upload and process data first ('Input' tab)."
      ),
      need(nrow(rv$processed_data) > 0, "Processed data is empty.")
    )
    
    pathogen_freq <- rv$processed_data %>%
      dplyr::distinct(PatientID, SampleID, Pathogen) %>% # Count each pathogen once per sample
      dplyr::count(Pathogen, sort = TRUE, name = "Frequency")
    
    # Check if calculation resulted in data
    validate(need(nrow(pathogen_freq) > 0, "No pathogen data to summarize."))
    
    # --- Optional: Group less frequent pathogens into 'Other' ---
    top_n_pathogens <- 15 # Show top N pathogens separately
    if (nrow(pathogen_freq) > top_n_pathogens) {
      pathogen_freq <- pathogen_freq %>%
        dplyr::mutate(
          Pathogen_Group = dplyr::case_when(
            dplyr::row_number() <= top_n_pathogens ~ Pathogen,
            TRUE ~ "Other (< Min Frequency)" # Group the rest
          )
        ) %>%
        dplyr::group_by(Pathogen_Group) %>%
        dplyr::summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
        dplyr::rename(Pathogen = Pathogen_Group) # Rename for plotting
    }
    
    # Create the plot using ggplot2
    gg <- ggplot(pathogen_freq, aes(x = reorder(Pathogen, Frequency), y = Frequency)) +
      geom_col(fill = "#2c7fb8", width = 0.7) + # Use a color for bars
      coord_flip() + # Flip coordinates for better readability of labels
      labs(title = "Frequency of Identified Pathogens", x = "Pathogen", y = "Number of Samples") +
      theme_minimal(base_size = 12) + # Use a clean theme
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        # Center title
        panel.grid.major.y = element_blank(),
        # Remove horizontal grid lines
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        # Adjust text size if needed
        axis.text.x = element_text(size = 10)
      ) +
      # Add frequency labels to the bars
      geom_text(
        aes(label = scales::comma(Frequency), y = Frequency * 1.08),
        hjust = -0.2,
        size = 5,
        color = "black"
      )
    
    # Return the plot
    ggplotly(gg)
    
  })
  
  observe({
    req(rv$mdr_data)
    list_gram_negative <- rv$mdr_data %>%
      dplyr::distinct(PatientID, SampleID, Pathogen, kingdom, gram_stain) %>%
      dplyr::filter(kingdom == "Bacteria" &
                      gram_stain == "Gram-negative") %>%
      dplyr::count(Pathogen, sort = TRUE, name = "Frequency") %>%
      pull(Pathogen)
    
    selected_choice <- if (length(list_gram_negative) >= 4) {
      # Use the value from the original vector for selection logic
      list_gram_negative[1:4]
    } else {
      NULL
    }
    
    updateVirtualSelect(
      session = session,
      inputId = "list_gram_negative",
      choices = list_gram_negative,
      selected = selected_choice
    )
  })
  
  
  
  output$ast_gram_negative_table <- render_gt({
    validate(need(
      rv$uploaded_data,
      "Please upload and process data first ('Input' tab)."
    ))
    validate(need(
      length(input$list_gram_negative) > 0,
      "Choose Gram-negative Pathogen."
    ))
    req(rv$mdr_data)
    data_sub <- rv$mdr_data %>%
      filter(Pathogen %in% input$list_gram_negative) %>%
      select(-c(PatientID, SampleID, mo_code, kingdom, gram_stain)) %>%
      select(where(~ !all(is.na(.)))) %>%
      rename_if(is.sir, ab_name) %>%
      mutate_if(
        is.sir,
        .funs = function(x)
          if_else(x == "R", TRUE, FALSE)
      )
    list_ab <- setdiff(names(data_sub), c("Pathogen", "MDR"))
    
    
    gt_table <- data_sub %>%
      tbl_summary(
        by = Pathogen,
        statistic = everything() ~ "{p}% ({n}/{N})",
        missing = "no",
        digits = everything() ~ c(1, 1)
      ) %>%
      modify_header(label = "") %>%
      modify_column_indent(columns = label, row = label != "MDR") %>%
      modify_footnote(all_stat_cols() ~ "Resitant Percent (%), (Resitant case / Total) ") %>%
      modify_table_body( ~ .x %>%
                           mutate(
                             across(all_stat_cols(), ~ gsub("^NA.*0.0/0.0)", "-", .)),
                             across(all_stat_cols(), ~ gsub("0\\.0 \\(NA%\\)", "-", .))
                           )) %>%
      as_gt() %>%
      add_group_antibiotic(list_ab = list_ab)
    return(gt_table)
  })
}


##### 3. Run App #####
shinyApp(ui = ui, server = server)