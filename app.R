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
library(scales)       # Formatting scales and axes

# Antimicrobial resistance (AMR) analysis tools
library(AMR)

# File input/output utilities
library(readxl)       # read Excel files
library(openxlsx)     # write Excel files
library(tools)        # file extension helpers

# Data presentation and summary tables
library(DT)           # interactive data tables
library(gt)           # HTML table generation
library(gtsummary)    # publication-quality summary tables

# Interactive plotting
library(plotly)       # Plotly for interactive charts

# Source user-defined utility functions and UI modules
source("utils/backend_utils.R", local = TRUE)
source("utils/amr_utils.R", local = TRUE)
source("ui/tab_dashboard.R", local = TRUE)
source("ui/tab_data_input.R", local = TRUE)
source("ui/tab_about.R", local = TRUE)

#App constants
MAX_FILE_SIZE_MB <- 10
MAX_FILE_SIZE_BYTES <- MAX_FILE_SIZE_MB * 1024^2
DATETIME_FORMATS <- c(
  "Ymd HMS",
  "Ymd HM",
  # ISO-like
  "dmY HMS",
  "dmY HM",
  # DMY
  "mdY IMSp",
  "mdY IMp",
  # MDY with PM/AM
  "Y-m-d H:M:S",
  "Y-m-d H:M",
  # hyphen
  "d/m/Y H:M:S",
  "d/m/Y H:M",
  # slash
  "d-b-Y H:M:S",
  "d-b-Y H:M",
  # d-M-Y abbreviated
  "b d Y H:M:S",
  "b d Y H:M",
  # Month d Y
  "Y-m-d",
  "d/m/Y",
  "m/d/Y",
  # dates only
  "dbY",
  "Ybd",
  "Ymd"
)


##### 1. UI/UX Definition #####
ui <- page_navbar(
  ##### 1.1 Setting UI/UX #####
  title = "AMRalyze Dashboard",
  theme = bs_theme(bootswatch = "cerulean"),
  header = tags$head(
    useShinyjs(),
    includeCSS("www/styles.css"),
    tags$script(src = "features.js"),
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
  ##### 2.1 Data input setting #####
  ##### 2.1.1. Initialize reactive Values #####
  meta_data <- reactiveValues(
    uploaded_data  = NULL,
    processed_data = NULL,
    mdr_data = NULL,
    unrecognized_ab = NULL,  # Store antibiotics that couldn't be coerced
    # Enhanced warnings storage
    processing_warnings = NULL  # Store all processing warnings with record details
  )
  
  ##### 2.1.2. File Input Handling #####
  output$sheet_selector <- renderUI({
    req(input$file_browse)
    if (is_valid_excel(input$file_browse)) {
      sheets <- tryCatch(
        excel_sheets(input$file_browse$datapath),
        error = function(e) character(0)
      )
      if (length(sheets) == 0) {
        showNotification("No sheets found in this Excel file!", type = "error")
        return(NULL)
      }
      virtualSelectInput("sheet_name", "Select Excel Sheet:", choices = sheets, selected = sheets[1])
    }
  })
  
  observeEvent(input$file_browse, {
    req(input$file_browse)
    
    if (input$file_browse$size > MAX_FILE_SIZE_BYTES) {
      showNotification(
        paste("File size exceeds", MAX_FILE_SIZE_MB, "MB limit"),
        type = "error",
        duration = 8
      )
      return(NULL)
    }
  })
  
  ##### 2.1.3. Data Uploading ######
  observeEvent(input$btn_upload, {
    req(input$file_browse)
    df <- tryCatch({
      if (is_valid_excel(input$file_browse)) {
        req(input$sheet_name)
        readxl::read_excel(
          input$file_browse$datapath,
          sheet = input$sheet_name,
          col_types = "text"
        )
      } else {
        data.table::fread(
          input$file_browse$datapath,
          header = TRUE,
          check.names = FALSE,
          colClasses = "character"
        ) %>%
          as.data.frame()
      }
    }, error = function(e) {
      showNotification(paste("Load Error:", e$message),
                       type = "error",
                       duration = 8)
      NULL
    })
    
    if (is.null(df)) {
      return() # Error already handled in read_file
    } else if (ncol(df) == 0) {
      showNotification("Data is empty.", type = "error", duration = 8)
    } else {
      meta_data$uploaded_data <- df
      mapping_list <- list(
        NoID = c("No", "No.", "study_id", "record_id"),
        PID          = c("PID", "Patient ID", "patient_id", "subject_id", "pid"),
        SID          = c("SID", "Sample ID", "sample_id", "specimen_id", "sid"),
        Sample_Date  = c("Sample Date", "collection_date", "sampling_date", "date_sampling", "sample_date"),
        Sample_Type  = c("Sample Type", "sample_type", "source", "specimen_type"),
        Pathogen     = c("Pathogen", "Bacteria", "organism", "bacteria", "microorganism"),
        AB_cols      = c(antimicrobials$name, antimicrobials$ab)
      )
      
      map_columns(colnames(df), mapping_list)
      
      showNotification(
        paste("Loaded:", input$file_browse$name),
        type = "message",
        duration = 8
      )
    }
  })
  
  ##### 2.1.4. Data Processing ######
  observeEvent(input$btn_process, {
    req(input$file_browse)
    withProgress(message = 'Processing Data',
                 value = 0,
                 style = "notification",
                 {
                   tryCatch({
                     ##### 2.1.4.1 Validating inputs #####
                     incProgress(0.1, detail = "Validating inputs...")
                     check_index_col <- paste0(input$NoID, input$PID, input$SID)
                     
                     validate(
                       need(
                         check_index_col != "",
                         "Mapping Error: Select at least one identifier column"
                       ),
                       need(input$Pathogen != "", "Mapping Error: Select Pathogen.")
                     )
                     ##### 2.1.4.2 Preparing data columns #####
                     incProgress(0.1, detail = "Preparing data columns...")
                     
                     core_map <- list(
                       NoID = input$NoID,
                       PatientID = input$PID,
                       SampleID = input$SID,
                       SampleDate = input$Sample_Date,
                       SampleType = input$Sample_Type,
                       Pathogen = input$Pathogen
                     )
                     
                     provided_map <- core_map %>% keep(~ !is.null(.) &&
                                                         . != "")
                     missing_keys <- names(core_map)[!names(core_map) %in% names(provided_map)]
                     provided_cols  <- unlist(provided_map)
                     provided_names <- names(provided_map)
                     
                     incProgress(0.1, detail = "Preparing data columns...")
                     
                     # Build rename vector for mapping original columns to standard names
                     rename_vec <- setNames(provided_cols, provided_names)
                     
                     # Get antibiotic columns that actually exist in the data
                     # and are not being used as metadata columns
                     ab_cols_to_pivot <- setdiff(input$AB_cols, provided_cols)
                     ab_cols_to_pivot <- intersect(ab_cols_to_pivot, colnames(meta_data$uploaded_data))
                     
                     validate(
                       need(
                         length(ab_cols_to_pivot) > 0,
                         "Mapping Error: No valid antibiotic columns selected. Please select columns containing AST results."
                       )
                     )
                     
                     df_processed <- meta_data$uploaded_data %>%
                       select(any_of(provided_cols), any_of(ab_cols_to_pivot)) %>%
                       rename(any_of(rename_vec)) %>%
                       pivot_longer(
                         cols       = all_of(ab_cols_to_pivot),
                         names_to   = c("Antibiotic_Name", "Method_init"),
                         names_pattern = "([^_\\-]+)[_\\-]?(.*)",
                         values_to  = "Result",
                         values_drop_na = TRUE
                       )
                     
                     incProgress(0.2, detail = "Preparing AST guideline ...")
                     
                     # Detect unrecognized antibiotics before processing
                     unique_ab_names <- unique(df_processed$Antibiotic_Name)
                     ab_codes_test <- suppressWarnings(as.ab(unique_ab_names))
                     unrecognized_idx <- is.na(ab_codes_test)
                     
                     if (any(unrecognized_idx)) {
                       unrecognized_list <- data.frame(
                         Antibiotic_Input = unique_ab_names[unrecognized_idx],
                         Status = "Not recognized by AMR package",
                         stringsAsFactors = FALSE
                       )
                       # Find original column names for these antibiotics
                       unrecognized_list$Original_Columns <- sapply(unrecognized_list$Antibiotic_Input, function(ab) {
                         matched_cols <- ab_cols_to_pivot[grepl(paste0("^\\Q", ab, "\\E"), ab_cols_to_pivot)]
                         paste(matched_cols, collapse = ", ")
                       })
                       meta_data$unrecognized_ab <- unrecognized_list
                     } else {
                       meta_data$unrecognized_ab <- NULL
                     }
                     
                     # Initialize warnings collection
                     processing_warnings <- list(
                       unrecognized_pathogens = NULL,
                       mic_conversions = NULL,
                       date_issues = NULL,
                       sir_interpretation_issues = NULL
                     )
                     
                     # Get ID column names for warning tracking
                     id_cols <- intersect(c("NoID", "PatientID", "SampleID"), names(df_processed))
                     
                     df_processed <- df_processed %>%
                       mutate(
                         TempInterpretation = str_extract(Result, "R|S|I|SSD|NI"),
                         mo_code = as.mo(Pathogen),
                         kingdom = mo_kingdom(mo_code),
                         gram_stain = mo_gramstain(mo_code),
                         ab_code = as.ab(Antibiotic_Name),
                         AntibioticName = ab_name(ab_code),
                         Method = case_when(
                           str_detect(Method_init, "\\bND") ~ "Disk",
                           str_detect(Method_init, "\\bNE") ~ "E-Test",
                           str_detect(Method_init, "\\bNM") ~ "MIC",
                           TRUE                             ~ "MIC"
                         ),
                         # Assign priority: MIC=1, E-Test=2, Disk=3 (lower is better)
                         Method_Priority = case_when(
                           Method == "MIC" ~ 1L,
                           Method == "E-Test" ~ 2L,
                           Method == "Disk" ~ 3L,
                           TRUE ~ 4L
                         ),
                         Value  = str_extract(Result, "(<=|>=|<|>)?\\s*[0-9.]+"),
                         MIC_raw = if_else(Method %in% c("E-Test", "MIC"), Value, NA_character_),
                         Zone   = if_else(Method == "Disk", Value, NA_character_),
                         # Add row identifier for later joining
                         .row_id = row_number()
                       )
                     
                     # Track unrecognized pathogens (mo_code is NA)
                     unrecognized_mo_rows <- df_processed %>%
                       filter(is.na(mo_code)) %>%
                       select(any_of(c(id_cols, "Pathogen", "Antibiotic_Name"))) %>%
                       distinct()
                     
                     if (nrow(unrecognized_mo_rows) > 0) {
                       # Get first available ID column for display
                       sample_id_col <- intersect(c("SampleID", "PatientID", "NoID"), names(unrecognized_mo_rows))[1]
                       
                       processing_warnings$unrecognized_pathogens <- unrecognized_mo_rows %>%
                         group_by(Pathogen) %>%
                         summarise(
                           `Affected Records` = n(),
                           `Sample IDs (first 5)` = if (!is.null(sample_id_col) && sample_id_col %in% names(.)) {
                             paste(head(unique(.data[[sample_id_col]]), 5), collapse = ", ")
                           } else {
                             "N/A"
                           },
                           .groups = "drop"
                         ) %>%
                         rename(`Pathogen Name` = Pathogen)
                     }
                     
                     # Capture MIC conversion issues - track values that get adjusted
                     mic_values_original <- df_processed$MIC_raw
                     mic_converted <- as.mic(mic_values_original)
                     mic_as_char <- as.character(mic_converted)
                     
                     # Find rows where MIC was modified (excluding NA to NA conversions)
                     mic_modified_idx <- which(
                       !is.na(mic_values_original) & 
                       !is.na(mic_as_char) & 
                       mic_values_original != mic_as_char
                     )
                     
                     if (length(mic_modified_idx) > 0) {
                       mic_issues <- df_processed[mic_modified_idx, ] %>%
                         mutate(
                           MIC_Original = mic_values_original[mic_modified_idx],
                           MIC_Converted = mic_as_char[mic_modified_idx]
                         ) %>%
                         select(any_of(c(id_cols, "Antibiotic_Name", "MIC_Original", "MIC_Converted"))) %>%
                         head(50)  # Limit to first 50 for display
                       
                       processing_warnings$mic_conversions <- mic_issues
                     }
                     
                     # Apply MIC conversion
                     df_processed$MIC <- mic_converted
                     df_processed$Zone <- as.disk(df_processed$Zone)
                     
                     # Split data: rows with valid mo_code AND ab_code for SIR interpretation
                     df_valid <- df_processed %>%
                       filter(!is.na(mo_code) & !is.na(ab_code))
                     
                     df_invalid <- df_processed %>%
                       filter(is.na(mo_code) | is.na(ab_code)) %>%
                       mutate(
                         InterpretationMIC = NA_sir_,
                         InterpretationZone = NA_sir_
                       )
                     
                     # Process valid rows with as.sir()
                     if (nrow(df_valid) > 0) {
                       df_valid <- df_valid %>%
                         mutate(
                           InterpretationMIC = as.sir(
                             MIC,
                             mo = mo_code,
                             ab = ab_code,
                             guideline = input$guideline
                           ),
                           InterpretationZone = as.sir(
                             Zone,
                             mo = mo_code,
                             ab = ab_code,
                             guideline = input$guideline
                           )
                         )
                     } else {
                       df_valid <- df_valid %>%
                         mutate(
                           InterpretationMIC = NA_sir_,
                           InterpretationZone = NA_sir_
                         )
                     }
                     
                     # Combine back and finalize
                     df_processed <- bind_rows(df_valid, df_invalid) %>%
                       arrange(.row_id) %>%
                       mutate(
                         # Priority: Direct interpretation > MIC/E-Test > Disk
                         Interpretation = case_when(
                           !is.na(TempInterpretation) ~ as.sir(TempInterpretation),
                           !is.na(InterpretationMIC) ~ as.sir(InterpretationMIC),
                           !is.na(InterpretationZone) ~ as.sir(InterpretationZone)
                         )
                       )
                     
                     # Track SIR interpretation issues (valid ab/mo but no interpretation)
                     sir_issues <- df_processed %>%
                       filter(!is.na(mo_code) & !is.na(ab_code) & is.na(Interpretation)) %>%
                       filter(!is.na(MIC) | !is.na(Zone)) %>%
                       select(any_of(c(id_cols, "Pathogen", "AntibioticName", "MIC", "Zone"))) %>%
                       distinct() %>%
                       head(50)
                     
                     if (nrow(sir_issues) > 0) {
                       processing_warnings$sir_interpretation_issues <- sir_issues
                     }
                     
                     # Handle SampleDate with tracking
                     if ("SampleDate" %in% names(df_processed)) {
                       original_dates <- df_processed$SampleDate
                       
                       parsed_dates <- parse_date_time2(original_dates, orders = DATETIME_FORMATS)
                       # catch pure-numeric dates (Excel-style)
                       parsed_dates <- if_else(
                         is.na(parsed_dates) & str_detect(original_dates, "^[0-9]+(\\.[0-9]+)?$"),
                         as.POSIXct(
                           as.numeric(original_dates),
                           origin = "1900-01-01",
                           tz = ""
                         ),
                         parsed_dates
                       )
                       
                       # Track date parsing issues
                       date_issue_idx <- which(!is.na(original_dates) & is.na(parsed_dates))
                       if (length(date_issue_idx) > 0) {
                         date_issues <- df_processed[date_issue_idx, ] %>%
                           mutate(Original_Date = original_dates[date_issue_idx]) %>%
                           select(any_of(c(id_cols, "Original_Date"))) %>%
                           distinct() %>%
                           head(50)
                         
                         processing_warnings$date_issues <- date_issues
                       }
                       
                       df_processed$SampleDate <- parsed_dates
                     }
                     
                     df_processed <- df_processed %>%
                       select(-c(Method_init, Result, Value, TempInterpretation, Antibiotic_Name, MIC_raw, .row_id))
                     
                     # Store processing warnings
                     meta_data$processing_warnings <- processing_warnings
                     
                     meta_data$processed_data <- df_processed
                     ##### 2.1.4.3 Calculating MDR columns #####
                     if (input$MDR_cal) {
                       incProgress(0.4, detail = "Calculating MDR status...")
                       
                       # For MDR: Keep only best interpretation per sample/antibiotic
                       # Priority: MIC > E-Test > Disk (use Method_Priority)
                       mdr_input <- df_processed %>%
                         filter(!is.na(Interpretation)) %>%
                         group_by(across(all_of(c(provided_names, "mo_code", "kingdom", "gram_stain", "ab_code")))) %>%
                         # Keep the row with highest priority method (lowest Method_Priority value)
                         slice_min(Method_Priority, n = 1, with_ties = FALSE) %>%
                         ungroup()
                       
                       mdr_data <- mdr_input %>%
                         tidyr::pivot_wider(
                           id_cols = c(all_of(provided_names), mo_code, kingdom, gram_stain),
                           names_from = ab_code,
                           values_from = Interpretation,
                           values_fn = first  # In case of remaining ties, take first
                         ) %>% mutate(
                           MDR = mdro(
                             guideline = "CMI2012",
                             pct_required_classes = 0.5
                           ),
                           MDR = fct_na_value_to_level(MDR, "Not Determined")
                         ) %>%
                         relocate(MDR, .after = Pathogen)
                       
                       meta_data$mdr_data <- mdr_data
                       incProgress(0.1, detail = "Finalizing...")
                     } else {
                       incProgress(0.5, detail = "Finalizing...")
                     }
                     
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
                   })
                 })
  })
  
  ##### 2.1.6 Render Unrecognized Antibiotics Card #####
  output$unrecognized_ab_card <- renderUI({
    req(meta_data$unrecognized_ab)
    
    n_unrecognized <- nrow(meta_data$unrecognized_ab)
    
    card(
      class = "mb-3 border-warning",
      card_header(
        class = "bg-warning text-dark d-flex justify-content-between align-items-center",
        span(icon("exclamation-triangle"), " Unrecognized Antibiotics"),
        span(class = "badge bg-dark", paste(n_unrecognized, "columns"))
      ),
      card_body(
        tags$p(class = "text-muted small",
          "These antibiotic codes could not be recognized. They will have NA values for SIR interpretation."
        ),
        div(
          style = "max-height: 200px; overflow-y: auto;",
          tableOutput("unrecognized_ab_table")
        )
      )
    )
  })
  
  output$unrecognized_ab_table <- renderTable({
    req(meta_data$unrecognized_ab)
    meta_data$unrecognized_ab %>%
      select(
        `Antibiotic Code` = Antibiotic_Input,
        `Original Column(s)` = Original_Columns,
        `Status` = Status
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", spacing = "s")
  
  ##### 2.1.6c Processing Warnings Card #####
  output$processing_warnings_card <- renderUI({
    req(meta_data$processing_warnings)
    warnings <- meta_data$processing_warnings
    
    # Check if there are any warnings to show
    has_warnings <- any(sapply(warnings, function(x) !is.null(x) && nrow(x) > 0))
    if (!has_warnings) return(NULL)
    
    # Count total issues
    total_issues <- sum(sapply(warnings, function(x) if(!is.null(x)) nrow(x) else 0))
    
    card(
      class = "mb-3 border-warning",
      card_header(
        class = "bg-warning text-dark d-flex justify-content-between align-items-center",
        span(icon("exclamation-triangle"), " Processing Warnings"),
        span(class = "badge bg-dark", paste(total_issues, "issues"))
      ),
      card_body(
        tags$p(class = "text-muted small mb-3",
          "The following issues were detected during data processing."
        ),
      
      # Unrecognized Pathogens Section
      if (!is.null(warnings$unrecognized_pathogens) && nrow(warnings$unrecognized_pathogens) > 0) {
        div(
          class = "mb-2",
          tags$details(
            class = "border rounded p-2",
            tags$summary(
              class = "fw-bold",
              icon("bug"), " Unrecognized Pathogens ",
              span(class = "badge bg-danger", nrow(warnings$unrecognized_pathogens))
            ),
            div(
              class = "mt-2",
              tags$small(class = "text-muted", "These organism names could not be matched to the AMR database."),
              div(
                style = "max-height: 150px; overflow-y: auto; margin-top: 8px;",
                tableOutput("warn_pathogens_table")
              )
            )
          )
        )
      },
      
      # MIC Conversion Section
      if (!is.null(warnings$mic_conversions) && nrow(warnings$mic_conversions) > 0) {
        div(
          class = "mb-2",
          tags$details(
            class = "border rounded p-2",
            tags$summary(
              class = "fw-bold",
              icon("flask"), " MIC Value Adjustments ",
              span(class = "badge bg-warning text-dark", nrow(warnings$mic_conversions))
            ),
            div(
              class = "mt-2",
              tags$small(class = "text-muted", "These MIC values were converted to the nearest log2 dilution level."),
              div(
                style = "max-height: 150px; overflow-y: auto; margin-top: 8px;",
                tableOutput("warn_mic_table")
              )
            )
          )
        )
      },
      
      # Date Parsing Issues Section
      if (!is.null(warnings$date_issues) && nrow(warnings$date_issues) > 0) {
        div(
          class = "mb-2",
          tags$details(
            class = "border rounded p-2",
            tags$summary(
              class = "fw-bold",
              icon("calendar-times"), " Date Parsing Issues ",
              span(class = "badge bg-info", nrow(warnings$date_issues))
            ),
            div(
              class = "mt-2",
              tags$small(class = "text-muted", "These date values could not be parsed. SampleDate will be NA for these records."),
              div(
                style = "max-height: 150px; overflow-y: auto; margin-top: 8px;",
                tableOutput("warn_dates_table")
              )
            )
          )
        )
      },
      
      # SIR Interpretation Issues Section
      if (!is.null(warnings$sir_interpretation_issues) && nrow(warnings$sir_interpretation_issues) > 0) {
        div(
          class = "mb-2",
          tags$details(
            class = "border rounded p-2",
            tags$summary(
              class = "fw-bold",
              icon("question-circle"), " SIR Interpretation Unavailable ",
              span(class = "badge bg-secondary", nrow(warnings$sir_interpretation_issues))
            ),
            div(
              class = "mt-2",
              tags$small(class = "text-muted", "No breakpoint available for these pathogen-antibiotic combinations in the selected guideline."),
              div(
                style = "max-height: 150px; overflow-y: auto; margin-top: 8px;",
                tableOutput("warn_sir_table")
              )
            )
          )
        )
      }
      )
    )
  })
  
  # Warning tables
  output$warn_pathogens_table <- renderTable({
    req(meta_data$processing_warnings$unrecognized_pathogens)
    meta_data$processing_warnings$unrecognized_pathogens
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", spacing = "s")
  
  output$warn_mic_table <- renderTable({
    req(meta_data$processing_warnings$mic_conversions)
    meta_data$processing_warnings$mic_conversions %>%
      rename_with(~ gsub("_", " ", .), everything())
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", spacing = "s")
  
  output$warn_dates_table <- renderTable({
    req(meta_data$processing_warnings$date_issues)
    meta_data$processing_warnings$date_issues %>%
      rename_with(~ gsub("_", " ", .), everything())
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", spacing = "s")
  
  output$warn_sir_table <- renderTable({
    req(meta_data$processing_warnings$sir_interpretation_issues)
    meta_data$processing_warnings$sir_interpretation_issues
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", spacing = "s")

  ##### 2.1.6b Processing Summary Card #####
  output$processing_summary_card <- renderUI({
    req(meta_data$processed_data)
    
    # Calculate statistics
    n_records <- nrow(meta_data$processed_data)
    n_pathogens <- length(unique(meta_data$processed_data$Pathogen))
    n_antibiotics <- length(unique(meta_data$processed_data$ab_code[!is.na(meta_data$processed_data$ab_code)]))
    n_interpreted <- sum(!is.na(meta_data$processed_data$Interpretation))
    pct_interpreted <- round(n_interpreted / n_records * 100, 1)
    
    # MDR stats if available
    mdr_stats <- NULL
    if (!is.null(meta_data$mdr_data)) {
      n_mdr <- sum(meta_data$mdr_data$MDR == "Multi-drug-resistant (MDR)", na.rm = TRUE)
      n_xdr <- sum(meta_data$mdr_data$MDR == "Extensively drug-resistant (XDR)", na.rm = TRUE)
      n_pdr <- sum(meta_data$mdr_data$MDR == "Pandrug-resistant (PDR)", na.rm = TRUE)
      mdr_stats <- list(mdr = n_mdr, xdr = n_xdr, pdr = n_pdr, total = nrow(meta_data$mdr_data))
    }
    
    tagList(
      # Main stats using layout_columns
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,
        class = "mb-3",
        value_box(
          title = "AST Records",
          value = format(n_records, big.mark = ","),
          theme = value_box_theme(bg = "#e8f4fd", fg = "#1565c0"),
          showcase = bsicons::bs_icon("clipboard-data")
        ),
        value_box(
          title = "Pathogens",
          value = n_pathogens,
          theme = value_box_theme(bg = "#e8f5e9", fg = "#2e7d32"),
          showcase = bsicons::bs_icon("bug")
        ),
        value_box(
          title = "Antibiotics",
          value = n_antibiotics,
          theme = value_box_theme(bg = "#f3e5f5", fg = "#7b1fa2"),
          showcase = bsicons::bs_icon("capsule")
        ),
        value_box(
          title = "Interpreted",
          value = paste0(pct_interpreted, "%"),
          theme = value_box_theme(bg = "#e0f2f1", fg = "#00695c"),
          showcase = bsicons::bs_icon("check-circle")
        )
      ),
      # MDR stats if available
      if (!is.null(mdr_stats)) {
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          fill = FALSE,
          class = "mb-3",
          value_box(
            title = "MDR",
            value = mdr_stats$mdr,
            theme = value_box_theme(bg = "#fff3e0", fg = "#e65100"),
            showcase = bsicons::bs_icon("shield-exclamation")
          ),
          value_box(
            title = "XDR",
            value = mdr_stats$xdr,
            theme = value_box_theme(bg = "#ffebee", fg = "#c62828"),
            showcase = bsicons::bs_icon("shield-x")
          ),
          value_box(
            title = "PDR",
            value = mdr_stats$pdr,
            theme = value_box_theme(bg = "#f3e5f5", fg = "#6a1b9a"),
            showcase = bsicons::bs_icon("shield-fill-x")
          ),
          value_box(
            title = "Resistant Rate",
            value = paste0(round((mdr_stats$mdr + mdr_stats$xdr + mdr_stats$pdr) / mdr_stats$total * 100, 1), "%"),
            theme = value_box_theme(bg = "#eceff1", fg = "#455a64"),
            showcase = bsicons::bs_icon("percent")
          )
        )
      }
    )
  })
  
  ##### 2.1.7 Show Patient Data #####
  output$data_AST_Table <- renderDT({
    req(meta_data$processed_data)
    datatable(
      meta_data$processed_data,
      class = "compact stripe hover",
      filter = "top",
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100)
      )
    )
  })
  
  output$data_MDR_Table <- renderDT({
    req(meta_data$mdr_data)
    datatable(
      meta_data$mdr_data,
      class = "compact stripe hover",
      filter = "top",
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100)
      )
    )
  })
  ##### 2.1.8 Download Data #####
  ##### 2.1.8.1 Download AST Data #####
  output$download_ast_data <- downloadHandler(
    filename = function() {
      paste0("AST_data_", Sys.Date(), ".csv")
    },
    content = function(con) {
      write.csv(meta_data$processed_data,con,row.names = FALSE)
    }
  )
  ##### 2.1.8.2 Download MDR Data #####
  output$download_mdr_data <- downloadHandler(
    filename = function() {
      paste0("MDR_data_", Sys.Date(), ".csv")
    },
    content = function(con) {
      write.csv(meta_data$mdr_data,con,row.names = FALSE)
    }
  )
  ##### 2.2 Server Logic For Dashboard #####
  
  ###### 2.2.1 Dashboard Metrics  ######
  output$total_records <- renderText({
    if (is.null(meta_data$uploaded_data))
      "-"
    else
      scales::comma(nrow(meta_data$uploaded_data))
  })
  
  output$total_patients <- renderText({
    if (is.null(meta_data$processed_data))
    {
      "-"
    }
    else{
      if ("PatientID" %in% names(meta_data$processed_data)) {
        scales::comma(n_distinct(meta_data$processed_data$PatientID, na.rm = TRUE))
      } else {
        "No PatientID column"
      }
    }
  })
  
  output$total_samples <- renderText({
    if (is.null(meta_data$processed_data))
    {
      "-"
    }
    else{
      if ("SampleID" %in% names(meta_data$processed_data)) {
        scales::comma(n_distinct(meta_data$processed_data$SampleID, na.rm = TRUE))
      } else {
        "No SampleID column"
      }
    }
  })
  
  output$timeline <- renderText({
    if (is.null(meta_data$processed_data))
    {
      "-"
    }
    else{
      if ("SampleDate" %in% names(meta_data$processed_data)) {
        valid_dates <- meta_data$processed_data$SampleDate[!is.na(meta_data$processed_data$SampleDate)]
        if (length(valid_dates) > 0) {
          min_date <- format(min(valid_dates, na.rm = TRUE), "%Y-%m-%d")
          max_date <- format(max(valid_dates, na.rm = TRUE), "%Y-%m-%d")
          paste(min_date, "to", max_date)
        } else {
          "No valid dates"
        }
      } else {
        "No Date column"
      }
    }
  })
  
  # MDR Statistics
  output$mdr_count <- renderText({
    if (is.null(meta_data$mdr_data)) {
      "-"
    } else {
      n <- sum(meta_data$mdr_data$MDR == "Multi-drug-resistant (MDR)", na.rm = TRUE)
      scales::comma(n)
    }
  })
  
  output$xdr_count <- renderText({
    if (is.null(meta_data$mdr_data)) {
      "-"
    } else {
      n <- sum(meta_data$mdr_data$MDR == "Extensively drug-resistant (XDR)", na.rm = TRUE)
      scales::comma(n)
    }
  })
  
  output$mdr_rate <- renderText({
    if (is.null(meta_data$mdr_data)) {
      "-"
    } else {
      total <- nrow(meta_data$mdr_data)
      mdr <- sum(meta_data$mdr_data$MDR %in% c("Multi-drug-resistant (MDR)", 
                                                "Extensively drug-resistant (XDR)",
                                                "Pandrug-resistant (PDR)"), na.rm = TRUE)
      paste0(round(mdr / total * 100, 1), "%")
    }
  })
  
  output$top_resistant <- renderText({
    if (is.null(meta_data$mdr_data)) {
      "-"
    } else {
      mdr_by_pathogen <- meta_data$mdr_data %>%
        filter(MDR %in% c("Multi-drug-resistant (MDR)", 
                          "Extensively drug-resistant (XDR)",
                          "Pandrug-resistant (PDR)")) %>%
        count(Pathogen, sort = TRUE)
      
      if (nrow(mdr_by_pathogen) > 0) {
        # Shorten pathogen name if too long
        top_name <- mdr_by_pathogen$Pathogen[1]
        if (nchar(top_name) > 20) {
          top_name <- paste0(substr(top_name, 1, 18), "...")
        }
        top_name
      } else {
        "None"
      }
    }
  })
  
  # Sample Type Distribution Plot
  output$sample_type_plot <- renderPlotly({
    validate(
      need(meta_data$processed_data, "Please upload and process data first."),
      need("SampleType" %in% names(meta_data$processed_data), "No Sample Type column available.")
    )
    
    sample_freq <- meta_data$processed_data %>%
      select(any_of(c("NoID", "PatientID", "SampleID", "SampleType"))) %>%
      unique() %>%
      filter(!is.na(SampleType)) %>%
      count(SampleType, sort = TRUE, name = "Count") %>%
      mutate(Percent = round(Count / sum(Count) * 100, 1))
    
    validate(need(nrow(sample_freq) > 0, "No sample type data available."))
    
    plot_ly(sample_freq, 
            labels = ~SampleType, 
            values = ~Count, 
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            marker = list(colors = RColorBrewer::brewer.pal(min(nrow(sample_freq), 8), "Set2")),
            hovertemplate = "%{label}<br>Count: %{value}<br>%{percent}<extra></extra>") %>%
      layout(showlegend = TRUE,
             legend = list(orientation = "v", x = 1, y = 0.5))
  })
  
  # Resistance Trend Over Time Plot
  output$resistance_trend_plot <- renderPlotly({
    validate(
      need(meta_data$processed_data, "Please upload and process data first."),
      need("SampleDate" %in% names(meta_data$processed_data), "No Date column available for trend analysis.")
    )
    
    # Calculate monthly resistance rate
    trend_data <- meta_data$processed_data %>%
      filter(!is.na(SampleDate), !is.na(Interpretation)) %>%
      mutate(Month = floor_date(SampleDate, "month")) %>%
      group_by(Month) %>%
      summarise(
        Total = n(),
        Resistant = sum(Interpretation == "R", na.rm = TRUE),
        Rate = round(Resistant / Total * 100, 1),
        .groups = "drop"
      ) %>%
      filter(!is.na(Month))
    
    validate(need(nrow(trend_data) > 1, "Insufficient data for trend analysis."))
    
    plot_ly(trend_data, x = ~Month, y = ~Rate, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#e74c3c', width = 2),
            marker = list(color = '#e74c3c', size = 8),
            hovertemplate = "Date: %{x}<br>Resistance Rate: %{y}%<br>Total Tests: %{customdata}<extra></extra>",
            customdata = ~Total) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Resistance Rate (%)", rangemode = "tozero"),
        hovermode = "x unified"
      )
  })
  
  ###### 2.2.2. Main Content Area Preview ######
  output$pathogen_summary_plot <- renderPlotly({
    # Require processed data
    validate(
      need(
        meta_data$processed_data,
        "Please upload and process data first ('Input' tab)."
      ),
      need(nrow(meta_data$processed_data) > 0, "Processed data is empty.")
    )
    
    pathogen_freq <- meta_data$processed_data %>%
      select(any_of(c("NoID", "PatientID", "SampleID", "Pathogen"))) %>% 
      unique() %>%
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
        dplyr::rename(Pathogen = Pathogen_Group)
    }
    pathogen_freq <- pathogen_freq %>%
      dplyr::mutate(Pathogen = fct_reorder(Pathogen, Frequency))
    # Create the plot using ggplot2
    gg <- ggplot(pathogen_freq, aes(x = Pathogen, y = Frequency)) +
      geom_col(fill = "#2c7fb8", width = 0.7) + # Use a color for bars
      coord_flip() + # Flip coordinates for better readability of labels
      labs(y = "Number of Samples") +
      theme_minimal(base_size = 12) + # Use a clean theme
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x =  element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)
      )
    # Return the plot
    ggplotly(gg)
  })
  
  ##### Gram-Negative Selector UI #####
  output$gram_neg_selector <- renderUI({
    req(meta_data$processed_data)
    list_gram_negative <- get_pathogen_list(
      meta_data$processed_data,
      kingdom_filter = "Bacteria",
      gram_stain_filter = "Gram-negative"
    )
    
    if (length(list_gram_negative) == 0) {
      return(tags$p(class = "text-muted", "No Gram-negative pathogens found."))
    }
    
    selected_choice <- if (length(list_gram_negative) >= 4) {
      list_gram_negative[1:4]
    } else {
      list_gram_negative
    }
    
    virtualSelectInput(
      inputId = "list_gram_negative",
      label = "Select Pathogens:",
      choices = list_gram_negative,
      selected = selected_choice,
      multiple = TRUE,
      showValueAsTags = TRUE,
      search = TRUE
    )
  })
  
  ##### Gram-Positive Selector UI #####
  output$gram_pos_selector <- renderUI({
    req(meta_data$processed_data)
    list_gram_positive <- get_pathogen_list(
      meta_data$processed_data,
      kingdom_filter = "Bacteria",
      gram_stain_filter = "Gram-positive"
    )
    
    if (length(list_gram_positive) == 0) {
      return(tags$p(class = "text-muted", "No Gram-positive pathogens found."))
    }
    
    selected_choice <- if (length(list_gram_positive) >= 4) {
      list_gram_positive[1:4]
    } else {
      list_gram_positive
    }
    
    virtualSelectInput(
      inputId = "list_gram_positive",
      label = "Select Pathogens:",
      choices = list_gram_positive,
      selected = selected_choice,
      multiple = TRUE,
      showValueAsTags = TRUE,
      search = TRUE
    )
  })
  
  ##### Fungal Selector UI #####
  output$fungal_selector <- renderUI({
    req(meta_data$processed_data)
    list_fungal <- get_pathogen_list(
      meta_data$processed_data,
      kingdom_filter = "Fungi"
    )
    
    if (length(list_fungal) == 0) {
      return(tags$p(class = "text-muted", "No fungal pathogens found."))
    }
    
    selected_choice <- if (length(list_fungal) >= 4) {
      list_fungal[1:4]
    } else {
      list_fungal
    }
    
    virtualSelectInput(
      inputId = "list_fungal",
      label = "Select Pathogens:",
      choices = list_fungal,
      selected = selected_choice,
      multiple = TRUE,
      showValueAsTags = TRUE,
      search = TRUE
    )
  })
  
  ##### Gram-Negative Table #####
  output$ast_gram_negative_table <- render_gt({
    validate(need(
      meta_data$processed_data,
      "Please upload and process data first ('Input' tab)."
    ))
    validate(need(
      length(input$list_gram_negative) > 0,
      "Choose Gram-negative Pathogen."
    ))
    
    build_ast_summary_table(
      meta_data$processed_data,
      meta_data$mdr_data,
      input$list_gram_negative
    )
  })
  
  ##### Gram-Positive Table #####
  output$ast_gram_positive_table <- render_gt({
    validate(need(
      meta_data$processed_data,
      "Please upload and process data first ('Input' tab)."
    ))
    validate(need(
      length(input$list_gram_positive) > 0,
      "Choose Gram-positive Pathogen."
    ))
    
    build_ast_summary_table(
      meta_data$processed_data,
      meta_data$mdr_data,
      input$list_gram_positive
    )
  })
  
  ##### Fungal Table #####
  output$ast_fungal_table <- render_gt({
    validate(need(
      meta_data$processed_data,
      "Please upload and process data first ('Input' tab)."
    ))
    validate(need(
      length(input$list_fungal) > 0,
      "Choose Fungal Pathogen."
    ))
    
    build_ast_summary_table(
      meta_data$processed_data,
      meta_data$mdr_data,
      input$list_fungal
    )
  })
  
  ##### Download Handlers for AST Tables #####
  output$download_gram_neg <- downloadHandler(
    filename = function() {
      paste0("Gram_Negative_AST_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(meta_data$processed_data, input$list_gram_negative)
      data <- meta_data$processed_data %>%
        filter(Pathogen %in% input$list_gram_negative) %>%
        filter(!is.na(Interpretation))
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_gram_pos <- downloadHandler(
    filename = function() {
      paste0("Gram_Positive_AST_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(meta_data$processed_data, input$list_gram_positive)
      data <- meta_data$processed_data %>%
        filter(Pathogen %in% input$list_gram_positive) %>%
        filter(!is.na(Interpretation))
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_fungal <- downloadHandler(
    filename = function() {
      paste0("Fungal_AST_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(meta_data$processed_data, input$list_fungal)
      data <- meta_data$processed_data %>%
        filter(Pathogen %in% input$list_fungal) %>%
        filter(!is.na(Interpretation))
      write.csv(data, file, row.names = FALSE)
    }
  )
}

##### 3. Run App #####
shinyApp(ui = ui, server = server)