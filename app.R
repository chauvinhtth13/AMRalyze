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
  ##### 2.1.1. Initialize reactive Values #####
  meta_data <- reactiveValues(uploaded_data  = NULL,
                              processed_data = NULL,
                              mdr_data = NULL)
  
  output$sheet_selector <- renderUI({
    req(input$file_browse)
    if (is_valid_excel(input$file_browse)) {
      selectInput("sheet_name", "Select Excel Sheet:", choices = NULL)
    }
  })
  
  ##### 2.1.2. File Input Handling #####
  observeEvent(input$file_browse, {
    if (input$file_browse$size > MAX_FILE_SIZE_BYTES) {
      return(showNotification(paste(
        "Kích thước vượt", MAX_FILE_SIZE_MB, "MB"
      )),
      type = "error",
      duration = 8)
    }
    
    if (is_valid_excel(input$file_browse)) {
      sheets <- tryCatch({
        excel_sheets(input$file_browse$datapath)
      }, error = function(e) {
        showNotification(
          "Lỗi khi đọc file. Vui lòng tải lên file .csv hoặc .xlsx hợp lệ.",
          type = "error",
          duration = 8
        )
        NULL
      })
      updateSelectInput(
        session,
        "sheet_name",
        choices = sheets %||% NULL,
        selected = sheets[1] %||% NULL
      )
    } else {
      # Clear selectInput for non-Excel files (e.g., .csv)
      updateSelectInput(session,
                        "sheet_name",
                        choices = NULL,
                        selected = NULL)
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
      show_notification("Dữ liệu rỗng.", duration = 8)
    } else {
      meta_data$uploaded_data <- df
      mapping_list <- list(
        No = c("No", "No."),
        PID          = c("PID", "Patient ID", "patient_id", "subject_id"),
        SID          = c("SID", "Sample ID", "sample_id", "specimen_id"),
        Sample_Date  = c("Sample Date", "collection_date", "sampling_date"),
        Sample_Type  = c("Sample Type", "sample_type", "source"),
        Pathogen     = c("Pathogen", "Bacteria", "organism"),
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
                     check_index_col <- paste0(input$No, input$PID, input$SID)
                     
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
                       No = input$No,
                       PatientID = input$PID,
                       SampleID = input$SID,
                       SampleDate = input$Sample_Date,
                       SampleType = input$Sample_Type,
                       Pathogen = input$Pathogen
                     ) %>% purrr::discard(~ is.null(.) ||
                                            . == "")
                     
                     df_processed <- meta_data$uploaded_data %>%
                       select(any_of(unlist(core_map)), any_of(input$AB_cols)) %>%
                       pivot_longer(
                         cols       = all_of(input$AB_cols),
                         names_to   = c("Antibiotic_Name", "Method_init"),
                         names_pattern = "([^_\\-]+)[_\\-]?(.*)",
                         values_to  = "Result",
                         values_drop_na = TRUE
                       )
                     
                     colnames(df_processed) <- c(names(core_map), "Antibiotic_Name", "Method_init", "Result")
                     df_processed <- df_processed %>%
                       mutate(
                         Method = case_when(
                           str_detect(Method_init, "\\bND") ~ "Disk",
                           str_detect(Method_init, "\\bNE") ~ "E-Test",
                           TRUE                             ~ "MIC"
                         ),
                         TempInterpretation = as.sir(str_extract(Result, "R|S|I|SSD|NI")),
                         Value  = str_extract(Result, "(<=|>=|<|>)?\\s*[0-9.]+"),
                         MIC    = if_else(Method %in% c("E-Test", "MIC"), Value, NA_character_),
                         Zone   = if_else(Method == "Disk", Value, NA_character_),
                         MIC = as.mic(MIC),
                         Zone = as.disk(Zone),
                         mo_code = as.mo(Pathogen),
                         kingdom = mo_kingdom(mo_code),
                         gram_stain = mo_gramstain(mo_code),
                         ab_code = as.ab(Antibiotic_Name),
                         AntibioticName = ab_name(ab_code),
                         Interpretation = case_when(
                           !is.na(TempInterpretation) ~ as.sir(TempInterpretation),
                           !is.na(MIC) ~ as.sir(MIC,mo = mo_code,ab = ab_code, guideline = input$guideline),
                           !is.na(Zone) ~ as.sir(Zone,mo = mo_code,ab = ab_code, guideline = input$guideline)),
                         SampleDate = if ("SampleDate" %in% names(.)) {
                           parsed <- parse_date_time2(SampleDate,
                                                      orders = DATETIME_FORMATS)
                           # catch pure-numeric dates (Excel-style)
                           if_else(
                             is.na(parsed) & str_detect(SampleDate, "^[0-9]+(\\.[0-9]+)?$"),
                             as.POSIXct(as.numeric(SampleDate),origin = "1900-01-01",tz = ""),
                             parsed
                           )
                         } else {
                           NULL
                         }
                       ) %>% 
                       select(-c(Method_init, Result, Value, TempInterpretation))
                     meta_data$processed_data <- df_processed
                     ##### 2.1.4.3 Calculating MDR columns #####
                     if(input$MDR_cal){
                       incProgress(0.6, detail = "Calculating MDR status...")
                       mdr_data <- df_processed %>% filter(!is.na(Interpretation)) %>%
                         tidyr::pivot_wider(
                           id_cols = c(PatientID, SampleID, Pathogen, mo_code, kingdom, gram_stain),
                           names_from = ab_code,
                           values_from = Interpretation
                         ) %>% mutate(
                           MDR = mdro(guideline = "CMI2012", pct_required_classes = 0.5),
                           MDR = if_else(is.na(MDR), "Not Determined", MDR)
                         )
                       meta_data$mdr_data <- mdr_data
                       incProgress(0.4, detail = "Finalizing...")
                     } else {
                       incProgress(0.8, detail = "Finalizing...")
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
  
}

##### 3. Run App #####
shinyApp(ui = ui, server = server)