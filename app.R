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
    mdr_data = NULL
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
                     
                     df_processed <- meta_data$uploaded_data %>%
                       select(any_of(provided_cols), any_of(input$AB_cols)) %>%
                       rename(any_of(rename_vec)) %>%
                       pivot_longer(
                         cols       = all_of(input$AB_cols),
                         names_to   = c("Antibiotic_Name", "Method_init"),
                         names_pattern = "([^_\\-]+)[_\\-]?(.*)",
                         values_to  = "Result",
                         values_drop_na = TRUE
                       )
                     
                     incProgress(0.2, detail = "Preparing AST guideline ...")
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
                         MIC    = if_else(Method %in% c("E-Test", "MIC"), Value, NA_character_),
                         Zone   = if_else(Method == "Disk", Value, NA_character_),
                         MIC = as.mic(MIC),
                         Zone = as.disk(Zone),
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
                         ),
                         # Priority: Direct interpretation > MIC/E-Test > Disk
                         Interpretation = case_when(
                           !is.na(TempInterpretation) ~ as.sir(TempInterpretation),
                           !is.na(InterpretationMIC) ~ as.sir(InterpretationMIC),
                           !is.na(InterpretationZone) ~ as.sir(InterpretationZone)
                         ),
                         SampleDate = if ("SampleDate" %in% names(.)) {
                           parsed <- parse_date_time2(SampleDate, orders = DATETIME_FORMATS)
                           # catch pure-numeric dates (Excel-style)
                           if_else(
                             is.na(parsed) & str_detect(SampleDate, "^[0-9]+(\\.[0-9]+)?$"),
                             as.POSIXct(
                               as.numeric(SampleDate),
                               origin = "1900-01-01",
                               tz = ""
                             ),
                             parsed
                           )
                         } else {
                           NULL
                         }
                       ) %>%
                       select(-c(Method_init, Result, Value, TempInterpretation, Antibiotic_Name))
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
  ##### 2.1.7 Show Patient Data #####
  output$data_AST_Table <- renderDT(
    meta_data$processed_data,
    # data
    class = "display nowrap compact",
    # style
    filter = "top" # location of column filters
  )
  
  output$data_MDR_Table <- renderDT(
    meta_data$mdr_data,
    # data
    class = "display nowrap compact",
    # style
    filter = "top" # location of column filters
  )
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
        paste(min(meta_data$processed_data$SampleDate), "to", max(meta_data$processed_data$SampleDate))
      } else {
        "No Date column"
      }
    }
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
  
  observe({
    req(meta_data$processed_data)
    list_gram_negative <- get_pathogen_list(
      meta_data$processed_data,
      kingdom_filter = "Bacteria",
      gram_stain_filter = "Gram-negative"
    )

    selected_choice <- if (length(list_gram_negative) >= 4) {
      list_gram_negative[1:4]
    } else if (length(list_gram_negative) > 0) {
      list_gram_negative
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
  
  ##### Gram-Positive Observer #####
  observe({
    req(meta_data$processed_data)
    list_gram_positive <- get_pathogen_list(
      meta_data$processed_data,
      kingdom_filter = "Bacteria",
      gram_stain_filter = "Gram-positive"
    )

    selected_choice <- if (length(list_gram_positive) >= 4) {
      list_gram_positive[1:4]
    } else if (length(list_gram_positive) > 0) {
      list_gram_positive
    } else {
      NULL
    }

    updateVirtualSelect(
      session = session,
      inputId = "list_gram_positive",
      choices = list_gram_positive,
      selected = selected_choice
    )
  })
  
  ##### Fungal Observer #####
  observe({
    req(meta_data$processed_data)
    list_fungal <- get_pathogen_list(
      meta_data$processed_data,
      kingdom_filter = "Fungi"
    )

    selected_choice <- if (length(list_fungal) >= 4) {
      list_fungal[1:4]
    } else if (length(list_fungal) > 0) {
      list_fungal
    } else {
      NULL
    }

    updateVirtualSelect(
      session = session,
      inputId = "list_fungal",
      choices = list_fungal,
      selected = selected_choice
    )
  })
  
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