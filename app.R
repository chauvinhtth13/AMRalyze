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
        showNotification("Không tìm thấy sheet trong file Excel này!", type = "error")
        return(NULL)
      }
      virtualSelectInput("sheet_name", "Select Excel Sheet:", choices = sheets, selected = sheets[1])
    }
  })
  
  observeEvent(input$file_browse, {
    req(input$file_browse)
    
    if (input$file_browse$size > MAX_FILE_SIZE_BYTES) {
      showNotification(
        paste("Kích thước vượt", MAX_FILE_SIZE_MB, "MB"),
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
      show_notification("Dữ liệu rỗng.", duration = 8)
    } else {
      meta_data$uploaded_data <- df
      mapping_list <- list(
        NoID = c("No", "No."),
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
                     df_processed <- meta_data$uploaded_data %>%
                       select(any_of(provided_cols), any_of(input$AB_cols)) %>%
                       #mutate(!!!set_names(rep(list(""), length(missing_keys)), missing_keys)) %>%
                       pivot_longer(
                         cols       = all_of(input$AB_cols),
                         names_to   = c("Antibiotic_Name", "Method_init"),
                         names_pattern = "([^_\\-]+)[_\\-]?(.*)",
                         values_to  = "Result",
                         values_drop_na = TRUE
                       )
                     
                     incProgress(0.2, detail = "Preparing AST guildeline ...")
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
                           TRUE                             ~ "MIC"
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
                       select(-c(Method_init, Result, Value, TempInterpretation,Antibiotic_Name))
                     meta_data$processed_data <- df_processed
                     ##### 2.1.4.3 Calculating MDR columns #####
                     if (input$MDR_cal) {
                       incProgress(0.4, detail = "Calculating MDR status...")
                       mdr_data <- df_processed %>% filter(!is.na(Interpretation)) %>%
                         tidyr::pivot_wider(
                           id_cols = c(provided_names, mo_code, kingdom, gram_stain),
                           names_from = ab_code,
                           values_from = Interpretation
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
        "No PatientID collumn"
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
        "No SampleID collumn"
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
        "No Date collumn"
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
      select(any_of(c("No", "PatientID", "SampleID", "Pathogen"))) %>% 
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
    list_gram_negative <- meta_data$processed_data %>%
      select(any_of(c("No", "PatientID", "SampleID", "Pathogen","kingdom","gram_stain"))) %>%
      unique() %>%
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
      meta_data$uploaded_data,
      "Please upload and process data first ('Input' tab)."
    ))
    validate(need(
      length(input$list_gram_negative) > 0,
      "Choose Gram-negative Pathogen."
    ))
    if(is.null(meta_data$mdr_data))
    {
      data_sub <- meta_data$processed_data %>% 
        filter(!is.na(Interpretation)) %>%
        filter(Pathogen %in% input$list_gram_negative) %>%
        select(any_of(
          c(
            "No",
            "PatientID",
            "SampleID",
            "Pathogen",
            "ab_code",
            "Interpretation"
          )
        )) %>%
        tidyr::pivot_wider(
          names_from = ab_code,
          values_from = Interpretation
        ) %>%  select(-c(any_of(
          c(
            "No",
            "PatientID",
            "SampleID"
          )
        ))) %>%
        select(where(~ !all(is.na(.)))) %>%
        rename_if(is.sir, ab_name) %>%
        mutate_if(
          is.sir,
          .funs = function(x)
            if_else(x == "R", TRUE, FALSE)
        )
    } else {
      data_sub <- meta_data$mdr_data %>%
        filter(Pathogen %in% input$list_gram_negative) %>%
        select(-c(any_of(
          c(
            "No",
            "PatientID",
            "SampleID",
            "SampleType",
            "SampleDate",
            "mo_code",
            "kingdom",
            "gram_stain"
          )
        ))) %>%
        select(where(~ !all(is.na(.)))) %>%
        rename_if(is.sir, ab_name) %>%
        mutate_if(
          is.sir,
          .funs = function(x)
            if_else(x == "R", TRUE, FALSE)
        )
    }

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
      modify_table_body(~ .x %>%
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