# --- 0. Load Libraries ---
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(DT)
library(AMR)          # Assumed source of AMR-specific data/functions
library(tidyverse)    # Includes dplyr, stringr, tidyr, purrr, lubridate, etc.
library(readxl)       # For reading Excel files
library(tools)        # For file extension checking
library(scales)       # For comma formatting

source("utils/ui_utils.R", local = TRUE)

##### 1. UI Definition #####
ui <- page_navbar(
  title = "AMRalyze Dashboard",
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  header = tags$head(
    shinyjs::useShinyjs(),
    tags$style(HTML("#file_browse_progress.progress { display: none !important; }")),
    tags$script(HTML(
      "$(document).on('shiny:connected', function(event) {", 
      "  var vsInputId = 'AB_cols';",
      "  var lastSelectedIndex = -1;",
      "  var virtualSelectElement = document.getElementById(vsInputId);",
      "  if (virtualSelectElement && virtualSelectElement.virtualSelect) {",
      "      $(virtualSelectElement).on('click', '.vscomp-option', function(e) {", 
      "          var clickedOption = $(this);",
      "          var currentIndex = clickedOption.data('index');",
      "          console.log('Clicked index:', currentIndex, 'Shift:', e.shiftKey, 'Last:', lastSelectedIndex);",
      "          if (e.shiftKey && lastSelectedIndex !== -1) {",
      "              e.preventDefault(); // Stop default toggle? Maybe not needed.",
      "              var start = Math.min(lastSelectedIndex, currentIndex);",
      "              var end = Math.max(lastSelectedIndex, currentIndex);",
      "              var optionsToSelect = [];",
      "              for (var i = start; i <= end; i++) {",
      "              }",
      "              console.log('Range selected:', optionsToSelect);",
      "          } else {",
      "              lastSelectedIndex = currentIndex;",
      "          }",
      "      });",
      "  } else {",
      "     console.error('Could not find Virtual Select instance for:', vsInputId);",
      "  }",
      "});"
      )
    )
  ),
  ##### 1.1. Dashboard Tab #####
  nav_panel(
    title = "Dashboard",
    card(
      layout_columns(
        col_widths = c(4, 4, 4),
        max_height = "200px",
        value_box(title = "Total Records Uploaded", value = textOutput("total_records"), showcase = bsicons::bs_icon("file-earmark-ruled"), theme= "primary"),
        value_box(title = "Total Patients", value = textOutput("total_patients"), showcase = bsicons::bs_icon("person-vcard"), theme = "info"),
        value_box(title = "Total Samples", value = textOutput("total_samples"), showcase = bsicons::bs_icon("radioactive"), theme = "success")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Raw Data"), card_body(DT::dataTableOutput("raw_data_view_table"))),
        card(card_header("Placeholder Plot 2"), card_body("Plot Area 2: Requires processed data."))
      )
    )
  ),
  
  ##### 1.2. Input Tab #####
  nav_panel(
    title = "Input",
    ##### 1.2.1. Sidebar Configure #####
    layout_sidebar(
      sidebar = sidebar(
        width = "30%", # Example width
        fileInput("file_browse", h5("1. Upload CSV/XLSX File (Max 10MB)"), accept = c(".csv", ".xlsx", "text/csv", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"), buttonLabel = "Browse...", placeholder = "No file selected"),
        conditionalPanel("output.show_sheet_selector == true", selectInput("sheet_name", "Select Sheet", choices = NULL)),
        conditionalPanel("output.show_upload_button == true", actionButton("btn_upload", "2. Upload & Preview Data", class = "btn-primary w-100")),
        conditionalPanel(
          condition = "output.show_mapping_ui == true",
          hr(),
          h5("3. Choose Guideline:"),
          pickerInput("guideline", "AST Guidelines", choices = sort(unique(clinical_breakpoints$guideline)), selected = get_newest_guideline(), options = list(`actions-box` = TRUE, `live-search`=TRUE)),
          hr(),
          h5("4. Map Columns:"),
          p("Select the corresponding columns from your data. Auto-detection attempted."),
          virtualSelectInput("PID", "Patient ID Column", choices = NULL, multiple = FALSE, search = TRUE),
          virtualSelectInput("SID", "Sample ID Column", choices = NULL, multiple = FALSE, search = TRUE),
          virtualSelectInput("Sample_Date", "Sampling Date Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
          virtualSelectInput("Sample_Type", "Sample Type Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
          virtualSelectInput("Pathogen", "Pathogen Column", choices = NULL, multiple = FALSE, search = TRUE),
          hr(),
          radioButtons("data_structure", "AST Data Structure:", choices = list("Wide (One row per isolate)" = "wide", "Long (One row per test)" = "long"), selected = "long"),
          conditionalPanel("input.data_structure == 'wide'",
                           virtualSelectInput("AB_cols", "Select ALL Antibiotic Result Columns", choices = NULL, multiple = TRUE, search = TRUE, showValueAsTags = TRUE, placeholder="Select columns...", disableSelectAll = FALSE, optionsCount = 8)
          ),
          conditionalPanel("input.data_structure == 'long'",
                           virtualSelectInput("AB_name", "Antibiotic Name Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
                           virtualSelectInput("MIC", "MIC Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
                           virtualSelectInput("Zone_Size", "Zone Size Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
                           virtualSelectInput("Interpretation", "Interpretation Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
                           virtualSelectInput("Methods", "Test Method Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE)
          ),
          hr(),
          actionButton("btn_process", "5. Process Data", class = "btn-success w-100")
        ),
      ), 
      ##### 1.2.2. Main Content Area #####
      card(
        card_header("Each Patient Review"),
        card_body(DT::dataTableOutput("data_preview_table"))
      )
    )
  )
)


##### 2. Server Logic #####
server <- function(input, output, session) {
  
  ##### 2.1. Server Logic for Input Tab ######
  
  ##### 2.1.1. Initialize reactiveValues #####
  rv <- reactiveValues(
    file_info = list(name = NULL, ext = NULL, sheets = NULL, datapath = NULL, ready_for_upload = FALSE),
    uploaded_data = NULL, column_names = NULL, show_mapping = FALSE, processed_data = NULL
  )
  
  ##### 2.1.2. File Input Handling #####
  observeEvent(input$file_browse, {
    req(input$file_browse)
    rv$uploaded_data <- NULL; rv$column_names <- NULL; rv$show_mapping <- FALSE; rv$processed_data <- NULL
    rv$file_info <- list(name = NULL, ext = NULL, sheets = NULL, datapath = NULL, ready_for_upload = FALSE)
    fileinfo <- input$file_browse; path <- fileinfo$datapath; ext <- tolower(tools::file_ext(fileinfo$name))
    validation_error <- tryCatch({
      if (!ext %in% c("csv", "xlsx")) stop("Invalid file type. Please upload CSV or XLSX.")
      if (file.size(path) > 10*1024*1024) stop("File size exceeds 10MB limit.")
      rv$file_info$name <- fileinfo$name; rv$file_info$ext <- ext; rv$file_info$datapath <- path
      if (ext == "xlsx") {
        sheets <- readxl::excel_sheets(path); if (is.null(sheets) || length(sheets) == 0) stop("Could not read sheets from Excel file.")
        rv$file_info$sheets <- sheets; updateSelectInput(session, "sheet_name", choices = sheets, selected = sheets[1]); rv$file_info$ready_for_upload <- TRUE
      } else { rv$file_info$sheets <- NULL; updateSelectInput(session, "sheet_name", choices = character(0)); rv$file_info$ready_for_upload <- TRUE }
      NULL
    }, error = function(e) { e$message })
    if (!is.null(validation_error)) {
      showNotification(paste("File Validation Error:", validation_error), type = "error", duration = 10)
      shinyjs::reset("file_browse"); rv$file_info <- list(name = NULL, ext = NULL, sheets = NULL, datapath = NULL, ready_for_upload = FALSE)
    }
  })
  
  ##### 2.1.3. Conditional UI Logic #####
  output$show_sheet_selector <- reactive({ !is.null(rv$file_info$ext) && rv$file_info$ext == "xlsx" }); outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)
  output$show_upload_button <- reactive({ rv$file_info$ready_for_upload }); outputOptions(output, "show_upload_button", suspendWhenHidden = FALSE)
  output$show_mapping_ui <- reactive({ rv$show_mapping }); outputOptions(output, "show_mapping_ui", suspendWhenHidden = FALSE)
  
  ##### 2.1.4. Data Uploading ######
  observeEvent(input$btn_upload, {
    req(rv$file_info$datapath)
    ext <- rv$file_info$ext
    path <- rv$file_info$datapath 
    sheet <- if(ext == "xlsx") input$sheet_name else NULL
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
        df_read <- utils::read.csv(path, stringsAsFactors = FALSE, header = TRUE, check.names = FALSE) 
        }
      if (is.null(df_read) || ncol(df_read) == 0) stop("Failed to read data or data is empty.")
      
      rv$uploaded_data <- dplyr::mutate_all(df_read, as.character)
      rv$column_names <- colnames(rv$uploaded_data)
      rv$show_mapping <- TRUE
      showNotification(paste0("Successfully uploaded '", rv$file_info$name, if(!is.null(sheet)) paste(" (Sheet:", sheet, ")") else "", "'."), type = "message")
    }, error = function(e) {
      showNotification(paste("Error Reading File:", e$message), type = "error", duration = 10)
      rv$uploaded_data <- NULL; rv$column_names <- NULL; rv$show_mapping <- FALSE
    })
  })
  
  ##### 2.1.5. Update Picker Inputs ######
  observeEvent(rv$show_mapping, { # Trigger when mapping UI is shown
    cols <- rv$column_names; req(cols)
    
    # Define potential matches (customize these)
    pid_matches <- c("PID", "Patient ID", "Study ID", "patient_id", "subject_id")
    sid_matches <- c("SID", "Sample ID", "sample_id", "specimen_id")
    date_matches <- c("Sample Date", "Date Sample", "Sampling Date", "Date Sampling", "sample_date", 
                      "collection_date", "sampling_date","date_sampling")
    type_matches <- c("Sample Type", "Type Sample", "sample_type", "specimen_type", "source")
    pathogen_matches <- c("Pathogen", "Bacteria", "Bacteria Name", "Name Pathogen", "organism")
    ab_name_matches <- c("AB", "Antibiotics", "Name Antibiotics", "antibiotic", "drug", "agent")
    mic_matches <- c("MIC", "Minimum Inhibitory Concentration", "mic_value") 
    zone_matches <- c("Zone Size", "Disk", "disk_diameter", "zone_diameter")
    interp_matches <- c("Interpretation", "SIR", "RIS", "breakpoint_result") 
    method_matches <- c("Methods", "Test Methods", "method", "ast_method")
    ab_cols_matches <- unique(c(antimicrobials$name, antimicrobials$ab))
    
    #Update inputs using detect_column if available, otherwise NULL selected
    updateVirtualSelect(session=session, inputId = "PID", choices = cols, selected = detect_column(cols, pid_matches))
    updateVirtualSelect(session=session, inputId = "SID", choices = cols, selected = detect_column(cols, sid_matches))
    updateVirtualSelect(session=session, inputId = "Sample_Date", choices = cols, selected = detect_column(cols, date_matches))
    updateVirtualSelect(session=session, inputId = "Sample_Type", choices = cols, selected = detect_column(cols, type_matches))
    updateVirtualSelect(session=session, inputId = "Pathogen", choices = cols, selected = detect_column(cols, pathogen_matches))
    updateVirtualSelect(session=session, inputId = "AB_cols", choices = cols, selected = detect_column(cols, ab_cols_matches))
    updateVirtualSelect(session=session, inputId = "AB_name", choices = cols, selected = detect_column(cols, ab_name_matches))
    updateVirtualSelect(session=session, inputId = "MIC", choices = cols, selected = detect_column(cols, mic_matches))
    updateVirtualSelect(session=session, inputId = "Zone_Size", choices = cols, selected = detect_column(cols, zone_matches))
    updateVirtualSelect(session=session, inputId = "Interpretation", choices = cols, selected = detect_column(cols, interp_matches))
    updateVirtualSelect(session=session, inputId = "Methods", choices = cols, selected = detect_column(cols, method_matches))
  })
  
  ##### 2.1.6 Process Data Button #####
  observeEvent(input$btn_process, {
    
    rv$processed_data <- NULL
    
    tryCatch({
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
      
      core_map <- list(PatientID = input$PID, SampleID = input$SID, SampleDate = input$Sample_Date, SampleType = input$Sample_Type, Pathogen = input$Pathogen )
      core_map <- core_map[sapply(core_map, function(x) !is.null(x) && x != "")]
      
      if(input$data_structure == 'wide'){
        
        df_processed <- rv$uploaded_data %>% select(all_of(unname(unlist(core_map))), all_of(input$AB_cols)) %>%
          tidyr::pivot_longer(cols = all_of(input$AB_cols), names_to = "Antibiotic_Name", values_to = "Result", values_drop_na = TRUE) 
        
        colnames(df_processed) <- c(na.omit(names(core_map)[match(colnames(df_processed), unname(unlist(core_map)))]),
                                    "Antibiotic_Name","Result")
        
        df_processed <- df_processed %>% mutate(
          mo_code = as.mo(Pathogen),
          ab_code = as.ab(Antibiotic_Name),
          AntibioticName = ab_name(ab_code),
          MIC = as.mic(str_extract(Result, "(<=|>=|<|>)?\\s*[0-9.]+")),
          TempInterpretation = as.sir(str_extract(Result, "R|S|I|SSD|NI")),
          Interpretation = if_else(!is.na(MIC) & is.na(TempInterpretation),
            as.sir(
              MIC,
              mo = mo_code,
              ab = ab_code,
              guideline = input$guideline
            ),
            TempInterpretation
          )
        ) %>% 
          select(-c(TempInterpretation,Result,Antibiotic_Name))
      } else { # long format
        long_map <- list(Antibiotic_Name = input$AB_name, MIC = input$MIC, Zone = input$Zone_Size, TempInterpretation = input$Interpretation, Method = input$Methods)
        long_map <- long_map[sapply(long_map, function(x) !is.null(x) && x != "")]
        
        df_processed <- rv$uploaded_data %>% select(all_of(unname(unlist(core_map))),all_of(unname(unlist(long_map))))
        
        colnames(df_processed) <- c(na.omit(names(core_map)[match(colnames(df_processed), unname(unlist(core_map)))]),
                                    na.omit(names(long_map)[match(colnames(df_ast_part), unname(unlist(long_map)))]))
        
        df_processed <- df_processed %>%
          mutate(
            mo_code = as.mo(Pathogen),
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
              ),
              !is.na(Zone) & is.na(TempInterpretation) ~ as.sir(
                Zone,
                mo = mo_code,
                ab = ab_code,
                guideline = input$guideline
              ),
              as.sir(TempInterpretation)
            )
          ) %>% 
          select(-c(TempInterpretation,Antibiotic_Name))
      }
      
      # --- Data Type Conversion & Cleaning ---
      if ("SampleDate" %in% names(df_processed)) {
        date_formats_to_try <- c("%Y-%m-%d",
                                 "%d/%m/%Y",
                                 "%m/%d/%Y",
                                 "%d-%b-%y",
                                 "%Y/%m/%d",
                                 "%b %d %Y",
                                 "%d-%b-%Y") # Expanded formats
        df_processed <- df_processed %>% mutate(
          Sample_Date_Format = lubridate::parse_date_time2(SampleDate, orders = date_formats_to_try) %>% as.Date(),
          Sample_Date_Final = if_else(
            is.na(Sample_Date_Format),
            as.Date(SampleDate, origin = "1900-01-01"),
            Sample_Date_Format
          ),
          SampleDate = Sample_Date_Final
        ) %>%
          select(-c(Sample_Date_Format, Sample_Date_Final))
      }

      # --- Store Processed Data ---
      rv$processed_data <- df_processed
      removeNotification("processing_msg"); showNotification("Data processed successfully! Dashboard updated.", type="message")
      
    }, error = function(e){
      removeNotification("processing_msg"); showNotification(paste("Error during processing:", e$message), type = "error", duration = 10); rv$processed_data <- NULL
    })
  })
  
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
  output$raw_data_view_table <- DT::renderDataTable({
    validate(need(
      rv$uploaded_data,
      "Upload data using the sidebar to see preview."
    ))
    datatable(
      rv$uploaded_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
}


##### 3. Run App #####
shinyApp(ui = ui, server = server)