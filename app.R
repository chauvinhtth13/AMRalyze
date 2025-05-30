###### 0. Load Libraries #####
# Core Shiny framework
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)

# Theming and icons
library(bslib)
library(bsicons)

# Data manipulation and general-purpose tidyverse tools
library(tidyverse)    # Includes dplyr, tidyr, stringr, purrr, readr, etc.
library(magrittr)     # Pipe operations
library(data.table)   # Fast data manipulation
library(reshape2)     # Data reshaping
library(scales)       # Number formatting

# AMR analysis
library(AMR)          # Antimicrobial resistance data and analysis

# File reading and writing
library(readxl)       # Read Excel files
library(openxlsx)     # Write Excel files
library(tools)        # File extension checking

# Data display and summaries
library(DT)           # Interactive tables
library(gt)           # Table generation
library(gtsummary)    # Summary tables

# Plotting
library(plotly)       # Interactive plots


source("utils/ui_utils.R", local = TRUE)
source("utils/amr_utils.R", local = TRUE)
source("ui/ui_tab_dashboard.R",local = TRUE)

##### 1. UI Definition #####
ui <- page_navbar(
  title = "AMRalyze Dashboard",
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  header = tags$head(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    tags$script(src = "www/features.js"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
  ),
  ##### 1.1. Dashboard Tab #####
  dashboard(),
  
  ##### 1.2. Data Input Tab #####
  nav_panel(
    title = "Data Input",
    icon = bsicons::bs_icon("upload"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = "35%",
        
        # Step 1: File Upload
        div(
          h4("Step 1: Upload Data File", class = "step-header required-field"),
          p(
            "Upload your antimicrobial susceptibility test data (CSV or Excel format)",
            class = "info-text"
          ),
          fileInput(
            "file_browse",
            NULL,
            accept = c(
              ".csv",
              ".xlsx",
              "text/csv",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            ),
            buttonLabel = "Choose File",
            placeholder = "No file selected (Max 10MB)"
          ),
          
          # Sheet selector for Excel files
          conditionalPanel(
            "output.show_sheet_selector == true",
            selectInput("sheet_name", "Select Excel Sheet:", choices = NULL)
          ),
          
          # Upload button
          conditionalPanel(
            "output.show_upload_button == true",
            actionButton(
              "btn_upload",
              "Load & Preview Data",
              class = "btn-primary w-100 mb-3",
              icon = icon("eye")
            )
          )
        ),
        
        hr(),
        
        # Step 2: Configure Analysis
        conditionalPanel(
          condition = "output.show_mapping_ui == true",
          
          div(
            h4("Step 2: Analysis Configuration", class = "step-header"),
            
            # Guideline Selection
            h5("AST Guidelines", class = "required-field"),
            p("Select the clinical breakpoint guidelines to use", class = "info-text"),
            pickerInput(
              "guideline",
              NULL,
              choices = sort(unique(clinical_breakpoints$guideline)),
              selected = get_newest_guideline(),
              options = list(`actions-box` = TRUE, `live-search` = TRUE)
            ),
            
            br(),
            
            # Data Structure Selection
            h5("Data Structure", class = "required-field"),
            p("How is your AST data organized?", class = "info-text"),
            radioButtons(
              "data_structure",
              NULL,
              choices = list(
                "Long format (recommended) - One row per test result" = "long",
                "Wide format - One row per isolate with multiple columns" = "wide"
              ),
              selected = "long"
            )
          ),
          
          br(),
          
          # Step 3: Column Mapping
          div(
            h4("Step 3: Column Mapping", class = "step-header"),
            p("Map your data columns to the required fields", class = "info-text"),
            
            # Essential columns
            h6("Essential Fields", style = "color: #dc3545; font-weight: bold;"),
            
            virtualSelectInput(
              "PID",
              "Patient ID Column *",
              choices = NULL,
              multiple = FALSE,
              search = TRUE,
              placeholder = "Select patient identifier column"
            ),
            
            virtualSelectInput(
              "SID",
              "Sample ID Column *",
              choices = NULL,
              multiple = FALSE,
              search = TRUE,
              placeholder = "Select sample identifier column"
            ),
            
            virtualSelectInput(
              "Pathogen",
              "Pathogen Column *",
              choices = NULL,
              multiple = FALSE,
              search = TRUE,
              placeholder = "Select organism/pathogen column"
            ),
            
            br(),
            
            # Optional columns
            h6("Optional Fields", style = "color: #6c757d; font-weight: bold;"),
            
            virtualSelectInput(
              "Sample_Date",
              "Sampling Date Column",
              choices = NULL,
              multiple = FALSE,
              search = TRUE,
              showValueAsTags = TRUE,
              hideClearButton = FALSE,
              placeholder = "Select date column (optional)"
            ),
            
            virtualSelectInput(
              "Sample_Type",
              "Sample Type Column",
              choices = NULL,
              multiple = FALSE,
              search = TRUE,
              showValueAsTags = TRUE,
              hideClearButton = FALSE,
              placeholder = "Select specimen type column (optional)"
            ),
            
            br(),
            
            # AST-specific columns based on data structure
            h6("AST Result Fields", style = "color: #0d6efd; font-weight: bold;"),
            
            conditionalPanel(
              "input.data_structure == 'wide'",
              p("Select all columns containing antibiotic test results", class = "info-text"),
              virtualSelectInput(
                "AB_cols",
                "Antibiotic Result Columns *",
                choices = NULL,
                multiple = TRUE,
                search = TRUE,
                showValueAsTags = TRUE,
                placeholder = "Select all antibiotic columns...",
                disableSelectAll = FALSE,
                optionsCount = 12
              )
            ),
            
            conditionalPanel(
              "input.data_structure == 'long'",
              virtualSelectInput(
                "AB_name",
                "Antibiotic Name Column *",
                choices = NULL,
                multiple = FALSE,
                search = TRUE,
                placeholder = "Select antibiotic name column"
              ),
              
              virtualSelectInput(
                "MIC",
                "MIC Value Column",
                choices = NULL,
                multiple = FALSE,
                search = TRUE,
                hideClearButton = FALSE,
                placeholder = "Select MIC column (optional)"
              ),
              
              virtualSelectInput(
                "Zone_Size",
                "Zone Diameter Column",
                choices = NULL,
                multiple = FALSE,
                search = TRUE,
                hideClearButton = FALSE,
                placeholder = "Select zone size column (optional)"
              ),
              
              virtualSelectInput(
                "Interpretation",
                "Interpretation Column",
                choices = NULL,
                multiple = FALSE,
                search = TRUE,
                hideClearButton = FALSE,
                placeholder = "Select S/I/R column (optional)"
              ),
              
              virtualSelectInput(
                "Methods",
                "Test Method Column",
                choices = NULL,
                multiple = FALSE,
                search = TRUE,
                hideClearButton = FALSE,
                placeholder = "Select test method column (optional)"
              )
            )
          ),
          
          br(),
          
          # Process button
          actionButton(
            "btn_process",
            "Process Data",
            class = "btn-success w-100",
            style = "height: 50px; font-size: 16px;",
            icon = icon("cogs")
          )
        )
      ),
      
      ##### 1.2.2. Main Content Area #####
      div(
        # Data preview and validation section
        card(
          card_header(div(
            h4("Data Review & Validation", class = "mb-0"),
            conditionalPanel(
              "output.show_patient_ui == true",
              span(
                downloadButton("download_processed_data", "Download Processed Data", class = "btn-outline-success btn-sm"),
                style = "float: right;"
              )
            )
          ), class = "bg-light"),
          card_body(
            # Processing status
            conditionalPanel(
              "output.show_processing_status == true",
              div(
                id = "processing_status",
                h5("Processing Status:", class = "success-message"),
                textOutput("processing_message"),
                br()
              )
            ),
            
            # Data exploration interface
            conditionalPanel(
              condition = "output.show_patient_ui == true",
              
              # Filters
              h5("Explore Your Data"),
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                virtualSelectInput(
                  "PID_show",
                  "Patient ID",
                  choices = NULL,
                  multiple = FALSE,
                  search = TRUE,
                  hideClearButton = FALSE,
                  placeholder = "All patients"
                ),
                virtualSelectInput(
                  "SID_show",
                  "Sample ID",
                  choices = NULL,
                  multiple = FALSE,
                  search = TRUE,
                  hideClearButton = FALSE,
                  placeholder = "All samples"
                ),
                virtualSelectInput(
                  "Pathogen_show",
                  "Pathogen",
                  choices = NULL,
                  multiple = FALSE,
                  search = TRUE,
                  hideClearButton = FALSE,
                  placeholder = "All pathogens"
                ),
                dateRangeInput(
                  "date_range_show",
                  "Date Range",
                  start = NULL,
                  end = NULL
                )
              ),
              
              hr(),
              
              # Sample details
              div(id = "sample_details", fluidRow(
                column(6, strong(textOutput(outputId = "SampleDate_text")), strong(textOutput(outputId = "SampleType_text"))),
                column(
                  6,
                  strong(textOutput(outputId = "Kingdom_text")),
                  strong(textOutput(outputId = "GramStain_text")),
                  strong(textOutput(outputId = "MDR_text"))
                )
              )),
              
              hr(),
              
              # AST Results Table
              h5("Antimicrobial Susceptibility Test Results"),
              DT::dataTableOutput("AST_data_view_table"),
              
              br(),
              
              # Data summary
              h5("Data Summary"),
              fluidRow(column(4, div(
                h6("Total Records"),
                verbatimTextOutput("data_summary_records")
              )), column(4, div(
                h6("Unique Pathogens"),
                verbatimTextOutput("data_summary_pathogens")
              )), column(4, div(
                h6("Unique Antibiotics"),
                verbatimTextOutput("data_summary_antibiotics")
              )))
            ),
            
            # Default message when no data loaded
            conditionalPanel(
              "output.show_patient_ui != true",
              div(
                style = "text-align: center; padding: 50px;",
                h4("No Data Loaded", style = "color: #6c757d;"),
                p("Upload and process your data to begin analysis", style = "color: #6c757d;"),
                icon("upload", style = "font-size: 48px; color: #dee2e6;")
              )
            )
          )
        )
      )
    )
  ),
  
  ##### 1.3. Help Tab #####
  nav_panel(
    title = "Help",
    icon = bsicons::bs_icon("question-circle"),
    card(
      card_header("AMRalyze Dashboard"),
      card_body(
        p(
          "AMRalyze Dashboard is an interactive web-based tool for analysing Antimicrobial Resistance (AMR) surveillance data. It enables users to upload raw data, process it according to selected guidelines, and visualise key AMR metrics and resistance patterns."
        ),
        h4("Workflow"),
        tags$ol(
          tags$li(
            tags$strong("Upload Data:"),
            " Users upload AMR data (CSV/XLSX) via the 'Input' tab."
          ),
          tags$li(
            tags$strong("Map Columns:"),
            " Users map their data columns to standard fields (Patient ID, Pathogen, etc.)."
          ),
          tags$li(
            tags$strong("Configure Processing:"),
            " Users select an AST guideline (e.g., CLSI, EUCAST) and specify the data format (wide/long)."
          ),
          tags$li(
            tags$strong("Process Data:"),
            " The application cleans, standardises (using the AMR package), interprets SIR results, and calculates MDR status."
          ),
          tags$li(
            tags$strong("Visualise & Review:"),
            " The 'Dashboard' provides summary stats, pathogen plots, and resistance tables. The 'Input' tab allows a detailed review of individual isolates."
          ),
          tags$li(
            tags$strong("Download:"),
            " Processed data and MDR results can be downloaded as an Excel file."
          )
        ),
        h4("Input Data Formats"),
        p(
          "The application requires specific columns in your uploaded data, depending on whether you choose the 'Wide' or 'Long' format on the 'Input' tab."
        ),
        h5(strong("Wide Format ('One row per isolate')")),
        p(
          "In this format, each row contains all information for a single isolate, including results for multiple antibiotics in separate columns."
        ),
        tags$strong("Required Mapping Inputs:"),
        tags$ul(
          tags$li(
            "Patient ID Column: ",
            code("PID"),
            " - Unique identifier for the patient."
          ),
          tags$li(
            "Sample ID Column: ",
            code("SID"),
            " - Unique identifier for the sample/isolate."
          ),
          tags$li(
            "Pathogen Column: ",
            code("Pathogen"),
            " - Name of the identified microorganism."
          ),
          tags$li(
            "Select ALL Antibiotic Result Columns: ",
            code("AB_cols"),
            " - You must select ",
            tags$strong("all"),
            " columns that contain the results for any antibiotic test (e.g., columns named ",
            code("AMP"),
            ", ",
            code("CIP"),
            ", ",
            code("AMX_NM"),
            ", ",
            code("CIP_ND30"),
            ", etc.). The values in these columns should be the result (e.g., '16', '<=0.5', 'R', 'S', '25')."
          )
        ),
        tags$strong("Optional Mapping Inputs:"),
        tags$ul(
          tags$li(
            "Sampling Date Column: ",
            code("Sample_Date"),
            " - Date the sample was collected."
          ),
          tags$li(
            "Sample Type Column: ",
            code("Sample_Type"),
            " - Type of specimen (e.g., 'Blood', 'Urine')."
          )
        ),
        p(
          tags$em(
            "Note: If your antibiotic column headers combine an ",
            tags$strong("antibiotic name or code (often a 3-letter WHONET code)"),
            " with a method/result type suffix like ",
            code("_NM"),
            ", ",
            code("_NE"),
            ", or ",
            code("_ND"),
            " (e.g., ",
            code("AMP_NM"),
            "), the application attempts to parse the antibiotic name (e.g., 'AMP') and interpret the value based on the suffix ('_NM' for MIC, '_ND' for Disk, '_NE' for E-test). Select these combined-name columns under 'Select ALL Antibiotic Result Columns'. If suffixes are absent (e.g., just ",
            code("AMP"),
            "), the application treats the header as the antibiotic name and primarily assumes numerical values are MICs, while attempting to extract S/I/R codes if present."
          )
        ),
        # Simplified explanation
        h5(strong("Long Format ('One row per test')")),
        p(
          "In this format, each row represents a single antibiotic test result for an isolate."
        ),
        tags$strong("Required Mapping Inputs:"),
        tags$ul(
          tags$li("Patient ID Column: ", code("PID")),
          tags$li("Sample ID Column: ", code("SID")),
          tags$li("Pathogen Column: ", code("Pathogen")),
          tags$li(
            "Antibiotic Name Column: ",
            code("AB_name"),
            " - Name of the antibiotic tested."
          ),
          tags$li(
            code("MIC Column"),
            tags$strong(" OR "),
            code("Zone Size Column"),
            tags$strong(" OR "),
            code("Interpretation Column"),
            " - Map at least one result column. Numerical columns (MIC/Zone) are needed for SIR calculation if Interpretation isn't provided or is incomplete."
          ) # Clarified OR logic
        ),
        tags$strong("Optional Mapping Inputs:"),
        tags$ul(
          tags$li("Sampling Date Column: ", code("Sample_Date")),
          tags$li("Sample Type Column: ", code("Sample_Type")),
          tags$li(
            "Interpretation Column: ",
            code("Interpretation"),
            " - Pre-existing S/I/R. Takes precedence if mapped."
          ),
          tags$li(
            "Test Method Column: ",
            code("Methods"),
            " - Method used (e.g., 'MIC', 'Disk'). For context."
          )
        ),
        p(
          tags$em(
            "Note: For long format, clearly separating the antibiotic name and its result (MIC, Zone, or Interpretation) into distinct columns is crucial."
          )
        ),
        h4("Key Features"),
        tags$ul(
          tags$li("Supports CSV and XLSX file uploads."),
          tags$li("Handles wide and long AST data formats."),
          tags$li("Utilises the 'AMR' package for robust data processing."),
          tags$li("Applies user-selected AST guidelines for interpretation."),
          tags$li(
            "Calculates Multidrug Resistance (MDR) based on CMI2012 definition."
          ),
          # Be specific
          tags$li(
            "Offers interactive visualisations and publication-ready tables."
          ),
          tags$li("Allows detailed data review and download.")
        ),
        h4("Core Technology"),
        tags$ul(
          tags$li("Language/Framework: R / Shiny"),
          tags$li("AMR Logic: 'AMR' package"),
          tags$li("Data Handling: 'tidyverse' suite, 'data.table'"),
          # Added data.table
          tags$li("Tables & Plots: 'DT', 'gt', 'gtsummary', 'plotly'"),
          tags$li("User Interface: 'bslib', 'shinyWidgets', 'shinyjs'")
        ),
        h4("Live Application Link"),
        p("Access the live version:"),
        p(
          tags$a(
            href = "https://chauvinh.shinyapps.io/amralyze/",
            target = "_blank",
            "https://chauvinh.shinyapps.io/amralyze/"
          )
        ),
        h4("Contact & Contribution"),
        p("For inquiries or contributions, contact Chau Vinh:"),
        p(
          tags$a(href = "mailto:chauvinhtth13@gmail.com", "chauvinhtth13@gmail.com")
        ),
        h4("Source Code & Citation"),
        p("Source code is on GitHub. If used, please cite:"),
        p(
          "Vinh, C. (2025). ",
          tags$em(
            "AMRalyze: Interactive Antimicrobial Resistance Data Analysis Dashboard "
          ),
          "[Software]. Source code: ",
          tags$a(href = "https://github.com/chauvinhtth13/AMRalyze", target = "_blank", "GitHub Repository"),
          ". Live application: ",
          tags$a(href = "https://chauvinh.shinyapps.io/amralyze/", target = "_blank", "ShinyApp Link"),
          "."
        ),
        h5("BibTeX Entry:"),
        tags$pre(tags$code(
          HTML(
            "@misc{Vinh2025AMRalyze,
  author         = {Vinh, Chau},
  title          = {{AMRalyze: Interactive Antimicrobial Resistance Data Analysis Dashboard}},
  year           = {2025},
  howpublished = {\\url{https://chauvinh.shinyapps.io/amralyze/}},
  note           = {Source code available at \\url{https://github.com/chauvinhtth13/AMRalyze}}
}"
          )
        )),
        p(
          tags$em(
            "(Note: Using \\url{} requires the \\usepackage{url} or \\usepackage{hyperref} package in LaTeX.)"
          )
        ),
        hr(),
        p(tags$em(
          "UI Rendered Timestamp (approximate): ",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
        ))
      ) # End card_body
    ) # End card
  )
)
##### 2. Server Logic #####
server <- function(input, output, session) {
  ##### 2.1. Server Logic for Input Tab ######
  
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