###### 0. Load Libraries #####
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(shinybusy)
library(DT)
library(AMR)         # Assumed source of AMR-specific data/functions
library(tidyverse)   # Includes dplyr, stringr, tidyr, purrr, lubridate, etc.
library(readxl)      # For reading Excel files
library(openxlsx)    # For writing Excel files
library(tools)       # For file extension checking
library(scales)      # For comma formatting and plotting scales
library(gt)
library(gtsummary)
library(plotly)
library(data.table)  # For fread

source("utils/ui_utils.R", local = TRUE)
source("utils/amr_utils.R", local = TRUE)

##### 1. UI Definition #####
ui <- page_navbar(
  title = "AMRalyze Dashboard",
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  header = css_function(),
  
  ##### 1.1. Dashboard Tab #####
  nav_panel(
    title = "Dashboard",
    # -- Row 1: Value Boxes --
    layout_columns(
      col_widths = c(4, 4, 4),
      # min_height = "120px", # Let content define height or set on value_box
      # max_height = "120px",
      fill = TRUE,
      value_box(
        # height = "120px",
        title = "Total Records Uploaded",
        value = textOutput("total_records"),
        showcase = bsicons::bs_icon("file-earmark-ruled"),
        theme = "primary"
      ),
      value_box(
        # height = "120px",
        title = "Total Patients",
        value = textOutput("total_patients"),
        showcase = bsicons::bs_icon("person-vcard"),
        theme = "info"
      ),
      value_box(
        # height = "120px",
        title = "Total Samples",
        value = textOutput("total_samples"),
        showcase = bsicons::bs_icon("radioactive"),
        theme = "success"
      )
    ),
    # -- Row 2: Pathogen Summary Plot --
    card(
      # Removed duplicate height settings
      min_height = "500px", # Set one consistent height if needed
      card_header("Summary Pathogen"),
      card_body(
        fill = TRUE,
        plotlyOutput("pathogen_summary_plot", width = "100%", height = "450px") # Adjusted height slightly
      )
    ),
    
    # -- Row 3: Antimicrobial Resistance Patterns --
    card(
      # Removed duplicate height settings
      min_height = "1000px", # Set one consistent height if needed
      card_header("Antimicrobial resistance patterns (% Resistant)"), # Corrected header text
      navset_card_tab(
        id = "ast_card_tabs",
        nav_panel(
          title = "Gram-Negative",
          layout_sidebar(
            sidebar = sidebar(
              width = "20%",
              virtualSelectInput(
                "list_gram_negative", "Choose Pathogen", choices = NULL,
                multiple = TRUE, search = TRUE, showValueAsTags = TRUE,
                placeholder = "Select pathogen...", disableSelectAll = FALSE
              )
            ),
            card(card_body(fill = TRUE, gt_output("ast_gram_negative_table")))
          )
        ),
        nav_panel(
          title = "Gram-Positive",
          layout_sidebar(
            sidebar = sidebar(
              width = "20%",
              virtualSelectInput(
                "list_gram_positive", "Choose Pathogen", choices = NULL,
                multiple = TRUE, search = TRUE, showValueAsTags = TRUE,
                placeholder = "Select pathogen...", disableSelectAll = FALSE
              )
            ),
            card(card_body(fill = TRUE, gt_output("ast_gram_positive_table")))
          )
        ),
        nav_panel(
          title = "Fungal",
          layout_sidebar(
            sidebar = sidebar(
              width = "20%",
              virtualSelectInput(
                "list_fungal", "Choose Pathogen", choices = NULL,
                multiple = TRUE, search = TRUE, showValueAsTags = TRUE,
                placeholder = "Select pathogen...", disableSelectAll = FALSE
              )
            ),
            card(card_body(fill = TRUE, gt_output("ast_fungal_table")))
          )
        )
      ) # End ast_card_tabs navset
    ), # End AST card
    
    # -- Row 4: MIC Distribution --
    card(
      # Removed duplicate height settings
      min_height = "1000px", # Set one consistent height if needed
      card_header("Summary Minimum Inhibitory Concentration (MIC) Distribution"),
      navset_card_tab(
        id = "mic_card_tabs", # CORRECTED ID
        nav_panel(
          title = "Gram-Negative",
          layout_sidebar(
            sidebar = sidebar(
              width = "20%",
              virtualSelectInput(
                "list_gram_negative_mic", "Choose Pathogen", choices = NULL,
                multiple = TRUE, search = TRUE, showValueAsTags = TRUE,
                placeholder = "Select pathogen...", disableSelectAll = FALSE
              ),
              virtualSelectInput(
                "list_ab_gram_negative_mic", "Choose Antibiotics", choices = NULL,
                multiple = TRUE, search = TRUE, showValueAsTags = TRUE,
                placeholder = "Select antibiotics...", disableSelectAll = FALSE
              )
            ),
            # Placeholder for MIC output - assuming this will be added later
            card(card_body(fill = TRUE, gt_output("ast_gram_negative_mic_table")))
          )
        ),
        nav_panel(
          title = "Gram-Positive",
          layout_sidebar(
            sidebar = sidebar(
              width = "20%",
              virtualSelectInput(
                "list_gram_positive_mic", "Choose Pathogen", choices = NULL,
                multiple = TRUE, search = TRUE, showValueAsTags = TRUE,
                placeholder = "Select pathogen...", disableSelectAll = FALSE
              ),
              virtualSelectInput(
                "list_ab_gram_positive_mic", "Choose Antibiotics", choices = NULL,
                multiple = TRUE, search = TRUE, showValueAsTags = TRUE,
                placeholder = "Select antibiotics...", disableSelectAll = FALSE
              )
            ),
            # Placeholder for MIC output - assuming this will be added later
            card(card_body(fill = TRUE, gt_output("ast_gram_positive_mic_table")))
          )
        )
      ) # End mic_card_tabs navset
    ) # End MIC card
  ), # End Dashboard nav_panel
  
  ##### 1.2. Input Tab #####
  nav_panel(
    title = "Input",
    layout_sidebar(
      sidebar = sidebar(
        width = "30%",
        fileInput(
          "file_browse", h5("1. Upload CSV/XLSX File (Max 10MB)"),
          accept = c(".csv", ".xlsx", "text/csv", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
          buttonLabel = "Browse...", placeholder = "No file selected"
        ),
        conditionalPanel(
          "output.show_sheet_selector == true",
          selectInput("sheet_name", "Select Sheet", choices = NULL)
        ),
        conditionalPanel(
          "output.show_upload_button == true",
          actionButton("btn_upload", "Upload & Preview Data", class = "btn-primary w-100")
        ),
        conditionalPanel(
          condition = "output.show_mapping_ui == true",
          hr(),
          h5("2. Choose Guideline:"),
          pickerInput(
            "guideline", "AST Guidelines",
            choices = sort(unique(AMR::clinical_breakpoints$guideline)), # Explicit namespace
            selected = tryCatch(get_newest_guideline(), error = function(e) NULL), # Added tryCatch for safety
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          ),
          hr(),
          h5("3. Map Columns and Process Data:"),
          p("Select the corresponding columns from your data. Auto-detection attempted."),
          virtualSelectInput("PID", "Patient ID Column", choices = NULL, multiple = FALSE, search = TRUE),
          virtualSelectInput("SID", "Sample ID Column", choices = NULL, multiple = FALSE, search = TRUE),
          virtualSelectInput("Sample_Date", "Sampling Date Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
          virtualSelectInput("Sample_Type", "Sample Type Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
          virtualSelectInput("Pathogen", "Pathogen Column", choices = NULL, multiple = FALSE, search = TRUE),
          radioButtons(
            "data_structure", "AST Data Structure:",
            choices = list("Wide (One row per isolate)" = "wide", "Long (One row per test)" = "long"),
            selected = "long"
          ),
          conditionalPanel(
            "input.data_structure == 'wide'",
            virtualSelectInput(
              "AB_cols", "Select ALL Antibiotic Result Columns", choices = NULL, multiple = TRUE, search = TRUE,
              showValueAsTags = TRUE, placeholder = "Select columns...", disableSelectAll = FALSE, optionsCount = 8
            )
          ),
          conditionalPanel(
            "input.data_structure == 'long'",
            virtualSelectInput("AB_name", "Antibiotic Name Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
            virtualSelectInput("MIC", "MIC Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
            virtualSelectInput("Zone_Size", "Zone Size Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
            virtualSelectInput("Interpretation", "Interpretation Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE),
            virtualSelectInput("Methods", "Test Method Column", choices = NULL, multiple = FALSE, search = TRUE, showValueAsTags = TRUE, hideClearButton = FALSE)
          ),
          actionButton("btn_process", "Process Data", class = "btn-success w-100")
        ) # End conditionalPanel for mapping UI
      ), # End sidebar
      
      # Main Content Area for Input Tab
      card(
        card_header("Processed Data Review"),
        card_body(
          downloadButton("download_processed_data", "Download Processed Data", class = "btn-sm"),
          hr(),
          conditionalPanel(
            condition = "output.show_patient_ui == true",
            layout_columns(
              col_widths = c(4, 4, 4),
              virtualSelectInput("PID_show", "Filter Patient ID", choices = NULL, multiple = FALSE, search = TRUE, hideClearButton = FALSE),
              virtualSelectInput("SID_show", "Filter Sample ID", choices = NULL, multiple = FALSE, search = TRUE, hideClearButton = FALSE),
              virtualSelectInput("Pathogen_show", "Filter Pathogen", choices = NULL, multiple = FALSE, search = TRUE, hideClearButton = FALSE)
            ),
            hr(),
            strong(textOutput(outputId = "SampleDate_text")),
            strong(textOutput(outputId = "SampleType_text")),
            strong(textOutput(outputId = "Kingdom_text")),
            strong(textOutput(outputId = "GramStain_text")),
            strong(textOutput(outputId = "MDR_text")),
            hr(),
            strong(p("Antimicrobial Susceptibility Test Results:")),
            DT::dataTableOutput("AST_data_view_table")
          ) # End conditionalPanel for patient UI
        ) # End card_body
      ) # End card
    ) # End layout_sidebar
  ), # End Input nav_panel
  
  ##### 1.3. About Tab #####
  nav_panel(
    title = "About",
    icon = bsicons::bs_icon("info-circle-fill"),
    card(
      card_header("AMRalyze Dashboard"),
      card_body(
        p("AMRalyze Dashboard is an interactive web-based tool for analysing Antimicrobial Resistance (AMR) surveillance data. It enables users to upload raw data, process it according to selected guidelines, and visualise key AMR metrics and resistance patterns."),
        h4("Workflow"),
        tags$ol(
          tags$li(tags$strong("Upload Data:"), " Users upload AMR data (CSV/XLSX) via the 'Input' tab."),
          tags$li(tags$strong("Map Columns:"), " Users map their data columns to standard fields (Patient ID, Pathogen, etc.)."),
          tags$li(tags$strong("Configure Processing:"), " Users select an AST guideline (e.g., CLSI, EUCAST) and specify the data format (wide/long)."),
          tags$li(tags$strong("Process Data:"), " The application cleans, standardises (using the AMR package), interprets SIR results, and calculates MDR status."),
          tags$li(tags$strong("Visualise & Review:"), " The 'Dashboard' provides summary stats, pathogen plots, and resistance tables. The 'Input' tab allows a detailed review of individual isolates."),
          tags$li(tags$strong("Download:"), " Processed data and MDR results can be downloaded as an Excel file.")
        ),
        h4("Input Data Formats"),
        p("The application requires specific columns in your uploaded data, depending on whether you choose the 'Wide' or 'Long' format on the 'Input' tab."),
        h5(strong("Wide Format ('One row per isolate')")),
        p("In this format, each row contains all information for a single isolate, including results for multiple antibiotics in separate columns."),
        tags$strong("Required Mapping Inputs:"),
        tags$ul(
          tags$li("Patient ID Column: ", code("PID"), " - Unique identifier for the patient."),
          tags$li("Sample ID Column: ", code("SID"), " - Unique identifier for the sample/isolate."),
          tags$li("Pathogen Column: ", code("Pathogen"), " - Name of the identified microorganism."),
          tags$li("Select ALL Antibiotic Result Columns: ", code("AB_cols"), " - You must select ", tags$strong("all"), " columns that contain the results for any antibiotic test (e.g., columns named ", code("AMP"), ", ", code("CIP"), ", ", code("AMX_NM"), ", ", code("CIP_ND30"), ", etc.). The values in these columns should be the result (e.g., '16', '<=0.5', 'R', 'S', '25').")
        ),
        tags$strong("Optional Mapping Inputs:"),
        tags$ul(
          tags$li("Sampling Date Column: ", code("Sample_Date"), " - Date the sample was collected."),
          tags$li("Sample Type Column: ", code("Sample_Type"), " - Type of specimen (e.g., 'Blood', 'Urine').")
        ),
        p(
          tags$em(
            "Note: If your antibiotic column headers combine an ",
            tags$strong("antibiotic name or code (often a 3-letter WHONET code)"),
            " with a method/result type suffix like ",
            code("_NM"), ", ", code("_NE"), ", or ", code("_ND"),
            " (e.g., ", code("AMP_NM"), "), the application attempts to parse the antibiotic name (e.g., 'AMP') and interpret the value based on the suffix ('_NM' for MIC, '_ND' for Disk, '_NE' for E-test). Select these combined-name columns under 'Select ALL Antibiotic Result Columns'. If suffixes are absent (e.g., just ", code("AMP"), "), the application treats the header as the antibiotic name and primarily assumes numerical values are MICs, while attempting to extract S/I/R codes if present."
          )
        ), # Simplified explanation
        h5(strong("Long Format ('One row per test')")),
        p("In this format, each row represents a single antibiotic test result for an isolate."),
        tags$strong("Required Mapping Inputs:"),
        tags$ul(
          tags$li("Patient ID Column: ", code("PID")),
          tags$li("Sample ID Column: ", code("SID")),
          tags$li("Pathogen Column: ", code("Pathogen")),
          tags$li("Antibiotic Name Column: ", code("AB_name"), " - Name of the antibiotic tested."),
          tags$li(code("MIC Column"), tags$strong(" OR "), code("Zone Size Column"), tags$strong(" OR "), code("Interpretation Column"), " - Map at least one result column. Numerical columns (MIC/Zone) are needed for SIR calculation if Interpretation isn't provided or is incomplete.") # Clarified OR logic
        ),
        tags$strong("Optional Mapping Inputs:"),
        tags$ul(
          tags$li("Sampling Date Column: ", code("Sample_Date")),
          tags$li("Sample Type Column: ", code("Sample_Type")),
          tags$li("Interpretation Column: ", code("Interpretation"), " - Pre-existing S/I/R. Takes precedence if mapped."),
          tags$li("Test Method Column: ", code("Methods"), " - Method used (e.g., 'MIC', 'Disk'). For context.")
        ),
        p(tags$em("Note: For long format, clearly separating the antibiotic name and its result (MIC, Zone, or Interpretation) into distinct columns is crucial.")),
        h4("Key Features"),
        tags$ul(
          tags$li("Supports CSV and XLSX file uploads."),
          tags$li("Handles wide and long AST data formats."),
          tags$li("Utilises the 'AMR' package for robust data processing."),
          tags$li("Applies user-selected AST guidelines for interpretation."),
          tags$li("Calculates Multidrug Resistance (MDR) based on CMI2012 definition."), # Be specific
          tags$li("Offers interactive visualisations and publication-ready tables."),
          tags$li("Allows detailed data review and download.")
        ),
        h4("Core Technology"),
        tags$ul(
          tags$li("Language/Framework: R / Shiny"),
          tags$li("AMR Logic: 'AMR' package"),
          tags$li("Data Handling: 'tidyverse' suite, 'data.table'"), # Added data.table
          tags$li("Tables & Plots: 'DT', 'gt', 'gtsummary', 'plotly'"),
          tags$li("User Interface: 'bslib', 'shinyWidgets', 'shinyjs'")
        ),
        h4("Live Application Link"),
        p("Access the live version:"),
        p(tags$a(href = "https://chauvinh.shinyapps.io/amralyze/", target = "_blank", "https://chauvinh.shinyapps.io/amralyze/")),
        h4("Contact & Contribution"),
        p("For inquiries or contributions, contact Chau Vinh:"),
        p(tags$a(href = "mailto:chauvinhtth13@gmail.com", "chauvinhtth13@gmail.com")),
        h4("Source Code & Citation"),
        p("Source code is on GitHub. If used, please cite:"),
        p("Vinh, C. (2025). ", tags$em("AMRalyze: Interactive Antimicrobial Resistance Data Analysis Dashboard "), "[Software]. Source code: ", tags$a(href = "https://github.com/chauvinhtth13/AMRalyze", target = "_blank", "GitHub Repository"), ". Live application: ", tags$a(href = "https://chauvinh.shinyapps.io/amralyze/", target = "_blank", "ShinyApp Link"), "."),
        h5("BibTeX Entry:"),
        tags$pre(tags$code(HTML(
          "@misc{Vinh2025AMRalyze,
  author         = {Vinh, Chau},
  title          = {{AMRalyze: Interactive Antimicrobial Resistance Data Analysis Dashboard}},
  year           = {2025},
  howpublished = {\\url{https://chauvinh.shinyapps.io/amralyze/}},
  note           = {Source code available at \\url{https://github.com/chauvinhtth13/AMRalyze}}
}"
        ))),
        p(tags$em("(Note: Using \\url{} requires the \\usepackage{url} or \\usepackage{hyperref} package in LaTeX.)")),
        hr(),
        p(tags$em("UI Rendered Timestamp (approximate): ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
      ) # End card_body
    ) # End card
  ) # End About nav_panel
) # End page_navbar



##### 2. Server Logic #####
server <- function(input, output, session) {
  
  ##### 2.1 Server Logic for Input Tab ######
  
  # Initialize reactiveValues
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
  
  # File Input Handling
  observeEvent(input$file_browse, {
    req(input$file_browse)
    # Reset relevant reactives
    rv$uploaded_data <- NULL
    rv$column_names <- NULL
    rv$show_mapping <- FALSE
    rv$processed_data <- NULL
    rv$mdr_data <- NULL
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
        sheets <- tryCatch(
          readxl::excel_sheets(path),
          error = function(e)
            NULL
        )
        if (is.null(sheets) ||
            length(sheets) == 0)
          stop("Could not read sheets from Excel file or file is empty/corrupt.")
        rv$file_info$sheets <- sheets
        updateSelectInput(session,
                          "sheet_name",
                          choices = sheets,
                          selected = sheets[1])
      } else {
        rv$file_info$sheets <- NULL
        updateSelectInput(session, "sheet_name", choices = character(0))
      }
      rv$file_info$ready_for_upload <- TRUE
      NULL # Success
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
      ) # Reset fully
    }
  })
  
  # Conditional UI Logic Outputs
  output$show_sheet_selector <- reactive({ !is.null(rv$file_info$ext) && rv$file_info$ext == "xlsx" })
  outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)
  output$show_upload_button <- reactive({ rv$file_info$ready_for_upload })
  outputOptions(output, "show_upload_button", suspendWhenHidden = FALSE)
  output$show_mapping_ui <- reactive({ rv$show_mapping })
  outputOptions(output, "show_mapping_ui", suspendWhenHidden = FALSE)
  output$show_patient_ui <- reactive({ !is.null(rv$processed_data) && nrow(rv$processed_data) > 0 })
  outputOptions(output, "show_patient_ui", suspendWhenHidden = FALSE)
  
  # Data Upload Button Action
  observeEvent(input$btn_upload, {
    req(rv$file_info$datapath)
    ext <- rv$file_info$ext
    path <- rv$file_info$datapath
    sheet <- if (ext == "xlsx")
      input$sheet_name
    else
      NULL
    
    # Reset downstream data
    rv$uploaded_data <- NULL
    rv$column_names <- NULL
    rv$show_mapping <- FALSE
    rv$processed_data <- NULL
    rv$mdr_data <- NULL
    
    shiny::withProgress(message = 'Reading File', value = 0.5, {
      tryCatch({
        if (ext == "xlsx") {
          req(sheet)
          df_read <- readxl::read_excel(path, sheet = sheet, col_types = "text") # Read all as text
        } else {
          # Use data.table::fread for speed and robustness
          df_read <- data.table::fread(
            path,
            stringsAsFactors = FALSE,
            header = TRUE,
            check.names = FALSE,
            quote = "\"",
            na.strings = c("NA", "")
          )
        }
        
        if (is.null(df_read) ||
            ncol(df_read) == 0 || nrow(df_read) == 0) {
          stop("Failed to read data, file is empty, or contains no columns.")
        }
        
        rv$uploaded_data <- dplyr::mutate_all(as.data.frame(df_read), as.character) # Ensure data frame and all char
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
            "'. Please map columns."
          ),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(paste("Error Reading File:", e$message),
                         type = "error",
                         duration = 10)
        rv$uploaded_data <- NULL
        rv$column_names <- NULL
        rv$show_mapping <- FALSE
      }) # end tryCatch
    }) # end withProgress
  })
  
  # Update Mapping Picker Inputs (Auto-detection)
  observeEvent(rv$show_mapping, { # Trigger when column names are available
    cols <- rv$column_names
    req(cols)
    print(cols)

    # Define potential matches (case-insensitive) - expanded list
    pid_matches <- c("PID", "Patient ID", "Study ID", "patient_id", "subject_id", "patnr", "hn")
    sid_matches <- c("SID", "Sample ID", "sample_id", "specimen_id", "lab_id", "isolate_id", "accession", "acc_no")
    date_matches <- c("Sample Date", "Date Sample", "Sampling Date", "Date Sampling", "sample_date", "collection_date", "sampling_date", "date_sampling", "specdate", "order_date", "recv_date")
    type_matches <- c("Sample Type", "Type Sample", "sample_type", "specimen_type", "source", "spectype", "specimen")
    pathogen_matches <- c("Pathogen", "Bacteria", "Bacteria Name", "Name Pathogen", "organism", "org", "mo", "microorganism", "species")
    ab_name_matches <- c("AB", "Antibiotics", "Name Antibiotics", "antibiotic", "drug", "agent", "ab_name", "antimicrobial")
    mic_matches <- c("MIC", "Minimum Inhibitory Concentration", "mic_value", "mic")
    zone_matches <- c("Zone Size", "Disk", "disk_diameter", "zone_diameter", "zone", "disk")
    interp_matches <- c("Interpretation", "SIR", "RIS", "breakpoint_result", "interp", "sensitivity", "final_interp")
    method_matches <- c("Methods", "Test Methods", "method", "ast_method", "test_method")
    # Include common WHONET codes/names and suffixes in potential AB columns
    ab_cols_matches <- unique(c(AMR::antimicrobials$name, AMR::antimicrobials$ab, grep("_(NM|NE|ND)$", cols, value = TRUE, ignore.case = TRUE)))

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

  # Process Data Button Action
  observeEvent(input$btn_process, {
    rv$processed_data <- NULL; rv$mdr_data <- NULL # Reset results

    shiny::withProgress(message = 'Processing Data', value = 0, style = "notification", {
      tryCatch({
        incProgress(0.1, detail = "Validating inputs...")
        validate(
          need(!is.null(rv$uploaded_data) && nrow(rv$uploaded_data) > 0, "No uploaded data found."),
          need(input$PID != "", "Mapping Error: Select Patient ID column."),
          need(input$SID != "", "Mapping Error: Select Sample ID column."),
          need(input$Pathogen != "", "Mapping Error: Select Pathogen column."),
          need(input$guideline != "" && !is.null(input$guideline), "Mapping Error: Select an AST Guideline.")
        )
        if (input$data_structure == 'wide') {
          validate(need(!is.null(input$AB_cols) && length(input$AB_cols) > 0, "Mapping Error: Select one or more Antibiotic Result columns for wide format."))
        } else { # 'long' format
          validate(
            need(input$AB_name != "", "Mapping Error: Select the Antibiotic Name column for long format."),
            need(input$MIC != "" || input$Zone_Size != "" || input$Interpretation != "", "Mapping Error: Select at least one result column (MIC, Zone Size, or Interpretation) for long format.")
          )
        }

        incProgress(0.1, detail = "Preparing data columns...")
        df_input <- rv$uploaded_data
        core_map <- list(PatientID = input$PID, SampleID = input$SID, SampleDate = input$Sample_Date, SampleType = input$Sample_Type, Pathogen = input$Pathogen)
        core_map <- core_map[sapply(core_map, function(x) !is.null(x) && x != "")]
        core_cols_original <- unname(unlist(core_map))
        core_cols_target <- names(core_map)

        # --- Wide Format Processing ---
        if (input$data_structure == 'wide') {
          incProgress(0.1, detail = "Processing wide format...")
          ab_cols_original <- input$AB_cols
          df_processed_wide <- df_input %>%
            select(all_of(c(core_cols_original, ab_cols_original))) %>%
            rename(!!!setNames(core_cols_original, core_cols_target))

          df_processed <- df_processed_wide %>%
            tidyr::pivot_longer(
              cols = all_of(ab_cols_original),
              names_to = "Original_AB_Col", values_to = "Result",
              values_drop_na = FALSE # Keep NA initially
            ) %>%
            filter(!is.na(Result) & Result != "") # Drop empty results

          # Parse wide format results (simplified logic)
          df_processed <- df_processed %>%
            mutate(
              Method_Suffix = toupper(str_extract(Original_AB_Col, "(?<=_|-)(NM|NE|ND)$")),
              Antibiotic_Name_Raw = str_remove(Original_AB_Col, "[_|-](NM|NE|ND)$"),
              Method_Parsed = case_when(
                !is.na(Method_Suffix) & Method_Suffix == "ND" ~ "Disk",
                !is.na(Method_Suffix) & Method_Suffix == "NE" ~ "E-Test",
                !is.na(Method_Suffix) & Method_Suffix == "NM" ~ "MIC",
                grepl("^(R|S|I|SSD|NI)$", trimws(Result), ignore.case = TRUE) ~ "InterpretationOnly", # If only SIR
                grepl("(<=|>=|<|>)?\\s*[0-9.]+", trimws(Result)) & !grepl("[a-zA-Z]", Result) ~ "Numeric", # Likely MIC or Disk if just numeric
                TRUE ~ "Unknown"
              ),
              TempInterpretation_Raw = str_extract(Result, "\\b(R|S|I|SSD|NI)\\b"),
              Value_Raw = str_extract(Result, "(<=|>=|<|>)?\\s*[0-9.]+"),
              MIC = case_when(
                Method_Parsed %in% c("MIC", "E-Test") ~ Value_Raw,
                Method_Parsed == "Numeric" ~ Value_Raw, # Assume MIC for ambiguous numeric
                TRUE ~ ""
              ),
              Zone = case_when(
                Method_Parsed == "Disk" ~ Value_Raw,
                # Could add heuristic for Zone if Method_Parsed is "Numeric" (e.g., integer > 5)
                TRUE ~ ""
              ),
              TempInterpretation = TempInterpretation_Raw,
              Antibiotic_Name = Antibiotic_Name_Raw,
              Method = Method_Parsed # Keep parsed method info if available
            ) %>%
            select(all_of(core_cols_target), Antibiotic_Name, MIC, Zone, TempInterpretation, Method) # Select standard columns
        }
        # --- Long Format Processing ---
        else {
          incProgress(0.1, detail = "Processing long format...")
          long_map <- list(Antibiotic_Name = input$AB_name, MIC = input$MIC, Zone = input$Zone_Size, TempInterpretation = input$Interpretation, Method = input$Methods)
          long_map <- long_map[sapply(long_map, function(x) !is.null(x) && x != "")]
          long_cols_original <- unname(unlist(long_map))
          long_cols_target <- names(long_map)

          df_processed <- df_input %>%
            select(all_of(c(core_cols_original, long_cols_original))) %>%
            rename(!!!setNames(core_cols_original, core_cols_target)) %>%
            rename(!!!setNames(long_cols_original, long_cols_target)) # CORRECTED renaming

          # Ensure standard columns exist, fill with NA if not mapped
          if (!"MIC" %in% names(df_processed)) df_processed$MIC <- NA_character_
          if (!"Zone" %in% names(df_processed)) df_processed$Zone <- NA_character_
          if (!"TempInterpretation" %in% names(df_processed)) df_processed$TempInterpretation <- NA_character_
          if (!"Method" %in% names(df_processed)) df_processed$Method <- NA_character_
        }

        incProgress(0.2, detail = "Standardizing data (AMR package)...")
        # Ensure required columns have character type before AMR functions
        df_processed <- df_processed %>%
          mutate(across(any_of(c("MIC", "Zone", "TempInterpretation", "Antibiotic_Name", "Pathogen")), as.character)) %>%
          filter(!is.na(Pathogen) & Pathogen != "", !is.na(Antibiotic_Name) & Antibiotic_Name != "") # Filter essential missing

        df_processed <- df_processed %>%
          mutate(
            mo_code = AMR::as.mo(Pathogen),
            PathogenName = ifelse(is.na(mo_code), Pathogen, AMR::mo_name(mo_code)),
            kingdom = AMR::mo_kingdom(mo_code),
            gram_stain = AMR::mo_gramstain(mo_code),
            ab_code = AMR::as.ab(Antibiotic_Name),
            AntibioticName = AMR::ab_name(ab_code),
            MIC_obj = tryCatch(AMR::as.mic(MIC), error = function(e) AMR::as.mic(NA)),
            Zone_obj = tryCatch(AMR::as.disk(Zone), error = function(e) AMR::as.disk(NA)),
            TempInterpretation_obj = tryCatch(AMR::as.sir(TempInterpretation), error = function(e) AMR::as.sir(NA)),
            Interpretation = case_when( # Prioritize existing interp, then MIC, then Zone
              !is.na(TempInterpretation_obj) ~ TempInterpretation_obj,
              !is.na(MIC_obj) & !is.na(mo_code) & !is.na(ab_code) ~ AMR::sir_interpret(mic = MIC_obj, mo = mo_code, ab = ab_code, guideline = input$guideline),
              !is.na(Zone_obj) & !is.na(mo_code) & !is.na(ab_code) ~ AMR::sir_interpret(disk = Zone_obj, mo = mo_code, ab = ab_code, guideline = input$guideline),
              TRUE ~ AMR::as.sir(NA)
            )
          ) %>%
          select( # Select final standardized columns
            PatientID, SampleID, any_of(c("SampleDate", "SampleType")),
            Pathogen = PathogenName, mo_code, kingdom, gram_stain,
            AntibioticName, ab_code,
            MIC = MIC_obj, Zone = Zone_obj, Interpretation,
            any_of("Method")
          ) %>%
          filter(!is.na(ab_code)) %>% # Remove rows with unrecognized antibiotics
          filter(!(is.na(Interpretation) & is.na(MIC) & is.na(Zone))) # Must have some result

        # --- Date Parsing ---
        if ("SampleDate" %in% names(df_processed)) {
          incProgress(0.1, detail = "Parsing dates...")
          datetime_formats_to_try <- c( # Common formats
            "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
            "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
            "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
            "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M", "%d-%m-%Y",
            "%d-%b-%Y %H:%M:%S", "%d-%b-%Y %H:%M", "%d-%b-%Y",
            "%b %d %Y %H:%M:%S", "%b %d %Y %H:%M", "%b %d %Y",
            "%Y%m%d%H%M%S", "%Y%m%d"
          )
          df_processed <- df_processed %>%
            mutate(
              SampleDate_chr = as.character(SampleDate),
              Sample_Date_Parsed = suppressWarnings(lubridate::parse_date_time(SampleDate_chr, orders = datetime_formats_to_try, quiet = TRUE)),
              SampleDate = case_when( # Handle Excel numeric dates (origin 1899-12-30)
                is.na(Sample_Date_Parsed) & grepl("^[0-9]{5,}$", SampleDate_chr) ~ as.POSIXct(as.numeric(SampleDate_chr) * 86400, origin = "1899-12-30", tz = "UTC"),
                is.na(Sample_Date_Parsed) & grepl("^[0-9]+\\.[0-9]+$", SampleDate_chr) ~ as.POSIXct(as.numeric(SampleDate_chr) * 86400, origin = "1899-12-30", tz = "UTC"),
                TRUE ~ Sample_Date_Parsed
              )
            ) %>%
            select(-c(SampleDate_chr, Sample_Date_Parsed))
        }

        incProgress(0.3, detail = "Calculating MDR status...")
        # Prioritize MIC over Disk if both exist for the same isolate/antibiotic
        mdr_input_long <- df_processed %>%
          group_by(PatientID, SampleID, Pathogen, mo_code, kingdom, gram_stain, ab_code) %>%
          arrange(desc(!is.na(MIC)), .by_group = TRUE) %>% # MIC results first
          slice(1) %>% # Keep only the top prioritized result
          ungroup()

        mdr_pivoted <- mdr_input_long %>%
          select(PatientID, SampleID, Pathogen, mo_code, kingdom, gram_stain, ab_code, Interpretation) %>%
          filter(!is.na(ab_code) & !is.na(Interpretation)) %>% # Need ab code and interp for MDR
          tidyr::pivot_wider(
            id_cols = c(PatientID, SampleID, Pathogen, mo_code, kingdom, gram_stain),
            names_from = ab_code, values_from = Interpretation, values_fill = NA
          )

        # Calculate MDR (only if there are antibiotic columns to evaluate)
        ab_cols_for_mdr <- setdiff(colnames(mdr_pivoted), c("PatientID", "SampleID", "Pathogen", "mo_code", "kingdom", "gram_stain"))

        if(length(ab_cols_for_mdr) > 0) {
          rv$mdr_data <- mdr_pivoted %>%
            mutate(across(all_of(ab_cols_for_mdr), as.character)) %>% # Ensure character for mdro
            mutate(
              # Ensure the function exists and handles potential errors
              MDR = tryCatch(
                AMR::mdro(., guideline = "CMI2012"), # Using standard CMI2012 definition
                error = function(e) {
                  warning("MDR calculation failed: ", e$message); "Error"
                }
              ),
              MDR = if_else(is.na(MDR), "Not Determined", MDR)
            ) %>%
            select(PatientID, SampleID, Pathogen, MDR, everything()) # Bring MDR near front
        } else {
          # Handle case with no antibiotic results - create MDR data with NA/Not Determined
          rv$mdr_data <- mdr_pivoted %>%
            mutate(MDR = "Not Determined") %>%
            select(PatientID, SampleID, Pathogen, MDR, everything())
        }


        rv$processed_data <- df_processed # Store final long data

        if (nrow(rv$processed_data) == 0) {
          showNotification("Warning: Processing completed, but the resulting dataset is empty. Check input data and mappings.", type = "warning", duration = 10)
        } else {
          showNotification("Data processed successfully! Dashboard updated.", type = "message", duration = 5)
        }

      }, error = function(e) {
        showNotification(paste("Error during processing:", e$message), type = "error", duration = 15)
        rv$processed_data <- NULL; rv$mdr_data <- NULL # Reset on error
      }) # end tryCatch
    }) # end withProgress
  })
  
  # --- Reactives and Observers for Data Review Filters ---
  patient_ids <- reactive({ req(rv$processed_data); sort(unique(as.character(rv$processed_data$PatientID))) })
  sample_ids <- reactive({ req(rv$processed_data, input$PID_show); rv$processed_data %>% filter(PatientID == input$PID_show) %>% pull(SampleID) %>% unique() %>% as.character() %>% sort() })
  pathogen_names <- reactive({ req(rv$processed_data, input$PID_show, input$SID_show); rv$processed_data %>% filter(PatientID == input$PID_show, SampleID == input$SID_show) %>% pull(Pathogen) %>% unique() %>% as.character() %>% sort() })
  
  observe({
    choices <- patient_ids()
    updateVirtualSelect(
      session = session,
      inputId = "PID_show",
      choices = choices,
      selected = if (length(choices) > 0)
        choices[1]
      else
        NULL
    )
  })
  observe({
    req(input$PID_show)
    choices <- sample_ids()
    updateVirtualSelect(
      session = session,
      inputId = "SID_show",
      choices = choices,
      selected = if (length(choices) > 0)
        choices[1]
      else
        NULL
    )
  })
  observe({
    req(input$PID_show, input$SID_show)
    choices <- pathogen_names()
    updateVirtualSelect(
      session = session,
      inputId = "Pathogen_show",
      choices = choices,
      selected = if (length(choices) > 0)
        choices[1]
      else
        NULL
    )
  })
  
  # Filtered data for review
  filtered_review_data <- reactive({
    req(rv$processed_data, input$PID_show, input$SID_show, input$Pathogen_show)
    rv$processed_data %>% filter(PatientID == input$PID_show & SampleID == input$SID_show & Pathogen == input$Pathogen_show)
  })
  mdr_review_data <- reactive({
    req(rv$mdr_data, input$PID_show, input$SID_show, input$Pathogen_show)
    rv$mdr_data %>% filter(PatientID == input$PID_show & SampleID == input$SID_show & Pathogen == input$Pathogen_show)
  })
  
  # Outputs for Data Review Section
  output$SampleDate_text <- renderText({
    data_sub <- filtered_review_data(); validate(need(nrow(data_sub) > 0, "No data for current filter selection."))
    if ("SampleDate" %in% names(data_sub) && !all(is.na(data_sub$SampleDate))) {
      first_date <- first(na.omit(data_sub$SampleDate)); paste("Date Sample Collected:", format(first_date, "%Y-%m-%d"))
    } else { "Date Sample Collected: Not Available" }
  })
  output$SampleType_text <- renderText({
    data_sub <- filtered_review_data(); validate(need(nrow(data_sub) > 0, ""))
    if ("SampleType" %in% names(data_sub) && !all(is.na(data_sub$SampleType))) {
      unique_types <- first(na.omit(data_sub$SampleType)); paste("Sample Type:", unique_types)
    } else { "Sample Type: Not Available" }
  })
  output$Kingdom_text <- renderText({ data_sub <- filtered_review_data(); validate(need(nrow(data_sub) > 0, "")); unique_val <- first(na.omit(data_sub$kingdom)); paste("Kingdom:", ifelse(is.na(unique_val), "N/A", unique_val)) })
  output$GramStain_text <- renderText({ data_sub <- filtered_review_data(); validate(need(nrow(data_sub) > 0, "")); unique_val <- first(na.omit(data_sub$gram_stain)); paste("Gram stain:", ifelse(is.na(unique_val), "N/A", unique_val)) })
  output$MDR_text <- renderText({ data_sub <- mdr_review_data(); validate(need(nrow(data_sub) > 0, "")); unique_val <- first(na.omit(data_sub$MDR)); paste("MDR Status (CMI2012):", ifelse(is.na(unique_val), "N/A", unique_val)) }) # CORRECTED to $MDR
  
  output$AST_data_view_table <- DT::renderDataTable({
    data_sub <- filtered_review_data()
    validate(need(nrow(data_sub) > 0, "Select Patient/Sample/Pathogen to view results."))
    
    # Prepare data for display - select, format NAs, rename
    table_data <- data_sub %>%
      select(ab_code, AntibioticName, MIC, Zone, Interpretation, any_of("Method")) %>%
      mutate(across(everything(), ~ as.character(.))) %>% # Convert all (incl. AMR classes) to character
      mutate(across(everything(), ~ replace_na(., "-"))) # Replace NA with "-"
    
    # Define display names
    col_names_display <- c(`Antibiotic Code` = "ab_code", `Antibiotic Name` = "AntibioticName", `MIC (mg/L)` = "MIC", `Zone Size (mm)` = "Zone", `Interpretation` = "Interpretation")
    if ("Method" %in% names(table_data)) col_names_display["Method"] <- "Method"
    
    final_table_data <- table_data %>% select(all_of(unname(col_names_display))) %>% rename(!!!setNames(names(.), names(col_names_display)))
    
    DT::datatable(
      final_table_data, options = list(paging = FALSE, scrollY = "300px", scrollX = TRUE, searching = TRUE, lengthChange = FALSE, info = FALSE, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = '_all'))),
      rownames = FALSE, filter = 'top', class = 'display compact'
    )
  })
  
  # Download Handler
  output$download_processed_data <- downloadHandler(
    filename = function() { paste0("amralyze_processed_data_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(rv$processed_data, rv$mdr_data)
      # Convert AMR classes to character for export
      processed_export <- rv$processed_data %>% mutate(across(where(AMR::is.amr_any), as.character))
      mdr_export <- rv$mdr_data %>% mutate(across(where(AMR::is.amr_any), as.character))
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Processed_Data_Long")
      openxlsx::addWorksheet(wb, "MDR_Data_Wide")
      openxlsx::writeData(wb, sheet = 1, x = processed_export)
      openxlsx::writeData(wb, sheet = 2, x = mdr_export)
      header_style <- openxlsx::createStyle(textDecoration = "bold")
      openxlsx::addStyle(wb, sheet = 1, style = header_style, rows = 1, cols = 1:ncol(processed_export), gridExpand = TRUE)
      openxlsx::addStyle(wb, sheet = 2, style = header_style, rows = 1, cols = 1:ncol(mdr_export), gridExpand = TRUE)
      openxlsx::setColWidths(wb, sheet = 1, cols = 1:ncol(processed_export), widths = "auto")
      openxlsx::setColWidths(wb, sheet = 2, cols = 1:ncol(mdr_export), widths = "auto")
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  ##### 2.2 Server Logic For Dashboard #####
  
  # Dashboard Metrics
  output$total_records <- renderText({ df <- rv$uploaded_data; if (is.null(df)) "0" else scales::comma(nrow(df)) })
  output$total_patients <- renderText({ df <- rv$processed_data; if (is.null(df)) "0" else scales::comma(n_distinct(df$PatientID, na.rm = TRUE)) })
  output$total_samples <- renderText({ df <- rv$processed_data; if (is.null(df)) "0" else scales::comma(n_distinct(df$SampleID, na.rm = TRUE)) })
  
  # Pathogen Summary Plot
  output$pathogen_summary_plot <- renderPlotly({
    validate(need(!is.null(rv$processed_data), "Please upload and process data first ('Input' tab)."), need(nrow(rv$processed_data) > 0, "Processed data is empty."))
    pathogen_freq <- rv$processed_data %>% dplyr::distinct(PatientID, SampleID, Pathogen) %>% dplyr::count(Pathogen, sort = TRUE, name = "Frequency") %>% filter(!is.na(Pathogen) & Frequency > 0)
    validate(need(nrow(pathogen_freq) > 0, "No valid pathogen data to summarize."))
    top_n_pathogens <- 15
    if (nrow(pathogen_freq) > top_n_pathogens) {
      top_pathogens <- pathogen_freq %>% slice_head(n = top_n_pathogens) %>% pull(Pathogen)
      pathogen_freq <- pathogen_freq %>% mutate(Pathogen_Group = if_else(Pathogen %in% top_pathogens, Pathogen, "Other")) %>% group_by(Pathogen_Group) %>% summarise(Frequency = sum(Frequency), .groups = 'drop') %>% rename(Pathogen = Pathogen_Group) %>% arrange(desc(Pathogen != "Other"), desc(Frequency))
    } else { pathogen_freq <- pathogen_freq %>% arrange(desc(Frequency)) }
    
    gg <- ggplot(pathogen_freq, aes(x = reorder(Pathogen, Frequency), y = Frequency, text = paste("Pathogen:", Pathogen, "<br>Samples:", scales::comma(Frequency)))) +
      geom_col(fill = "#2c7fb8", width = 0.7) + coord_flip() + labs(title = NULL, x = NULL, y = "Number of Samples") + theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(linetype = "dashed", color = "grey85"), axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10), plot.margin = margin(5, 20, 5, 5))
    plotly::ggplotly(gg, tooltip = "text") %>% layout(yaxis = list(title = list(text = 'Pathogen', standoff = 15)))
  })
  
  # --- AST Summary Table Logic (Re-implemented with gtsummary/gt best practices) ---
  
  # Shared function to generate AST summary GT table
  generate_ast_gt <- function(data, filter_col, filter_val, title_prefix) {
    data_sub <- data %>%
      filter(.data[[filter_col]] == filter_val) %>%
      # Select only pathogen, MDR (if exists), and antibiotic columns (SIR class)
      select(Pathogen, any_of("MDR"), where(AMR::is.sir)) %>%
      # Keep only columns with at least one non-NA value
      select(where(~ !all(is.na(.))))
    
    # Check if MDR column exists after selection
    has_mdr <- "MDR" %in% names(data_sub)
    # Identify antibiotic columns correctly
    list_ab <- setdiff(names(data_sub), c("Pathogen", if(has_mdr) "MDR"))
    list_ab <- list_ab[AMR::is.ab(list_ab)] # Ensure they are valid ab codes
    
    # Stop if no antibiotic columns left
    validate(need(length(list_ab) > 0, paste("No valid antibiotic/antifungal columns found for", title_prefix)))
    
    # Prepare data for tbl_summary: convert SIR to logical (TRUE=Resistant)
    data_summary_input <- data_sub %>%
      mutate(across(all_of(list_ab), ~ .x == "R"))
    
    # Define include list, putting MDR first if present
    include_vars <- c(if(has_mdr) "MDR", list_ab)
    
    # Build tbl_summary
    tbl <- data_summary_input %>%
      gtsummary::tbl_summary(
        by = Pathogen,
        include = all_of(include_vars),
        type = list(all_of(list_ab) ~ "categorical"), # Treat resistance (T/F) as categorical
        statistic = list(all_categorical() ~ "{p}% ({n}/{N})"), # Show % resistant
        value = list(all_of(list_ab) ~ "TRUE"), # Calculate stats for TRUE (Resistant)
        missing = "no",
        digits = list(all_categorical() ~ c(1, 0, 0)), # pct, n, N
        label = if(has_mdr) list(MDR ~ "MDR Status (CMI2012)") else NULL
      ) %>%
      gtsummary::modify_header(label = "**Antibiotic / Status**") %>%
      gtsummary::modify_footnote(all_stat_cols() ~ "Resistant Percent (%), (Resistant cases / Total isolates tested)", abbreviation = TRUE) %>%
      gtsummary::modify_table_body( # Clean up labels and stats representation
        ~ .x %>% mutate(
          # Use original variable name (ab code) for label processing
          label = ifelse(variable %in% list_ab, variable, label),
          # Replace verbose NA/0 stats with "-"
          across(all_stat_cols(), ~ gsub("NA% \\(NA/NA\\)|0% \\(0/0\\)", "-", .))
        )
      ) %>%
      gtsummary::bold_labels() %>%
      gtsummary::as_gt()
    
    # Grouping and Styling using gt functions
    # Group MDR first if it exists
    if (has_mdr) {
      tbl <- tbl %>% gt::tab_row_group(label = md("**MDR Status**"), rows = variable == "MDR")
    }
    
    # Group antibiotics by class
    group_ab_classes <- tryCatch(sort(unique(AMR::ab_class(list_ab))), error = function(e) NULL)
    if (!is.null(group_ab_classes)) {
      for (ab_class in group_ab_classes) {
        ab_in_class <- list_ab[AMR::ab_class(list_ab) == ab_class & !is.na(AMR::ab_class(list_ab))] # Handle potential NA classes
        if(length(ab_in_class) > 0) {
          tbl <- tbl %>% gt::tab_row_group(label = md(paste0("**", ab_class, "**")), rows = variable %in% ab_in_class)
        }
      }
      # Add a group for antibiotics with no class if any exist
      ab_no_class <- list_ab[is.na(AMR::ab_class(list_ab))]
      if (length(ab_no_class) > 0) {
        tbl <- tbl %>% gt::tab_row_group(label = md("**Other**"), rows = variable %in% ab_no_class)
      }
    } else { # Fallback if classes cannot be determined
      tbl <- tbl %>% gt::tab_row_group(label = md("**Antibiotics/Antifungals**"), rows = variable %in% list_ab)
    }
    
    
    # Convert antibiotic codes to names and apply styling
    tbl <- tbl %>%
      gt::text_transform(
        locations = cells_body(columns = label, rows = label %in% list_ab),
        fn = function(x) AMR::ab_name(x) # Use AMR::ab_name
      ) %>%
      gt::fmt_markdown(columns = label) %>% # Render markdown in labels/groups
      gt::sub_missing(columns = everything(), missing_text = "-") %>%
      gt::tab_options(
        row_group.font.weight = "bold",
        row_group.background.color = "#f2f2f2",
        table.font.size = px(12),
        data_row.padding = px(3),
        table.width = pct(100) # Ensure table fills container
      ) %>%
      gt::tab_style(
        style = cell_text(indent = px(15)), # Indent antibiotic names
        locations = cells_body(columns = label, rows = label %in% list_ab)
      )
    
    return(tbl)
  }
  
  
  # Gram-Negative Observer and Table
  observe({
    req(rv$mdr_data)
    pathogens <- rv$mdr_data %>% filter(kingdom == "Bacteria" & gram_stain == "Gram-negative") %>% count(Pathogen, sort = TRUE) %>% filter(n > 0 & !is.na(Pathogen)) %>% pull(Pathogen)
    selected_choice <- head(pathogens, 4)
    updateVirtualSelect(session = session,
                        inputId = "list_gram_negative", choices = pathogens, selected = selected_choice)
  })
  output$ast_gram_negative_table <- render_gt({
    validate(need(!is.null(rv$mdr_data), "Process data first."), need(!is.null(input$list_gram_negative) && length(input$list_gram_negative) > 0, "Choose Gram-negative Pathogen."))
    generate_ast_gt(rv$mdr_data, filter_col = "Pathogen", filter_val = input$list_gram_negative, title_prefix = "Gram-negative")
  }, height = 800)
  
  # Gram-Positive Observer and Table
  observe({
    req(rv$mdr_data)
    pathogens <- rv$mdr_data %>% filter(kingdom == "Bacteria" & gram_stain == "Gram-positive") %>% count(Pathogen, sort = TRUE) %>% filter(n > 0 & !is.na(Pathogen)) %>% pull(Pathogen)
    selected_choice <- head(pathogens, 4)
    updateVirtualSelect(session = session,
                        inputId = "list_gram_positive", choices = pathogens, selected = selected_choice)
  })
  output$ast_gram_positive_table <- render_gt({
    validate(need(!is.null(rv$mdr_data), "Process data first."), need(!is.null(input$list_gram_positive) && length(input$list_gram_positive) > 0, "Choose Gram-positive Pathogen."))
    generate_ast_gt(rv$mdr_data, filter_col = "Pathogen", filter_val = input$list_gram_positive, title_prefix = "Gram-positive")
  }, height = 800)
  
  # Fungal Observer and Table
  observe({
    req(rv$mdr_data)
    pathogens <- rv$mdr_data %>% filter(kingdom == "Fungi") %>% count(Pathogen, sort = TRUE) %>% filter(n > 0 & !is.na(Pathogen)) %>% pull(Pathogen)
    selected_choice <- head(pathogens, 4)
    updateVirtualSelect(session = session,
                        inputId = "list_fungal", choices = pathogens, selected = selected_choice)
  })
  output$ast_fungal_table <- render_gt({
    validate(need(!is.null(rv$mdr_data), "Process data first."), need(!is.null(input$list_fungal) && length(input$list_fungal) > 0, "Choose Fungal Pathogen."))
    # Note: MDR column likely won't exist/apply for Fungi in mdr_data, generate_ast_gt handles this
    generate_ast_gt(rv$mdr_data, filter_col = "Pathogen", filter_val = input$list_fungal, title_prefix = "Fungal")
  }, height = 800)
  
  
  # --- Placeholder Outputs for MIC Distribution Tables ---
  output$ast_gram_negative_mic_table <- render_gt({ gt::gt(data.frame(Status = "MIC distribution tables not yet implemented.")) })
  output$ast_gram_positive_mic_table <- render_gt({ gt::gt(data.frame(Status = "MIC distribution tables not yet implemented.")) })
  
} # End Server function

##### 3. Run App #####
shinyApp(ui = ui, server = server)