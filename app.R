# --- 0. Load Libraries ---
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(shinybusy)
library(DT)
library(AMR)          # Assumed source of AMR-specific data/functions
library(tidyverse)    # Includes dplyr, stringr, tidyr, purrr, lubridate, etc.
library(readxl)       # For reading Excel files
library(openxlsx)
library(tools)        # For file extension checking
library(scales)       # For comma formatting
library(reshape2)
library(magrittr)
library(DescTools)
library(scales)
library(binaryLogic)
library(gt)
library(gtsummary)

source("utils/ui_utils.R", local = TRUE)


##### 1. UI Definition #####
ui <- page_navbar(
  title = "AMRalyze Dashboard",
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  header = tags$head(
    shinyjs::useShinyjs(),
    tags$style(
      HTML("#file_browse_progress.progress { display: none !important; }") # Keep hiding default progress
    ),
    tags$script(
      HTML(
        # --- JAVASCRIPT BUG NOTE ---
        # The JavaScript below aims to enable Shift+Click range selection
        # for the 'AB_cols' Virtual Select input.
        # *** CURRENTLY, THE LOGIC IS INCOMPLETE. ***
        # The for-loop `for (var i = start; i <= end; i++) {}` is empty
        # and does not actually select the range.
        # To fix this, the JS needs to be updated to:
        # 1. Find the option elements within the calculated range (start to end).
        # 2. Get their values.
        # 3. Use the Virtual Select JS API (e.g., `virtualSelectElement.setValue(...)`)
        #    to programmatically select those options.
        # --- END JAVASCRIPT BUG NOTE ---
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
        "              e.preventDefault(); // Prevent default toggle on shift-click range",
        "              var start = Math.min(lastSelectedIndex, currentIndex);",
        "              var end = Math.max(lastSelectedIndex, currentIndex);",
        "              var optionsToSelect = [];",
        "              // --- FIX NEEDED HERE ---",
        "              // Loop should identify options and add values to optionsToSelect",
        "              // Example (conceptual - needs testing with VirtualSelect structure):",
        "              // $(virtualSelectElement).find('.vscomp-option').each(function() {",
        "              //   var idx = $(this).data('index'); ",
        "              //   if (idx >= start && idx <= end) {",
        "              //     optionsToSelect.push($(this).data('value')); ",
        "              //   }",
        "              // });",
        "              // virtualSelectElement.setValue(optionsToSelect); // Call API to select",
        "              // --- END FIX NEEDED ---",
        "              console.log('Range selected (JS NEEDS FIX):', optionsToSelect);",
        "          } else {",
        "              lastSelectedIndex = clickedOption.data('index'); // Update last index only if not shift-clicking range",
        "          }",
        "      });",
        "  } else {",
        "      console.error('Could not find Virtual Select instance for:', vsInputId);",
        "  }",
        "});"
      )
    )
  ),
  ##### 1.1. Dashboard Tab #####
  nav_panel(title = "Dashboard", card(
    layout_columns(
      col_widths = c(4, 4, 4),
      max_height = "200px",
      value_box(
        title = "Total Records Uploaded",
        value = textOutput("total_records"),
        showcase = bsicons::bs_icon("file-earmark-ruled"),
        theme = "primary"
      ),
      value_box(
        title = "Total Patients",
        value = textOutput("total_patients"),
        showcase = bsicons::bs_icon("person-vcard"),
        theme = "info"
      ),
      value_box(
        title = "Total Samples",
        value = textOutput("total_samples"),
        showcase = bsicons::bs_icon("radioactive"),
        theme = "success"
      )
    ),
    card(
      card_header("Summary Pathogen"),
      card_body(plotOutput("pathogen_summary_plot", height = "100%"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Raw Data"), card_body(
        DT::dataTableOutput("ast_overview_table")
      )),
      card(card_header("Raw Data"), card_body(
        DT::dataTableOutput("mic_overview_table")
      ))
    )
  )),
  
  ##### 1.2. Input Tab #####
  nav_panel(title = "Input", 
            ##### 1.2.1. Sidebar Configure #####
            layout_sidebar(
              sidebar = sidebar(
                width = "30%",
                # Example width
                fileInput(
                  "file_browse",
                  h5("1. Upload CSV/XLSX File (Max 10MB)"),
                  accept = c(
                    ".csv",
                    ".xlsx",
                    "text/csv",
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                  ),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                ),
                conditionalPanel(
                  "output.show_sheet_selector == true",
                  selectInput("sheet_name", "Select Sheet", choices = NULL)
                ),
                conditionalPanel(
                  "output.show_upload_button == true",
                  actionButton("btn_upload", "2. Upload & Preview Data", class = "btn-primary w-100")
                ),
                conditionalPanel(
                  condition = "output.show_mapping_ui == true",
                  hr(),
                  h5("2. Choose Guideline:"),
                  pickerInput(
                    "guideline",
                    "AST Guidelines",
                    choices = sort(unique(clinical_breakpoints$guideline)),
                    selected = get_newest_guideline(),
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)
                  ),
                  hr(),
                  h5("3. Map Columns and Process Data:"),
                  p(
                    "Select the corresponding columns from your data. Auto-detection attempted."
                  ),
                  virtualSelectInput(
                    "PID",
                    "Patient ID Column",
                    choices = NULL,
                    multiple = FALSE,
                    search = TRUE
                  ),
                  virtualSelectInput(
                    "SID",
                    "Sample ID Column",
                    choices = NULL,
                    multiple = FALSE,
                    search = TRUE
                  ),
                  virtualSelectInput(
                    "Sample_Date",
                    "Sampling Date Column",
                    choices = NULL,
                    multiple = FALSE,
                    search = TRUE,
                    showValueAsTags = TRUE,
                    hideClearButton = FALSE
                  ),
                  virtualSelectInput(
                    "Sample_Type",
                    "Sample Type Column",
                    choices = NULL,
                    multiple = FALSE,
                    search = TRUE,
                    showValueAsTags = TRUE,
                    hideClearButton = FALSE
                  ),
                  virtualSelectInput(
                    "Pathogen",
                    "Pathogen Column",
                    choices = NULL,
                    multiple = FALSE,
                    search = TRUE
                  ),
                  
                  radioButtons(
                    "data_structure",
                    "AST Data Structure:",
                    choices = list(
                      "Wide (One row per isolate)" = "wide",
                      "Long (One row per test)" = "long"
                    ),
                    selected = "long"
                  ),
                  conditionalPanel(
                    "input.data_structure == 'wide'",
                    virtualSelectInput(
                      "AB_cols",
                      "Select ALL Antibiotic Result Columns",
                      choices = NULL,
                      multiple = TRUE,
                      search = TRUE,
                      showValueAsTags = TRUE,
                      placeholder = "Select columns...",
                      disableSelectAll = FALSE,
                      optionsCount = 8
                    )
                  ),
                  conditionalPanel(
                    "input.data_structure == 'long'",
                    virtualSelectInput(
                      "AB_name",
                      "Antibiotic Name Column",
                      choices = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      showValueAsTags = TRUE,
                      hideClearButton = FALSE
                    ),
                    virtualSelectInput(
                      "MIC",
                      "MIC Column",
                      choices = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      showValueAsTags = TRUE,
                      hideClearButton = FALSE
                    ),
                    virtualSelectInput(
                      "Zone_Size",
                      "Zone Size Column",
                      choices = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      showValueAsTags = TRUE,
                      hideClearButton = FALSE
                    ),
                    virtualSelectInput(
                      "Interpretation",
                      "Interpretation Column",
                      choices = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      showValueAsTags = TRUE,
                      hideClearButton = FALSE
                    ),
                    virtualSelectInput(
                      "Methods",
                      "Test Method Column",
                      choices = NULL,
                      multiple = FALSE,
                      search = TRUE,
                      showValueAsTags = TRUE,
                      hideClearButton = FALSE
                    )
                  ),
                  actionButton("btn_process", "Process Data", class = "btn-success w-100")
                ),
              ),
              ##### 1.2.2. Main Content Area #####
              card(
                card_header("Processed Data Review"),
                card_body(
                  downloadButton("download_processed_data", "Download Processed Data", class =
                                   "btn-sm"),
                  hr(),
                  conditionalPanel(
                    condition = "output.show_patient_ui == true",
                    layout_columns(
                      # Arrange filters horizontally
                      col_widths = c(4, 4, 4),
                      virtualSelectInput(
                        "PID_show",
                        "Filter Patient ID",
                        choices = NULL,
                        multiple = FALSE,
                        search = TRUE,
                        hideClearButton = FALSE
                      ),
                      virtualSelectInput(
                        "SID_show",
                        "Filter Sample ID",
                        choices = NULL,
                        multiple = FALSE,
                        search = TRUE,
                        hideClearButton = FALSE
                      ),
                      virtualSelectInput(
                        "Pathogen_show",
                        "Filter Pathogen",
                        choices = NULL,
                        multiple = FALSE,
                        search = TRUE,
                        hideClearButton = FALSE
                      )
                    ),
                    hr(),
                    # Display details for the selected isolate
                    strong(textOutput(outputId = "SampleDate_text")),
                    strong(textOutput(outputId = "SampleType_text")),
                    strong(textOutput(outputId = "Kingdom_text")),
                    strong(textOutput(outputId = "GramStain_text")),
                    strong(textOutput(outputId = "MDR_text")),
                    hr(),
                    strong(p("Antimicrobial Susceptibility Test Results:")),
                    DT::dataTableOutput("AST_data_view_table")
                  )
                )
              )
            ))
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
  output$show_sheet_selector <- reactive({ !is.null(rv$file_info$ext) && rv$file_info$ext == "xlsx" })
  outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)

  output$show_upload_button <- reactive({ rv$file_info$ready_for_upload })
  outputOptions(output, "show_upload_button", suspendWhenHidden = FALSE)

  output$show_mapping_ui <- reactive({ rv$show_mapping })
  outputOptions(output, "show_mapping_ui", suspendWhenHidden = FALSE)

  output$show_patient_ui <- reactive({ !is.null(rv$processed_data) && nrow(rv$processed_data) > 0 })
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
        df_read <- utils::read.csv(
          path,
          stringsAsFactors = FALSE,
          header = TRUE,
          check.names = FALSE
        )
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
    shiny::withProgress(message = 'Processing Data', value = 0, style = "notification", {
    
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
            tidyr::pivot_longer(
              cols = all_of(input$AB_cols),
              names_to = "Antibiotic_Name",
              values_to = "Result",
              values_drop_na = TRUE
            )
          
          colnames(df_processed) <- c(na.omit(names(core_map)[match(colnames(df_processed), unname(unlist(core_map)))]), "Antibiotic_Name", "Result")
          
          check_whonet_format <- df_processed %>% mutate(Method_init = str_split_i(Antibiotic_Name,"_|-",2),
                                                         Method = case_when(grepl("\\bND",Method_init) ~ "Disk",
                                                                            grepl("\\bNE",Method_init) ~ "E-Test",
                                                                            grepl("\\bNM",Method_init) ~ "Disk"
                                                         ))
          
          if(sum(grepl("\\bND|\\bNM|\\bNE",unique(check_whonet_format$Method))) == length(unique(check_whonet_format$Method)))
          {
            df_processed_new <- check_whonet_format %>% select(names(core_map)) %>% unique()
            for(i in unique(check_whonet_format$Method)){
              temp <- check_whonet_format %>% filter(Method == i) %>% 
                mutate(Antibiotic_Name_Temp = str_split_i(Antibiotic_Name,"_|-",1),
                       Antibiotic_Name = Antibiotic_Name_Temp)
              
              temp <- temp %>% select(-c(Antibiotic_Name_Temp,Result))
              df_processed_new <- left_join(df_processed_new,temp)
            }
          }
          
          
          incProgress(0.1, detail = "Standardizing (AST)...")
          df_processed <- df_processed %>% mutate(
            mo_code = as.mo(Pathogen),
            kingdom = mo_kingdom(mo_code),
            gram_stain = mo_gramstain(mo_code),
            ab_code = as.ab(Antibiotic_Name),
            AntibioticName = ab_name(ab_code),
            MIC = as.mic(str_extract(
              Result, "(<=|>=|<|>)?\\s*[0-9.]+"
            )),
            TempInterpretation = as.sir(str_extract(Result, "R|S|I|SSD|NI")),
            Interpretation = if_else(
              !is.na(MIC) & is.na(TempInterpretation),
              as.sir(
                MIC,
                mo = mo_code,
                ab = ab_code,
                guideline = input$guideline
              ),
              TempInterpretation
            )
          ) %>%
            select(-c(TempInterpretation, Result, Antibiotic_Name))
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
          incProgress(0.1, detail = "Standardizing (AST)...")
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
                ),!is.na(Zone) & is.na(TempInterpretation) ~ as.sir(
                  Zone,
                  mo = mo_code,
                  ab = ab_code,
                  guideline = input$guideline
                ),
                as.sir(TempInterpretation)
              )
            ) %>%
            select(-c(TempInterpretation, Antibiotic_Name))
        }
        
        # --- Data Type Conversion & Cleaning ---
        if ("SampleDate" %in% names(df_processed)) {
          date_formats_to_try <- c(
            "%Y-%m-%d",
            "%d/%m/%Y",
            "%m/%d/%Y",
            "%d-%b-%y",
            "%Y/%m/%d",
            "%b %d %Y",
            "%d-%b-%Y"
          ) # Expanded formats
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
        incProgress(0.3, detail = "Calculating MDR status...")
        rv$mdr_data <- df_processed %>% tidyr::pivot_wider(
          id_cols = c(PatientID, SampleID, Pathogen, mo_code),
          names_from = ab_code,
          values_from = Interpretation
        ) %>% mutate(mdr = mdro(guideline = "CMI2012", pct_required_classes = 0.25))
        
        # --- Store Processed Data ---
        incProgress(0.4, detail = "Finalizing...")
        rv$processed_data <- df_processed
        showNotification("Data processed successfully! Dashboard updated.", type =
                           "message")
        
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
  observe({ updateVirtualSelect(session = session,
                                inputId = "PID_show", choices = patient_ids(), selected = ifelse(length(patient_ids())>0, patient_ids()[1], "")) })
  observe({ updateVirtualSelect(session = session,
                                inputId = "SID_show", choices = sample_ids(), selected = ifelse(length(sample_ids())>0, sample_ids()[1], "")) })
  observe({ updateVirtualSelect(session = session,
                                inputId = "Pathogen_show", choices = pathogen_names(), selected = ifelse(length(pathogen_names())>0, pathogen_names()[1], "")) })
  
  filtered_review_data <- reactive({
    req(rv$processed_data, input$PID_show, input$SID_show, input$Pathogen_show)
    rv$processed_data %>%
      filter(
        PatientID == input$PID_show &
        SampleID == input$SID_show &
        Pathogen == input$Pathogen_show
      )
  })
  
  mdr_review_data <- reactive({
    req(rv$mdr_data, input$PID_show, input$SID_show, input$Pathogen_show)
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
      paste("Date Sample Collected:", paste(format(unique_dates, "%d-%m-%Y"), collapse=", "))
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
      paste("Sample Type:", paste(unique_types, collapse=", "))
    } else {
      "Sample Type: Not Available"
    }
  })
  
  output$Kingdom_text <- renderText({
    data_sub <- filtered_review_data()
    validate(need(nrow(data_sub) > 0, "")) 
    unique_types <- unique(data_sub$kingdom)
    paste("Kingdom:", paste(unique_types, collapse=", "))
  })
  
  output$GramStain_text <- renderText({
    data_sub <- filtered_review_data()
    validate(need(nrow(data_sub) > 0, "")) 
    unique_types <- unique(data_sub$gram_stain)
    paste("Gram stain:", paste(unique_types, collapse=", "))
  })
  
  output$MDR_text <- renderText({
    data_sub <- mdr_review_data()
    validate(need(nrow(data_sub) > 0, "")) 
    unique_types <- unique(data_sub$mdr)
    paste("MDR Status (CMI2012):", paste(unique_types, collapse=", "))
  })
  
  output$AST_data_view_table <- DT::renderDataTable({
    data_sub <- filtered_review_data()
    cols_AST <- c("AntibioticName", "MIC", "Zone", "Interpretation", "Menthod")
    data_sub <- data_sub %>% select(any_of(cols_AST))
    datatable(
      data_sub,
      #width = "100%",
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
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
      req(rv$processed_data,rv$mdr_data) # Need processed data
      
      data_processed_to_download <- rv$processed_data
      data_mdr_to_download <- rv$mdr_data
      
      wb <- createWorkbook()
      addWorksheet(wb, "Processed Data")
      addWorksheet(wb, "MDR Data")
      
      writeData(wb,1,data_processed_to_download)
      writeData(wb,2,data_mdr_to_download)
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
  output$raw_data_view_table <- DT::renderDataTable({
    validate(need(
      rv$uploaded_data,
    ))
    datatable(
      rv$uploaded_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = TRUE,
        lengthChange = FALSE,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  output$pathogen_summary_plot <- renderPlot({
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
        aes(label = scales::comma(Frequency)),
        hjust = -0.2,
        size = 3.5,
        color = "black"
      ) +
      # Add buffer to x-axis (now y after coord_flip) for labels
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
    
    # Return the plot
    print(gg)
    
  }, # Set resolution for better plot quality if needed
  res = 96) # End pathogen_summary_plot renderPlot
  
}


##### 3. Run App #####
shinyApp(ui = ui, server = server)