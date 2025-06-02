library(AMR)  
source("utils/ui_utils.R", local = TRUE)
source("utils/amr_utils.R", local = TRUE)

ui_data_input < function(){
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
                column(6, strong(
                  textOutput(outputId = "SampleDate_text")
                ), strong(
                  textOutput(outputId = "SampleType_text")
                )),
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
  )
}