card_data_preview <- function() {
  card(
    card_header(div(
      h6("Data Review & Validation", class = "mb-0"),
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
}