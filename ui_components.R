# ui_components.R
# UI Component Functions

# File Upload Section
file_upload_ui <- function() {
  div(
    fileInput("file_browse", "Upload CSV/XLSX File", accept = c(".csv", ".xlsx"),
              buttonLabel = "Browse...", placeholder = "No file selected"),
    conditionalPanel(condition = "output.show_sheet_selector",
                     selectInput("sheet_name", "Select Sheet", choices = NULL)),
    actionBttn("btn_upload", "Upload Data", style = "gradient", color = "primary"),
    tags$hr()
  )
}

# Data Configuration Section
data_config_ui <- function() {
  div(
    conditionalPanel(
      condition = "output.data_uploaded",
      selectInput("PID", "Patient ID Column", choices = NULL),
      selectInput("SID", "Sample ID Column", choices = NULL),
      selectInput("Sample_Data", "Sampling Date Column", choices = NULL),
      selectInput("Sample_Type", "Sample Type Column", choices = NULL),
      selectInput("Pathogen", "Pathogen Column", choices = NULL),
      radioButtons("data_structure", "AST Data Structure:",
                   choiceNames = list(
                     "Multiple antibiotic columns with MIC/Interpretation",
                     "Separate antibiotic and MIC/Zone Size columns (Recommended)"
                   ), choiceValues = list("0", "1"), selected = "1"),
      conditionalPanel(condition = "input.data_structure == '0'",
                       selectInput("AB_type_0", "Antibiotics Columns", choices = NULL, multiple = TRUE)),
      conditionalPanel(condition = "input.data_structure == '1'",
                       selectInput("AB_type_1", "Antibiotics Column", choices = NULL),
                       selectInput("interpretation", "Interpretation Column (optional)", choices = NULL),
                       selectInput("MIC", "MIC Column", choices = NULL),
                       selectInput("Zone_Size", "Zone Size Column", choices = NULL),
                       selectInput("Methods", "Test Method", choices = NULL)),
      actionBttn("btn_process", "Process Data", style = "gradient", color = "success")
    )
  )
}