#source("ui/card_data_preview.R", local = TRUE)

tab_data_input <- function() {
  nav_panel(
    title = "Data Input",
    icon = bsicons::bs_icon("upload"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = "20%",
        
        ##### Step 1: File Upload #####
        h5("Step 1: Upload Data File", class = "step-header required-field"),
        p(
          "Upload your antimicrobial susceptibility test data (CSV or Excel format)",
          class = "info-text"
        ),
        fileInput(
          "file_browse",
          NULL,
          accept = c(
            "text/csv",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
          ),
          buttonLabel = "Choose File",
          placeholder = "No file selected (Max 10MB)"
        ),
        
        uiOutput("sheet_selector"),
        
        # Upload button
        actionButton(
          "btn_upload",
          "Upload Data",
          class = "btn-primary w-100",
          icon = icon("eye")
        ),
        
        hr(),
        
        ##### Step 2: Configure Analysis #####
        h5("Step 2: Analysis Configuration", class = "step-header"),
        
        virtualSelectInput(
          "guideline",
          tags$strong("Clinical breakpoint guidelines"),
          choices = sort(unique(clinical_breakpoints$guideline)),
          selected = get_newest_guideline(),
          multiple = FALSE,
          search = TRUE,
          placeholder = "Select row number or record index column"
        ),
        
        p(
          tags$strong("Multi-Drug Resistant Calculated", class = "required-field")
        ),
        radioButtons(
          inputId = "MDR_cal",
          label = NULL,
          choices = c("Yes" = TRUE, "No" = FALSE),
          selected = TRUE,
          inline = TRUE
        ),
        
        hr(),
        
        ##### Step 3: Column Mapping #####
        h5("Step 3: Column Mapping", class = "step-header"),
        p(
          "Assign the appropriate columns from your uploaded dataset to the required fields below. Fields marked with * are mandatory.",
          class = "info-text"
        ),
        
        h6("Select at least one identifier column", style = "color: #0d6efd; font-weight: bold;"),
        
        virtualSelectInput(
          "NoID",
          tags$strong("No. Column"),
          choices = NULL,
          multiple = FALSE,
          search = TRUE,
          hideClearButton = FALSE,
          placeholder = "Select row number or record index column"
        ),
        
        virtualSelectInput(
          "PID",
          tags$strong("Patient ID Column"),
          choices = NULL,
          multiple = FALSE,
          search = TRUE,
          hideClearButton = FALSE,
          placeholder = "Select column containing patient ID"
        ),
        
        virtualSelectInput(
          "SID",
          tags$strong("Sample ID Column"),
          choices = NULL,
          multiple = FALSE,
          search = TRUE,
          hideClearButton = FALSE,
          placeholder = "Select column containing sample ID"
        ),
        
        h6("Select sample metadata fields", style = "color: #0d6efd; font-weight: bold;"),
        
        p(
          "Please ensure you select the Organism/Pathogen column below. This is a required field for downstream analysis.",
          class = "info-text"
        ),
        
        virtualSelectInput(
          "Sample_Date",
          tags$strong("Sampling Date Column"),
          choices = NULL,
          multiple = FALSE,
          search = TRUE,
          showValueAsTags = TRUE,
          hideClearButton = FALSE,
          placeholder = "Select sampling date column (optional)"
        ),
        
        virtualSelectInput(
          "Sample_Type",
          tags$strong("Sample Type Column"),
          choices = NULL,
          multiple = FALSE,
          search = TRUE,
          showValueAsTags = TRUE,
          hideClearButton = FALSE,
          placeholder = "Select specimen type column (optional)"
        ),
        
        virtualSelectInput(
          "Pathogen",
          tags$strong("Organism/Pathogen Column *"),
          choices = NULL,
          multiple = FALSE,
          search = TRUE,
          hideClearButton = FALSE,
          placeholder = "Select column containing pathogen or organism name"
        ),
        
        h6("Antibiotic Susceptibility Test (AST) Results", style = "color: #0d6efd; font-weight: bold;"),
        
        virtualSelectInput(
          "AB_cols",
          NULL,
          choices = NULL,
          multiple = TRUE,
          search = TRUE,
          showValueAsTags = TRUE,
          placeholder = "Select all columns containing antibiotic test results",
          optionsCount = 12
        ),
        
        hr(),
        
        actionButton(
          "btn_process",
          "Process Data",
          class = "btn-success w-100",
          style = "height: 50px; font-size: 16px;",
          icon = icon("cogs")
        )
      ),
      card(
        card_header(div(
          h6("Overview AST Data", class = "mb-0"),
          span(
            downloadButton("download_ast_data", "Download AST Data", class = "btn-outline-success btn-sm"),
            style = "float: right;"
          )
        ), class = "bg-light"),
        card_body(dataTableOutput("data_AST_Table"))
      ),
      card(
        card_header(div(
          h6("Overview MDR Data", class = "mb-0"),
          span(
            downloadButton("download_mdr_data", "Download MDR Data", class = "btn-outline-success btn-sm"),
            style = "float: right;"
          )
        ), class = "bg-light"),
        card_body(dataTableOutput("data_MDR_Table"))
      )
    )
  )
}
