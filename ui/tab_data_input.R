##### Data Input Tab #####
tab_data_input <- function() {
  nav_panel(
    title = "Data Input",
    icon = bsicons::bs_icon("upload"),
    
    layout_sidebar(
      fillable = FALSE,  # Allow content to scroll naturally
      sidebar = sidebar(
        width = "320px",
        open = TRUE,
        
        ##### Step 1: Upload #####
        div(
          class = "mb-3 p-3 bg-white border rounded",
          h6(class = "fw-bold text-primary mb-3", "1. Upload File"),
          fileInput(
            "file_browse", NULL,
            accept = c("text/csv", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
            buttonLabel = "Browse...",
            placeholder = "CSV or Excel file"
          ),
          uiOutput("sheet_selector"),
          actionButton("btn_upload", "Load Data", class = "btn-outline-primary w-100 mt-2")
        ),
        
        ##### Step 2: Settings #####
        div(
          class = "mb-3 p-3 bg-white border rounded",
          h6(class = "fw-bold text-primary mb-3", "2. Analysis Settings"),
          virtualSelectInput(
            "guideline", "Breakpoint Guideline",
            choices = sort(unique(clinical_breakpoints$guideline)),
            selected = get_newest_guideline(),
            search = TRUE
          ),
          div(
            class = "d-flex justify-content-between align-items-center mt-3",
            span("Calculate MDR", class = "small fw-medium"),
            switchInput("MDR_cal", value = TRUE, size = "mini", onLabel = "Yes", offLabel = "No")
          )
        ),
        
        ##### Step 3: Mapping #####
        div(
          class = "mb-3 p-3 bg-white border rounded",
          h6(class = "fw-bold text-primary mb-3", "3. Column Mapping"),
          
          tags$small(class = "text-muted fw-bold d-block mb-2", "IDENTIFIERS"),
          virtualSelectInput("NoID", "Row ID", choices = NULL, placeholder = "Optional", search = TRUE),
          virtualSelectInput("PID", "Patient ID", choices = NULL, placeholder = "Optional", search = TRUE),
          virtualSelectInput("SID", "Sample ID", choices = NULL, placeholder = "Optional", search = TRUE),
          
          hr(class = "my-3"),
          
          tags$small(class = "text-muted fw-bold d-block mb-2", "SAMPLE INFO"),
          virtualSelectInput("Sample_Date", "Date", choices = NULL, placeholder = "Optional", search = TRUE),
          virtualSelectInput("Sample_Type", "Sample Type", choices = NULL, placeholder = "Optional", search = TRUE),
          virtualSelectInput(
            "Pathogen", 
            tags$span("Pathogen ", tags$span(class = "text-danger", "*")), 
            choices = NULL, 
            placeholder = "Required", 
            search = TRUE
          ),
          
          hr(class = "my-3"),
          
          tags$small(class = "text-muted fw-bold d-block mb-2", "AST COLUMNS"),
          virtualSelectInput(
            "AB_cols", "Antibiotic Columns",
            choices = NULL, multiple = TRUE, showValueAsTags = TRUE,
            placeholder = "Select columns...", optionsCount = 12
          )
        ),
        
        ##### Process Button #####
        actionButton(
          "btn_process", "Process Data", 
          class = "btn-primary w-100 py-2",
          icon = icon("cogs")
        )
      ),
      
      ##### Main Content Area #####
      # Warnings and Summary
      uiOutput("unrecognized_ab_card"),
      uiOutput("processing_warnings_card"),
      uiOutput("processing_summary_card"),
      
      # AST Data Card
      card(
        class = "mb-3",
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("AST Data", class = "fw-bold"),
          downloadButton("download_ast_data", "Export CSV", class = "btn-outline-success btn-sm")
        ),
        card_body(
          DTOutput("data_AST_Table")
        )
      ),
      
      # MDR Data Card
      card(
        class = "mb-3",
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("MDR Analysis", class = "fw-bold"),
          downloadButton("download_mdr_data", "Export CSV", class = "btn-outline-success btn-sm")
        ),
        card_body(
          DTOutput("data_MDR_Table")
        )
      )
    )
  )
}
