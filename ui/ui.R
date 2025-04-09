ui <- page_navbar(
  title = "AMRalyze Dashboard",
  
  #####Dashboard Tab#####
  nav_panel(title = "Dashboard", # Main content for the "Plot" tab
            card(
              layout_columns(
                col_widths = c(4, 4, 4),
                # Divide into 3 equal columns
                value_box(
                  title = "Total Record",
                  value = textOutput("total_recode"),
                  showcase = bs_icon("file-text"),
                ),
                value_box(
                  title = "Total Patient",
                  value = textOutput("total_patient"),
                  showcase = bs_icon("people"),
                ),
                value_box(
                  title = "Total Sample",
                  value = textOutput("Total Sample"),
                  showcase = bsicons::bs_icon("radioactive"),
                  # Example icon
                )
              ),
              layout_columns(col_widths = c(6, 6), # Divide into 3 equal columns
                             card(), card())
            )),
  ##### Input Tab #####
  nav_panel(title = "Input", # Use layout_sidebar to create a sidebar *within this tab*
            layout_sidebar(
              sidebar = sidebar(
                title = "Input Congfiguration",
                fileInput(
                  "file_browse",
                  "Upload CSV/XLSX File",
                  accept = c(".csv", ".xlsx"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                ),
                conditionalPanel(
                  condition = "output.show_sheet_selector",
                  selectInput("sheet_name", "Select Sheet", choices = NULL)
                ),
                conditionalPanel(condition = "output.data_path", actionButton("btn_upload", "Upload Data")),
                conditionalPanel(
                  condition = "output.data_uploaded",
                  pickerInput(
                    "PID",
                    "Patient ID"),
                  pickerInput(
                    "SID", "Sample ID"
                  ),
                  pickerInput(
                    "Sample_Data", "Sampling Date"
                  ),
                  pickerInput(
                    "Sample_Type", "Sample Type"
                  ),
                  pickerInput(
                    "Pathogen", "Pathogen"
                  ),
                  pickerInput(
                    "AB_type_1", "Antibiotics"
                  ),
                  pickerInput(
                    "MIC", "MIC"
                  )
                ),
                card()
              )
            ))
)