input_ui <- function() {
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
                conditionalPanel(condition = "output.data_path", actionButton("btn_upload", "Upload Data"))
              ),
              card(
              )
            ))
}
