file_upload_ui <- function() {
  div(
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
    conditionalPanel(
      condition = "output.data_path",
      actionBttn(
        "btn_upload",
        "Upload Data",
        style = "gradient",
        color = "primary"
      )
    ),
    tags$hr()
  )
}
