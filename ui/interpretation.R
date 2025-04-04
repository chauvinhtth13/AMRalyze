interpretation_ui <- function() {
  div(
    style = "margin-top: 20px;",
    selectInput(
      "interpretation_mode",
      "Interpretation Mode",
      choices = unique(clinical_breakpoints$guideline),
      selected = "CLSI 2024"
    ),
    actionBttn(
      "btn_interpret",
      "Interpret",
      style = "gradient",
      color = "primary"
    ),
    conditionalPanel(
      condition = "input.btn_interpret",
      div(
        style = "margin-top: 20px;",
        selectInput("patient_id", "Patient ID", choices = NULL),
        selectInput("sample_id", "Sample ID", choices = NULL),
        selectInput("pathogen", "Pathogen", choices = NULL)
      )
    )
  )
}