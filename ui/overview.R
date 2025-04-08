overview_ui <- function() {
  div(
    style = "margin-top: 20px;",
    textOutput("summary_records"),
    textOutput("summary_patients"),
    textOutput("summary_samples"),
    textOutput("summary_period"),
    div(
      style = "margin-top: 20px;",
      DTOutput("processed_table"))
  )
}