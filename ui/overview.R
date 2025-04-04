overview_ui <- function() {
  div(
    conditionalPanel(
      condition = "output.data_processed",
      box(
        title = "Data Summary",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        textOutput("summary_records"),
        textOutput("summary_patients"),
        textOutput("summary_samples"),
        textOutput("summary_period")
      )
    ),
    DTOutput("processed_table")
  )
}