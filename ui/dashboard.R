dashboard_ui <- function() {
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
              layout_columns(
                col_widths = c(6, 6),
                # Divide into 3 equal columns
                card(
                  card_header("Data Overview"),
                  card_body(plotOutput("distPlot")),
                  card_footer("Data overview controlled from the 'Input' tab.")
                ),
                card(
                  card_header("Data Overview"),
                  card_body(plotOutput("distPlot")),
                  card_footer("Data overview controlled from the 'Input' tab.")
                )
              )
            ))
}
