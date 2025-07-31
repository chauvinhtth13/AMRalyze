##### 1.1. Dashboard Tab #####
tab_dashboard <- function() {
  nav_panel(
    title = "Dashboard",
    icon = bsicons::bs_icon("graph-up"),
    
    ##### Summary Cards Section #####
    div(
      class = "container-fluid px-3 py-3",
      div(
        class = "mb-4",
        h3("Data Overview", class = "mb-4 text-primary fw-bold"),
        div(
          class = "row g-4",
          div(
            class = "col-12 col-md-6 col-lg-3",
            value_box(
              title = "Total Records",
              value = textOutput("total_records"),
              showcase = bsicons::bs_icon("file-earmark-ruled"),
              class = "h-100",
              style = "background-color: #D6E4FF !important; color: #003E8A !important; border: 1px solid #B8D4FF;"
            )
          )
        )
      ),
      
      ##### Pathogen Summary Section #####
      div(
        class = "mb-4",
        card(
          class = "shadow-sm",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              h4("Pathogen Distribution", class = "mb-0 text-primary fw-bold")
            ),
            class = "bg-light border-0"
          ),
          card_body(
            plotlyOutput("pathogen_summary_plot", width = "100%", height = "450px"),
            class = "p-3"
          ),
          style = "min-height: 500px;"
        )
      ),
      
      ##### Resistance Patterns Section #####
      card(
        class = "shadow-sm",
        card_header(
          h4("Antimicrobial Resistance Patterns", class = "mb-0 text-primary fw-bold"),
          class = "bg-light border-0"
        ),
        navset_card_tab(
          id = "ast_card_tabs",
          
          ##### Gram-Negative Tab #####
          nav_panel(
            title = "Gram-Negative",
            layout_sidebar(
              sidebar = sidebar(
                width = "300px",
                open = "always",
                class = "bg-light rounded p-3",
                div(
                  class = "mb-3",
                  h5("Filter Options", class = "text-primary fw-bold mb-2"),
                  p("Select pathogens to analyze resistance patterns", class = "text-muted small mb-3")
                ),
                virtualSelectInput(
                  "list_gram_negative", "Pathogen Selection",
                  choices = NULL, multiple = TRUE, search = TRUE,
                  showValueAsTags = TRUE, placeholder = "Select pathogen(s)...",
                  disableSelectAll = FALSE, optionsCount = 10
                )
              ),
              div(
                class = "flex-fill",
                card(
                  class = "h-100",
                  card_header(
                    div(
                      class = "d-flex justify-content-between align-items-center",
                      h5("Resistance Summary Table", class = "mb-0"),
                      downloadButton("download_gram_neg", "Export Results", class = "btn btn-outline-primary btn-sm")
                    )
                  ),
                  card_body(gt_output("ast_gram_negative_table"), class = "overflow-auto")
                )
              )
            )
          ),
          
          ##### Gram-Positive Tab #####
          nav_panel(
            title = "Gram-Positive",
            layout_sidebar(
              sidebar = sidebar(
                width = "300px",
                open = "always",
                class = "bg-light rounded p-3",
                div(
                  class = "mb-3",
                  h5("Filter Options", class = "text-primary fw-bold mb-2")
                ),
                virtualSelectInput(
                  "list_gram_positive", "Pathogen Selection",
                  choices = NULL, multiple = TRUE, search = TRUE,
                  showValueAsTags = TRUE, placeholder = "Select pathogen(s)...",
                  disableSelectAll = FALSE
                )
              ),
              div(
                class = "flex-fill",
                card(
                  class = "h-100",
                  card_header(
                    div(
                      class = "d-flex justify-content-between align-items-center",
                      h5("Resistance Summary Table", class = "mb-0"),
                      downloadButton("download_gram_pos", "Export Results", class = "btn btn-outline-primary btn-sm")
                    )
                  ),
                  card_body(gt_output("ast_gram_positive_table"), class = "overflow-auto")
                )
              )
            )
          ),
          
          ##### Fungal Tab #####
          nav_panel(
            title = "Fungal",
            layout_sidebar(
              sidebar = sidebar(
                width = "300px",
                open = "always",
                class = "bg-light rounded p-3",
                div(
                  class = "mb-3",
                  h5("Filter Options", class = "text-primary fw-bold mb-2")
                ),
                virtualSelectInput(
                  "list_fungal", "Pathogen Selection",
                  choices = NULL, multiple = TRUE, search = TRUE,
                  showValueAsTags = TRUE, placeholder = "Select pathogen(s)...",
                  disableSelectAll = FALSE
                )
              ),
              div(
                class = "flex-fill",
                card(
                  class = "h-100",
                  card_header(
                    div(
                      class = "d-flex justify-content-between align-items-center",
                      h5("Antifungal Resistance Summary", class = "mb-0"),
                      downloadButton("download_fungal", "Export Results", class = "btn btn-outline-primary btn-sm")
                    )
                  ),
                  card_body(gt_output("ast_fungal_table"), class = "overflow-auto")
                )
              )
            )
          )
        )
      )
    )
  )
}
