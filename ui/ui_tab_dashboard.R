nav_panel(
  title = "Dashboard",
  icon = bsicons::bs_icon("graph-up"),
  
  # Summary Cards Section
  h3("Data Overview", class = "mb-3"),
  layout_columns(
    col_widths = c(4, 4, 4),
    min_height = "120px",
    max_height = "120px",
    fill = TRUE,
    value_box(
      height = "120px",
      title = "Total Records",
      value = textOutput("total_records"),
      showcase = bsicons::bs_icon("file-earmark-ruled"),
      theme = "primary"
    ),
    value_box(
      height = "120px",
      title = "Unique Patients",
      value = textOutput("total_patients"),
      showcase = bsicons::bs_icon("person-vcard"),
      theme = "info"
    ),
    value_box(
      height = "120px",
      title = "Total Samples",
      value = textOutput("total_samples"),
      showcase = bsicons::bs_icon("droplet"),
      theme = "success"
    )
  ),
  
  # Pathogen Summary Section
  br(),
  card(
    min_height = "500px",
    max_height = "500px",
    card_header(h4("Pathogen Distribution", class = "mb-0"), class = "bg-light"),
    card_body(
      fill = TRUE,
      plotlyOutput("pathogen_summary_plot", width = "100%", height = "400px")
    )
  ),
  
  # Resistance Patterns Section
  br(),
  card(
    min_height = "800px",
    card_header(h4(
      "Antimicrobial Resistance Patterns", class = "mb-0"
    ), class = "bg-light"),
    navset_card_tab(
      id = "ast_card_tabs",
      
      # Gram-Negative Tab
      nav_panel(title = "Gram-Negative", layout_sidebar(
        sidebar = sidebar(
          width = "25%",
          h5("Filter Options", class = "step-header"),
          p("Select pathogens to analyze resistance patterns", class = "info-text"),
          virtualSelectInput(
            "list_gram_negative",
            "Pathogen Selection",
            choices = NULL,
            multiple = TRUE,
            search = TRUE,
            showValueAsTags = TRUE,
            placeholder = "Select pathogen(s)...",
            disableSelectAll = FALSE,
            optionsCount = 10
          ),
          hr(),
          # Add summary statistics
          h6("Quick Stats"),
          verbatimTextOutput("gram_neg_stats", placeholder = TRUE)
        ),
        card(
          card_header("Resistance Summary Table"),
          card_body(
            fill = TRUE,
            gt_output("ast_gram_negative_table"),
            br(),
            downloadButton("download_gram_neg", "Export Results", class = "btn-outline-primary btn-sm")
          )
        )
      )),
      
      # Gram-Positive Tab
      nav_panel(title = "Gram-Positive", layout_sidebar(
        sidebar = sidebar(
          width = "25%",
          h5("Filter Options", class = "step-header"),
          virtualSelectInput(
            "list_gram_positive",
            "Pathogen Selection",
            choices = NULL,
            multiple = TRUE,
            search = TRUE,
            showValueAsTags = TRUE,
            placeholder = "Select pathogen(s)...",
            disableSelectAll = FALSE
          )
        ),
        card(
          card_header("Resistance Summary Table"),
          card_body(
            fill = TRUE,
            gt_output("ast_gram_positive_table"),
            br(),
            downloadButton("download_gram_pos", "Export Results", class = "btn-outline-primary btn-sm")
          )
        )
      )),
      
      # Fungal Tab
      nav_panel(title = "Fungal", layout_sidebar(
        sidebar = sidebar(
          width = "25%",
          h5("Filter Options", class = "step-header"),
          virtualSelectInput(
            "list_fungal",
            "Pathogen Selection",
            choices = NULL,
            multiple = TRUE,
            search = TRUE,
            showValueAsTags = TRUE,
            placeholder = "Select pathogen(s)...",
            disableSelectAll = FALSE
          )
        ),
        card(
          card_header("Antifungal Resistance Summary"),
          card_body(
            fill = TRUE,
            gt_output("ast_fungal_table"),
            br(),
            downloadButton("download_fungal", "Export Results", class = "btn-outline-primary btn-sm")
          )
        )
      ))
    )
  )
)