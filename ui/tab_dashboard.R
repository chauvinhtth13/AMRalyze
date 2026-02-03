##### Dashboard Tab #####
tab_dashboard <- function() {
  nav_panel(
    title = "Dashboard",
    icon = bsicons::bs_icon("bar-chart"),
    
    div(
      class = "p-3",
      
      ##### Row 1: Key Metrics #####
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,
        
        value_box(
          title = "Total Records",
          value = textOutput("total_records"),
          theme = value_box_theme(bg = "#e8f4fd", fg = "#1565c0"),
          showcase = bsicons::bs_icon("clipboard-data")
        ),
        value_box(
          title = "Patients",
          value = textOutput("total_patients"),
          theme = value_box_theme(bg = "#e8f5e9", fg = "#2e7d32"),
          showcase = bsicons::bs_icon("people")
        ),
        value_box(
          title = "Samples",
          value = textOutput("total_samples"),
          theme = value_box_theme(bg = "#f3e5f5", fg = "#7b1fa2"),
          showcase = bsicons::bs_icon("droplet")
        ),
        value_box(
          title = "Date Range",
          value = textOutput("timeline"),
          theme = value_box_theme(bg = "#fff8e1", fg = "#f57c00"),
          showcase = bsicons::bs_icon("calendar-range")
        )
      ),
      
      ##### Row 2: MDR Metrics #####
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,
        class = "mt-3",
        
        value_box(
          title = "MDR Isolates",
          value = textOutput("mdr_count"),
          theme = value_box_theme(bg = "#fff3e0", fg = "#e65100"),
          showcase = bsicons::bs_icon("shield-exclamation")
        ),
        value_box(
          title = "XDR Isolates",
          value = textOutput("xdr_count"),
          theme = value_box_theme(bg = "#ffebee", fg = "#c62828"),
          showcase = bsicons::bs_icon("shield-x")
        ),
        value_box(
          title = "MDR Rate",
          value = textOutput("mdr_rate"),
          theme = value_box_theme(bg = "#e0f2f1", fg = "#00695c"),
          showcase = bsicons::bs_icon("percent")
        ),
        value_box(
          title = "Top Resistant",
          value = textOutput("top_resistant"),
          theme = value_box_theme(bg = "#fce4ec", fg = "#ad1457"),
          showcase = bsicons::bs_icon("bug")
        )
      ),
      
      ##### Row 3: Charts #####
      layout_columns(
        col_widths = c(8, 4),
        fill = TRUE,
        class = "mt-4",
        
        card(
          full_screen = TRUE,
          card_header("Pathogen Distribution"),
          card_body(
            plotlyOutput("pathogen_summary_plot", height = "350px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Sample Types"),
          card_body(
            plotlyOutput("sample_type_plot", height = "350px")
          )
        )
      ),
      
      ##### Row 4: Trend Chart #####
      card(
        class = "mt-3",
        full_screen = TRUE,
        card_header("Resistance Trends Over Time"),
        card_body(
          plotlyOutput("resistance_trend_plot", height = "300px")
        )
      ),
      
      ##### Row 5: Resistance Patterns #####
      card(
        class = "mt-3",
        full_screen = TRUE,
        card_header("Antimicrobial Resistance Patterns"),
        card_body(
          navset_underline(
            nav_panel(
              "Gram-negative", 
              div(class = "mb-2", uiOutput("gram_neg_selector")),
              gt_output("ast_gram_negative_table")
            ),
            nav_panel(
              "Gram-positive", 
              div(class = "mb-2", uiOutput("gram_pos_selector")),
              gt_output("ast_gram_positive_table")
            ),
            nav_panel(
              "Fungal", 
              div(class = "mb-2", uiOutput("fungal_selector")),
              gt_output("ast_fungal_table")
            )
          )
        )
      )
    )
  )
}
