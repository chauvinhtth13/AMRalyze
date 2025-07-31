
##### 2.2. Server Logic For Dashboard #####

###### 2.2.1 Dashboard Metrics  ######
output$total_records <- renderText({
  df <- rv$uploaded_data
  if (is.null(df))
    "0"
  else
    scales::comma(nrow(df))
})
output$total_patients <- renderText({
  df <- rv$processed_data
  if (is.null(df))
    "0"
  else
    scales::comma(n_distinct(df$PatientID, na.rm = TRUE))
})
output$total_samples <- renderText({
  df <- rv$processed_data
  if (is.null(df))
    "0"
  else
    scales::comma(n_distinct(df$SampleID, na.rm = TRUE))
})

###### 2.1.7. Main Content Area Preview ######


output$pathogen_summary_plot <- renderPlotly({
  # Require processed data
  validate(
    need(
      rv$processed_data,
      "Please upload and process data first ('Input' tab)."
    ),
    need(nrow(rv$processed_data) > 0, "Processed data is empty.")
  )
  
  pathogen_freq <- rv$processed_data %>%
    dplyr::distinct(PatientID, SampleID, Pathogen) %>% # Count each pathogen once per sample
    dplyr::count(Pathogen, sort = TRUE, name = "Frequency")
  
  # Check if calculation resulted in data
  validate(need(nrow(pathogen_freq) > 0, "No pathogen data to summarize."))
  
  # --- Optional: Group less frequent pathogens into 'Other' ---
  top_n_pathogens <- 15 # Show top N pathogens separately
  if (nrow(pathogen_freq) > top_n_pathogens) {
    pathogen_freq <- pathogen_freq %>%
      dplyr::mutate(
        Pathogen_Group = dplyr::case_when(
          dplyr::row_number() <= top_n_pathogens ~ Pathogen,
          TRUE ~ "Other (< Min Frequency)" # Group the rest
        )
      ) %>%
      dplyr::group_by(Pathogen_Group) %>%
      dplyr::summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
      dplyr::rename(Pathogen = Pathogen_Group) # Rename for plotting
  }
  
  # Create the plot using ggplot2
  gg <- ggplot(pathogen_freq, aes(x = reorder(Pathogen, Frequency), y = Frequency)) +
    geom_col(fill = "#2c7fb8", width = 0.7) + # Use a color for bars
    coord_flip() + # Flip coordinates for better readability of labels
    labs(title = "Frequency of Identified Pathogens", x = "Pathogen", y = "Number of Samples") +
    theme_minimal(base_size = 12) + # Use a clean theme
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      # Center title
      panel.grid.major.y = element_blank(),
      # Remove horizontal grid lines
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(size = 10),
      # Adjust text size if needed
      axis.text.x = element_text(size = 10)
    ) +
    # Add frequency labels to the bars
    geom_text(
      aes(label = scales::comma(Frequency), y = Frequency * 1.08),
      hjust = -0.2,
      size = 5,
      color = "black"
    )
  
  # Return the plot
  ggplotly(gg)
  
})

observe({
  req(rv$mdr_data)
  list_gram_negative <- rv$mdr_data %>%
    dplyr::distinct(PatientID, SampleID, Pathogen, kingdom, gram_stain) %>%
    dplyr::filter(kingdom == "Bacteria" &
                    gram_stain == "Gram-negative") %>%
    dplyr::count(Pathogen, sort = TRUE, name = "Frequency") %>%
    pull(Pathogen)
  
  selected_choice <- if (length(list_gram_negative) >= 4) {
    # Use the value from the original vector for selection logic
    list_gram_negative[1:4]
  } else {
    NULL
  }
  
  updateVirtualSelect(
    session = session,
    inputId = "list_gram_negative",
    choices = list_gram_negative,
    selected = selected_choice
  )
})



output$ast_gram_negative_table <- render_gt({
  validate(need(
    rv$uploaded_data,
    "Please upload and process data first ('Input' tab)."
  ))
  validate(need(
    length(input$list_gram_negative) > 0,
    "Choose Gram-negative Pathogen."
  ))
  req(rv$mdr_data)
  data_sub <- rv$mdr_data %>%
    filter(Pathogen %in% input$list_gram_negative) %>%
    select(-c(PatientID, SampleID, mo_code, kingdom, gram_stain)) %>%
    select(where( ~ !all(is.na(.)))) %>%
    rename_if(is.sir, ab_name) %>%
    mutate_if(
      is.sir,
      .funs = function(x)
        if_else(x == "R", TRUE, FALSE)
    )
  list_ab <- setdiff(names(data_sub), c("Pathogen", "MDR"))
  
  
  gt_table <- data_sub %>%
    tbl_summary(
      by = Pathogen,
      statistic = everything() ~ "{p}% ({n}/{N})",
      missing = "no",
      digits = everything() ~ c(1, 1)
    ) %>%
    modify_header(label = "") %>%
    modify_column_indent(columns = label, row = label != "MDR") %>%
    modify_footnote(all_stat_cols() ~ "Resitant Percent (%), (Resitant case / Total) ") %>%
    modify_table_body(~ .x %>%
                        mutate(
                          across(all_stat_cols(), ~ gsub("^NA.*0.0/0.0)", "-", .)),
                          across(all_stat_cols(), ~ gsub("0\\.0 \\(NA%\\)", "-", .))
                        )) %>%
    as_gt() %>%
    add_group_antibiotic(list_ab = list_ab)
  return(gt_table)
})