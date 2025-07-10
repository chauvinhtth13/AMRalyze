
##### 2.1.7 Show Patient Data #####
# Reactive expression for available Patient IDs after processing
patient_ids <- reactive({
  req(rv$processed_data)
  sort(unique(rv$processed_data$PatientID))
})
# Reactive expression for available Sample IDs based on selected Patient ID
sample_ids <- reactive({
  req(rv$processed_data, input$PID_show)
  rv$processed_data %>%
    filter(PatientID == input$PID_show) %>%
    pull(SampleID) %>%
    unique() %>%
    sort()
})
# Reactive expression for available Pathogens based on selected Patient and Sample ID
pathogen_names <- reactive({
  req(rv$processed_data, input$PID_show, input$SID_show)
  rv$processed_data %>%
    filter(PatientID == input$PID_show, SampleID == input$SID_show) %>%
    pull(Pathogen) %>% # Use the standardized Pathogen name
    unique() %>%
    sort()
})

# Update filter choices reactively
observe({
  updateVirtualSelect(
    session = session,
    inputId = "PID_show",
    choices = patient_ids(),
    selected = patient_ids()[1]
  )
})
observe({
  updateVirtualSelect(
    session = session,
    inputId = "SID_show",
    choices = sample_ids(),
    selected = sample_ids()[1]
  )
})
observe({
  updateVirtualSelect(
    session = session,
    inputId = "Pathogen_show",
    choices = pathogen_names(),
    selected = pathogen_names()[1]
  )
})

filtered_review_data <- reactive({
  req(rv$processed_data,
      input$PID_show,
      input$SID_show,
      input$Pathogen_show)
  rv$processed_data %>%
    filter(
      PatientID == input$PID_show &
        SampleID == input$SID_show &
        Pathogen == input$Pathogen_show
    )
})

mdr_review_data <- reactive({
  req(rv$mdr_data,
      input$PID_show,
      input$SID_show,
      input$Pathogen_show)
  rv$mdr_data %>%
    filter(
      PatientID == input$PID_show &
        SampleID == input$SID_show &
        Pathogen == input$Pathogen_show
    )
})

output$SampleDate_text <- renderText({
  data_sub <- filtered_review_data()
  validate(need(nrow(data_sub) > 0, "No data for current filter selection."))
  
  if ("SampleDate" %in% names(data_sub)) {
    unique_dates <- unique(data_sub$SampleDate)
    paste("Date Sample Collected:", unique_dates)
  } else {
    "Date Sample Collected: Not Available"
  }
})

output$SampleType_text <- renderText({
  data_sub <- filtered_review_data()
  validate(need(nrow(data_sub) > 0, "")) # Don't show message if date shown
  # Check if SampleType column exists after processing
  if ("SampleType" %in% names(data_sub)) {
    unique_types <- unique(data_sub$SampleType)
    paste("Sample Type:", unique_types)
  } else {
    "Sample Type: Not Available"
  }
})

output$Kingdom_text <- renderText({
  data_sub <- filtered_review_data()
  validate(need(nrow(data_sub) > 0, ""))
  unique_types <- unique(data_sub$kingdom)
  paste("Kingdom:", unique_types)
})

output$GramStain_text <- renderText({
  data_sub <- filtered_review_data()
  validate(need(nrow(data_sub) > 0, ""))
  unique_types <- unique(data_sub$gram_stain)
  paste("Gram stain:", unique_types)
})

output$MDR_text <- renderText({
  data_sub <- mdr_review_data()
  validate(need(nrow(data_sub) > 0, ""))
  unique_types <- unique(data_sub$MDR)
  paste("MDR Status (CMI2012):", unique_types)
})

output$AST_data_view_table <- DT::renderDataTable({
  data_sub <- filtered_review_data()
  data_sub <- data_sub %>% select(c(ab_code, AntibioticName, MIC, Zone, Interpretation, Method)) %>%
    mutate(ab_code = as.character(ab_code)) %>% filter(!(is.na(MIC) &
                                                           is.na(Zone) &
                                                           is.na(Interpretation)))
  names(data_sub) <- c(
    "Antibiotic Code",
    "Antibiotic Name",
    "MIC (mg/L)",
    "Zone Size (mm)",
    "Interpretation",
    "Method"
  )
  datatable(
    data_sub,
    options = list(
      paging = FALSE,
      scrollX = TRUE,
      searching = TRUE,
      lengthChange = FALSE,
      autoWidth = FALSE
    ),
    rownames = FALSE,
    filter = 'top'
  )
})



##### 2.1.8 Download Processed Data #####
output$download_processed_data <- downloadHandler(
  filename = function() {
    paste0("processed__all_data_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    req(rv$processed_data, rv$mdr_data) # Need processed data
    
    data_processed_to_download <- rv$processed_data
    data_mdr_to_download <- rv$mdr_data
    
    wb <- createWorkbook()
    addWorksheet(wb, "Processed Data")
    addWorksheet(wb, "MDR Data")
    
    writeData(wb, 1, data_processed_to_download)
    writeData(wb, 2, data_mdr_to_download)
    saveWorkbook(wb, file)
  }
)

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