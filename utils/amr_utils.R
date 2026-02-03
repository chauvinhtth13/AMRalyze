##### Tranform Symbol MIC####
tran_symbol <- function(x)
{
  x <- str_replace_all(x, "<=", "\u2264")
  x <- str_replace_all(x, "< =", "\u2264")
  x <- str_replace_all(x, ">=", "\u2265")
  x <- str_replace_all(x, "> =", "\u2265")
}

##### MIC 59-90 function #####
mic_x_cal <- function(list_mic, x = 0.5)
{
  mic_sort <- sort(list_mic)
  mic_sort <- as.character(mic_sort[!is.na(mic_sort)])
  n_length <- length(mic_sort)
  mic_x <-
    ifelse(n_length %% 2 == 0, mic_sort[n_length * x], mic_sort[(n_length +
                                                                   1) * x])
  mic_x <- tran_symbol(mic_x)
  return(mic_x)
}

##### MIC Min-Max #####
min_max_mic <- function(list_mic)
{
  mic_sort <- sort(list_mic)
  mic_sort <- as.character(mic_sort[!is.na(mic_sort)])
  n_length <- length(mic_sort)
  
  mic_range <- paste0(mic_sort[1], " to ", mic_sort[n_length])
  
  mic_range <- tran_symbol(mic_range)
  
  return(mic_range)
}

##### MIC Mode #####
mode_mic <- function(list_mic)
{
  mic_sort <- sort(list_mic)
  mic_sort <- as.character(mic_sort[!is.na(mic_sort)])
  freq <- data.frame(table(mic_sort))
  mic_mode <- subset(freq, Freq == max(Freq))
  mic_mode <-
    paste0(mic_mode[1, 1], " (", mic_mode[1, 2], "/", sum(freq[, 2]), ")")
  
  mic_mode <- tran_symbol(mic_mode)
  
  return(mic_mode)
}

##### MIC Skewness#####
skewness_mic <- function(list_mic)
{
  skewness_score <-
    round(skewness(na.omit(as.numeric(list_mic)), na.rm = TRUE), 2)
  skewness_score <-
    ifelse(is.na(skewness_score), "", as.character(skewness_score))
}

##### Multi_Plot_Function

plot_multi_mic <- function(data_freq, data_quantile)
{
  full_plot <- list()
  glegend <- 0
  for (i in unique(data_freq$variable)) {
    data_fig_freq <- subset(data_freq, variable == i)
    data_fig_quantile <-  subset(data_quantile, variable == i)
    sum_count <- sum(data_fig_freq$count)
    data_fig_freq$mic <- as.mic(data_fig_freq$mic)
    
    fig <- ggplot(data_fig_freq, mapping = aes(x = mic)) +
      geom_col(
        mapping = aes(y = count / sum_count, fill = sir_new),
        color = "black",
        position = "dodge"
      ) +
      geom_step(
        mapping = aes(
          y = cdf,
          group = variable,
          colour = 'line',
          linetype = "line"
        ),
        linewidth = 0.75
      ) +
      geom_point(mapping = aes(
        y = cdf,
        group = variable,
        colour = 'line'
      ),
      size = 2) +
      geom_segment(
        data_fig_quantile,
        mapping = aes(
          x = -Inf,
          xend = value,
          y = c(0.5, 0.9),
          yend = c(0.5, 0.9),
          linetype = mic,
          colour = mic,
          group = mic
        ),
        linewidth = 1
      ) +
      geom_segment(
        data_fig_quantile,
        mapping = aes(
          x = value,
          xend = value,
          y = c(0, 0),
          yend = c(0.5, 0.9),
          linetype = mic,
          colour = mic,
          group = mic
        ),
        linewidth = 1
      ) +
      scale_y_continuous(
        name = "Number of isolate",
        labels = function(y)
          round(y * sum_count, 0),
        sec.axis = sec_axis( ~ . , labels = percent_format(), name = "Cumulative Probability")
      ) +
      scale_x_discrete(
        labels = function(x)
          tran_symbol(x)
      ) +
      scale_linetype_manual(
        values = c("dashed", "dashed", "solid"),
        name = "",
        labels = c("MIC50", "MIC90", "Cumulative Step Line")
      ) +
      scale_colour_manual(
        values = c('#43A20F', '#6A3CAE', "#000000"),
        name = "",
        labels = c("MIC50", "MIC90", "Cumulative Step Line")
      ) +
      scale_fill_manual(
        values = c(
          "Susceptible" = "#3CAEA3",
          "Non-Susceptible" = "#ED553B"
        ),
        name = "Interpretation"
      ) +
      facet_wrap(variable ~ ., nrow = 4) +
      theme_pubr(border = TRUE) +
      theme(panel.grid.major.y = element_line(),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Minimum inhibitory concentration - MIC (mg/l)")
    
    if (length(glegend) < length(get_legend(fig)))
    {
      glegend =  get_legend(fig)
    }
    temp_plot <- ggplotGrob(fig + theme(legend.position = "none"))
    if (!is.null(temp_plot))
    {
      full_plot[[i]] <- temp_plot
    }
  }
  return(list(full_plot, glegend))
}


format_antibiotic_label <- function(x)
{
  x <- str_replace(x, "_clavulanic_acid", "/Clavulanic Acid")
  x <- str_replace(x, "_", "/")
  x <- str_to_title(x)
  return(x)
}

select_antibiotic2group <- function(label, groups) {
  antibiotics_group <- antibiotics %>% select(name, group) %>%
    mutate(name = str_to_title(name)) %>% filter(group == groups)
  return(label %in% antibiotics_group$name)
}

add_group_antibiotic <- function(gt_table, list_ab)
{
  groups_ab <- ab_group(list_ab)
  ab_data <- data.frame(list_ab, groups_ab)
    names_ab <- format_antibiotic_label(list_ab)
  groups_ab <- ab_group(names_ab)
  ab_data <- data.frame(names_ab, groups_ab)
  for (i in sort(unique(groups_ab), decreasing = TRUE))
  {
    sub_ab <- ab_data %>% filter(groups_ab == i)
    gt_table <- gt_table %>%
    tab_row_group(label = i, row = label %in% sub_ab$list_ab)
  }
  gt_table %>% btab_row_group(label = i, row = label %in% sub_ab$names_ab)
}


binary2gene <- function(x, n, list_gen)
{
  vec <- unlist(str_split(x, n = n, pattern = ":"))
  position <- which(vec == "1")
  if (length(position) == 1)
  {
    encode <- list_gen[position]
  } else {
    encode <- paste(list_gen[position], collapse = " + ")
  }
  if (encode == "")
  {
    encode <- "None"
  }
  return(encode)
}

##### Reusable AST Table Builder #####
# Creates a standardized gt table for AST data by organism group
# @param processed_data The processed AST data
# @param mdr_data The MDR calculated data (can be NULL)
# @param pathogen_filter Vector of pathogen names to filter
# @return A gt table object
build_ast_summary_table <- function(processed_data, mdr_data, pathogen_filter) {
  if (is.null(mdr_data)) {
    # When no MDR data, we need to deduplicate by method priority
    # Priority: MIC > E-Test > Disk (use Method_Priority column if exists)
    data_sub <- processed_data %>%
      filter(!is.na(Interpretation)) %>%
      filter(Pathogen %in% pathogen_filter)
    
    # If Method_Priority exists, use it to keep best result per antibiotic
    if ("Method_Priority" %in% names(data_sub)) {
      data_sub <- data_sub %>%
        group_by(across(any_of(c("NoID", "PatientID", "SampleID", "Pathogen", "ab_code")))) %>%
        slice_min(Method_Priority, n = 1, with_ties = FALSE) %>%
        ungroup()
    }
    
    data_sub <- data_sub %>%
      select(any_of(c("NoID", "PatientID", "SampleID", "Pathogen", "ab_code", "Interpretation"))) %>%
      tidyr::pivot_wider(
        names_from = ab_code,
        values_from = Interpretation,
        values_fn = first  # Handle any remaining duplicates
      ) %>%
      select(-c(any_of(c("NoID", "PatientID", "SampleID")))) %>%
      select(where(~ !all(is.na(.)))) %>%
      rename_if(is.sir, ab_name) %>%
      mutate_if(is.sir, .funs = function(x) if_else(x == "R", TRUE, FALSE))
  } else {
    # MDR data already has deduplicated interpretations
    data_sub <- mdr_data %>%
      filter(Pathogen %in% pathogen_filter) %>%
      select(-c(any_of(c("NoID", "PatientID", "SampleID", "SampleType", "SampleDate", "mo_code", "kingdom", "gram_stain")))) %>%
      select(where(~ !all(is.na(.)))) %>%
      rename_if(is.sir, ab_name) %>%
      mutate_if(is.sir, .funs = function(x) if_else(x == "R", TRUE, FALSE))
  }
  
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
    modify_footnote(all_stat_cols() ~ "Resistant Percent (%), (Resistant case / Total)") %>%
    modify_table_body(~ .x %>%
                        mutate(
                          across(all_stat_cols(), ~ gsub("^NA.*0.0/0.0)", "-", .)),
                          across(all_stat_cols(), ~ gsub("0\\.0 \\(NA%\\)", "-", .))
                        )) %>%
    as_gt() %>%
    add_group_antibiotic(list_ab = list_ab)
  
  return(gt_table)
}

##### Get Pathogen List by Organism Group #####
# @param processed_data The processed AST data
# @param kingdom_filter Filter for kingdom (e.g., "Bacteria", "Fungi")
# @param gram_stain_filter Filter for gram stain (e.g., "Gram-negative", "Gram-positive")
# @return Vector of pathogen names sorted by frequency
get_pathogen_list <- function(processed_data, kingdom_filter = NULL, gram_stain_filter = NULL) {
  result <- processed_data %>%
    select(any_of(c("NoID", "PatientID", "SampleID", "Pathogen", "kingdom", "gram_stain"))) %>%
    unique()
  
  if (!is.null(kingdom_filter)) {
    result <- result %>% filter(kingdom == kingdom_filter)
  }
  
  if (!is.null(gram_stain_filter)) {
    result <- result %>% filter(gram_stain == gram_stain_filter)
  }
  
  result %>%
    count(Pathogen, sort = TRUE, name = "Frequency") %>%
    pull(Pathogen)
}
