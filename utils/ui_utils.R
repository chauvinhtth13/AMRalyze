css_function <- function() {
  tags$head(
    shinyjs::useShinyjs(),
    tags$style(HTML("#file_browse_progress.progress { display: none !important; }")),
    tags$script(HTML(
      # JavaScript for enabling Shift+Click range selection in VirtualSelect input 'AB_cols'
      "$(document).on('shiny:connected', function(event) {
         var vsInputId = 'AB_cols';
         var lastSelectedIndex = -1;
         var virtualSelectElement = document.getElementById(vsInputId);

         if (virtualSelectElement && virtualSelectElement.virtualSelect) {
             $(virtualSelectElement).on('click', '.vscomp-option', function(e) {
                 var clickedOption = $(this);
                 var currentIndex = clickedOption.data('index');
                 // console.log('Clicked index:', currentIndex, 'Shift:', e.shiftKey, 'Last:', lastSelectedIndex);

                 if (e.shiftKey && lastSelectedIndex !== -1) {
                     e.preventDefault(); // Prevent default toggle behaviour
                     var start = Math.min(lastSelectedIndex, currentIndex);
                     var end = Math.max(lastSelectedIndex, currentIndex);
                     var optionsToSelect = [];
                     var currentValues = virtualSelectElement.virtualSelect.getSelectedValue(); // Get already selected values

                     // Iterate through visible options to find those in the range
                     $(virtualSelectElement).find('.vscomp-option').each(function() {
                         var idx = $(this).data('index');
                         var val = $(this).data('value');
                         if (idx >= start && idx <= end) {
                            // Add to selection (avoid duplicates if already selected)
                            if (!optionsToSelect.includes(val)) {
                                optionsToSelect.push(val);
                            }
                         }
                     });

                     // Combine newly selected range with previously selected items OUTSIDE the range
                     // This mimics standard shift-click behaviour (adds range to existing selection)
                     var finalSelection = Array.from(new Set([...currentValues, ...optionsToSelect]));

                     // Use VirtualSelect API to set the value
                     virtualSelectElement.virtualSelect.setValue(finalSelection);
                     // console.log('Range selected:', optionsToSelect, 'Final Selection:', finalSelection);

                     // Update lastSelectedIndex to the end of the range clicked
                     lastSelectedIndex = currentIndex;

                 } else {
                     // Regular click (not shift or first click) - update last selected index
                     lastSelectedIndex = currentIndex;
                 }
             });
         } else {
             console.error('Could not find Virtual Select instance for:', vsInputId);
         }
     });"
    ))
  )
}



detect_column <- function(original_headers, patterns) {

  # Store original headers to return the correct value later
  headers_original_case <- original_headers # Keep the original casing
  
  # Prepare headers and patterns for matching (lowercase, space for underscore)
  headers_processed <- tolower(original_headers)
  patterns_processed <- paste0("\\b", tolower(patterns), "\\b") # Add word boundaries

  regex_pattern <- paste(patterns_processed, collapse = "|")
  
  # Find indices of matches in the processed headers
  matches_indices <- grep(regex_pattern, headers_processed, ignore.case = FALSE) # ignore.case=FALSE because already lowercased
  
  if (length(matches_indices) > 0) {
    # Return the header with the original casing
    return(headers_original_case[matches_indices[1]])
  } else {
    # Return NULL if no match is found
    return(NULL)
  }
}

get_newest_guideline <- function()
{
  data_guideline <- str_split_i(clinical_breakpoints$guideline," ",2)
  paste("CLSI",max(as.numeric(data_guideline),na.rm =TRUE))
}

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
        sec.axis = sec_axis(~ . , labels = percent_format(), name = "Cumulative Probability")
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

select_antibiotic2group <- function(label, groups){
  antibiotics_group <- antibiotics %>% select(name,group) %>%
    mutate(name = str_to_title(name)) %>% filter(group == groups)
  return(label %in% antibiotics_group$name)
}

add_group_antibiotic <- function(gt_table, list_ab)
{
  names_ab <- format_antibiotic_label(list_ab)
  groups_ab <- ab_group(names_ab)
  ab_data <- data.frame(names_ab,groups_ab)
  for (i in sort(unique(groups_ab),decreasing = TRUE))
  {
    sub_ab <- ab_data %>% filter(groups_ab == i)
    gt_table <- gt_table %>%
      tab_row_group(label = i, row = label %in% sub_ab$names_ab)
  }
  gt_table %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups())
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