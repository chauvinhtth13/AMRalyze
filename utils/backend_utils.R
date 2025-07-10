# Khởi tạo / reset reactiveValues
is_valid_excel <- function(file) {
  grepl("\\.xlsx$", file$name, ignore.case = TRUE) &&
    file$type %in% c(
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      "application/vnd.ms-excel"
    )
}

detect_column <- function(headers,
                          patterns,
                          single_match = TRUE,
                          strict = TRUE) {
  patterns <- gsub("([[:punct:]])", "\\\\\\1", patterns)
  patterns <- if (strict)
    paste0("\\b", patterns, "\\b")
  else
    patterns
  regex_pattern <- paste(patterns, collapse = "|")
  
  match_indices <- grep(regex_pattern, headers, ignore.case = TRUE)
  
  if (length(match_indices) == 0) {
    return(NULL)
  } else if (single_match) {
    exact_match <- which(tolower(headers) %in% tolower(patterns))
    if (length(exact_match) > 0) {
      headers[exact_match[1]]
    } else {
      headers[match_indices[1]]
    }
  } else {
    headers[match_indices]
  }
}

map_columns <- function(cols, mapping_list) {
  for (id in names(mapping_list)) {
    vals   <- mapping_list[[id]]
    is_multi <- id == "AB_cols"
    selected <- detect_column(cols,
                              vals,
                              single_match = !is_multi,
                              strict = TRUE) %||% NULL
    updateVirtualSelect(inputId = id,
                        choices = cols,
                        selected = selected)
  }
}


get_newest_guideline <- function()
{
  data_guideline <- str_split_i(clinical_breakpoints$guideline, " ", 2)
  paste("CLSI", max(as.numeric(data_guideline), na.rm = TRUE))
}
