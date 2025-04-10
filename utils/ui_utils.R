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