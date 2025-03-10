detect_column <- function(colnames, patterns) {
  # Loop through patterns and return the first match
  df_match <- c()
  for (pattern in patterns) {
    match_value <- grep(pattern, colnames, ignore.case = TRUE, value = TRUE)
    if (length(match) > 0) {
      df_match <- c(df_match, match_value)
    }
  }
  return(df_match)  # Return NULL if no match is found
}