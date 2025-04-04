detect_column <- function(headers, patterns) {
  headers <- tolower(headers)
  patterns <- tolower(patterns)
  headers <- gsub("_", " ", headers)
  matches <- grep(paste(patterns, collapse = "|"), headers, ignore.case = FALSE)
  if (length(matches) > 0) headers[matches[1]] else "None"
}