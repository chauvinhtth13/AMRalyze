# utils.R
# Helper Functions

# Detect matching column names
detect_column <- function(headers, patterns) {
  matches <- sapply(patterns, function(p) grep(p, headers, ignore.case = TRUE))
  matched_cols <- headers[unlist(matches)]
  if (length(matched_cols) > 0) matched_cols[1] else NULL
}

# Read file based on extension
read_file <- function(file_path, ext, sheet = NULL) {
  if (ext == "csv") {
    read.csv(file_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  } else if (ext == "xlsx") {
    read_excel(file_path, sheet = sheet)
  } else {
    stop("Unsupported file type")
  }
}

# Process AST data based on structure
process_ast_data <- function(df, structure, inputs) {
  if (structure == "0") {
    req(inputs$AB_type_0)
    df %>%
      select(inputs$PID, inputs$SID, inputs$Sample_Data, inputs$Sample_Type,
             inputs$Pathogen, all_of(inputs$AB_type_0)) %>%
      mutate(across(inputs$AB_type_0, as.character))
  } else {
    req(inputs$AB_type_1, inputs$MIC)
    df %>%
      select(inputs$PID, inputs$SID, inputs$Sample_Data, inputs$Sample_Type,
             inputs$Pathogen, inputs$AB_type_1, inputs$MIC,
             if (!is.null(inputs$interpretation)) inputs$interpretation,
             if (!is.null(inputs$Zone_Size)) inputs$Zone_Size,
             if (!is.null(inputs$Methods)) inputs$Methods) %>%
      mutate(Pathogen = as.mo(.data[[inputs$Pathogen]]),
             MIC = as.mic(.data[[inputs$MIC]]))
  }
}