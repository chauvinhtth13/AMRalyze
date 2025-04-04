read_file <- function(file_path, ext, sheet = NULL) {
  if (!file.exists(file_path)) stop("File not found")
  if (ext == "csv") {
    read.csv(file_path, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
  } else if (ext == "xlsx") {
    read_excel(file_path, sheet = sheet %||% 1, col_types = "text")
  } else {
    stop("Unsupported file type")
  }
}

select_collumn_data <- function(df, structure, inputs) {
  if (structure == "0") {
    req(inputs$AB_type_0)
    df %>%
      select(
        inputs$PID,
        inputs$SID,
        inputs$Sample_Data,
        inputs$Sample_Type,
        inputs$Pathogen,
        all_of(inputs$AB_type_0)
      ) %>%
      mutate(across(inputs$AB_type_0, as.character))
  } else {
    req(inputs$AB_type_1, inputs$MIC)
    df %>%
      select(
        inputs$PID,
        inputs$SID,
        inputs$Sample_Data,
        inputs$Sample_Type,
        inputs$Pathogen,
        inputs$AB_type_1,
        inputs$MIC,
        inputs$interpretation,
        inputs$Zone_Size,
        inputs$Methods
      )
  }
}

process_ast_data <- function(df, structure, inputs) {
  if (structure == "0") {
    df %>%
      pivot_longer(
        cols = all_of(inputs$AB_type_0),
        names_to = "Antibiotic",
        values_to = "Result"
      ) %>%
      filter(!is.na(Result) & Result != "") %>%
      mutate(
        Result = as.character(Result),
        mo_code = as.mo(inputs$Pathogen),
        ab_code = as.ab(Antibiotic),
        name_ab = ab_name(ab_code),
        MIC = as.mic(str_extract(Result, "(<=|>=|<|>)?\\s*[0-9.]+")),
        SIR = as.sir(str_extract(Result, "R|S|I|SSD|NI"))
      )
  } else {
    df %>%
      mutate(
        mo_code = as.mo(inputs$Pathogen),
        ab_code = as.ab(inputs$AB_type_1),
        name_ab = ab_name(ab_code),
        MIC = as.mic(inputs$MIC),
        Zone_Size = as.disk(inputs$Zone_Size),
        SIR = if_else(
          is.na(Zone_Size),
          as.sir(MIC, mo = mo_code, ab = ab_code, guideline = inputs$interpretation),
          as.sir(Zone_Size, mo = mo_code, ab = ab_code, guideline = inputs$interpretation)
        )
      )
  }
}