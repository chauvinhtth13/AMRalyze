# Helper function for picker inputs
create_select <- function(id,
                          label,
                          default = "Select column",
                          multiple = FALSE) {
  pickerInput(
    id,
    label,
    choices = NULL,
    selected = default,
    multiple = multiple,
    options = list(`actions-box` = TRUE, 
                   `live-search` = TRUE
                   )
  )
}

data_config_ui <- function() {
  div(
    conditionalPanel(
      condition = "output.data_uploaded",
      # Core column selectors
      create_select("PID", "Patient ID"),
      create_select("SID", "Sample ID"),
      create_select("Sample_Data", "Sampling Date"),
      create_select("Sample_Type", "Sample Type"),
      create_select("Pathogen", "Pathogen"),
      
      # Data structure radio buttons
      radioButtons(
        "data_structure",
        "AST Data Structure:",
        choices = list(
          "Multiple Antibiotics columns" = "0",
          "Separate AB & MIC & Zone" = "1"
        ),
        selected = "1"
      ),
      
      # Conditional inputs based on data structure
      conditionalPanel(
        condition = "input.data_structure == '0'",
        create_select("AB_type_0", "Antibiotics Columns", multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.data_structure == '1'",
        create_select("AB_type_1", "Antibiotics"),
        create_select("MIC", "MIC"),
        create_select("interpretation", "Interpretation (optional)", "None"),
        create_select("Zone_Size", "Zone Size", "None"),
        create_select("Methods", "Test Method", "None")
      ),
      
      # Process button
      actionBttn("btn_process", "Process Data", style = "gradient", color = "success")
    )
  )
}