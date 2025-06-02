tab_about <- function() {
  nav_panel(
    title = "About",
    icon = bsicons::bs_icon("question-circle"),
    card(
      card_header("AMRalyze Dashboard"),
      card_body(
        p(
          "AMRalyze Dashboard is an interactive web-based tool for analysing Antimicrobial Resistance (AMR) surveillance data. It enables users to upload raw data, process it according to selected guidelines, and visualise key AMR metrics and resistance patterns."
        ),
        h4("Workflow"),
        tags$ol(
          tags$li(
            tags$strong("Upload Data:"),
            " Users upload AMR data (CSV/XLSX) via the 'Input' tab."
          ),
          tags$li(
            tags$strong("Map Columns:"),
            " Users map their data columns to standard fields (Patient ID, Pathogen, etc.)."
          ),
          tags$li(
            tags$strong("Configure Processing:"),
            " Users select an AST guideline (e.g., CLSI, EUCAST) and specify the data format (wide/long)."
          ),
          tags$li(
            tags$strong("Process Data:"),
            " The application cleans, standardises (using the AMR package), interprets SIR results, and calculates MDR status."
          ),
          tags$li(
            tags$strong("Visualise & Review:"),
            " The 'Dashboard' provides summary stats, pathogen plots, and resistance tables. The 'Input' tab allows a detailed review of individual isolates."
          ),
          tags$li(
            tags$strong("Download:"),
            " Processed data and MDR results can be downloaded as an Excel file."
          )
        ),
        h4("Input Data Formats"),
        p(
          "The application requires specific columns in your uploaded data, depending on whether you choose the 'Wide' or 'Long' format on the 'Input' tab."
        ),
        h5(strong("Wide Format ('One row per isolate')")),
        p(
          "In this format, each row contains all information for a single isolate, including results for multiple antibiotics in separate columns."
        ),
        tags$strong("Required Mapping Inputs:"),
        tags$ul(
          tags$li(
            "Patient ID Column: ",
            code("PID"),
            " - Unique identifier for the patient."
          ),
          tags$li(
            "Sample ID Column: ",
            code("SID"),
            " - Unique identifier for the sample/isolate."
          ),
          tags$li(
            "Pathogen Column: ",
            code("Pathogen"),
            " - Name of the identified microorganism."
          ),
          tags$li(
            "Select ALL Antibiotic Result Columns: ",
            code("AB_cols"),
            " - You must select ",
            tags$strong("all"),
            " columns that contain the results for any antibiotic test (e.g., columns named ",
            code("AMP"),
            ", ",
            code("CIP"),
            ", ",
            code("AMX_NM"),
            ", ",
            code("CIP_ND30"),
            ", etc.). The values in these columns should be the result (e.g., '16', '<=0.5', 'R', 'S', '25')."
          )
        ),
        tags$strong("Optional Mapping Inputs:"),
        tags$ul(
          tags$li(
            "Sampling Date Column: ",
            code("Sample_Date"),
            " - Date the sample was collected."
          ),
          tags$li(
            "Sample Type Column: ",
            code("Sample_Type"),
            " - Type of specimen (e.g., 'Blood', 'Urine')."
          )
        ),
        p(
          tags$em(
            "Note: If your antibiotic column headers combine an ",
            tags$strong("antibiotic name or code (often a 3-letter WHONET code)"),
            " with a method/result type suffix like ",
            code("_NM"),
            ", ",
            code("_NE"),
            ", or ",
            code("_ND"),
            " (e.g., ",
            code("AMP_NM"),
            "), the application attempts to parse the antibiotic name (e.g., 'AMP') and interpret the value based on the suffix ('_NM' for MIC, '_ND' for Disk, '_NE' for E-test). Select these combined-name columns under 'Select ALL Antibiotic Result Columns'. If suffixes are absent (e.g., just ",
            code("AMP"),
            "), the application treats the header as the antibiotic name and primarily assumes numerical values are MICs, while attempting to extract S/I/R codes if present."
          )
        ),
        # Simplified explanation
        h5(strong("Long Format ('One row per test')")),
        p(
          "In this format, each row represents a single antibiotic test result for an isolate."
        ),
        tags$strong("Required Mapping Inputs:"),
        tags$ul(
          tags$li("Patient ID Column: ", code("PID")),
          tags$li("Sample ID Column: ", code("SID")),
          tags$li("Pathogen Column: ", code("Pathogen")),
          tags$li(
            "Antibiotic Name Column: ",
            code("AB_name"),
            " - Name of the antibiotic tested."
          ),
          tags$li(
            code("MIC Column"),
            tags$strong(" OR "),
            code("Zone Size Column"),
            tags$strong(" OR "),
            code("Interpretation Column"),
            " - Map at least one result column. Numerical columns (MIC/Zone) are needed for SIR calculation if Interpretation isn't provided or is incomplete."
          ) # Clarified OR logic
        ),
        tags$strong("Optional Mapping Inputs:"),
        tags$ul(
          tags$li("Sampling Date Column: ", code("Sample_Date")),
          tags$li("Sample Type Column: ", code("Sample_Type")),
          tags$li(
            "Interpretation Column: ",
            code("Interpretation"),
            " - Pre-existing S/I/R. Takes precedence if mapped."
          ),
          tags$li(
            "Test Method Column: ",
            code("Methods"),
            " - Method used (e.g., 'MIC', 'Disk'). For context."
          )
        ),
        p(
          tags$em(
            "Note: For long format, clearly separating the antibiotic name and its result (MIC, Zone, or Interpretation) into distinct columns is crucial."
          )
        ),
        h4("Key Features"),
        tags$ul(
          tags$li("Supports CSV and XLSX file uploads."),
          tags$li("Handles wide and long AST data formats."),
          tags$li("Utilises the 'AMR' package for robust data processing."),
          tags$li("Applies user-selected AST guidelines for interpretation."),
          tags$li(
            "Calculates Multidrug Resistance (MDR) based on CMI2012 definition."
          ),
          # Be specific
          tags$li(
            "Offers interactive visualisations and publication-ready tables."
          ),
          tags$li("Allows detailed data review and download.")
        ),
        h4("Core Technology"),
        tags$ul(
          tags$li("Language/Framework: R / Shiny"),
          tags$li("AMR Logic: 'AMR' package"),
          tags$li("Data Handling: 'tidyverse' suite, 'data.table'"),
          # Added data.table
          tags$li("Tables & Plots: 'DT', 'gt', 'gtsummary', 'plotly'"),
          tags$li("User Interface: 'bslib', 'shinyWidgets', 'shinyjs'")
        ),
        h4("Live Application Link"),
        p("Access the live version:"),
        p(
          tags$a(
            href = "https://chauvinh.shinyapps.io/amralyze/",
            target = "_blank",
            "https://chauvinh.shinyapps.io/amralyze/"
          )
        ),
        h4("Contact & Contribution"),
        p("For inquiries or contributions, contact Chau Vinh:"),
        p(
          tags$a(href = "mailto:chauvinhtth13@gmail.com", "chauvinhtth13@gmail.com")
        ),
        h4("Source Code & Citation"),
        p("Source code is on GitHub. If used, please cite:"),
        p(
          "Vinh, C. (2025). ",
          tags$em(
            "AMRalyze: Interactive Antimicrobial Resistance Data Analysis Dashboard "
          ),
          "[Software]. Source code: ",
          tags$a(href = "https://github.com/chauvinhtth13/AMRalyze", target = "_blank", "GitHub Repository"),
          ". Live application: ",
          tags$a(href = "https://chauvinh.shinyapps.io/amralyze/", target = "_blank", "ShinyApp Link"),
          "."
        ),
        h5("BibTeX Entry:"),
        tags$pre(tags$code(
          HTML(
"@misc{Vinh2025AMRalyze,
  author         = {Vinh, Chau},
  title          = {{AMRalyze: Interactive Antimicrobial Resistance Data Analysis Dashboard}},
  year           = {2025},
  howpublished = {\\url{https://chauvinh.shinyapps.io/amralyze/}},
  note           = {Source code available at \\url{https://github.com/chauvinhtth13/AMRalyze}}
}"
          )
        )),
        p(
          tags$em(
            "(Note: Using \\url{} requires the \\usepackage{url} or \\usepackage{hyperref} package in LaTeX.)"
          )
        ),
        hr(),
        p(tags$em(
          "UI Rendered Timestamp (approximate): ",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
        ))
      ) # End card_body
    ) # End card
  )
}