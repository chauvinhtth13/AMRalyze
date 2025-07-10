tab_about <- function() {
  nav_panel(
    title = "About",
    icon = bsicons::bs_icon("question-circle"),
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
      h4("Input Data Format"),
      tags$table(class = "table table-bordered", tags$thead(
        tags$tr(
          tags$th("Patient ID / Sample ID / No"),
          tags$th("Sampling Date"),
          tags$th("Sample Type"),
          tags$th("Pathogen"),
          tags$th("Name Antibiotic / Code Antibiotic (WHONET Format)")
        )
      )),
      h5("WHONET Antimicrobial Codes"),
      tags$ul(tags$li(
        tags$a(
          href = "https://whonet.org/WebDocs/WHONET_Antimicrobial_Codes.pdf",
          target = "_blank",
          "https://whonet.org/WebDocs/WHONET_Antimicrobial_Codes.pdf"
        )
      )), 
      h5("Antibiotic Column Interpretation"),
      tags$ul(
        tags$li(
          tags$code("*_NM"),
          ": MIC value (Minimum Inhibitory Concentration)."
        ),
        tags$li(tags$code("*_NE"), ": MIC value (E-test)."),
        tags$li(tags$code("*_ND"), ": Zone size (disk diffusion diameter)."),
        tags$li(
          tags$code("[no suffix]"),
          ": Treated as MIC value; if the value is formatted like 'R:<=...', displays interpretation (S/I/R) and the corresponding MIC."
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
      hr(),
      p(tags$em(
        "UI Rendered Timestamp (approximate): ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      ))
    )
  )
}
