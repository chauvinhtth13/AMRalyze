# AMRalyze Dashboard

AMRalyze Dashboard is an interactive web-based tool for analysing Antimicrobial Resistance (AMR) surveillance data. It enables users to upload raw data, process it according to selected guidelines, and visualise key AMR metrics and resistance patterns.

## Workflow

1.  **Upload Data:** Users upload AMR data (CSV/XLSX) via the 'Input' tab.
2.  **Map Columns:** Users map their data columns to standard fields (Patient ID, Pathogen, etc.).
3.  **Configure Processing:** Users select an AST guideline (e.g., CLSI, EUCAST) and specify the data format (wide/long).
4.  **Process Data:** The application cleans, standardises (using the AMR package), interprets SIR results, and calculates MDR status.
5.  **Visualise & Review:** The 'Dashboard' provides summary stats, pathogen plots, and resistance tables. The 'Input' tab allows a detailed review of individual isolates.
6.  **Download:** Processed data and MDR results can be downloaded as an Excel file.

## Input Data Formats

The application requires specific columns in your uploaded data, depending on whether you choose the 'Wide' or 'Long' format on the 'Input' tab.

### **Wide Format ('One row per isolate')**

In this format, each row contains all information for a single isolate, including results for multiple antibiotics in separate columns.

**Required Mapping Inputs:**

* Patient ID Column: `` `PID` `` - Unique identifier for the patient.
* Sample ID Column: `` `SID` `` - Unique identifier for the sample/isolate.
* Pathogen Column: `` `Pathogen` `` - Name of the identified microorganism.
* Select ALL Antibiotic Result Columns: `` `AB_cols` `` - You must select **all** columns that contain the results for any antibiotic test (e.g., columns named `AMP`, `CIP`, `AMX_NM`, `CIP_ND30`, etc.). The values in these columns should be the result (e.g., '16', '<=0.5', 'R', 'S', '25').

**Optional Mapping Inputs:**

* Sampling Date Column: `` `Sample_Date` `` - Date the sample was collected.
* Sample Type Column: `` `Sample_Type` `` - Type of specimen (e.g., 'Blood', 'Urine').

*Note: If your antibiotic column headers combine an **antibiotic name or code (often a 3-letter WHONET code)** with a method/result type suffix like `` `_NM` ``, `` `_NE` ``, or `` `_ND` `` (e.g., `` `AMP_NM` ``, where 'AMP' represents the antibiotic and '_NM' indicates the result type; other examples might be `` `CTX_NE` ``, `` `GEN_ND` ``), the application attempts to use this structure:*

* *The part **before** the suffix (like 'AMP', 'CTX', 'GEN') is treated as the antibiotic identifier.*
* *The suffix indicates how to interpret the value in the cell:*
    * *`` `_NM` ``: Value is a Minimum Inhibitory Concentration (MIC).*
    * *`` `_NE` ``: Value is an MIC determined by E-test.*
    * *`` `_ND` ``: Value is a Zone Size diameter (in mm) from disk diffusion.*

*Select these combined-name columns under the 'Select ALL Antibiotic Result Columns' mapping input. Suppose such combined headers/suffixes are not detected. In that case, the application will treat the entire column header as the antibiotic identifier and attempt to parse the cell value directly (expecting simple numerical MIC values, numerical zone sizes, or S/I/R codes).*

### **Long Format ('One row per test')**

In this format, each row represents a single antibiotic test result for an isolate, which will typically have multiple rows in the dataset.

**Required Mapping Inputs:**

* Patient ID Column: `` `PID` ``
* Sample ID Column: `` `SID` ``
* Pathogen Column: `` `Pathogen` ``
* Antibiotic Name Column: `` `AB_name` `` - The name of the antibiotic tested in this row (e.g., 'Ampicillin', 'Ciprofloxacin').
* `` `MIC Column` `` **OR** `` `Zone Size Column` `` - You must map at least one containing the numerical result (e.g., '16' for MIC, '25' for Zone Size). Both can be mapped if available. These are needed for interpretation if an 'Interpretation Column' is not mapped or is incomplete.

**Optional Mapping Inputs:**

* Sampling Date Column: `` `Sample_Date` ``
* Sample Type Column: `` `Sample_Type` ``
* Interpretation Column: `` `Interpretation` `` - Column containing pre-existing S/I/R interpretation. If mapped, this takes precedence over calculated interpretation.
* Test Method Column: `` `Methods` `` - The method used for the test (e.g., 'MIC', 'Disk', 'E-test'). Can be helpful for context, but isn't typically used in SIR calculation logic directly here.

*Note: For long format, clearly separating the antibiotic name and its corresponding result (MIC or Zone) into distinct columns is crucial.*

## Key Features

* Supports CSV and XLSX file uploads.
* Handles wide and long AST data formats.
* Utilises the 'AMR' package for robust data processing.
* Applies user-selected AST guidelines for interpretation.
* Calculates Multidrug Resistance (MDR).
* Offers interactive visualisations and publication-ready tables.
* Allows detailed data review and download.

## Core Technology

* Language/Framework: R / Shiny
* AMR Logic: 'AMR' package
* Data Handling: 'tidyverse' suite
* Tables & Plots: 'DT', 'gt', 'gtsummary', 'plotly'
* User Interface: 'bslib', 'shinyWidgets', 'shinyjs'

## Live Application Link

You can access the live version of the AMRalyze Dashboard here:

[https://chauvinh.shinyapps.io/amralyze/](https://chauvinh.shinyapps.io/amralyze/)

## Contact & Contribution

For support inquiries, bug reports, or if you wish to contribute to the AMRalyze Dashboard project, please get in touch with Chau Vinh via email:

[chauvinhtth13@gmail.com](mailto:chauvinhtth13@gmail.com)

## Source Code & Citation

The source code for the AMRalyze Dashboard is hosted on GitHub. If you use this application or adapt the code for your research or work, please consider citing the repository:

Vinh, C. (2025). *AMRalyze: Interactive Antimicrobial Resistance Data Analysis Dashboard* \[Software]. Source code is available at (https://github.com/chauvinhtth13/AMRalyze). Live application hosted at: (https://chauvinh.shinyapps.io/amralyze/).

**BibTeX Entry:**

```bibtex
@misc{Vinh2025AMRalyze,
  author       = {Vinh, Chau},
  title        = {{AMRalyze: Interactive Antimicrobial Resistance Data Analysis Dashboard}},
  year         = {2025},
  howpublished = {\url{[https://chauvinh.shinyapps.io/amralyze/](https://chauvinh.shinyapps.io/amralyze/)}},
  note         = {Source code available at \url{[https://github.com/chauvinhtth13/AMRalyze](https://github.com/chauvinhtth13/AMRalyze)}}
}
