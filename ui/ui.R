source("ui/file_upload.R")
source("ui/data_config.R")
source("ui/overview.R")
source("ui/interpretation.R")

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  sidebarLayout(
    sidebarPanel(
      file_upload_ui(),
      data_config_ui()
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Overview & Configure", icon = icon("table"), overview_ui()),
        tabPanel("AST Interpretation/Multidrug Resistance", icon = icon("microscope"), interpretation_ui()),
        tabPanel("AMR Patterns", icon = icon("chart-bar")),
        tabPanel("Multidrug Resistance", icon = icon("prescription-bottle"))
      )
    )
  )
)
