library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)


devtools::load_all()


# --- Interfaz de Usuario Principal ---
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),


  titlePanel("Explorador de Contenido RScience"),

  fluidRow(
    column(
      width = 12,
      navset_card_underline(
        title = span(icon("database"), " Panel de Recopilación"),

        # Pestaña 1: Teoría
        nav_panel(
          title = "Teoría",
          icon = icon("book"),
          mod_03_00_collector01_ui_01_theory("txt_1")
        ),

        # Pestaña 2: Bibliografía
        nav_panel(
          title = "Bibliografía",
          icon = icon("list-ul"),
          mod_03_00_collector01_ui_02_bibliography("txt_1")
        ),

        # Pestaña 3: Citas
        nav_panel(
          title = "Citas y Referencias",
          icon = icon("quote-right"),
          mod_03_00_collector01_ui_03_cite("txt_1")
        )
      )
    )
  )
)

# --- Servidor Principal ---
server <- function(input, output, session) {


  folder_prueba <- reactive({

    pkg_path  <- system.file(package = "Rscience2027")
    if (pkg_path == "") pkg_path <- "inst"
    base_path <- file.path(pkg_path, "shiny", "fn03_tool_script", 'tool_0001_script_002')

  })

  mod_03_00_collector01_server_UNIVERSAL(id = "txt_1", folder_path_tool_script = folder_prueba())
}

# --- Ejecución ---
shinyApp(ui, server)
