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
          mod_04_00_collector02_settings_ui("txt_1")
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

  mod_04_00_collector02_settings_server(id = "txt_1", df_input = reactive(mtcars), folder_path_tool_script = folder_prueba)
}

# --- Ejecución ---
shinyApp(ui, server)
