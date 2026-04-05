library(shiny)
library(bslib)
library(shinyjs)
library(listviewer)

# Asumiendo que devtools::load_all() carga el archivo mod_05_00_collector03.R
# con las funciones que definimos anteriormente.
devtools::load_all()

# --- Interfaz de Usuario Principal ---
ui <- page_fluid(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  titlePanel("Explorador de Contenido RScience"),

  fluidRow(
    column(
      width = 12,
      navset_card_underline(
        title = span(icon("database"), " Panel de Recopilación"),

        # 1. Pestaña de DEBUG (Ahora es la primera)
        nav_panel(
          title = "DEBUG SISTEMA",
          icon = icon("bug"),
          mod_05_00_collector03_ui_debug("txt_3") # Llamada al nuevo UI de Debug
        ),

        # 2. Pestaña: Shiny
        nav_panel(
          title = "Shiny Output",
          icon = icon("book"),
          mod_05_00_collector03_ui_01_shiny_output("txt_3")
        ),

        # 3. Pestaña: ASA
        nav_panel(
          title = "ASA",
          icon = icon("list-ul"),
          mod_05_00_collector03_ui_02_asa("txt_3")
        ),

        # 4. Pestaña: Citas
        nav_panel(
          title = "Script and Comments",
          icon = icon("quote-right"),
          mod_05_00_collector03_ui_03_script_comment("txt_3")
        ),

        # 5. Pestaña: Script Only
        nav_panel(
          title = "Script Only",
          icon = icon("code"),
          mod_05_00_collector03_ui_04_script_only("txt_3")
        ),

        # 6. Pestaña: Reporting
        nav_panel(
          title = "Reporting",
          icon = icon("file-medical"),
          mod_05_00_collector03_ui_05_reporting("txt_3")
        )
      )
    )
  )
)

# --- Servidor Principal ---
server <- function(input, output, session) {

  # Definición de la ruta de origen (como reactive)
  folder_prueba <- reactive({
    pkg_path <- system.file(package = "Rscience2027")
    if (pkg_path == "") pkg_path <- "inst"

    # Ruta específica del tool
    file.path(pkg_path, "shiny", "fn03_tool_script", 'tool_0001_script_002')
  })

  # EJECUCIÓN DEL MÓDULO UNIVERSAL
  # Guardamos el retorno en 'datos_bundle' para usarlo si es necesario en otros sitios
  datos_bundle <- mod_05_00_collector03_server_UNIVERSAL(
    id = "txt_3",
    folder_path_tool_script = folder_prueba
  )

  # Ejemplo de cómo podrías usar la info del bundle en la consola de la App
  observe({
    req(datos_bundle())
    res <- datos_bundle()
    #print(res)
    if(res$is_ready) {
      message("--- [App Main] Carpeta temporal lista en: ", res$temporal$path_final)
    }
  })

}

# --- Ejecución ---
shinyApp(ui, server)
