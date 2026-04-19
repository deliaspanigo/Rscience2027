# ==============================================================================
# ORQUESTADOR RSCIENCE 2027 - BSLIB + FULL HEIGHT (CORREGIDO)
# ==============================================================================
library(shiny)
library(bslib)

devtools::load_all()

# 2. Configuración de rutas robusta
# Buscamos la carpeta inst/www/css que es el estándar de paquetes R
path_www <- system.file("www", package = "Rscience2027")
if (path_www == "") path_www <- "inst/www" # Fallback para desarrollo con devtools

addResourcePath("RS-STYLES", normalizePath(path_www))
# 3. Definir la ruta de test de archivos QMD
# Asegúrate de que esta ruta realmente devuelva algo



ui <- page_fillable(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("RS-STYLES/css/style_000.css?v=", as.numeric(Sys.time())) # Nota el /css/ adicional si registraste 'www'
    )
  ),

  # navset_hidden SIN el argumento fillable
  navset_hidden(
    id = "main_nav",

    nav_panel_hidden(
      value = "page_launchpad",
      mod_01_00_launchpad_ui("launchpad_v1")
    ),

    nav_panel_hidden(
      value = "page_engine",
      #mod_02_00_rscience_ui("engine_v1")
      mod_rscience_engine_ui("engine_v1")
    )
  )
)

server <- function(input, output, session) {

  launchpad_res <- mod_01_00_launchpad_server("launchpad_v1")
  mod_rscience_engine_server("engine_v1")

  # Navegación Launchpad → Engine
  observeEvent(launchpad_res(), {
    status <- launchpad_res()
    req(status$nav_trigger > 0)

    if (status$target_page == "engine") {
      nav_select(
        id = "main_nav",
        selected = "page_engine",
        session = session
      )
    }
  })

  # Navegación Engine → Launchpad
  observeEvent(input[["engine_v1-btn_go_home"]], {
    nav_select(
      id = "main_nav",
      selected = "page_launchpad",
      session = session
    )
  })
}

shinyApp(ui, server)
