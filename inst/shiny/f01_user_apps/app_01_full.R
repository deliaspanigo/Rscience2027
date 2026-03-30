# ==============================================================================
# ORQUESTADOR RSCIENCE 2027 - BSLIB + FULL HEIGHT (CORREGIDO)
# ==============================================================================
library(shiny)
library(bslib)

devtools::load_all()

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # navset_hidden SIN el argumento fillable
  navset_hidden(
    id = "main_nav",

    nav_panel_hidden(
      value = "page_launchpad",
      mod_01_00_launchpad_ui("launchpad_v1")
    ),

    nav_panel_hidden(
      value = "page_engine",
      mod_02_00_rscience_ui("engine_v1")
    )
  )
)

server <- function(input, output, session) {

  launchpad_res <- mod_01_00_launchpad_server("launchpad_v1")
  mod_02_00_rscience_server("engine_v1")

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
