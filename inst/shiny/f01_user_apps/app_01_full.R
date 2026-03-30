# ==============================================================================
# ORQUESTADOR RSCIENCE 2027 - VERSIÓN MÍNIMA (SIN CSS)
# ==============================================================================
devtools::load_all()

ui <- fluidPage(
  # Usamos el tema básico para que bslib maneje los espacios
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  tabsetPanel(
    id = "main_nav",
    type = "hidden", # Mantiene las pestañas ocultas visualmente

    # Página 1
    tabPanelBody(
      value = "page_launchpad",
      mod_01_00_launchpad_ui("launchpad_v1")
    ),

    # Página 2: El Engine
    tabPanelBody(
      value = "page_engine",
      # Sin DIVs intermedios, directo al módulo
      mod_02_00_rscience_ui("engine_v1")
    )
  )
)

server <- function(input, output, session) {

  # Servidores
  launchpad_res <- mod_01_00_launchpad_server("launchpad_v1")
  mod_02_00_rscience_server("engine_v1")

  # Navegación Ir
  observeEvent(launchpad_res(), {
    status <- launchpad_res()
    req(status$nav_trigger > 0)
    if (status$target_page == "engine") {
      updateTabsetPanel(session, "main_nav", selected = "page_engine")
    }
  })

  # Navegación Volver
  observeEvent(input[["engine_v1-btn_go_home"]], {
    updateTabsetPanel(session, "main_nav", selected = "page_launchpad")
  })
}

shinyApp(ui, server)
