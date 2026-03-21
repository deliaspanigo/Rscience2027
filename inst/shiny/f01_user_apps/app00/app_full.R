# ==============================================================================
# ORQUESTADOR RSCIENCE 2027 - v.0.0.1 (ESTADO UNIFICADO)
# ==============================================================================
devtools::load_all()

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  # Contenedor para evitar saltos de scroll al cambiar de módulo
  tags$style("body, html { overflow: hidden; margin: 0; padding: 0; }"),

  # 1. Módulo LAUNCHPAD
  conditionalPanel(
    condition = "output.current_page == 'launchpad'",
    mod_launchpad_ui("launchpad_v1")
  ),

  # 2. Módulo ENGINE
  conditionalPanel(
    condition = "output.current_page == 'engine'",
    div(style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; z-index: 9999; background: white;",
        mod_rscience_ui("engine_v1")
    )
  )
)

server <- function(input, output, session) {

  # Estado de la página principal
  page_state <- reactiveVal("launchpad")

  # --- INICIALIZACIÓN DE MÓDULOS ---
  # launchpad_res ahora es UNA sola función reactiva que devuelve la lista
  launchpad_res <- mod_launchpad_server("launchpad_v1", show_debug = TRUE)
  mod_rscience_server("engine_v1")

  # --- LÓGICA DE NAVEGACIÓN: Launchpad -> Engine ---
  observeEvent(launchpad_res(), {
    # Extraemos los datos del objeto unificado
    status <- launchpad_res()

    # Validamos que haya habido un clic real
    req(status$nav_trigger > 0)

    # Logs para consola de R con el nuevo timestamp
    message(sprintf("[Orquestador] Evento detectado a las: %s", format(status$last_click, "%H:%M:%S")))

    if (status$target_page == "engine") {
      message("[Orquestador] Navegando al Engine...")
      page_state("engine")
    }
    # Aquí podrías agregar lógica para 'distributions', 'classroom', etc.
  })

  # --- LÓGICA DE NAVEGACIÓN: Engine -> Launchpad ---
  # Escuchamos el botón de salida del módulo engine (vía namespace)
  observeEvent(input[["engine_v1-btn_go_home"]], {
    message("[Orquestador] Volviendo al Launchpad...")
    page_state("launchpad")
  })

  # --- EXPOSICIÓN DEL ESTADO A LA UI ---
  output$current_page <- renderText({ page_state() })
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
