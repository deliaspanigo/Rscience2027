# --- LANZADOR DE LA APP Rscience 2027 ---
devtools::load_all()

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  # Usamos conditionalPanel para que el HTML y el JS de los módulos no se destruyan
  # 1. LAUNCHPAD
  conditionalPanel(
    condition = "output.current_page == 'launchpad'",
    mod_launchpad_ui("launchpad_v1")
  ),

  # 2. ENGINE (Cargado en segundo plano, visible bajo demanda)
  conditionalPanel(
    condition = "output.current_page == 'engine'",
    div(class = "module-host",
        style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; z-index: 9999; background: white;",
        mod_rscience_ui("engine_v1")
    )
  )
)

server <- function(input, output, session) {

  # Variable reactiva central para controlar la navegación
  page_state <- reactiveVal("launchpad")

  # --- INICIALIZACIÓN DE SERVIDORES ---
  # Importante: Se llaman fuera de observadores para que persistan
  launchpad_res <- mod_launchpad_server("launchpad_v1", show_debug = TRUE)
  mod_rscience_server("engine_v1")

  # --- NAVEGACIÓN: Launchpad -> Engine ---
  # Ahora escuchamos el 'nav_trigger' (el contador) en lugar del string
  observeEvent(launchpad_res$nav_trigger(), {
    # Validamos que el contador haya subido (evita el disparo inicial en 0)
    req(launchpad_res$nav_trigger() > 0)

    # Leemos la intención del clic
    dest <- launchpad_res$clicked_destination()

    if (!is.null(dest) && dest == "engine") {
      message("[Orquestador] Navegando al Engine...")
      page_state("engine")
    }
  })

  # --- NAVEGACIÓN: Engine -> Launchpad ---
  # Seguimiento del botón interno del módulo engine (namespace manual)
  observeEvent(input[["engine_v1-btn_go_home"]], {
    message("[Orquestador] Volviendo al Launchpad...")
    page_state("launchpad")
  })

  # --- COMUNICACIÓN CON LA UI ---
  # Exponemos el estado para los conditionalPanels
  output$current_page <- renderText({ page_state() })

  # CRÍTICO: Mantiene el output vivo incluso cuando el panel está oculto
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
