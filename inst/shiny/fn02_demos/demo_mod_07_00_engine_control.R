library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
# library(listviewer) # Asegúrate de tenerlo para el modo debug

# ==============================================================================
# APP.R - TESTING ENGINE CONTROL v.0.1.5 (ISOLATED)
# ==============================================================================
devtools::load_all()

ui <- fluidPage(
  useShinyjs(),

  theme = bs_theme(
    version = 5,
    bg = "#0d1117",
    fg = "#ffffff",
    primary = "#00e5ff"
  ),


  div(class = "container", style = "padding: 50px 15px;",

      h2("Engine Control Lab v.0.1.5"),
      p("Aislamiento de CSS y control por señales externas.", style = "color: #8b949e;"),
      br(),

      # CASO 1: FULL DEBUG (Visible todo)
      div(class = "section-container",
          h4("1. Full Debug Mode"),
          p(class = "instruction-text",
            "Configuración: show_debug = T, show_ghost = T. Ideal para desarrollo."),
          mod_07_00_engine_control_ui("full_debug")
      ),

      # CASO 2: TESTER MODE (Solo botón de auxilio)
      div(class = "section-container",
          h4("2. Tester Mode"),
          p(class = "instruction-text",
            "Configuración: show_debug = F, show_ghost = T. Solo el botón Ghost es visible."),
          mod_07_00_engine_control_ui("tester_hub")
      ),

      # CASO 3: PRODUCTION MODE (Limpio)
      div(class = "section-container",
          h4("3. Production Mode"),
          p(class = "instruction-text",
            "Configuración: show_debug = F, show_ghost = F. El motor es minimalista."),
          mod_07_00_engine_control_ui("prod_hub")
      ),

      # CONTROL MAESTRO
      div(
        style = "background: rgba(255, 145, 0, 0.05); padding: 40px; border-radius: 25px; border: 1px dashed #ff9100; margin-top: 50px;",
        h4("Control Maestro Externo", style = "color: #ff9100; margin-top: 0;"),
        p(style = "color: #ff9100; opacity: 0.8;",
          "Al pulsar este botón, se envía un clic virtual al ID del botón Ghost de cada módulo."),
        actionButton(
          "trigger_external",
          "SIMULAR DESBLOQUEO GLOBAL",
          class = "btn-outline-warning",
          style = "font-weight: 900; width: 100%; letter-spacing: 2px; padding: 15px; border-radius: 12px;"
        )
      )
  )
)

server <- function(input, output, session) {

  # 1. Full Debug: show_debug y show_ghost como TRUE
  mod_07_00_engine_control_server("full_debug", show_debug = TRUE, show_ghost = TRUE)

  # 2. Tester: Solo mostramos el botón ghost
  mod_07_00_engine_control_server("tester_hub", show_debug = FALSE, show_ghost = TRUE)

  # 3. Production: Todo oculto (pero el botón ghost sigue escuchando)
  mod_07_00_engine_control_server("prod_hub", show_debug = FALSE, show_ghost = FALSE)

  # --- LÓGICA DE SEÑAL GLOBAL ---
  observeEvent(input$trigger_external, {

    # La señal viaja al ID del botón ghost de cada instancia
    shinyjs::click("full_debug-btn_unlock_ghost")
    shinyjs::click("tester_hub-btn_unlock_ghost")
    shinyjs::click("prod_hub-btn_unlock_ghost")

    showNotification(
      "Señal de desbloqueo enviada satisfactoriamente.",
      type = "warning",
      duration = 3
    )
  })
}

shinyApp(ui, server)
