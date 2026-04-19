library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body { background-color: #0d1117; color: white; font-family: 'Segoe UI', sans-serif; }

      .engine-container {
        display: flex;
        justify-content: center;
        gap: 40px;
        margin-top: 80px;
      }

      /* Base de la Tarjeta */
      .engine-card {
        width: 180px;
        height: 180px;
        background: #161b22;
        border: 2px solid #30363d;
        border-radius: 30px; /* Redondeado pronunciado */
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
        position: relative;
      }

      .engine-card i { font-size: 45px; margin-bottom: 15px; }
      .engine-card span { font-weight: 600; letter-spacing: 1.5px; font-size: 0.7rem; }

      /* Colores específicos solicitados */
      #opt_unlock { color: #00e5ff; } /* Cyan */
      #opt_lock   { color: #00ff88; } /* Verde */
      #opt_reset  { color: #ff9100; } /* Naranja */

      /* Efecto Hover */
      .engine-card:hover {
        transform: translateY(-5px);
        border-color: currentColor;
        box-shadow: 0 0 20px -5px currentColor;
      }

      /* ESTADO SELECCIONADO */
      .selected {
        background: currentColor !important;
        border-color: white !important;
        transform: scale(1.15);
        z-index: 10;
        box-shadow: 0 0 40px -5px currentColor;
      }

      /* El icono y texto dentro del seleccionado se vuelven oscuros para contraste */
      .selected i, .selected span { color: #0d1117 !important; }

      /* ESTADO DESACTIVADO (Los otros) */
      .disabled-card {
        opacity: 0.15;
        filter: blur(1px) grayscale(0.5);
        pointer-events: none;
        transform: scale(0.85);
      }

      .reset-btn {
        margin-top: 60px;
        background: #21262d;
        border: 1px solid #30363d;
        color: #c9d1d9;
        padding: 12px 30px;
        border-radius: 10px;
        font-weight: bold;
        transition: 0.3s;
      }
      .reset-btn:hover { background: #30363d; color: white; border-color: #8b949e; }
    "))
  ),

  div(class = "text-center",
      h1("ENGINE CONTROL UNIT", style = "letter-spacing: 8px; margin-top: 40px; font-weight: 900;"),
      p("SYSTEM STATUS: STANDBY", style = "color: #8b949e; font-family: monospace;")
  ),

  div(class = "engine-container",
      # Cyan - Unlock
      div(id = "opt_unlock", class = "engine-card",
          icon("lock-open"), span("UNLOCK SYSTEM")),

      # Verde - Lock
      div(id = "opt_lock", class = "engine-card",
          icon("lock"), span("LOCK ENGINE")),

      # Naranja - Reset
      div(id = "opt_reset", class = "engine-card",
          icon("sync-alt"), span("RESET CORE"))
  ),

  div(class = "text-center",
      actionButton("reset_all", "REESTABLECER PANEL", class = "reset-btn")
  )
)

server <- function(input, output, session) {

  ids <- c("opt_unlock", "opt_lock", "opt_reset")

  # Observador dinámico para los clics
  lapply(ids, function(id) {
    onclick(id, {
      # 1. Aplicar clase seleccionada al clickeado
      addClass(id, "selected")
      removeClass(id, "disabled-card")

      # 2. Desactivar y opacar los otros dos
      others <- ids[ids != id]
      for(other in others) {
        addClass(other, "disabled-card")
        removeClass(other, "selected")
      }

      # Notificación visual sutil
      showNotification(paste("Ejecutando:", id), type = "message", duration = 2)
    })
  })

  # Resetear el panel a su estado original
  observeEvent(input$reset_all, {
    for(id in ids) {
      removeClass(id, "selected")
      removeClass(id, "disabled-card")
    }
  })
}

shinyApp(ui, server)
