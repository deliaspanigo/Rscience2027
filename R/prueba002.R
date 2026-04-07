library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: ACCORDION-FLOW (v.0.0.1 CYAN EDITION)
# ==============================================================================
css_rscience_flow <- "
body { background-color: #0f171e; color: #e1e1e1; font-family: 'Inter', sans-serif; }

/* Contenedor del Sidebar Custom */
.rscience-sidebar {
  padding: 15px;
  background: linear-gradient(180deg, #16222c 0%, #0f171e 100%);
  height: 100vh;
  border-right: 2px solid #00d4ff33;
}

/* Títulos de Fase (Colapsables) */
.phase-group {
  margin-bottom: 15px;
  border-radius: 8px;
  background: rgba(255,255,255,0.02);
  border: 1px solid rgba(0,212,255,0.1);
  transition: all 0.3s ease;
}

.phase-group.active-group {
  border-color: #00d4ff;
  background: rgba(0,212,255,0.05);
  box-shadow: 0 0 15px rgba(0,212,255,0.1);
}

.group-header {
  padding: 10px 15px;
  display: flex;
  justify-content: space-between;
  align-items: center;
  cursor: pointer;
  text-transform: uppercase;
  font-size: 0.75rem;
  font-weight: 800;
  letter-spacing: 1px;
  color: #555;
}

.active-group .group-header { color: #00d4ff; }

/* Sub-pasos (Tus antiguas opciones) */
.sub-step {
  padding: 12px 20px;
  display: flex;
  align-items: center;
  gap: 15px;
  opacity: 0.4;
  pointer-events: none;
  transition: all 0.2s;
  border-left: 3px solid transparent;
}

/* Estado Ready: Disponible para click */
.sub-step.ready {
  opacity: 1;
  pointer-events: auto;
  cursor: pointer;
  color: #fff;
}

.sub-step.ready:hover { background: rgba(0,212,255,0.1); }

/* Estado Active: Seleccionado actualmente */
.sub-step.active {
  background: rgba(0,212,255,0.2);
  border-left-color: #00d4ff;
  font-weight: 700;
}

/* Estado Locked: Bloqueado (Verde) */
.sub-step.locked {
  color: #28a745;
}

.sub-step i { width: 20px; text-align: center; }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_flow))),

  fluidRow(
    column(4, class = "rscience-sidebar",
           div(class="text-center mb-4", h3("RSCIENCE", style="color:#00d4ff; font-weight:900;")),

           # GRUPO 1: SETUP
           div(id="grp_setup", class="phase-group active-group",
               div(class="group-header", span("1. Setup Phase"), icon("chevron-down")),
               div(id="step_dataset", class="sub-step ready active", icon("database"), "Dataset Selection", icon("unlock")),
               div(id="step_tool",    class="sub-step", icon("screwdriver-wrench"), "Tool Engine", icon("lock")),
               div(id="step_script",  class="sub-step", icon("code"), "Script Engine", icon("lock")),
               div(id="step_settings",class="sub-step", icon("sliders"), "Settings", icon("lock"))
           ),

           # GRUPO 2: INFO
           div(id="grp_info", class="phase-group",
               div(class="group-header", span("2. Information"), icon("chevron-right")),
               div(id="step_theory", class="sub-step", icon("book"), "Theory", icon("lock")),
               div(id="step_biblio", class="sub-step", icon("list-ol"), "Bibliography", icon("lock")),
               div(id="step_cite",   class="sub-step", icon("quote-left"), "Cite!", icon("lock"))
           ),

           # GRUPO 3: ANALYSIS
           div(id="grp_analysis", class="phase-group",
               div(class="group-header", span("3. Data Analysis"), icon("chevron-right")),
               div(id="step_shiny",  class="sub-step", icon("desktop"), "Shiny Outputs", icon("lock")),
               div(id="step_asesor", class="sub-step", icon("robot"), "ASA Asesor", icon("lock")),
               div(id="step_report", class="sub-step", icon("file-export"), "Reporting", icon("lock"))
           )
    ),

    column(8,
           wellPanel(
             h3("Panel de Control Simulado"),
             actionButton("go_next", "Simular: Bloquear paso actual y avanzar", class="btn-primary"),
             hr(),
             textOutput("debug_nav")
           )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # Lista ordenada de tus IDs para manejar el flujo
  steps_order <- c("step_dataset", "step_tool", "step_script", "step_settings",
                   "step_theory", "step_biblio", "step_cite",
                   "step_shiny", "step_asesor", "step_report")

  # Tracking de qué paso está 'locked' y qué paso está 'active'
  state <- reactiveValues(
    current_idx = 1,
    locked_steps = c()
  )

  # JS para detectar clicks en los pasos
  observe({
    lapply(steps_order, function(id) {
      runjs(sprintf("$('#%s').click(function() { Shiny.setInputValue('clicked_step', '%s', {priority: 'event'}); });", id, id))
    })
  })

  # Al hacer click en un paso (Solo si está ready)
  observeEvent(input$clicked_step, {
    # Aquí iría tu lógica de updateTabsetPanel
    state$current_idx <- which(steps_order == input$clicked_step)

    # UI: Limpiar activos y marcar el nuevo
    lapply(steps_order, function(id) removeClass(id, "active"))
    addClass(input$clicked_step, "active")
  })

  # Simulación de tu lógica de candados
  observeEvent(input$go_next, {
    curr_id <- steps_order[state$current_idx]

    # 1. Bloquear el paso actual (Poner en verde)
    addClass(curr_id, "locked")
    runjs(sprintf("$('#%s i:last-child').removeClass('fa-lock-open fa-lock').addClass('fa-check-circle');", curr_id))

    # 2. Habilitar el siguiente
    if(state$current_idx < length(steps_order)) {
      next_id <- steps_order[state$current_idx + 1]
      addClass(next_id, "ready")
      runjs(sprintf("$('#%s i:last-child').removeClass('fa-lock').addClass('fa-unlock');", next_id))

      # 3. Lógica de expansión automática de grupos
      # Si el siguiente paso pertenece a un nuevo grupo, lo expandimos
      if(next_id == "step_theory") {
        removeClass("grp_setup", "active-group")
        addClass("grp_info", "active-group")
      } else if (next_id == "step_shiny") {
        removeClass("grp_info", "active-group")
        addClass("grp_analysis", "active-group")
      }
    }
  })

  output$debug_nav <- renderText({
    paste("Navegando en:", steps_order[state$current_idx])
  })
}

shinyApp(ui, server)
