library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# 1. CSS CUSTOM: THE GLASS PHASE CARDS
# ==============================================================================
css_rscience_glass <- "
/* Fondo oscuro para contraste Cyan */
body {
  background-color: #1a252f;
  color: white;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
}

/* Contenedor principal de fases */
.phase-container {
  max-width: 400px;
  margin: 20px auto;
  padding: 10px;
}

/* --- LA TARJETA BASE (Glassmorphism) --- */
.phase-card {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid rgba(255, 255, 255, 0.05);
  border-left: 5px solid #555; /* Borde Gris por defecto (LOCKED) */
  border-radius: 12px;
  padding: 18px;
  margin-bottom: 15px;
  display: flex;
  align-items: center;
  position: relative;
  overflow: hidden;
  transition: all 0.4s cubic-bezier(0.165, 0.84, 0.44, 1); /* Animación suave */

  /* Estado inicial: Bloqueado */
  opacity: 0.5;
  cursor: not-allowed;
  pointer-events: none; /* No recibe clicks */
}

/* Efecto de brillo de fondo (Glow) */
.phase-card::before {
  content: '';
  position: absolute;
  top: 0; left: 0; width: 100%; height: 100%;
  background: radial-gradient(circle at center, rgba(0, 212, 255, 0.1) 0%, rgba(0, 0, 0, 0) 70%);
  opacity: 0;
  transition: opacity 0.4s ease;
}

/* --- ESTADO 1: DISPONIBLE (READY) --- */
.phase-card.ready {
  opacity: 1;
  border-left-color: #00d4ff; /* Borde Cyan */
  background: rgba(0, 212, 255, 0.03);
  cursor: pointer;
  pointer-events: auto; /* Ya se puede clickear */
}

.phase-card.ready:hover {
  background: rgba(0, 212, 255, 0.08);
  transform: translateY(-3px);
  box-shadow: 0 8px 15px rgba(0, 212, 255, 0.15);
}

.phase-card.ready:hover::before {
  opacity: 1;
}

/* --- ESTADO 2: SELECCIONADO (ACTIVE) --- */
.phase-card.active {
  background: #00d4ff !important; /* Fondo Cyan Sólido */
  border-left-color: #00d4ff !important;
  color: #1a252f !important; /* Texto Oscuro para contraste */
  transform: scale(1.03);
  box-shadow: 0 10px 25px rgba(0, 212, 255, 0.4);
}

.phase-card.active::before { opacity: 0; } /* Quitamos el glow interno */

/* --- ELEMENTOS INTERNOS DE LA TARJETA --- */
.phase-icon {
  font-size: 2rem;
  margin-right: 18px;
  width: 40px;
  text-align: center;
  transition: color 0.3s ease;
  color: #888; /* Gris por defecto */
}

.phase-card.ready .phase-icon { color: #00d4ff; } /* Icono Cyan si está ready */
.phase-card.active .phase-icon { color: #1a252f; } /* Icono Oscuro si está active */

.phase-content { flex-grow: 1; }

.phase-number {
  display: block;
  text-transform: uppercase;
  font-weight: 800;
  font-size: 0.7rem;
  letter-spacing: 1px;
  margin-bottom: 2px;
  color: #888; /* Gris por defecto */
}

.phase-card.ready .phase-number { color: #00d4ff; } /* Número Cyan si está ready */
.phase-card.active .phase-number { color: rgba(26, 37, 47, 0.7); } /* Número Oscuro tenue si está active */

.phase-title {
  display: block;
  font-weight: 700;
  font-size: 1.1rem;
  color: white; /* Texto blanco por defecto */
}

.phase-card.active .phase-title { color: #1a252f; } /* Título Oscuro si está active */

/* --- EL CANDADO (FEEDBACK VISUAL) --- */
.phase-lock {
  font-size: 1.2rem;
  color: #555; /* Gris si está bloqueado */
  transition: all 0.3s ease;
}

.phase-card.ready .phase-lock { color: #00d4ff; } /* Candado Cyan si está ready */
.phase-card.active .phase-lock { color: #1a252f; } /* Candado Oscuro si está active */

/* --- ANIMACIÓN PULSE PARA EL PASO SIGUIENTE --- */
@keyframes pulse-cyan {
  0% { box-shadow: 0 0 0 0 rgba(0, 212, 255, 0.4); }
  70% { box-shadow: 0 0 0 10px rgba(0, 212, 255, 0); }
  100% { box-shadow: 0 0 0 0 rgba(0, 212, 255, 0); }
}

.phase-card.ready.next-step {
  animation: pulse-cyan 2s infinite;
}
"

# ==============================================================================
# 2. UI: THE PHASE NAVIGATION LAYOUT
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  useShinyjs(), # Obligatorio para controlar las clases
  tags$head(tags$style(HTML(css_rscience_glass))),

  titlePanel("Rscience UI Concept - Phase Cards v.0.0.1"),
  br(),

  fluidRow(
    column(6,
           tags$div(class = "phase-container",
                    h4("Control de Fases (Motor RScience)", class = "text-muted text-center mb-4"),

                    # --- PHASE 01: DATASET ---
                    # Iniciamos 'ready' y 'active' para simular que es el primer paso
                    tags$div(id = "card_dataset", class = "phase-card ready active",
                             tags$div(class = "phase-icon", icon("database")),
                             tags$div(class = "phase-content",
                                      tags$span(class = "phase-number", "PHASE 01"),
                                      tags$span(class = "phase-title", "Dataset Selection")
                             ),
                             tags$div(class = "phase-lock", icon("unlock"))
                    ),

                    # --- PHASE 02: TOOL ENGINE ---
                    # Iniciamos bloqueado (sin clase ready/active)
                    tags$div(id = "card_tool", class = "phase-card",
                             tags$div(class = "phase-icon", icon("cogs")),
                             tags$div(class = "phase-content",
                                      tags$span(class = "phase-number", "PHASE 02"),
                                      tags$span(class = "phase-title", "Tool Engine")
                             ),
                             tags$div(class = "phase-lock", icon("lock"))
                    ),

                    # --- PHASE 03: SCRIPT ENGINE ---
                    tags$div(id = "card_script", class = "phase-card",
                             tags$div(class = "phase-icon", icon("file-code")),
                             tags$div(class = "phase-content",
                                      tags$span(class = "phase-number", "PHASE 03"),
                                      tags$span(class = "phase-title", "Script Engine")
                             ),
                             tags$div(class = "phase-lock", icon("lock"))
                    )
           )
    ),

    column(6,
           wellPanel(
             h4("Simulador de Lógica (Server)"),
             p("Usa estos botones para simular que completas una fase en el módulo."),
             hr(),
             actionButton("btn_lock_dataset", "1. Bloquear Dataset (Fase 1 completada)", class = "btn-outline-cyan w-100 mb-2"),
             actionButton("btn_lock_tool", "2. Bloquear Tool (Fase 2 completada)", class = "btn-outline-cyan w-100 mb-2"),
             actionButton("btn_reset", "Reiniciar Proceso", class = "btn-outline-danger w-100 mt-3")
           ),
           wellPanel(
             h4("Estado de Navegación:"),
             verbatimTextOutput("status")
           )
    )
  )
)

# ==============================================================================
# 3. SERVER: LOGIC & STYLE CONTROL
# ==============================================================================
server <- function(input, output, session) {

  # Usamos reactiveValues para rastrear qué fase está activa
  nav_state <- reactiveValues(active_phase = "dataset")

  # --- TRUCO: Simular Clicks en las Tarjetas ---
  # Shinyjs no detecta 'onclick' en divs por defecto. Usamos runjs para enviar
  # un mensaje a Shiny cuando se hace click en el div.
  runjs("
    $('#card_dataset').click(function() { Shiny.setInputValue('click_dataset', true, {priority: 'event'}); });
    $('#card_tool').click(function() { Shiny.setInputValue('click_tool', true, {priority: 'event'}); });
    $('#card_script').click(function() { Shiny.setInputValue('click_script', true, {priority: 'event'}); });
  ")

  # --- LÓGICA DE NAVEGACIÓN (Simulada) ---

  # Al hacer click en Fase 1 (Dataset)
  observeEvent(input$click_dataset, {
    nav_state$active_phase <- "dataset"
    # Actualizamos clases visually
    addClass("card_dataset", "active")
    removeClass("card_tool", "active")
    removeClass("card_script", "active")
  })

  # Al hacer click en Fase 2 (Tool) - Solo si está 'ready'
  observeEvent(input$click_tool, {
    nav_state$active_phase <- "tool"
    # Actualizamos clases visually
    removeClass("card_dataset", "active")
    addClass("card_tool", "active")
    removeClass("card_script", "active")
  })

  # --- SIMULADOR DE FLUJO (Lógica de los Candados) ---

  # Simular Completar Fase 1 (Dataset Bloqueado -> Fase 2 Disponible)
  observeEvent(input$btn_lock_dataset, {
    # 1. Fase 1: Cambiamos icono a candado cerrado (UNLOCK -> LOCK)
    runjs("$('#card_dataset .phase-lock i').removeClass('fa-unlock').addClass('fa-lock');")

    # 2. Fase 2: La hacemos DISPONIBLE (Ready) y le damos el pulse de 'next step'
    addClass("card_tool", "ready")
    addClass("card_tool", "next-step")
    runjs("$('#card_tool .phase-lock i').removeClass('fa-lock').addClass('fa-unlock');")
  })

  # Simular Completar Fase 2 (Tool Bloqueado -> Fase 3 Disponible)
  observeEvent(input$btn_lock_tool, {
    # 1. Fase 2: Candado cerrado y quitamos el pulse
    runjs("$('#card_tool .phase-lock i').removeClass('fa-unlock').addClass('fa-lock');")
    removeClass("card_tool", "next-step")

    # 2. Fase 3: Disponible (Ready) y pulse
    addClass("card_script", "ready")
    addClass("card_script", "next-step")
    runjs("$('#card_script .phase-lock i').removeClass('fa-lock').addClass('fa-unlock');")
  })

  # Reiniciar todo
  observeEvent(input$btn_reset, {
    nav_state$active_phase <- "dataset"

    # Reset Visual Fases 2 y 3
    removeClass("card_tool", "ready active next-step")
    removeClass("card_script", "ready active next-step")
    runjs("$('#card_tool .phase-lock i').removeClass('fa-unlock').addClass('fa-lock');")
    runjs("$('#card_script .phase-lock i').removeClass('fa-unlock').addClass('fa-lock');")

    # Reset Visual Fase 1
    addClass("card_dataset", "ready active")
    runjs("$('#card_dataset .phase-lock i').removeClass('fa-lock').addClass('fa-unlock');")
  })

  output$status <- renderPrint({
    paste("Fase Activa en el Motor:", toupper(nav_state$active_phase))
  })
}

shinyApp(ui, server)
