library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: GLASS CARDS CON EFECTO SLIDE
# ==============================================================================
css_rscience_slider <- "
body { background-color: #0f171e; color: white; overflow-x: hidden; }

.main-container { padding: 20px; }

/* El 'Viewport' que recorta la vista */
.slider-viewport {
  width: 100%;
  overflow: hidden;
  position: relative;
  padding: 10px;
}

/* El contenedor que se mueve */
.slider-track {
  display: flex;
  width: 200%; /* Dos packs, uno al lado del otro */
  transition: transform 0.6s cubic-bezier(0.85, 0, 0.15, 1);
}

.pack-group {
  width: 50%; /* Cada pack ocupa la mitad del track (el ancho del viewport) */
  padding-right: 20px;
}

/* --- ESTILO DE TARJETAS (Glass Phase Cards) --- */
.phase-card {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid rgba(255, 255, 255, 0.05);
  border-left: 5px solid #444;
  border-radius: 12px;
  padding: 15px;
  margin-bottom: 12px;
  display: flex;
  align-items: center;
  transition: all 0.3s ease;
  opacity: 0.4;
  cursor: not-allowed;
  pointer-events: none;
}

.phase-card.ready {
  opacity: 1;
  border-left-color: #00d4ff;
  background: rgba(0, 212, 255, 0.05);
  cursor: pointer;
  pointer-events: auto;
}

.phase-card.active {
  background: #00d4ff !important;
  color: #1a252f !important;
  transform: scale(1.02);
  box-shadow: 0 0 20px rgba(0, 212, 255, 0.3);
}

.phase-card.active .phase-icon,
.phase-card.active .phase-title { color: #1a252f !important; }

.phase-icon { font-size: 1.5rem; margin-right: 15px; color: #00d4ff; }
.phase-content { flex-grow: 1; }
.phase-title { font-weight: 700; font-size: 0.95rem; }

/* Botón de cambio de pack */
.pack-toggle-btn {
  background: transparent;
  border: 1px solid #00d4ff;
  color: #00d4ff;
  border-radius: 20px;
  padding: 5px 15px;
  font-size: 0.7rem;
  margin-bottom: 20px;
  transition: 0.3s;
}
.pack-toggle-btn:hover { background: #00d4ff; color: #1a252f; }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_slider))),

  fluidRow(
    column(4, # Espacio lateral para las opciones
           div(class="main-container",
               actionButton("btn_switch_pack", "CAMBIAR DE PACK", class="pack-toggle-btn"),

               div(class="slider-viewport",
                   div(id="master_track", class="slider-track",

                       # --- PACK 01: SETUP ---
                       div(class="pack-group",
                           h5("PACK 01: ENGINE SETUP", style="color:#00d4ff; font-size:0.7rem; margin-bottom:15px;"),
                           div(id="card_dataset", class="phase-card ready active",
                               div(class="phase-icon", icon("database")),
                               div(class="phase-content", div(class="phase-title", "Dataset Selection"))
                           ),
                           div(id="card_tool", class="phase-card ready",
                               div(class="phase-icon", icon("screwdriver-wrench")),
                               div(class="phase-content", div(class="phase-title", "Tool Engine"))
                           ),
                           div(id="card_script", class="phase-card ready",
                               div(class="phase-icon", icon("code")),
                               div(class="phase-content", div(class="phase-title", "Script Engine"))
                           ),
                           div(id="card_settings", class="phase-card ready",
                               div(class="phase-icon", icon("sliders")),
                               div(class="phase-content", div(class="phase-title", "Settings"))
                           )
                       ),

                       # --- PACK 02: ANALYSIS ---
                       div(class="pack-group",
                           h5("PACK 02: ANALYSIS", style="color:#00d4ff; font-size:0.7rem; margin-bottom:15px;"),
                           div(id="card_theory", class="phase-card",
                               div(class="phase-icon", icon("book")),
                               div(class="phase-content", div(class="phase-title", "Theory"))
                           ),
                           div(id="card_shiny", class="phase-card",
                               div(class="phase-icon", icon("desktop")),
                               div(class="phase-content", div(class="phase-title", "Shiny Outputs"))
                           ),
                           div(id="card_report", class="phase-card",
                               div(class="phase-icon", icon("file-export")),
                               div(class="phase-content", div(class="phase-title", "Reporting"))
                           )
                       )
                   )
               )
           )
    ),

    column(8,
           card(
             card_header("Visualización de Resultados"),
             p("Completa los pasos del Pack 01 para que el sistema 'corra' las opciones al Pack 02."),
             actionButton("go_pack2", "Habilitar y Saltar al Pack 02", class="btn btn-outline-info")
           )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # Tracking de posición
  current_pack <- reactiveVal(1)

  # JS para clicks en tarjetas
  all_cards <- c("card_dataset", "card_tool", "card_script", "card_settings",
                 "card_theory", "card_shiny", "card_report")

  observe({
    lapply(all_cards, function(id) {
      runjs(sprintf("$('#%s').click(function() { Shiny.setInputValue('clicked_card', '%s', {priority: 'event'}); });", id, id))
    })
  })

  # Manejo de Selección Visual
  observeEvent(input$clicked_card, {
    lapply(all_cards, function(id) removeClass(id, "active"))
    addClass(input$clicked_card, "active")
  })

  # Lógica de Cambio de Pack (El Corrimiento)
  observeEvent(input$btn_switch_pack, {
    if(current_pack() == 1) {
      runjs("$('#master_track').css('transform', 'translateX(-50%)');")
      current_pack(2)
    } else {
      runjs("$('#master_track').css('transform', 'translateX(0%)');")
      current_pack(1)
    }
  })

  # Simular habilitación del Pack 02
  observeEvent(input$go_pack2, {
    addClass("card_theory", "ready")
    addClass("card_shiny", "ready")
    addClass("card_report", "ready")
    # Hacer el corrimiento automático
    runjs("$('#master_track').css('transform', 'translateX(-50%)');")
    current_pack(2)
  })
}

shinyApp(ui, server)
