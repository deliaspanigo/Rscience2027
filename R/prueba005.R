library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: TRIPLE SLIDER & PLAY BUTTON
# ==============================================================================
css_rscience_triple <- "
body { background-color: #0b0f13; color: white; overflow-x: hidden; }

/* El Viewport ahora es el marco de tu sidebar */
.sidebar-viewport {
  width: 100%;
  overflow: hidden;
  position: relative;
  padding: 20px 10px;
  background: #0f171e;
  height: 100vh;
  border-right: 1px solid rgba(0, 212, 255, 0.2);
}

/* El track tiene un ancho del 300% para los 3 packs */
.slider-track {
  display: flex;
  width: 300%;
  transition: transform 0.7s cubic-bezier(0.86, 0, 0.07, 1);
}

.pack-group {
  width: 33.333%; /* Cada pack es exactamente 1/3 del track */
  padding: 0 15px;
}

/* --- TARJETAS PHASE CARDS --- */
.phase-card {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid rgba(255, 255, 255, 0.05);
  border-left: 4px solid #444;
  border-radius: 10px;
  padding: 12px 15px;
  margin-bottom: 10px;
  display: flex;
  align-items: center;
  transition: all 0.3s ease;
  cursor: pointer;
}

.phase-card.active {
  background: rgba(0, 212, 255, 0.1);
  border-left-color: #00d4ff;
  box-shadow: 0 0 15px rgba(0, 212, 255, 0.1);
}

.phase-card:hover { background: rgba(255, 255, 255, 0.08); }

.phase-icon { font-size: 1.2rem; margin-right: 12px; color: #00d4ff; width: 25px; text-align: center;}
.phase-title { font-weight: 600; font-size: 0.9rem; color: #cbd5e0; }
.active .phase-title { color: #00d4ff; }

/* --- BOTÓN PLAY ESPECIAL --- */
.btn-play-engine {
  background: linear-gradient(135deg, #00d4ff 0%, #007691 100%);
  color: #0b0f13 !important;
  font-weight: 900;
  border: none;
  border-radius: 8px;
  padding: 15px;
  width: 100%;
  margin-top: 10px;
  letter-spacing: 2px;
  box-shadow: 0 4px 15px rgba(0, 212, 255, 0.4);
  transition: 0.3s;
}
.btn-play-engine:hover { transform: scale(1.02); box-shadow: 0 6px 20px rgba(0, 212, 255, 0.6); }

/* Indicadores de navegación (Puntos abajo) */
.nav-dots { display: flex; justify-content: center; gap: 10px; margin-bottom: 20px; }
.dot { width: 8px; height: 8px; background: #333; border-radius: 50%; transition: 0.3s; }
.dot.active { background: #00d4ff; box-shadow: 0 0 8px #00d4ff; }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_triple))),

  fluidRow(
    column(3, class = "p-0", # Columna de Sidebar
           div(class="sidebar-viewport",

               # Puntos indicadores
               div(class="nav-dots",
                   div(id="dot1", class="dot active"),
                   div(id="dot2", class="dot"),
                   div(id="dot3", class="dot")
               ),

               div(id="master_track", class="slider-track",

                   # --- PACK 01: ENGINE SETUP ---
                   div(class="pack-group",
                       h6("I. CONFIGURATION", class="text-muted mb-3"),
                       div(id="c_dataset", class="phase-card active", div(class="phase-icon", icon("database")), div(class="phase-content", div(class="phase-title", "Dataset"))),
                       div(id="c_tool",    class="phase-card", div(class="phase-icon", icon("screwdriver-wrench")), div(class="phase-content", div(class="phase-title", "Tools"))),
                       div(id="c_script",  class="phase-card", div(class="phase-icon", icon("code")), div(class="phase-content", div(class="phase-title", "Script"))),
                       div(id="c_settings",class="phase-card", div(class="phase-icon", icon("sliders")), div(class="phase-content", div(class="phase-title", "Settings"))),
                       actionButton("play_engine", "RUN ENGINE", icon = icon("play"), class="btn-play-engine")
                   ),

                   # --- PACK 02: RESOURCES ---
                   div(class="pack-group",
                       h6("II. KNOWLEDGE BASE", class="text-muted mb-3"),
                       div(id="c_theory", class="phase-card", div(class="phase-icon", icon("book")), div(class="phase-content", div(class="phase-title", "Theory"))),
                       div(id="c_biblio", class="phase-card", div(class="phase-icon", icon("list-ol")), div(class="phase-content", div(class="phase-title", "Bibliography"))),
                       div(id="c_cite",   class="phase-card", div(class="phase-icon", icon("quote-left")), div(class="phase-content", div(class="phase-title", "Cite!")))
                   ),

                   # --- PACK 03: OUTPUTS ---
                   div(class="pack-group",
                       h6("III. RESULTS", class="text-muted mb-3"),
                       div(id="c_shiny",   class="phase-card", div(class="phase-icon", icon("desktop")), div(class="phase-content", div(class="phase-title", "Shiny Outputs"))),
                       div(id="c_asa",     class="phase-card", div(class="phase-icon", icon("robot")), div(class="phase-content", div(class="phase-title", "ASA Assistant"))),
                       div(id="c_scripts_out", class="phase-card", div(class="phase-icon", icon("file-code")), div(class="phase-content", div(class="phase-title", "Generated Scripts"))),
                       div(id="c_download",class="phase-card", div(class="phase-icon", icon("download")), div(class="phase-content", div(class="phase-title", "Download Report")))
                   )
               ),

               # Botones de navegación manual (Opcionales, para probar)
               div(class="mt-5 d-flex justify-content-between",
                   actionButton("go_prev", "Volver", class="btn-sm btn-outline-secondary"),
                   actionButton("go_next", "Siguiente", class="btn-sm btn-outline-cyan")
               )
           )
    ),

    column(9,
           card(
             card_header("Rscience Workstation"),
             div(style="height: 500px; display: flex; align-items: center; justify-content: center;",
                 h2(textOutput("current_view"), style="color: #00d4ff; opacity: 0.5;")
             )
           )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # Posición actual del slider: 0, 1 o 2
  pos <- reactiveVal(0)

  # Lógica de movimiento
  move_slider <- function(new_pos) {
    pos(new_pos)
    shift <- new_pos * -33.333
    runjs(sprintf("$('#master_track').css('transform', 'translateX(%f%%)');", shift))

    # Actualizar puntos (dots)
    runjs("$('.dot').removeClass('active');")
    runjs(sprintf("$('#dot%d').addClass('active');", new_pos + 1))
  }

  observeEvent(input$go_next, { if(pos() < 2) move_slider(pos() + 1) })
  observeEvent(input$go_prev, { if(pos() > 0) move_slider(pos() - 1) })

  # Evento especial: Al dar clic en PLAY, procesamos y saltamos al Pack 3
  observeEvent(input$play_engine, {
    # Aquí iría tu código de procesamiento...
    # Luego del proceso, saltamos directamente a los resultados (Pack 3)
    move_slider(2)
  })

  # Manejo de selección de tarjetas
  all_cards <- c("c_dataset", "c_tool", "c_script", "c_settings",
                 "c_theory", "c_biblio", "c_cite",
                 "c_shiny", "c_asa", "c_scripts_out", "c_download")

  selected_card <- reactiveVal("Dataset Selection")

  lapply(all_cards, function(id) {
    runjs(sprintf("$('#%s').click(function() { Shiny.setInputValue('card_click', '%s', {priority: 'event'}); });", id, id))
  })

  observeEvent(input$card_click, {
    lapply(all_cards, function(id) removeClass(id, "active"))
    addClass(input$card_click, "active")
    selected_card(input$card_click)
  })

  output$current_view <- renderText({ selected_card() })
}

shinyApp(ui, server)
