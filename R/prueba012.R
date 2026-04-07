library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: GLASSMORPHISM + SIDEBAR + PACK NAVIGATION
# ==============================================================================
css_rscience_final <- "
body { background-color: #0b1218; color: white; font-family: 'Segoe UI', Roboto, sans-serif; }

/* SIDEBAR REFINADO */
.bslib-sidebar-content {
  background-color: #0f171e !important;
  padding: 0 !important;
  border-right: 1px solid rgba(0, 212, 255, 0.15);
  overflow: hidden !important;
}

/* NAVEGACIÓN POR PUNTOS (TOP) */
.nav-header {
  display: flex; justify-content: center; align-items: center; gap: 20px;
  padding: 20px 0; background: #16222c; border-bottom: 1px solid rgba(0, 212, 255, 0.1);
}
.dot { width: 10px; height: 10px; background: #333; border-radius: 50%; transition: 0.4s; }
.nav-dot-wrapper { display: flex; flex-direction: column; align-items: center; cursor: pointer; }
.nav-dot-wrapper.active .dot { background: #00d4ff; box-shadow: 0 0 10px #00d4ff; transform: scale(1.2); }
.dot-label { font-size: 0.65rem; font-weight: 700; color: #555; text-transform: uppercase; margin-top: 5px; }
.active .dot-label { color: #00d4ff; }

/* VIEWPORT Y SLIDER */
.sidebar-viewport { width: 100%; overflow: hidden; height: 100%; }
.slider-track { display: flex; width: 300%; transition: transform 0.6s cubic-bezier(0.85, 0, 0.15, 1); }
.pack-group { width: 33.333%; padding: 20px 15px; }

/* --- GLASS PHASE CARDS (ESTÉTICA SOLICITADA) --- */
.phase-card {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid rgba(255, 255, 255, 0.05);
  border-left: 5px solid #444;
  border-radius: 12px;
  padding: 15px 18px;
  margin-bottom: 12px;
  display: flex; align-items: center;
  position: relative; overflow: hidden;
  transition: all 0.4s cubic-bezier(0.165, 0.84, 0.44, 1);
  cursor: pointer;
  opacity: 0.6; /* Por defecto un poco apagado si no está activo */
}

/* Efecto Glow de fondo */
.phase-card::before {
  content: ''; position: absolute; top: 0; left: 0; width: 100%; height: 100%;
  background: radial-gradient(circle at center, rgba(0, 212, 255, 0.1) 0%, transparent 70%);
  opacity: 0; transition: opacity 0.4s;
}

.phase-card:hover:not(.active) { background: rgba(255, 255, 255, 0.08); opacity: 1; }
.phase-card:hover::before { opacity: 1; }

/* ESTADO ACTIVO: CIAN SÓLIDO */
.phase-card.active {
  background: #00d4ff !important;
  border-left-color: #00d4ff !important;
  color: #1a252f !important;
  transform: scale(1.02);
  box-shadow: 0 8px 20px rgba(0, 212, 255, 0.3);
  opacity: 1;
}

.phase-icon { font-size: 1.5rem; margin-right: 15px; width: 35px; text-align: center; color: #00d4ff; }
.active .phase-icon { color: #1a252f !important; }

.phase-content { flex-grow: 1; }
.phase-number { font-size: 0.65rem; font-weight: 800; opacity: 0.6; display: block; letter-spacing: 1px; }
.active .phase-number { color: #1a252f; opacity: 0.8; }

.phase-title { font-weight: 700; font-size: 1rem; display: block; }
.active .phase-title { color: #1a252f; }

.phase-lock { font-size: 1rem; opacity: 0.4; }
.active .phase-lock { opacity: 1; color: #1a252f; }

/* SECCIÓN PROCESSING */
.processing-container {
  margin-top: 10px; padding: 15px; border-radius: 12px;
  background: rgba(0, 212, 255, 0.05); border: 1px dashed rgba(0, 212, 255, 0.3);
  text-align: center;
}
.btn-run-main {
  background: #00d4ff; color: #1a252f !important; font-weight: 800;
  border: none; width: 100%; padding: 12px; border-radius: 8px;
  font-size: 0.9rem; letter-spacing: 1px; box-shadow: 0 4px 10px rgba(0,212,255,0.2);
}

/* ÁREA DE TRABAJO */
.work-title { font-size: 2.2rem; font-weight: 800; color: #00d4ff; letter-spacing: -1px; }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  title = span("Rscience v.0.0.1", style="font-weight:800; color:#00d4ff;"),
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#fff", primary = "#00d4ff"),
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_final))),

  sidebar = sidebar(
    width = 320,
    id = "main_sidebar",

    # Navegación Superior
    div(class="nav-header",
        div(id="nav_p1", class="nav-dot-wrapper active", div(class="dot"), div(class="dot-label", "Setup")),
        div(id="nav_p2", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Theory")),
        div(id="nav_p3", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Out"))
    ),

    div(class="sidebar-viewport",
        div(id="master_track", class="slider-track",

            # PACK 01: ENGINE
            div(class="pack-group",
                div(id="c_dataset",  class="phase-card active",
                    div(class="phase-icon", icon("database")),
                    div(class="phase-content", span("PHASE 01", class="phase-number"), span("Dataset Selection", class="phase-title")),
                    div(class="phase-lock", icon("unlock"))
                ),
                div(id="c_tool",     class="phase-card",
                    div(class="phase-icon", icon("screwdriver-wrench")),
                    div(class="phase-content", span("PHASE 02", class="phase-number"), span("Tool Engine", class="phase-title")),
                    div(class="phase-lock", icon("lock"))
                ),
                div(id="c_process",  class="phase-card",
                    div(class="phase-icon", icon("gears")),
                    div(class="phase-content", span("PHASE 03", class="phase-number"), span("Processing", class="phase-title")),
                    div(class="phase-lock", icon("lock"))
                ),
                conditionalPanel(
                  condition = "input.card_click == 'c_process'",
                  div(class="processing-container",
                      actionButton("run_engine", "RUN ANALYSIS", icon = icon("play"), class="btn-run-main")
                  )
                )
            ),

            # PACK 02: THEORY
            div(class="pack-group",
                div(id="c_theory", class="phase-card",
                    div(class="phase-icon", icon("book")),
                    div(class="phase-content", span("DOCS", class="phase-number"), span("Theory & Math", class="phase-title")),
                    div(class="phase-lock", icon("unlock"))
                )
            ),

            # PACK 03: OUTPUT
            div(class="pack-group",
                div(id="c_shiny",   class="phase-card",
                    div(class="phase-icon", icon("desktop")),
                    div(class="phase-content", span("RESULTS", class="phase-number"), span("Visualizer", class="phase-title")),
                    div(class="phase-lock", icon("unlock"))
                )
            )
        )
    )
  ),

  # ÁREA PRINCIPAL
  div(style="padding: 40px;",
      h2(textOutput("display_title"), class="work-title"),
      hr(style="opacity:0.2; width: 60px; border-width: 3px; border-color: #00d4ff;"),
      uiOutput("dynamic_content")
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # Lógica de movimiento entre Packs
  move_to_pack <- function(pack_num) {
    shift <- (pack_num - 1) * -33.333
    runjs(sprintf("$('#master_track').css('transform', 'translateX(%f%%)');", shift))
    runjs("$('.nav-dot-wrapper').removeClass('active');")
    runjs(sprintf("$('#nav_p%d').addClass('active');", pack_num))
  }

  # Eventos de los Círculos
  runjs("
    $('#nav_p1').click(function() { Shiny.setInputValue('nav_p1_click', true, {priority: 'event'}); });
    $('#nav_p2').click(function() { Shiny.setInputValue('nav_p2_click', true, {priority: 'event'}); });
    $('#nav_p3').click(function() { Shiny.setInputValue('nav_p3_click', true, {priority: 'event'}); });
  ")

  observeEvent(input$nav_p1_click, { move_to_pack(1) })
  observeEvent(input$nav_p2_click, { move_to_pack(2) })
  observeEvent(input$nav_p3_click, { move_to_pack(3) })

  # Selección de Tarjetas
  all_cards <- c("c_dataset", "c_tool", "c_process", "c_theory", "c_shiny")
  lapply(all_cards, function(id) {
    runjs(sprintf("$('#%s').click(function() { Shiny.setInputValue('card_click', '%s', {priority: 'event'}); });", id, id))
  })

  observeEvent(input$card_click, {
    lapply(all_cards, function(id) removeClass(id, "active"))
    addClass(input$card_click, "active")
  })

  # Acción del Botón RUN
  observeEvent(input$run_engine, {
    move_to_pack(3)
    runjs("$('#c_shiny').click();")
  })

  output$display_title <- renderText({
    if(is.null(input$card_click)) return("DATASET SELECTION")
    toupper(gsub("c_", "", input$card_click))
  })

  output$dynamic_content <- renderUI({
    p("Contenido del módulo cargado con éxito. Usa el menú lateral para navegar por las fases del motor.",
      style="font-size:1.1rem; color:#888;")
  })
}

shinyApp(ui, server)
