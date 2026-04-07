library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: GLASS PHASE CARDS + TRIPLE SLIDER + INTERACTIVE DOTS
# ==============================================================================
css_rscience_glass_slider <- "
body { background-color: #0b0f13; color: white; overflow-x: hidden; font-family: 'Segoe UI', sans-serif; }

/* NAVEGACIÓN SUPERIOR */
.nav-header {
  display: flex; justify-content: center; align-items: center; gap: 30px;
  padding: 25px 0; background: #0f171e; border-bottom: 1px solid rgba(0, 212, 255, 0.1);
}
.nav-dot-wrapper { display: flex; flex-direction: column; align-items: center; cursor: pointer; transition: 0.3s; }
.dot { width: 14px; height: 14px; background: #333; border-radius: 50%; margin-bottom: 8px; transition: 0.4s; }
.nav-dot-wrapper.active .dot { background: #00d4ff; box-shadow: 0 0 15px #00d4ff; transform: scale(1.2); }
.dot-label { font-size: 0.7rem; font-weight: 800; color: #555; text-transform: uppercase; letter-spacing: 1px; }
.active .dot-label { color: #00d4ff; }

/* VIEWPORT Y SLIDER */
.sidebar-viewport {
  width: 100%; overflow: hidden; background: #0f171e;
  height: calc(100vh - 100px); border-right: 1px solid rgba(0, 212, 255, 0.1);
}
.slider-track { display: flex; width: 300%; transition: transform 0.8s cubic-bezier(0.85, 0, 0.15, 1); }
.pack-group { width: 33.333%; padding: 25px 20px; }

/* --- GLASS PHASE CARDS (GRANDES) --- */
.phase-card {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid rgba(255, 255, 255, 0.05);
  border-left: 6px solid #444;
  border-radius: 15px;
  padding: 22px;
  margin-bottom: 18px;
  display: flex; align-items: center;
  position: relative; overflow: hidden;
  transition: all 0.4s cubic-bezier(0.165, 0.84, 0.44, 1);
  cursor: pointer;
}

.phase-card::before {
  content: ''; position: absolute; top: 0; left: 0; width: 100%; height: 100%;
  background: radial-gradient(circle at center, rgba(0, 212, 255, 0.12) 0%, transparent 75%);
  opacity: 0; transition: opacity 0.4s;
}

.phase-card:hover { background: rgba(255, 255, 255, 0.07); transform: translateY(-3px); }
.phase-card:hover::before { opacity: 1; }

/* ESTADO ACTIVO (CYAN) */
.phase-card.active {
  background: #00d4ff !important;
  border-left-color: #00d4ff !important;
  color: #0b0f13 !important;
  transform: scale(1.02);
  box-shadow: 0 10px 30px rgba(0, 212, 255, 0.3);
}

.phase-icon { font-size: 2.2rem; margin-right: 20px; width: 45px; text-align: center; color: #00d4ff; transition: 0.3s; }
.active .phase-icon { color: #0b0f13 !important; }

.phase-title { font-weight: 700; font-size: 1.2rem; display: block; }
.phase-subtitle { font-size: 0.75rem; text-transform: uppercase; opacity: 0.6; letter-spacing: 1px; }

/* SECCIÓN PROCESSING */
.processing-container {
  margin-top: 10px; padding: 20px; border-radius: 12px;
  background: rgba(0, 212, 255, 0.05); border: 2px dashed rgba(0, 212, 255, 0.2);
  text-align: center; animation: fadeIn 0.5s;
}
.btn-play-big {
  background: #00d4ff; color: #0b0f13 !important; font-weight: 900;
  border: none; width: 100%; padding: 15px; border-radius: 10px;
  font-size: 1.1rem; letter-spacing: 2px; box-shadow: 0 5px 15px rgba(0,212,255,0.3);
}
@keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_glass_slider))),

  fluidRow(
    column(4, class = "p-0", # Sidebar algo más ancho para lucir las tarjetas
           div(class="nav-header",
               div(id="nav_p1", class="nav-dot-wrapper active", div(class="dot"), div(class="dot-label", "Config")),
               div(id="nav_p2", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Theory")),
               div(id="nav_p3", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Results"))
           ),

           div(class="sidebar-viewport",
               div(id="master_track", class="slider-track",

                   # --- PACK 01: SETUP & PROCESSING ---
                   div(class="pack-group",
                       div(id="c_dataset",  class="phase-card active",
                           div(class="phase-icon", icon("database")),
                           div(class="phase-content", span("PHASE 01", class="phase-subtitle"), span("Dataset Selection", class="phase-title"))
                       ),
                       div(id="c_tool",     class="phase-card",
                           div(class="phase-icon", icon("screwdriver-wrench")),
                           div(class="phase-content", span("PHASE 02", class="phase-subtitle"), span("Tool Engine", class="phase-title"))
                       ),
                       div(id="c_script",   class="phase-card",
                           div(class="phase-icon", icon("code")),
                           div(class="phase-content", span("PHASE 03", class="phase-subtitle"), span("Script Engine", class="phase-title"))
                       ),
                       div(id="c_settings", class="phase-card",
                           div(class="phase-icon", icon("sliders")),
                           div(class="phase-content", span("PHASE 04", class="phase-subtitle"), span("General Settings", class="phase-title"))
                       ),
                       div(id="c_process",  class="phase-card",
                           div(class="phase-icon", icon("gears")),
                           div(class="phase-content", span("PHASE 05", class="phase-subtitle"), span("Processing Unit", class="phase-title"))
                       ),
                       # Condicional para mostrar el botón PLAY
                       conditionalPanel(
                         condition = "input.card_click == 'c_process'",
                         div(class="processing-container",
                             p("SYSTEM READY FOR COMPUTATION", style="color:#00d4ff; font-weight:800; font-size:0.7rem; margin-bottom:15px;"),
                             actionButton("run_engine", "RUN ENGINE", icon = icon("play-circle"), class="btn-play-big")
                         )
                       )
                   ),

                   # --- PACK 02: THEORY ---
                   div(class="pack-group",
                       div(id="c_theory", class="phase-card",
                           div(class="phase-icon", icon("book")),
                           div(class="phase-content", span("RESOURCES", class="phase-subtitle"), span("Mathematical Theory", class="phase-title"))
                       ),
                       div(id="c_biblio", class="phase-card",
                           div(class="phase-icon", icon("list-ol")),
                           div(class="phase-content", span("RESOURCES", class="phase-subtitle"), span("Bibliography", class="phase-title"))
                       ),
                       div(id="c_cite",   class="phase-card",
                           div(class="phase-icon", icon("quote-left")),
                           div(class="phase-content", span("RESOURCES", class="phase-subtitle"), span("Cite Project", class="phase-title"))
                       )
                   ),

                   # --- PACK 03: RESULTS ---
                   div(class="pack-group",
                       div(id="c_shiny",   class="phase-card",
                           div(class="phase-icon", icon("desktop")),
                           div(class="phase-content", span("OUTPUT", class="phase-subtitle"), span("Shiny Interactive", class="phase-title"))
                       ),
                       div(id="c_asa",     class="phase-card",
                           div(class="phase-icon", icon("robot")),
                           div(class="phase-content", span("OUTPUT", class="phase-subtitle"), span("ASA Assistant", class="phase-title"))
                       ),
                       div(id="c_download",class="phase-card",
                           div(class="phase-icon", icon("download")),
                           div(class="phase-content", span("EXPORT", class="phase-subtitle"), span("Download Report", class="phase-title"))
                       )
                   )
               )
           )
    ),

    column(8,
           card(
             card_header(span("Rscience Engine Monitor", style="color:#00d4ff; font-weight:800;")),
             div(style="height: 700px; padding: 50px; background: #080c10; border-radius: 0 0 15px 15px;",
                 h1(textOutput("display_title"), style="color: #00d4ff; font-weight:900; letter-spacing:-1px;"),
                 hr(style="border-color: rgba(0,212,255,0.2);"),
                 uiOutput("dynamic_content")
             )
           )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # --- NAVEGACIÓN ENTRE PACKS ---
  move_to_pack <- function(pack_num) {
    shift <- (pack_num - 1) * -33.333
    runjs(sprintf("$('#master_track').css('transform', 'translateX(%f%%)');", shift))
    runjs("$('.nav-dot-wrapper').removeClass('active');")
    runjs(sprintf("$('#nav_p%d').addClass('active');", pack_num))
  }

  observeEvent(input$nav_p1_click, { move_to_pack(1) })
  observeEvent(input$nav_p2_click, { move_to_pack(2) })
  observeEvent(input$nav_p3_click, { move_to_pack(3) })

  # Inyectar eventos click a los círculos
  runjs("
    $('#nav_p1').click(function() { Shiny.setInputValue('nav_p1_click', true, {priority: 'event'}); });
    $('#nav_p2').click(function() { Shiny.setInputValue('nav_p2_click', true, {priority: 'event'}); });
    $('#nav_p3').click(function() { Shiny.setInputValue('nav_p3_click', true, {priority: 'event'}); });
  ")

  # --- SELECCIÓN DE TARJETAS ---
  all_cards <- c("c_dataset", "c_tool", "c_script", "c_settings", "c_process",
                 "c_theory", "c_biblio", "c_cite",
                 "c_shiny", "c_asa", "c_download")

  lapply(all_cards, function(id) {
    runjs(sprintf("$('#%s').click(function() { Shiny.setInputValue('card_click', '%s', {priority: 'event'}); });", id, id))
  })

  observeEvent(input$card_click, {
    lapply(all_cards, function(id) removeClass(id, "active"))
    addClass(input$card_click, "active")
  })

  # --- BOTÓN PLAY ---
  observeEvent(input$run_engine, {
    # Efecto visual: saltar a Pack 3 después de 'procesar'
    move_to_pack(3)
    runjs("$('#c_shiny').click();") # Abrir automáticamente resultados
  })

  # --- OUTPUTS ---
  output$display_title <- renderText({
    if(is.null(input$card_click)) return("DATASET SELECTION")
    toupper(gsub("c_", "", input$card_click))
  })

  output$dynamic_content <- renderUI({
    id <- input$card_click
    if(is.null(id)) id <- "c_dataset"

    tagList(
      h4(paste("Module:", id), style="color:white;"),
      p("Aquí se cargará la interfaz específica para este módulo de Rscience.", style="color:#888;")
    )
  })
}

shinyApp(ui, server)
