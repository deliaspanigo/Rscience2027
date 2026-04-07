library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: INTERACTIVE NAVIGATION & PROCESSING PHASE
# ==============================================================================
css_rscience_final <- "
body { background-color: #0b0f13; color: white; overflow-x: hidden; }

/* BARRA DE NAVEGACIÓN SUPERIOR (Círculos) */
.nav-header {
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 20px;
  padding: 20px 0;
  background: #0f171e;
  border-bottom: 1px solid rgba(0, 212, 255, 0.1);
}

.nav-dot-wrapper {
  display: flex;
  flex-direction: column;
  align-items: center;
  cursor: pointer;
  transition: 0.3s;
}

.dot {
  width: 12px;
  height: 12px;
  background: #333;
  border-radius: 50%;
  margin-bottom: 5px;
  transition: all 0.4s ease;
}

.nav-dot-wrapper.active .dot {
  background: #00d4ff;
  box-shadow: 0 0 12px #00d4ff;
  transform: scale(1.3);
}

.dot-label {
  font-size: 0.6rem;
  font-weight: 800;
  color: #555;
  text-transform: uppercase;
  letter-spacing: 1px;
}

.active .dot-label { color: #00d4ff; }

/* VIEWPORT DEL SIDEBAR */
.sidebar-viewport {
  width: 100%;
  overflow: hidden;
  background: #0f171e;
  height: calc(100vh - 80px);
  border-right: 1px solid rgba(0, 212, 255, 0.1);
}

.slider-track {
  display: flex;
  width: 300%;
  transition: transform 0.8s cubic-bezier(0.85, 0, 0.15, 1);
}

.pack-group {
  width: 33.333%;
  padding: 20px 15px;
}

/* PHASE CARDS REFINADAS */
.phase-card {
  background: rgba(255, 255, 255, 0.02);
  border: 1px solid rgba(255, 255, 255, 0.05);
  border-left: 4px solid #444;
  border-radius: 8px;
  padding: 12px 15px;
  margin-bottom: 10px;
  display: flex;
  align-items: center;
  transition: 0.3s ease;
  cursor: pointer;
}

.phase-card.active {
  background: rgba(0, 212, 255, 0.08);
  border-left-color: #00d4ff;
}

.phase-card:hover { background: rgba(255, 255, 255, 0.06); }

.phase-icon { font-size: 1.1rem; margin-right: 12px; color: #00d4ff; width: 20px; }
.phase-title { font-weight: 600; font-size: 0.85rem; color: #a0aec0; }
.active .phase-title { color: white; }

/* SECCIÓN PROCESSING & BOTÓN PLAY */
.processing-box {
  background: rgba(0, 212, 255, 0.03);
  border: 1px dashed rgba(0, 212, 255, 0.3);
  padding: 15px;
  border-radius: 10px;
  text-align: center;
}

.btn-run {
  background: #00d4ff;
  color: #0b0f13 !important;
  font-weight: 900;
  border: none;
  width: 100%;
  padding: 12px;
  margin-top: 10px;
  border-radius: 6px;
  box-shadow: 0 4px 15px rgba(0, 212, 255, 0.2);
}

.btn-run:hover { background: #00b8dd; transform: translateY(-2px); }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_final))),

  fluidRow(
    column(3, class = "p-0",
           # Cabecera con Círculos Interactivos
           div(class="nav-header",
               div(id="nav_p1", class="nav-dot-wrapper active", div(class="dot"), div(class="dot-label", "Setup")),
               div(id="nav_p2", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Theory")),
               div(id="nav_p3", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Results"))
           ),

           div(class="sidebar-viewport",
               div(id="master_track", class="slider-track",

                   # --- PACK 01: SETUP & PROCESSING ---
                   div(class="pack-group",
                       div(id="c_dataset",  class="phase-card active", icon("database"), span("Dataset Selection", class="phase-title")),
                       div(id="c_tool",     class="phase-card", icon("screwdriver-wrench"), span("Tool Engine", class="phase-title")),
                       div(id="c_script",   class="phase-card", icon("code"), span("Script Engine", class="phase-title")),
                       div(id="c_settings", class="phase-card", icon("sliders"), span("General Settings", class="phase-title")),

                       hr(style="border-top: 1px solid rgba(0,212,255,0.1)"),

                       div(id="c_process", class="phase-card", icon("gears"), span("Processing Stage", class="phase-title")),
                       # El botón solo aparece/es relevante cuando estamos en la opción Processing
                       conditionalPanel(
                         condition = "input.card_click == 'c_process'",
                         div(class="processing-box",
                             p("Ready to compute?", style="font-size:0.7rem; color:#00d4ff;"),
                             actionButton("run_engine", "RUN ANALYSIS", icon = icon("play"), class="btn-run")
                         )
                       )
                   ),

                   # --- PACK 02: RESOURCES ---
                   div(class="pack-group",
                       div(id="c_theory", class="phase-card", icon("book"), span("Theory", class="phase-title")),
                       div(id="c_biblio", class="phase-card", icon("list-ol"), span("Bibliography", class="phase-title")),
                       div(id="c_cite",   class="phase-card", icon("quote-left"), span("Cite!", class="phase-title"))
                   ),

                   # --- PACK 03: OUTPUTS ---
                   div(class="pack-group",
                       div(id="c_shiny",   class="phase-card", icon("desktop"), span("Shiny Outputs", class="phase-title")),
                       div(id="c_asa",     class="phase-card", icon("robot"), span("ASA Assistant", class="phase-title")),
                       div(id="c_download",class="phase-card", icon("download"), span("Download Report", class="phase-title"))
                   )
               )
           )
    ),

    column(9,
           card(
             card_header("Rscience Workstation v.0.0.1"),
             div(style="height: 600px; padding: 40px;",
                 h1(textOutput("display_title"), style="color: #00d4ff;"),
                 hr(),
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

  # 1. Función para mover el slider y actualizar círculos
  move_to_pack <- function(pack_num) {
    shift <- (pack_num - 1) * -33.333
    runjs(sprintf("$('#master_track').css('transform', 'translateX(%f%%)');", shift))

    # Actualizar UI de círculos
    runjs("$('.nav-dot-wrapper').removeClass('active');")
    runjs(sprintf("$('#nav_p%d').addClass('active');", pack_num))
  }

  # Eventos de los círculos
  observeEvent(input$nav_p1_click, { move_to_pack(1) })
  observeEvent(input$nav_p2_click, { move_to_pack(2) })
  observeEvent(input$nav_p3_click, { move_to_pack(3) })

  # Inyectar clicks para los círculos
  runjs("
    $('#nav_p1').click(function() { Shiny.setInputValue('nav_p1_click', true, {priority: 'event'}); });
    $('#nav_p2').click(function() { Shiny.setInputValue('nav_p2_click', true, {priority: 'event'}); });
    $('#nav_p3').click(function() { Shiny.setInputValue('nav_p3_click', true, {priority: 'event'}); });
  ")

  # 2. Manejo de Selección de Tarjetas
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

  # 3. Al dar clic en RUN ENGINE, saltamos automáticamente a los resultados
  observeEvent(input$run_engine, {
    # Aquí iría tu procesamiento real
    move_to_pack(3)
    # Seleccionamos automáticamente la primera tarjeta del pack 3
    runjs("$('#c_shiny').click();")
  })

  # 4. Outputs de ejemplo
  output$display_title <- renderText({
    if(is.null(input$card_click)) return("Dataset Selection")
    gsub("c_", "", input$card_click) |> toupper()
  })

  output$dynamic_content <- renderUI({
    if(is.null(input$card_click) || input$card_click == "c_process") {
      tagList(p("Ready to calculate? Click the button in the sidebar."))
    } else {
      tagList(p("Contenido detallado para la sección:", input$card_click))
    }
  })
}

shinyApp(ui, server)
