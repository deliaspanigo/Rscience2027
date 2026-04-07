library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: GIANT GLASS CARDS & COLLAPSIBLE SIDEBAR
# ==============================================================================
css_rscience_giant <- "
body { background-color: #0b0f13; color: white; font-family: 'Inter', 'Segoe UI', sans-serif; }

/* AJUSTES DEL SIDEBAR DE BSLIB */
.bslib-sidebar-content {
  background-color: #0f171e !important;
  padding: 0 !important;
  overflow: hidden !important;
  border-right: 2px solid rgba(0, 212, 255, 0.1);
}

/* NAVEGACIÓN SUPERIOR (Círculos) */
.nav-header {
  display: flex; justify-content: center; align-items: center; gap: 25px;
  padding: 30px 0; background: #16222c; border-bottom: 1px solid rgba(0, 212, 255, 0.1);
}
.nav-dot-wrapper { display: flex; flex-direction: column; align-items: center; cursor: pointer; }
.dot { width: 16px; height: 16px; background: #333; border-radius: 50%; margin-bottom: 10px; transition: 0.4s; }
.nav-dot-wrapper.active .dot { background: #00d4ff; box-shadow: 0 0 20px #00d4ff; transform: scale(1.3); }
.dot-label { font-size: 0.8rem; font-weight: 800; color: #555; text-transform: uppercase; }
.active .dot-label { color: #00d4ff; }

/* VIEWPORT Y TRACK */
.sidebar-viewport { width: 100%; overflow: hidden; height: 100%; }
.slider-track { display: flex; width: 300%; transition: transform 0.8s cubic-bezier(0.85, 0, 0.15, 1); }
.pack-group { width: 33.333%; padding: 30px 25px; }

/* --- GLASS PHASE CARDS (VERSION GIANT) --- */
.phase-card {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid rgba(255, 255, 255, 0.05);
  border-left: 8px solid #444;
  border-radius: 18px;
  padding: 25px;
  margin-bottom: 20px;
  display: flex; align-items: center;
  position: relative; overflow: hidden;
  transition: all 0.4s cubic-bezier(0.165, 0.84, 0.44, 1);
  cursor: pointer;
}

.phase-card.active {
  background: #00d4ff !important;
  border-left-color: #00d4ff !important;
  color: #0b0f13 !important;
  transform: scale(1.03);
  box-shadow: 0 15px 40px rgba(0, 212, 255, 0.4);
}

.phase-icon { font-size: 2.8rem; margin-right: 25px; width: 55px; text-align: center; color: #00d4ff; }
.active .phase-icon { color: #0b0f13 !important; }

.phase-title { font-weight: 900; font-size: 1.5rem; display: block; line-height: 1.1; }
.phase-subtitle { font-size: 0.85rem; text-transform: uppercase; opacity: 0.7; letter-spacing: 2px; font-weight: 700; }

/* PROCESSING BOX */
.processing-container {
  margin-top: 15px; padding: 25px; border-radius: 15px;
  background: rgba(0, 212, 255, 0.05); border: 2px dashed rgba(0, 212, 255, 0.3);
  text-align: center;
}
.btn-play-giant {
  background: #00d4ff; color: #0b0f13 !important; font-weight: 900;
  border: none; width: 100%; padding: 20px; border-radius: 12px;
  font-size: 1.4rem; letter-spacing: 3px; transition: 0.3s;
}
.btn-play-giant:hover { background: #fff; transform: scale(1.02); }

/* TITULOS DE AREA DE TRABAJO */
.work-title { font-size: 3.5rem; font-weight: 900; color: #00d4ff; letter-spacing: -2px; }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  title = span("RSCIENCE v.0.0.1", style="font-weight:900; color:#00d4ff; letter-spacing:2px;"),
  theme = bs_theme(version = 5, bg = "#0b0f13", fg = "#fff", primary = "#00d4ff"),
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_giant))),

  # SIDEBAR CONFIGURATION
  sidebar = sidebar(
    width = 450, # Sidebar más ancho para las letras grandes
    open = TRUE,
    id = "main_sidebar",

    # Cabecera de navegación (Dots)
    div(class="nav-header",
        div(id="nav_p1", class="nav-dot-wrapper active", div(class="dot"), div(class="dot-label", "Setup")),
        div(id="nav_p2", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Docs")),
        div(id="nav_p3", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Out"))
    ),

    # El slider de opciones
    div(class="sidebar-viewport",
        div(id="master_track", class="slider-track",

            # --- PACK 01 ---
            div(class="pack-group",
                div(id="c_dataset",  class="phase-card active", div(class="phase-icon", icon("database")), div(class="phase-content", span("STEP 01", class="phase-subtitle"), span("Dataset", class="phase-title"))),
                div(id="c_tool",     class="phase-card", div(class="phase-icon", icon("screwdriver-wrench")), div(class="phase-content", span("STEP 02", class="phase-subtitle"), span("Tools", class="phase-title"))),
                div(id="c_settings", class="phase-card", div(class="phase-icon", icon("sliders")), div(class="phase-content", span("STEP 03", class="phase-subtitle"), span("Settings", class="phase-title"))),
                div(id="c_process",  class="phase-card", div(class="phase-icon", icon("microchip")), div(class="phase-content", span("STEP 04", class="phase-subtitle"), span("Processing", class="phase-title"))),

                conditionalPanel(
                  condition = "input.card_click == 'c_process'",
                  div(class="processing-container",
                      actionButton("run_engine", "RUN ENGINE", icon = icon("play-circle"), class="btn-play-giant")
                  )
                )
            ),

            # --- PACK 02 ---
            div(class="pack-group",
                div(id="c_theory", class="phase-card", div(class="phase-icon", icon("book")), div(class="phase-content", span("INFO", class="phase-subtitle"), span("Theory", class="phase-title"))),
                div(id="c_cite",   class="phase-card", div(class="phase-icon", icon("quote-right")), div(class="phase-content", span("INFO", class="phase-subtitle"), span("Citations", class="phase-title")))
            ),

            # --- PACK 03 ---
            div(class="pack-group",
                div(id="c_shiny",   class="phase-card", div(class="phase-icon", icon("desktop")), div(class="phase-content", span("FINAL", class="phase-subtitle"), span("Visualizer", class="phase-title"))),
                div(id="c_download",class="phase-card", div(class="phase-icon", icon("file-export")), div(class="phase-content", span("FINAL", class="phase-subtitle"), span("Export PDF", class="phase-title")))
            )
        )
    )
  ),

  # MAIN AREA
  div(style="padding: 60px;",
      h1(textOutput("display_title"), class="work-title"),
      hr(style="border-color: #00d4ff; border-width: 3px; width: 100px;"),
      br(),
      uiOutput("dynamic_content")
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # Navegación entre Packs
  move_to_pack <- function(pack_num) {
    shift <- (pack_num - 1) * -33.333
    runjs(sprintf("$('#master_track').css('transform', 'translateX(%f%%)');", shift))
    runjs("$('.nav-dot-wrapper').removeClass('active');")
    runjs(sprintf("$('#nav_p%d').addClass('active');", pack_num))
  }

  observeEvent(input$nav_p1_click, { move_to_pack(1) })
  observeEvent(input$nav_p2_click, { move_to_pack(2) })
  observeEvent(input$nav_p3_click, { move_to_pack(3) })

  runjs("
    $('#nav_p1').click(function() { Shiny.setInputValue('nav_p1_click', true, {priority: 'event'}); });
    $('#nav_p2').click(function() { Shiny.setInputValue('nav_p2_click', true, {priority: 'event'}); });
    $('#nav_p3').click(function() { Shiny.setInputValue('nav_p3_click', true, {priority: 'event'}); });
  ")

  # Selección de tarjetas
  all_cards <- c("c_dataset", "c_tool", "c_settings", "c_process", "c_theory", "c_cite", "c_shiny", "c_download")
  lapply(all_cards, function(id) {
    runjs(sprintf("$('#%s').click(function() { Shiny.setInputValue('card_click', '%s', {priority: 'event'}); });", id, id))
  })

  observeEvent(input$card_click, {
    lapply(all_cards, function(id) removeClass(id, "active"))
    addClass(input$card_click, "active")
  })

  observeEvent(input$run_engine, {
    move_to_pack(3)
    runjs("$('#c_shiny').click();")
  })

  output$display_title <- renderText({
    if(is.null(input$card_click)) return("DATASET")
    toupper(gsub("c_", "", input$card_click))
  })

  output$dynamic_content <- renderUI({
    tagList(
      p("Este es el espacio de trabajo principal. Al ser un sidebar colapsable, puedes ocultar el menú de la izquierda para enfocarte en estos datos.", style="font-size:1.4rem; color:#888;")
    )
  })
}

shinyApp(ui, server)
