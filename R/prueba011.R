library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# CSS: REFINED GLASS CARDS & BALANCED SIDEBAR
# ==============================================================================
css_rscience_balanced <- "
body { background-color: #0b0f13; color: white; font-family: 'Segoe UI', system-ui, sans-serif; }

/* SIDEBAR REFINADO */
.bslib-sidebar-content {
  background-color: #0f171e !important;
  padding: 0 !important;
  border-right: 1px solid rgba(0, 212, 255, 0.15);
}

/* NAVEGACIÓN SUPERIOR (Dots) */
.nav-header {
  display: flex; justify-content: center; align-items: center; gap: 20px;
  padding: 20px 0; background: #16222c; border-bottom: 1px solid rgba(0, 212, 255, 0.1);
}
.dot { width: 10px; height: 10px; background: #333; border-radius: 50%; transition: 0.4s; }
.nav-dot-wrapper.active .dot { background: #00d4ff; box-shadow: 0 0 10px #00d4ff; }
.dot-label { font-size: 0.65rem; font-weight: 700; color: #555; text-transform: uppercase; margin-top: 5px; }
.active .dot-label { color: #00d4ff; }

/* VIEWPORT Y TRACK */
.sidebar-viewport { width: 100%; overflow: hidden; }
.slider-track { display: flex; width: 300%; transition: transform 0.6s cubic-bezier(0.85, 0, 0.15, 1); }
.pack-group { width: 33.333%; padding: 20px 15px; }

/* --- PHASE CARDS (EQUILIBRADAS) --- */
.phase-card {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid rgba(255, 255, 255, 0.05);
  border-left: 4px solid #444;
  border-radius: 12px;
  padding: 15px 18px;
  margin-bottom: 12px;
  display: flex; align-items: center;
  transition: all 0.3s ease;
  cursor: pointer;
}

.phase-card.active {
  background: rgba(0, 212, 255, 0.1) !important;
  border-left-color: #00d4ff !important;
  box-shadow: 0 5px 15px rgba(0, 212, 255, 0.1);
}

.phase-icon { font-size: 1.4rem; margin-right: 15px; width: 30px; text-align: center; color: #00d4ff; }
.phase-title { font-weight: 600; font-size: 1rem; color: #e0e0e0; display: block; }
.phase-subtitle { font-size: 0.6rem; text-transform: uppercase; opacity: 0.5; letter-spacing: 1px; }

/* PROCESSING BOX */
.processing-container {
  margin-top: 10px; padding: 15px; border-radius: 10px;
  background: rgba(0, 212, 255, 0.03); border: 1px dashed rgba(0, 212, 255, 0.2);
  text-align: center;
}
.btn-run-refined {
  background: #00d4ff; color: #0b0f13 !important; font-weight: 800;
  border: none; width: 100%; padding: 10px; border-radius: 8px;
  font-size: 0.9rem; letter-spacing: 1px;
}

/* MAIN AREA */
.work-title { font-size: 2.2rem; font-weight: 800; color: #00d4ff; }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  title = span("RSCIENCE", style="font-weight:800; color:#00d4ff; letter-spacing:1px;"),
  theme = bs_theme(version = 5, bg = "#0b0f13", fg = "#fff", primary = "#00d4ff"),
  useShinyjs(),
  tags$head(tags$style(HTML(css_rscience_balanced))),

  sidebar = sidebar(
    width = 320, # Ancho equilibrado
    open = TRUE,
    id = "main_sidebar",

    # Navegación compacta
    div(class="nav-header",
        div(id="nav_p1", class="nav-dot-wrapper active", div(class="dot"), div(class="dot-label", "Setup")),
        div(id="nav_p2", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Theory")),
        div(id="nav_p3", class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Out"))
    ),

    div(class="sidebar-viewport",
        div(id="master_track", class="slider-track",

            # PACK 01
            div(class="pack-group",
                div(id="c_dataset",  class="phase-card active", div(class="phase-icon", icon("database")), div(class="phase-content", span("STEP 01", class="phase-subtitle"), span("Dataset", class="phase-title"))),
                div(id="c_tool",     class="phase-card", div(class="phase-icon", icon("screwdriver-wrench")), div(class="phase-content", span("STEP 02", class="phase-subtitle"), span("Tools", class="phase-title"))),
                div(id="c_settings", class="phase-card", div(class="phase-icon", icon("sliders")), div(class="phase-content", span("STEP 03", class="phase-subtitle"), span("Settings", class="phase-title"))),
                div(id="c_process",  class="phase-card", div(class="phase-icon", icon("microchip")), div(class="phase-content", span("STEP 04", class="phase-subtitle"), span("Processing", class="phase-title"))),

                conditionalPanel(
                  condition = "input.card_click == 'c_process'",
                  div(class="processing-container",
                      actionButton("run_engine", "RUN ENGINE", icon = icon("play"), class="btn-run-refined")
                  )
                )
            ),

            # PACK 02
            div(class="pack-group",
                div(id="c_theory", class="phase-card", div(class="phase-icon", icon("book")), div(class="phase-content", span("INFO", class="phase-subtitle"), span("Theory", class="phase-title"))),
                div(id="c_cite",   class="phase-card", div(class="phase-icon", icon("quote-right")), div(class="phase-content", span("INFO", class="phase-subtitle"), span("Citations", class="phase-title")))
            ),

            # PACK 03
            div(class="pack-group",
                div(id="c_shiny",   class="phase-card", div(class="phase-icon", icon("desktop")), div(class="phase-content", span("FINAL", class="phase-subtitle"), span("Visualizer", class="phase-title"))),
                div(id="c_download",class="phase-card", div(class="phase-icon", icon("file-export")), div(class="phase-content", span("FINAL", class="phase-subtitle"), span("Export PDF", class="phase-title")))
            )
        )
    )
  ),

  # ÁREA PRINCIPAL REFINADA
  div(style="padding: 40px;",
      h2(textOutput("display_title"), class="work-title"),
      hr(style="opacity:0.2;"),
      uiOutput("dynamic_content")
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  move_to_pack <- function(pack_num) {
    shift <- (pack_num - 1) * -33.333
    runjs(sprintf("$('#master_track').css('transform', 'translateX(%f%%)');", shift))
    runjs("$('.nav-dot-wrapper').removeClass('active');")
    runjs(sprintf("$('#nav_p%d').addClass('active');", pack_num))
  }

  runjs("
    $('#nav_p1').click(function() { Shiny.setInputValue('nav_p1_click', true, {priority: 'event'}); });
    $('#nav_p2').click(function() { Shiny.setInputValue('nav_p2_click', true, {priority: 'event'}); });
    $('#nav_p3').click(function() { Shiny.setInputValue('nav_p3_click', true, {priority: 'event'}); });
  ")

  observeEvent(input$nav_p1_click, { move_to_pack(1) })
  observeEvent(input$nav_p2_click, { move_to_pack(2) })
  observeEvent(input$nav_p3_click, { move_to_pack(3) })

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
    if(is.null(input$card_click)) return("DATASET SELECTION")
    toupper(gsub("c_", "", input$card_click))
  })

  output$dynamic_content <- renderUI({
    p("Interfaz de trabajo lista. El menú lateral es ahora más compacto y profesional.", style="font-size:1.1rem; color:#aaa;")
  })
}

shinyApp(ui, server)
