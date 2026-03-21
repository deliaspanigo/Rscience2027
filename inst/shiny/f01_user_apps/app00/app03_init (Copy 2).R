library(shiny)
library(bslib)
library(ggplot2)

# ==============================================================================
# RSCIENCE 2026 v.0.0.1 - LAUNCHPAD TOTAL (ALL FEATURES RESTORED)
# ==============================================================================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  tags$head(
    tags$style(HTML("
      /* --- RESET Y FULL HEIGHT --- */
      body, html {
        margin: 0 !important; padding: 0 !important;
        height: 100vh !important; width: 100vw !important;
        overflow: hidden; background: #fff; font-family: 'Inter', sans-serif;
      }

      #main_ui_manager {
        height: 100vh !important; width: 100vw !important;
        display: flex !important; flex-direction: column !important;
        position: absolute; top: 0; left: 0;
      }

      .container-fluid { padding: 0 !important; margin: 0 !important; }

      /* --- PANEL IZQUIERDO --- */
      .main-body { display: flex; height: 75vh; width: 100vw; overflow: hidden; }

      .left-panel {
        flex: 0 0 40%; padding: 40px; border-right: 2px solid #00d4ff;
        display: flex; flex-direction: column; align-items: center; justify-content: center;
        background: #ffffff;
      }

      .floating-logo {
        width: 130px; margin-bottom: 1rem;
        animation: floatVertical 4s ease-in-out infinite;
        filter: drop-shadow(0 10px 15px rgba(0,212,255,0.25));
      }
      @keyframes floatVertical { 0%, 100% { transform: translateY(0px); } 50% { transform: translateY(-12px); } }

      .btn-rscience {
        background: #00d4ff; border: none; color: white; font-weight: 800;
        padding: 14px; border-radius: 12px; transition: 0.3s;
        text-transform: uppercase; letter-spacing: 1px;
      }
      .btn-rscience:hover { transform: translateY(-3px); box-shadow: 0 10px 20px rgba(0,212,255,0.4); background: #00b8e6; color: white; }
      .btn-launch-main { width: 320px; margin-bottom: 20px; }

      .utility-container { display: flex; gap: 8px; width: 320px; justify-content: center; margin-bottom: 15px; }
      .btn-utility {
        background: #f8f9fa; border: 1px solid #e9ecef; color: #6c757d; font-weight: 700;
        padding: 8px 5px; border-radius: 8px; font-size: 0.6rem; text-transform: uppercase;
        transition: 0.2s; flex: 1;
      }
      .btn-utility:hover { background: #fff; color: #00d4ff; border-color: #00d4ff; }

      .social-bar { display: flex; gap: 20px; margin-top: 5px; }
      .social-icon { color: #dee2e6; font-size: 1.3rem; transition: 0.3s; text-decoration: none; }
      .social-icon:hover { color: #00d4ff; transform: scale(1.1); }

      /* --- PANEL DERECHO Y CARRUSEL --- */
      .right-panel {
        flex: 0 0 60%; padding: 40px 60px; display: flex; flex-direction: column;
        justify-content: center; align-items: center; background: #fafafa;
      }

      .toolbar-right { display: flex; gap: 10px; margin-bottom: 30px; width: 100%; justify-content: center; }
      .btn-category { padding: 10px 20px; font-size: 0.8rem; width: auto; min-width: 120px; }

      .carousel-viewport {
        height: 280px; width: 100%; max-width: 600px;
        background: linear-gradient(135deg, #00d4ff 0%, #007bff 100%);
        border-radius: 25px; overflow: hidden; position: relative;
        box-shadow: 0 15px 30px rgba(0,123,255,0.15);
      }
      .carousel-rail { display: flex; width: 300%; height: 100%; transition: transform 0.8s cubic-bezier(0.65, 0, 0.35, 1); }
      .individual-slide { width: 33.33%; height: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center; color: white; padding: 40px; text-align: center; }

      .dots-nav { display: flex; gap: 10px; margin-top: 25px; }
      .dot-btn { width: 10px; height: 10px; border-radius: 20px; border: none; background: #ddd; transition: 0.4s; cursor: pointer; }
      .dot-btn.active { background: #00d4ff; width: 30px; }

      /* --- FOOTER LOGOS (MARQUEE) --- */
      .footer-logos { height: 25vh; border-top: 2px solid #00d4ff; background: white; display: flex; flex-direction: column; justify-content: center; overflow: hidden; }
      .marquee-row { display: flex; align-items: center; width: 100%; height: 40px; margin: 4px 0; }
      .category-label {
        flex: 0 0 130px; font-weight: 800; font-size: 0.6rem; text-transform: uppercase; color: #adb5bd;
        border-right: 2px solid #00d4ff; text-align: right; padding-right: 15px; background: white; z-index: 10;
      }
      .marquee-container { flex: 1; overflow: hidden; display: flex; }
      .marquee-content { display: flex; width: max-content; animation: scrollLeft linear infinite; }
      .marquee-content img { height: 30px; margin: 0 40px; opacity: 0.4; filter: grayscale(1); }

      .scroll-slow { animation-duration: 45s; }
      .scroll-fast { animation-duration: 30s; }
      @keyframes scrollLeft { from { transform: translateX(0); } to { transform: translateX(-50%); } }
    "))
  ),

  uiOutput("main_ui_manager")
)

server <- function(input, output, session) {

  current_page <- reactiveVal("launchpad")
  observeEvent(input$btn_launch_main, { current_page("engine") })
  observeEvent(input$btn_engine_sec, { current_page("engine") })
  observeEvent(input[["engine_v1-btn_go_home"]], { current_page("launchpad") })

  mod_rscience_server("engine_v1")

  output$main_ui_manager <- renderUI({
    if (current_page() == "launchpad") {
      tagList(
        div(class = "main-body",
            div(class = "left-panel",
                img(src = "Rscience_logo_sticker.png", class = "floating-logo"),
                div(class = "text-center", style = "margin-top: -15px; margin-bottom: 25px;",
                    span("v.0.0.1", class = "badge bg-dark", style = "font-family: monospace;")),
                #h2("RScience 2026", style="font-weight:900; color:#2c3e50; margin-bottom:20px;"),
                actionButton("btn_launch_main", "Launch Engine", class = "btn-rscience btn-launch-main"),
                div(class = "utility-container",
                    actionButton("btn_cite", "Cite", class="btn-utility"),
                    actionButton("btn_contact", "Contact", class="btn-utility"),
                    actionButton("btn_info", "Info", class="btn-utility"),
                    actionButton("btn_who", "Team", class="btn-utility")
                ),
                div(class = "social-bar",
                    tags$a(href = "https://github.com", target = "_blank", class = "social-icon", icon("github")),
                    tags$a(href = "https://linkedin.com", target = "_blank", class = "social-icon", icon("linkedin"))
                )
            ),
            div(class = "content-panel right-panel",
                # BOTONES SUPERIORES RESTAURADOS
                div(class = "toolbar-right",
                    actionButton("btn_engine_sec", "Engine", class = "btn-rscience btn-category"),
                    actionButton("btn_dist", "Distributions", class = "btn-rscience btn-category"),
                    actionButton("btn_class", "ClassRoom", class = "btn-rscience btn-category"),
                    actionButton("btn_extra", "Extra", class = "btn-rscience btn-category")
                ),
                div(class = "carousel-viewport", uiOutput("carousel_rail_ui")),
                uiOutput("dots_ui")
            )
        ),
        # FOOTER CON LAS DOS FILAS DE LOGOS
        div(class = "footer-logos",
            div(class = "marquee-row",
                div(class = "category-label", "Institutions"),
                div(class = "marquee-container", uiOutput("ui_institutions"))
            ),
            div(class = "marquee-row",
                div(class = "category-label", "Universities"),
                div(class = "marquee-container", uiOutput("ui_universities"))
            )
        )
      )
    } else {
      mod_rscience_ui("engine_v1")
    }
  })

  # --- CARRUSEL ---
  current_slide <- reactiveVal(1)
  auto_timer <- reactiveTimer(5000)
  observe({ auto_timer(); isolate({ current_slide(if(current_slide() == 3) 1 else current_slide() + 1) }) })

  output$carousel_rail_ui <- renderUI({
    offset <- (current_slide() - 1) * -33.33
    div(class = "carousel-rail", style = sprintf("transform: translateX(%f%%);", offset),
        div(class = "individual-slide", icon("code", "fa-3x mb-3"), h4("Open Ecosystem"), p("Integración con R y Shiny.")),
        div(class = "individual-slide", icon("bolt", "fa-3x mb-3"), h4("Smart Engine"), p("Automatización de flujos.")),
        div(class = "individual-slide", icon("shield-halved", "fa-3x mb-3"), h4("Scientific Rigor"), p("Validación de métodos."))
    )
  })

  output$dots_ui <- renderUI({
    div(class = "dots-nav",
        lapply(1:3, function(i) {
          is_active <- if(i == current_slide()) "active" else ""
          tags$button(class = paste("dot-btn", is_active), onclick = sprintf("Shiny.setInputValue('go_to_slide', %d)", i))
        })
    )
  })
  observeEvent(input$go_to_slide, { current_slide(input$go_to_slide) })

  # --- LOGOS FILA 1 ---
  output$ui_institutions <- renderUI({
    files <- list.files("www/f01_institutions")
    if(length(files) == 0) return(NULL)
    div(class = "marquee-content scroll-slow", lapply(c(files, files), function(f) img(src = paste0("f01_institutions/", f))))
  })

  # --- LOGOS FILA 2 ---
  output$ui_universities <- renderUI({
    files <- list.files("www/f02_universities")
    if(length(files) == 0) return(NULL)
    div(class = "marquee-content scroll-fast", lapply(c(files, files), function(f) img(src = paste0("f02_universities/", f))))
  })

  observeEvent(input$btn_cite, { showModal(modalDialog(title = "Cite Rscience", p("Rscience Team (2026). v.0.0.1."), easyClose = TRUE)) })
}

shinyApp(ui, server)
