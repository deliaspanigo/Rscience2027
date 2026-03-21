# ==============================================================================
# MÓDULO LAUNCHPAD - v.0.0.1 (ENCAPSULADO TOTAL + ESTADO UNIFICADO)
# ==============================================================================
library("bslib")
library("shiny")

mod_launchpad_ui <- function(id) {
  ns <- NS(id)
  wrapper_id <- ns("launch_wrapper")

  tagList(
    tags$head(
      tags$style(HTML(paste0("
        #", wrapper_id, " {
           margin: 0 !important; padding: 0 !important;
           height: 100vh !important; width: 100vw !important;
           overflow: hidden; background: #fff;
           font-family: 'Inter', sans-serif;
           display: block;
        }
        #", wrapper_id, " .container-fluid { padding: 0 !important; margin: 0 !important; }
        #", wrapper_id, " .main-body { display: flex; height: 75vh; width: 100vw; overflow: hidden; }
        #", wrapper_id, " .left-panel {
           flex: 0 0 40%; padding: 40px; border-right: 2px solid #00d4ff;
           display: flex; flex-direction: column; align-items: center; justify-content: center;
           background: #ffffff;
        }
        #", wrapper_id, " .floating-logo {
           width: 280px; margin-bottom: 1rem;
           animation: floatVertical 4s ease-in-out infinite;
           filter: drop-shadow(0 10px 15px rgba(0,212,255,0.25));
        }
        @keyframes floatVertical { 0%, 100% { transform: translateY(0px); } 50% { transform: translateY(-12px); } }
        #", wrapper_id, " .btn-rscience {
           background: #00d4ff; border: none; color: white; font-weight: 800;
           padding: 14px; border-radius: 12px; transition: 0.3s;
           text-transform: uppercase; letter-spacing: 1px;
        }
        #", wrapper_id, " .btn-rscience:hover { transform: translateY(-3px); box-shadow: 0 10px 20px rgba(0,212,255,0.4); background: #00b8e6; color: white; }
        #", wrapper_id, " .btn-launch-main { width: 320px; margin-bottom: 20px; }
        #", wrapper_id, " .utility-container { display: flex; gap: 8px; width: 320px; justify-content: center; margin-bottom: 15px; }
        #", wrapper_id, " .btn-utility {
           background: #f8f9fa; border: 1px solid #e9ecef; color: #6c757d; font-weight: 700;
           padding: 8px 5px; border-radius: 8px; font-size: 0.6rem; text-transform: uppercase;
           transition: 0.2s; flex: 1;
        }
        #", wrapper_id, " .btn-utility:hover { background: #fff; color: #00d4ff; border-color: #00d4ff; }
        #", wrapper_id, " .social-bar { display: flex; gap: 20px; margin-top: 5px; }
        #", wrapper_id, " .social-icon { color: #dee2e6; font-size: 1.3rem; transition: 0.3s; text-decoration: none; }
        #", wrapper_id, " .social-icon:hover { color: #00d4ff; transform: scale(1.1); }
        #", wrapper_id, " .right-panel {
           flex: 0 0 60%; padding: 40px 60px; display: flex; flex-direction: column;
           justify-content: center; align-items: center; background: #fafafa;
        }
        #", wrapper_id, " .toolbar-right { display: flex; gap: 10px; margin-bottom: 30px; width: 100%; justify-content: center; }
        #", wrapper_id, " .btn-category { padding: 10px 20px; font-size: 0.8rem; width: auto; min-width: 120px; }
        #", wrapper_id, " .carousel-viewport {
           height: 280px; width: 100%; max-width: 600px;
           background: linear-gradient(135deg, #00d4ff 0%, #007bff 100%);
           border-radius: 25px; overflow: hidden; position: relative;
           box-shadow: 0 15px 30px rgba(0,123,255,0.15);
        }
        #", wrapper_id, " .carousel-rail { display: flex; width: 300%; height: 100%; transition: transform 0.8s cubic-bezier(0.65, 0, 0.35, 1); }
        #", wrapper_id, " .individual-slide { width: 33.33%; height: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center; color: white; padding: 40px; text-align: center; }
        #", wrapper_id, " .dots-nav { display: flex; gap: 10px; margin-top: 25px; }
        #", wrapper_id, " .dot-btn { width: 10px; height: 10px; border-radius: 20px; border: none; background: #ddd; transition: 0.4s; cursor: pointer; }
        #", wrapper_id, " .dot-btn.active { background: #00d4ff; width: 30px; }
        #", wrapper_id, " .footer-logos { height: 25vh; border-top: 2px solid #00d4ff; background: white; display: flex; flex-direction: column; justify-content: center; overflow: hidden; }
        #", wrapper_id, " .marquee-row { display: flex; align-items: center; width: 100%; height: 40px; margin: 4px 0; }
        #", wrapper_id, " .category-label {
           flex: 0 0 130px; font-weight: 800; font-size: 0.6rem; text-transform: uppercase; color: #adb5bd;
           border-right: 2px solid #00d4ff; text-align: right; padding-right: 15px; background: white; z-index: 10;
        }
        #", wrapper_id, " .marquee-container { flex: 1; overflow: hidden; display: flex; }
        #", wrapper_id, " .marquee-content { display: flex; width: max-content; animation: scrollLeft linear infinite; }
        #", wrapper_id, " .marquee-content img { height: 30px; margin: 0 40px; opacity: 0.4; filter: grayscale(1); }
        #", wrapper_id, " .scroll-slow { animation-duration: 45s; }
        #", wrapper_id, " .scroll-fast { animation-duration: 30s; }
        @keyframes scrollLeft { from { transform: translateX(0); } to { transform: translateX(-50%); } }
      ")))
    ),

    div(id = wrapper_id,
        div(class = "main-body",
            div(class = "left-panel",
                img(src = "Rscience_logo_sticker.png", class = "floating-logo"),
                div(class = "text-center", style = "margin-top: -15px; margin-bottom: 25px;",
                    span("v.0.0.1", class = "badge bg-dark", style = "font-family: monospace;")),

                actionButton(ns("btn_enter"), "Launch Engine", class = "btn-rscience btn-launch-main"),

                div(class = "utility-container",
                    actionButton(ns("btn_cite"), "Cite", class="btn-utility"),
                    actionButton(ns("btn_contact"), "Contact", class="btn-utility"),
                    actionButton(ns("btn_info"), "Info", class="btn-utility"),
                    actionButton(ns("btn_who"), "Team", class="btn-utility")
                ),
                div(class = "social-bar",
                    tags$a(href = "https://github.com", target = "_blank", class = "social-icon", icon("github")),
                    tags$a(href = "https://linkedin.com", target = "_blank", class = "social-icon", icon("linkedin"))
                )
            ),
            div(class = "content-panel right-panel",
                div(class = "toolbar-right",
                    actionButton(ns("btn_sec_enter"), "Engine", class = "btn-rscience btn-category"),
                    actionButton(ns("btn_dist"), "Distributions", class = "btn-rscience btn-category"),
                    actionButton(ns("btn_class"), "ClassRoom", class = "btn-rscience btn-category"),
                    actionButton(ns("btn_extra"), "Extra", class = "btn-rscience btn-category")
                ),
                div(class = "carousel-viewport", uiOutput(ns("carousel_rail_ui"))),
                uiOutput(ns("dots_ui"))
            )
        ),
        div(class = "footer-logos",
            div(class = "marquee-row",
                div(class = "category-label", "Institutions"),
                div(class = "marquee-container", uiOutput(ns("ui_institutions")))
            ),
            div(class = "marquee-row",
                div(class = "category-label", "Universities"),
                div(class = "marquee-container", uiOutput(ns("ui_universities")))
            )
        )
    )
  )
}

mod_launchpad_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_path <- system.file("shiny/f01_user_apps/app01_launchpad/www", package = "Rscience2027")

    # --- ESTADOS REACTIVOS ---
    nav_trigger     <- reactiveVal(0)
    target_page     <- reactiveVal("welcome")
    last_click_time <- reactiveVal(NULL)
    current_slide   <- reactiveVal(1)

    # --- FUNCIÓN CENTRAL DE NAVEGACIÓN ---
    fire_navigation <- function(dest) {
      target_page(dest)
      nav_trigger(nav_trigger() + 1)
      last_click_time(Sys.time())

      if (show_debug) {
        message(sprintf("[DEBUG] Trigger: %d | Destino: %s", nav_trigger(), dest))
      }
    }

    # --- EVENTOS ---
    observeEvent(input$btn_enter,     { fire_navigation("engine") })
    observeEvent(input$btn_sec_enter, { fire_navigation("engine") })
    observeEvent(input$btn_dist,      { fire_navigation("distributions") })
    observeEvent(input$btn_class,     { fire_navigation("classroom") })
    observeEvent(input$btn_extra,     { fire_navigation("extra") })

    # --- CARRUSEL ---
    auto_timer <- reactiveTimer(5000)
    observe({
      auto_timer()
      isolate({ current_slide(if(current_slide() == 3) 1 else current_slide() + 1) })
    })
    observeEvent(input$go_to_slide, { current_slide(input$go_to_slide) })

    output$carousel_rail_ui <- renderUI({
      offset <- (current_slide() - 1) * -33.33
      div(class = "carousel-rail", style = sprintf("transform: translateX(%.2f%%);", offset),
          div(class = "individual-slide", icon("code", "fa-3x mb-3"), h4("Open Ecosystem"), p("Integración con R y Shiny.")),
          div(class = "individual-slide", icon("bolt", "fa-3x mb-3"), h4("Smart Engine"), p("Automatización de flujos.")),
          div(class = "individual-slide", icon("shield-halved", "fa-3x mb-3"), h4("Scientific Rigor"), p("Validación de métodos."))
      )
    })

    output$dots_ui <- renderUI({
      div(class = "dots-nav",
          lapply(1:3, function(i) {
            is_active <- if(i == current_slide()) "active" else ""
            tags$button(class = paste("dot-btn", is_active),
                        onclick = sprintf("Shiny.setInputValue('%s', %d)", ns("go_to_slide"), i))
          })
      )
    })

    # --- LOGOS ---
    output$ui_institutions <- renderUI({
      files <- list.files(file.path(base_path, "f01_institutions"))
      if(length(files) == 0) return(NULL)
      div(class = "marquee-content scroll-slow", lapply(c(files, files), function(f) img(src = paste0("f01_institutions/", f))))
    })

    output$ui_universities <- renderUI({
      files <- list.files(file.path(base_path, "f02_universities"))
      if(length(files) == 0) return(NULL)
      div(class = "marquee-content scroll-fast", lapply(c(files, files), function(f) img(src = paste0("f02_universities/", f))))
    })

    # --- MODALES ---
    observeEvent(input$btn_cite, {
      showModal(modalDialog(title = "Cite Rscience", p("Rscience Team (2026). v.0.0.1."), easyClose = TRUE))
    })

    # --- OBJETO UNIFICADO DE ESTADO ---
    # Este objeto será el mismo para el Debug y para el Retorno
    launchpad_status <- reactive({
      list(
        nav_trigger = nav_trigger(),
        target_page = target_page(),
        last_click_time  = last_click_time(),
        current_slide = current_slide()
      )
    })

    # --- DEBUG MONITOR ---
    if (show_debug) {
      insertUI(
        selector = paste0("#", ns("dots_ui")),
        where = "afterEnd",
        ui = div(
          style = "margin-top:20px; padding:12px; background:#fff3cd; border:1px solid #ffeeba; border-radius:8px; font-family:monospace; font-size:0.75rem; color:#856404;",
          h6("Launchpad Debug Monitor", style="margin:0 0 5px 0; font-weight:bold;"),
          verbatimTextOutput(ns("debug_console"))
        )
      )
      # Aquí usamos el objeto unificado
      output$debug_console <- renderPrint({
        launchpad_status()
      })
    }

    # --- RETORNO ---
    # Retornamos directamente la expresión reactiva unificada
    return(launchpad_status)
  })
}
