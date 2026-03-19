library(shiny)
library(bslib)
library(ggplot2)

# ==============================================================================
# RSCIENCE 2026 - LAUNCHPAD TOTAL (RESTORED + NO LAYOUT SHIFT)
# ==============================================================================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  tags$head(
    tags$style(HTML("
      /* --- RESET TOTAL --- */
      body, html {
        margin: 0 !important; padding: 0 !important;
        height: 100vh !important; width: 100vw !important;
        overflow: hidden; background: #fff; font-family: 'Inter', sans-serif;
      }

      .container-fluid { padding: 0 !important; margin: 0 !important; max-width: 100% !important; }

      /* --- ESTILOS DEL LAUNCHPAD --- */
      .main-body { display: flex; height: 75vh; width: 100vw; overflow: hidden; }

      .left-panel {
        flex: 0 0 40%; padding: 40px; border-right: 2px solid #00d4ff;
        display: flex; flex-direction: column; align-items: center; justify-content: center;
        background: #ffffff;
      }

      .floating-logo {
        width: 145px; margin-bottom: 1.5rem;
        animation: floatVertical 4s ease-in-out infinite;
        filter: drop-shadow(0 10px 15px rgba(0,212,255,0.25));
      }
      @keyframes floatVertical { 0%, 100% { transform: translateY(0px); } 50% { transform: translateY(-12px); } }

      /* Botones Principales */
      .btn-rscience {
        background: #00d4ff; border: none; color: white; font-weight: 800;
        padding: 14px; border-radius: 12px; transition: 0.3s;
        text-transform: uppercase; letter-spacing: 1px;
      }
      .btn-rscience:hover { transform: translateY(-3px); box-shadow: 0 10px 20px rgba(0,212,255,0.3); background: #00b8e6; color: white; }
      .btn-launch-main { width: 340px; margin-bottom: 25px; font-size: 1.1rem; }

      /* Botones de Utilidad Inferiores */
      .btn-utility {
        background: white; color: #7f8c8d; border: 1px solid #dee2e6;
        font-weight: 700; font-size: 0.65rem; text-transform: uppercase;
        padding: 8px 15px; border-radius: 8px; transition: 0.2s;
      }
      .btn-utility:hover { background: #f8f9fa; color: #00d4ff; border-color: #00d4ff; }

      /* Panel Derecho y Carrusel */
      .right-panel {
        flex: 0 0 60%; padding: 40px 80px; display: flex; flex-direction: column;
        justify-content: center; align-items: center; background: #fafafa; position: relative;
      }

      .toolbar-right { position: absolute; top: 40px; right: 40px; display: flex; gap: 10px; }

      .carousel-viewport {
        height: 320px; width: 100%; max-width: 650px;
        background: linear-gradient(135deg, #00d4ff 0%, #007bff 100%);
        border-radius: 30px; overflow: hidden; position: relative;
        box-shadow: 0 20px 40px rgba(0,123,255,0.15);
      }
      .carousel-rail { display: flex; width: 300%; height: 100%; transition: transform 0.8s cubic-bezier(0.65, 0, 0.35, 1); }
      .individual-slide { width: 33.33%; height: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center; color: white; padding: 40px; text-align: center; }

      /* --- FOOTER LOGOS MARQUEE --- */
      .footer-logos { height: 25vh; border-top: 2px solid #00d4ff; background: white; display: flex; flex-direction: column; justify-content: center; }
      .marquee-row { display: flex; align-items: center; width: 100%; height: 45px; margin: 5px 0; }
      .category-label {
        flex: 0 0 140px; font-weight: 800; font-size: 0.6rem; text-transform: uppercase; color: #adb5bd;
        border-right: 2px solid #00d4ff; text-align: right; padding-right: 20px; background: white; z-index: 10;
        letter-spacing: 1px;
      }
      .marquee-container { flex: 1; overflow: hidden; display: flex; }
      .marquee-content { display: flex; width: max-content; animation: scrollLeft linear infinite; }
      .marquee-content img { height: 35px; margin: 0 45px; opacity: 0.5; filter: grayscale(1); transition: 0.3s; }
      .marquee-content img:hover { opacity: 1; filter: grayscale(0); }
      @keyframes scrollLeft { from { transform: translateX(0); } to { transform: translateX(-50%); } }
    "))
  ),

  uiOutput("main_ui_manager")
)

server <- function(input, output, session) {

  current_page <- reactiveVal("launchpad")

  # Navegación
  observeEvent(input$btn_launch_main, { current_page("engine") })
  observeEvent(input$btn_engine_sec, { current_page("engine") })
  observeEvent(input[["engine_v1-btn_go_home"]], { current_page("launchpad") })

  # Inicializar el módulo RScience
  mod_rscience_server("engine_v1")

  output$main_ui_manager <- renderUI({
    if (current_page() == "launchpad") {
      # --- VOLVEMOS AL LAUNCHPAD COMPLETO ---
      tagList(
        div(class = "main-body",
            # Panel Izquierdo: Branding y Acceso
            div(class = "left-panel",
                img(src = "Rscience_logo_sticker.png", class = "floating-logo"),
                div(class = "text-center", style = "margin-top: -15px; margin-bottom: 30px;",
                    span("v.0.0.1", class = "badge bg-dark", style="font-family:monospace; padding: 5px 12px;")),

                actionButton("btn_launch_main", "Launch RScience Engine", class = "btn-rscience btn-launch-main"),

                # Fila de Botones de Utilidad (Cite, Contact, Info, etc.)
                div(style="display:flex; gap:10px; justify-content:center; width:340px;",
                    actionButton("btn_cite", icon=icon("quote-left"), "Cite", class="btn-utility"),
                    actionButton("btn_contact", icon=icon("envelope"), "Contact", class="btn-utility"),
                    actionButton("btn_info", icon=icon("info-circle"), "About", class="btn-utility")
                )
            ),

            # Panel Derecho: Carrusel y Engine Secundario
            div(class = "right-panel",
                div(class = "toolbar-right",
                    actionButton("btn_settings", icon=icon("cog"), "", class="btn-utility"),
                    actionButton("btn_engine_sec", "Engine v.0.0.1", class = "btn-rscience", style="padding:8px 20px; font-size:0.75rem;")
                ),
                div(class = "carousel-viewport", uiOutput("carousel_rail_ui"))
            )
        ),

        # Footer con Categorías de Logos (Institutions, Partners, etc.)
        div(class = "footer-logos",
            div(class = "marquee-row",
                div(class = "category-label", "Institutions"),
                div(class = "marquee-container", uiOutput("ui_institutions"))
            ),
            div(class = "marquee-row",
                div(class = "category-label", "Partners"),
                div(class = "marquee-container", uiOutput("ui_partners"))
            )
        )
      )
    } else {
      # --- MÓDULO BLINDADO (SIN SALTOS DE ALTO) ---
      div(
        style = "display: grid; grid-template-rows: 100vh; grid-template-columns: 100vw;
                 height: 100vh; width: 100vw; overflow: hidden; position: fixed;
                 top: 0; left: 0; z-index: 9999; background: white;",
        mod_rscience_ui("engine_v1")
      )
    }
  })

  # --- Lógica de Carrusel ---
  output$carousel_rail_ui <- renderUI({
    div(class = "carousel-rail",
        div(class = "individual-slide", icon("microscope", "fa-4x mb-3"), h3("Advanced Analysis"), p("Professional statistical tools at your fingertips.")),
        div(class = "individual-slide", icon("database", "fa-4x mb-3"), h3("Data Integrity"), p("Secure and reproducible workflows.")),
        div(class = "individual-slide", icon("share-nodes", "fa-4x mb-3"), h3("Collaboration"), p("Export and share your findings instantly."))
    )
  })

  # --- Lógica de Logos (Instituciones) ---
  output$ui_institutions <- renderUI({
    files <- list.files("www/f01_institutions")
    if(length(files) == 0) return(div(style="padding-left:20px; color:#ccc; font-size:0.7rem;", "No institutional logos found."))
    div(class = "marquee-content", style="animation: scrollLeft 40s linear infinite;",
        lapply(c(files, files), function(f) img(src = paste0("f01_institutions/", f))))
  })

  # --- Lógica de Logos (Partners) ---
  output$ui_partners <- renderUI({
    files <- list.files("www/f02_partners")
    if(length(files) == 0) return(div(style="padding-left:20px; color:#ccc; font-size:0.7rem;", "No partner logos found."))
    div(class = "marquee-content", style="animation: scrollLeft 50s linear infinite;",
        lapply(c(files, files), function(f) img(src = paste0("f02_partners/", f))))
  })
}

shinyApp(ui, server)
