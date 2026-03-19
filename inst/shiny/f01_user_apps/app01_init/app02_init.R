library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),

  tags$head(
    tags$style(HTML("
      body, html { margin: 0; padding: 0; height: 100vh; overflow: hidden; background: #fff; font-family: 'Inter', sans-serif; }
      .main-body { display: flex; height: 75vh; width: 100vw; }

      .left-panel { flex: 0 0 40%; display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 40px; }
      .right-panel { flex: 0 0 60%; display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 0 50px; }

      /* CONTENEDOR DEL CARRUSEL */
      .carousel-container {
        display: flex; flex-direction: column; align-items: center; width: 100%; max-width: 650px;
      }

      /* VENTANA DE VISUALIZACIÓN (Mascara) */
      .carousel-viewport {
        height: 33vh; width: 100%;
        background: linear-gradient(135deg, #00d4ff 0%, #007bff 100%);
        border-radius: 25px;
        box-shadow: 0 15px 35px rgba(0,180,255,0.25);
        position: relative;
        overflow: hidden; /* Importante para el slide */
      }

      /* RIELES DE LAS DIAPOSITIVAS */
      .carousel-rail {
        display: flex;
        width: 300%; /* 3 diapos = 300% */
        height: 100%;
        transition: transform 0.8s cubic-bezier(0.65, 0, 0.35, 1); /* Movimiento suave */
      }

      .individual-slide {
        width: 33.33%; /* Cada una ocupa un tercio del riel (100% de la tarjeta) */
        height: 100%;
        display: flex; flex-direction: column; align-items: center; justify-content: center;
        color: white; text-align: center; padding: 30px;
      }

      /* PUNTOS INTERACTIVOS (DOTS) */
      .dots-nav { display: flex; gap: 12px; margin-top: 15px; }
      .dot-btn {
        width: 10px; height: 10px; border-radius: 50%; border: none;
        background: #e0e0e0; transition: all 0.3s ease; cursor: pointer;
      }
      .dot-btn.active { background: #00d4ff; transform: scale(1.4); }

      /* LOGO Y BOTONES */
      .floating-logo { width: 140px; animation: floatVertical 4s ease-in-out infinite; margin-bottom: 1.5rem; }
      @keyframes floatVertical { 0%, 100% { transform: translateY(0px); } 50% { transform: translateY(-15px); } }
      .btn-main { background: #00d4ff; border: none; color: white; font-weight: 800; padding: 14px; border-radius: 12px; width: 100%; margin-bottom: 15px; }

      /* FOOTER LOGOS */
      .footer-logos { height: 25vh; width: 100vw; display: flex; flex-direction: column; justify-content: center; gap: 8px; border-top: 1px solid #f8f9fa; }
      .marquee-row { display: flex; align-items: center; width: 100%; overflow: hidden; height: 40px; }
      .category-label { flex: 0 0 140px; font-weight: 800; font-size: 0.6rem; text-transform: uppercase; color: #adb5bd; border-right: 2px solid #00d4ff; text-align: right; margin-right: 10px; padding-right: 10px;}
      .marquee-container { overflow: hidden; white-space: nowrap; flex-grow: 1; display: flex; }
      .marquee-content { display: inline-flex; animation: scroll 45s linear infinite; }
      .marquee-content img { height: 28px; margin: 0 35px; filter: grayscale(100%); opacity: 0.4; }
      @keyframes scroll { 0% { transform: translateX(0); } 100% { transform: translateX(-50%); } }
    "))
  ),

  div(class = "main-body",
      div(class = "left-panel text-center",
          img(src = "Rscience_logo_sticker.png", class = "floating-logo"),
          h3("Rscience 2027", style="font-weight:900; color:#2c3e50;"),
          div(style = "width: 280px;",
              actionButton("btn_launch", "LAUNCH ENGINE", class = "btn-main"),
              layout_column_wrap(width = 1/3,
                                 actionButton("btn_cite", "Cite", style="font-size:0.6rem;"),
                                 actionButton("btn_contact", "Contact", style="font-size:0.6rem;"),
                                 actionButton("btn_details", "R Details", style="font-size:0.6rem;")
              )
          )
      ),

      div(class = "right-panel",
          div(class = "carousel-container",
              # Ventana de recorte
              div(class = "carousel-viewport",
                  # Riel que se mueve
                  uiOutput("carousel_rail_ui")
              ),
              # Puntos de navegación
              uiOutput("dots_ui")
          )
      )
  ),

  div(class = "footer-logos",
      div(class = "marquee-row", div(class = "category-label", "Institutions"), div(class = "marquee-container", uiOutput("ui_institutions"))),
      div(class = "marquee-row", div(class = "category-label", "Universities"), div(class = "marquee-container", uiOutput("ui_universities")))
  )
)

server <- function(input, output, session) {

  current_slide <- reactiveVal(1)
  auto_timer <- reactiveTimer(5000)

  # Lógica del temporizador automático
  observe({
    auto_timer()
    isolate({
      new_val <- if(current_slide() == 3) 1 else current_slide() + 1
      current_slide(new_val)
    })
  })

  # Renderizar el riel de diapositivas
  output$carousel_rail_ui <- renderUI({
    # Calculamos el desplazamiento: 0% para diapo 1, -33.33% para diapo 2, -66.66% para diapo 3
    offset <- (current_slide() - 1) * -33.33

    div(class = "carousel-rail",
        style = sprintf("transform: translateX(%f%%);", offset),
        # Diapo 1
        div(class = "individual-slide",
            icon("layer-group", "fa-4x mb-3"),
            h4("Modular Design", style="font-weight:800;"),
            p("Switch datasets seamlessly.")
        ),
        # Diapo 2
        div(class = "individual-slide",
            icon("microchip", "fa-4x mb-3"),
            h4("Statistical Engine", style="font-weight:800;"),
            p("ANOVA & Linear Models.")
        ),
        # Diapo 3
        div(class = "individual-slide",
            icon("file-export", "fa-4x mb-3"),
            h4("Reporting", style="font-weight:800;"),
            p("Publication ready graphics.")
        )
    )
  })

  # Renderizar puntos (Dots)
  output$dots_ui <- renderUI({
    div(class = "dots-nav",
        lapply(1:3, function(i) {
          active_class <- if(i == current_slide()) "active" else ""
          tags$button(
            class = paste("dot-btn", active_class),
            onclick = sprintf("Shiny.setInputValue('go_to_slide', %d, {priority: 'event'})", i)
          )
        })
    )
  })

  observeEvent(input$go_to_slide, { current_slide(input$go_to_slide) })

  # Logos Aleatorios
  output$ui_institutions <- renderUI({
    files <- list.files("www/f01_institutions", full.names = FALSE)
    logos <- c(sample(files), sample(files))
    div(class = "marquee-content", lapply(logos, function(f) img(src = paste0("f01_institutions/", f))))
  })

  output$ui_universities <- renderUI({
    files <- list.files("www/f02_universities", full.names = FALSE)
    logos <- c(sample(files), sample(files))
    div(class = "marquee-content", lapply(logos, function(f) img(src = paste0("f02_universities/", f))))
  })
}

shinyApp(ui, server)
