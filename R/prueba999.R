library(shiny)
library(bslib)
library(shinyjs)





source(file =  system.file("shiny", "fn03_tool_script", "tool_0001_script_002", "f03_soft_opts",
                           "f01_theory", "f03_prod", "mod_special_theory.R", package = "Rscience2027"))

# ==============================================================================
# MÓDULO UI: ENTORNO AUTOCONTENIDO
# ==============================================================================
rscience_engine_ui <- function(id) {
  ns <- NS(id)

  # 1. LOCALIZACIÓN DE RECURSOS (CSS, Imágenes, JS)
  # Buscamos la carpeta www dentro del paquete instalado o en el directorio local
  lib_www_path_relative <- system.file("www", package = "Rscience2027")

  # Si estamos en desarrollo y el paquete no está instalado, usamos "www"
  if (lib_www_path_relative == "") lib_www_path_relative <- "www"

  lib_www_path_absolute <- normalizePath(lib_www_path_relative, mustWork = TRUE)

  # Creamos el alias 'lib_www' para que el navegador acceda a las imágenes
  addResourcePath("lib_www", lib_www_path_absolute)

  # Ruta física para que R lea el CSS e inyecte el código
  path_to_css <- file.path(lib_www_path_absolute, "styles.css")

  # 2. LAYOUT DEL MOTOR
  page_sidebar(
    #title = span("Rscience Engine v.0.0.1", style="font-weight:800; color:#00d4ff;"),
    theme = bs_theme(version = 5, bg = "#0b1218", fg = "#fff", primary = "#00d4ff"),

    tags$head(
      useShinyjs(),
      # Inyectamos el CSS directamente desde el archivo físico
      if (file.exists(path_to_css)) includeCSS(path_to_css)
    ),

    sidebar = sidebar(
      width = 320, id = ns("sidebar_panel"),

      # Logo desde el recurso compartido
      div(class = "text-center", style = "padding: 20px 0 5px 0;",
          img(src = "lib_www/Rscience_logo_sticker.png", style = "width: 180px;")
      ),

      # Navegación Superior (Packs)
      div(class="nav-header",
          div(id=ns("dot1"), class="nav-dot-wrapper active", div(class="dot"), div(class="dot-label", "Setup")),
          div(id=ns("dot2"), class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Out")),
          div(id=ns("dot3"), class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Extra"))
      ),

      # Viewport para el slider de los packs
      div(class="sidebar-viewport",
          div(id=ns("track"), class="slider-track",
              # PACK 01: SETUP
              div(class="pack-group",
                  div(id=ns("c_data"), class="phase-card active", icon("database"), span(" Dataset", style="margin-left:10px;")),
                  div(id=ns("c_tool"), class="phase-card", icon("gear"), span(" Tool Engine", style="margin-left:10px;")),
                  div(id=ns("c_script"), class="phase-card", icon("gear"), span(" Script Engine", style="margin-left:10px;")),
                  div(id=ns("c_settings"), class="phase-card", icon("gear"), span(" Settings", style="margin-left:10px;")),
                  div(id=ns("c_play"), class="phase-card", icon("gear"), span(" Proccessing", style="margin-left:10px;"))

              ),
              # PACK 02: OUTPUT
              div(class="pack-group",
                  div(id=ns("c_out"), class="phase-card", icon("desktop"), span(" Visualizer", style="margin-left:10px;"))
              ),
              # PACK 03: THEORY
              div(class="pack-group",
                  div(id=ns("c_theory"), class="phase-card", icon("book"), span(" Theory", style="margin-left:10px;")),
                  div(id=ns("c_bibliography"), class="phase-card", icon("book"), span(" Bibliography", style="margin-left:10px;")),
                  div(id=ns("c_cite"), class="phase-card", icon("book"), span(" Cite", style="margin-left:10px;")),
                  div(id=ns("c_faqs"), class="phase-card", icon("book"), span(" FAQ's", style="margin-left:10px;"))


              )

          )
      )
    ),

    # ÁREA DE TRABAJO
    div(style="padding: 20px;",
        h1(textOutput(ns("title")), class="work-title"),
        hr(style="opacity:0.1; width: 80px; border-width: 4px; border-color: #00d4ff;"),
        uiOutput(ns("main_view"))
    )
  )
}

# ==============================================================================
# MÓDULO SERVER: LÓGICA INTERNA
# ==============================================================================
# ==============================================================================
# MÓDULO SERVER: LÓGICA INTERNA
# ==============================================================================
rscience_engine_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ##################
    # --- 1. RUTA MULTIPLATAFORMA ---
    # Construcción robusta de la ruta interna del paquete
    rel_path <- file.path("quarto", "tool_0001_anova_1_way", "f01_theory", "file01")
    file_name <- "theory_anova.html"

    full_path <- system.file(rel_path, file_name, package = "Rscience2027")

    # --- 2. RECURSO SEGURO ---
    resource_prefix <- paste0("theory_core_", id)

    if (full_path != "") {
      # Forzamos barras '/' para compatibilidad con el navegador (web-standard)
      dir_to_share <- normalizePath(dirname(full_path), winslash = "/", mustWork = FALSE)
      addResourcePath(prefix = resource_prefix, directoryPath = dir_to_share)
    }

    # --- 3. RENDER ---
    output$iframe_handler <- renderUI({
      if (full_path != "") {
        tags$iframe(
          src = paste0(resource_prefix, "/", file_name),
          title = "RScience Theory Content",
          allow = "fullscreen; chalkboard"
        )
      } else {
        div(style = "padding: 40px; text-align: center;",
            icon("exclamation-triangle", style = "color: #dc3545; font-size: 3rem;"),
            h3("Teoría no encontrada"),
            p("Verifica la instalación del paquete Rscience2027."))
      }
    })
    ##############################################################################################
    # --- INICIALIZACIÓN ---
    # Esto fuerza a Shiny a reconocer "c_data" como la tarjeta activa al cargar
    shinyjs::runjs(sprintf("Shiny.setInputValue('%s', 'c_data');", ns("active_card")))

    # 1. Listeners para Navegación de Packs (Desplazamiento Horizontal)
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', 0); });", ns("dot1"), ns("move")));
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', -33.333); });", ns("dot2"), ns("move")));
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', -66.666); });", ns("dot3"), ns("move")));

    observeEvent(input$move, {
      runjs(sprintf("$('#%s').css('transform', 'translateX(%f%%)');", ns("track"), input$move))
      runjs(sprintf("$('.nav-dot-wrapper').removeClass('active');"))
      # Nota: Aquí usamos el ID sin ns() para addClass porque shinyjs lo maneja internamente si se configuró bien
      # pero para mayor seguridad en módulos usamos el ID completo:
      if(input$move == 0) addClass("dot1", "active")
      if(input$move == -33.333) addClass("dot2", "active")
      if(input$move == -66.666) addClass("dot3", "active")
    })

    # 2. Listeners para Selección de Tarjetas (LISTA AMPLIADA)
    # Agregamos los nuevos IDs que pusiste en la UI
    cards <- c("c_data", "c_tool", "c_script", "c_settings", "c_play",
               "c_theory", "c_bibliography", "c_cite", "c_faqs",
               "c_out")

    lapply(cards, function(card_id) {
      runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', '%s', {priority: 'event'}); });",
                    ns(card_id), ns("active_card"), card_id))
    })

    observeEvent(input$active_card, {
      # Removemos la clase de todas las tarjetas para que solo una brille
      lapply(cards, function(x) removeClass(x, "active"))
      addClass(input$active_card, "active")
    })

    # 3. Renderizado de Contenidos
    output$title <- renderText({
      req(input$active_card)
      # Limpiamos el ID para el título (ej: c_script -> SCRIPT)
      toupper(gsub("c_", "", input$active_card))
    })

    output$main_view <- renderUI({
      req(input$active_card)

      # Ejemplo de contenido condicional según la tarjeta
      if(input$active_card == "c_play") {
        tagList(
          h4("Ejecución del Motor", style="color:#00d4ff;"),
          p("Prepare los parámetros antes de iniciar el procesamiento global."),
          actionButton(ns("go"), "START PROCESS", class="btn-run-main")
        )
      }


      else {
        tagList(
          p(paste("Módulo activo:", input$active_card), style="color:#888;"),
          div(style="margin-top:20px; padding:20px; background:rgba(0,212,255,0.05); border: 1px solid rgba(0,212,255,0.1); border-radius:15px;",
              p("Interfaz de configuración para ", strong(input$active_card))
          )
        )
      }
    })




    # Servidores dedicados
    mod_special_theory_server(id = "ns_aver01")
  })
}

# ==============================================================================
# EJECUCIÓN DE LA APP (Limpia y Minimalista)
# ==============================================================================
ui <- rscience_engine_ui("mi_analisis_01")

server <- function(input, output, session) {
  rscience_engine_server("mi_analisis_01")
}

shinyApp(ui, server)
