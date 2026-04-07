mod_rscience_engine_ui <- function(id) {
  ns <- NS(id)

  # 1. Definir la ruta a la carpeta que contiene TODOS los .css
  # Apuntamos a la carpeta 'css', no al archivo individual
  css_folder <- system.file("www", "css", package = "Rscience2027")
  if (css_folder == "") css_folder <- "www/css" # Desarrollo local

  # 2. Registrar el prefijo 'RS-STYLES' para esa carpeta
  # Esto le dice a Shiny: "Si alguien pide RS-STYLES/archivo.css, búscalo en css_folder"
  try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)

  tagList(
    tags$head(
      useShinyjs(),
      # 3. Llamamos al archivo maestro usando el prefijo registrado
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0("RS-STYLES/style_000.css?v=", as.numeric(Sys.time()))
      )
    ),
  page_sidebar(
    theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),

    #theme = bs_theme(version = 5, bg = "#0b1218", fg = "#fff", primary = "#00d4ff"),

    # tags$head(
    #   useShinyjs(),
    #   if (!is.null(path_to_css)) {
    #     tags$link(rel = "stylesheet", type = "text/css", href = "lib_www/style_000.css")
    #   }
    # ),

    sidebar = sidebar(
      width = 320, id = ns("sidebar_panel"),

      div(class = "text-center", style = "padding: 20px 0 5px 0;",
          img(src = "lib_www/Rscience_logo_sticker.png", style = "width: 180px;")
      ),

      # --- SELECTOR DE PACKS (DOTS) ---
      div(class="nav-header",
          div(id=ns("dot1"), class="nav-dot-wrapper active", div(class="dot"), div(class="dot-label", "Setup")),
          div(id=ns("dot2"), class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Out")),
          div(id=ns("dot3"), class="nav-dot-wrapper", div(class="dot"), div(class="dot-label", "Extra"))
      ),

      # --- SLIDER DE TARJETAS (TODAS LAS CARDS) ---
      div(class="sidebar-viewport",
          div(id=ns("track"), class="slider-track",
              # PACK 01: SETUP
              div(class="pack-group",
                  div(id=ns("c_data"), class="phase-card active", icon("database"), span(" Dataset")),
                  div(id=ns("c_tool"), class="phase-card", icon("gear"), span(" Tool Engine")),
                  div(id=ns("c_script"), class="phase-card", icon("code"), span(" Script Engine")),
                  div(id=ns("c_settings"), class="phase-card", icon("sliders"), span(" Settings")),
                  div(id=ns("c_play"), class="phase-card", icon("play"), span(" Processing"))
              ),
              # PACK 02: OUTPUT
              div(class="pack-group",
                  div(id=ns("c_out"), class="phase-card", icon("desktop"), span(" Visualizer"))
              ),
              # PACK 03: EXTRA / THEORY
              div(class="pack-group",
                  div(id=ns("c_theory"), class="phase-card", icon("book"), span(" Theory")),
                  div(id=ns("c_bibliography"), class="phase-card", icon("list"), span(" Bibliography")),
                  div(id=ns("c_cite"), class="phase-card", icon("quote-left"), span(" Cite")),
                  div(id=ns("c_faqs"), class="phase-card", icon("question-circle"), span(" FAQ's"))
              )
          )
      )
    ),

    # --- ÁREA DE TRABAJO PRINCIPAL ---
    div(style="padding: 20px;",
        h1(textOutput(ns("title")), class="work-title"),
        hr(style="opacity:0.2; width: 80px; border-top: 4px solid #00d4ff;"),

        navset_hidden(
          id = ns("main_navset"),
          # Setup Panels
          nav_panel_hidden("c_data", mod_02_01_dataset_ui(ns("my_ns_dataset"))),
          nav_panel_hidden("c_tool", mod_02_02_00_tool_ui(id = ns("my_ns_tool"))),
          nav_panel_hidden("c_script", card(card_body("Editor de Scripts..."))),
          nav_panel_hidden("c_settings", card(card_body("Ajustes del Sistema..."))),
          nav_panel_hidden("c_play", card(card_body("Consola de Procesamiento..."))),

          # Output Panels
          nav_panel_hidden("c_out", card(card_body("Visualizador de Resultados..."))),

          # Extra Panels
          nav_panel_hidden("c_theory", mod_special_theory_ui(ns("theory_internal"))),
          nav_panel_hidden("c_bibliography", card(card_body("Referencias Bibliográficas..."))),
          nav_panel_hidden("c_cite", card(card_body("Información de Cita..."))),
          nav_panel_hidden("c_faqs", card(card_body("Preguntas Frecuentes...")))
        )
    )
  )
  )
}

mod_rscience_engine_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Listado maestro de todas las tarjetas para los listeners
    all_cards <- c("c_data", "c_tool", "c_script", "c_settings", "c_play",
                   "c_out", "c_theory", "c_bibliography", "c_cite", "c_faqs")

    # 1. Lógica de Cambio de Pestaña y Brillo
    observeEvent(input$active_card, {
      nav_select("main_navset", selected = input$active_card)

      # Resetear brillo de todas y activar la seleccionada
      lapply(all_cards, function(x) removeClass(x, "active"))
      addClass(input$active_card, "active")
    })

    # 2. Control del Slider Horizontal (Dots)
    observeEvent(input$move, {
      runjs(sprintf("$('#%s').css('transform', 'translateX(%f%%)');", ns("track"), input$move))
      runjs(sprintf("$('.nav-dot-wrapper').removeClass('active');"))
      if(input$move == 0) addClass("dot1", "active")
      if(input$move == -33.333) addClass("dot2", "active")
      if(input$move == -66.666) addClass("dot3", "active")
    })

    # 3. Inyectar Listeners JS para clics en Tarjetas
    lapply(all_cards, function(card_id) {
      runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', '%s', {priority: 'event'}); });",
                    ns(card_id), ns("active_card"), card_id))
    })

    # 4. Inyectar Listeners JS para clics en Dots
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', 0); });", ns("dot1"), ns("move")));
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', -33.333); });", ns("dot2"), ns("move")));
    runjs(sprintf("$('#%s').click(function(){ Shiny.setInputValue('%s', -66.666); });", ns("dot3"), ns("move")));

    # Título dinámico
    output$title <- renderText({
      req(input$active_card)
      toupper(gsub("c_", "", input$active_card))
    })

    # Inicialización forzada
    shinyjs::runjs(sprintf("Shiny.setInputValue('%s', 'c_data');", ns("active_card")))

    # Servidor del módulo de teoría
    #mod_02_01_dataset_server(id = ns("my_ns_dataset"))
    #resultado_final <- mod_02_02_00_tool_server(id = "my_ns_tool", show_debug = F)
    # Server del módulo de dataset
    rlist_dataset <- mod_02_01_dataset_server(id = "my_ns_dataset") # SIN ns()

    # Server del módulo de tool
    rlist_tool <- mod_02_02_00_tool_server(id = "my_ns_tool", show_debug = FALSE) # SIN ns()

    mod_special_theory_server("theory_internal")
  })
}
