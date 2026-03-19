library(shiny)
library(bslib)
library(ggplot2)

# ==============================================================================
# MÓDULO RSCIENCE (UI) - v.0.0.1 FIX FULL SCREEN & ID ERROR
# ==============================================================================
# ==============================================================================
# MÓDULO RSCIENCE (UI) - v.0.0.1 CYAN GRADIENT EDITION
# ==============================================================================
mod_rscience_ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),
    fillable = TRUE,

    header = tags$head(
      tags$style(HTML(paste0("
        /* OCULTAR EL ENCABEZADO DE LAS PESTAÑAS */
        #", ns("engine_switcher"), " { display: none; }

        aside.sidebar { height: 100vh !important; overflow: hidden !important; }
        .sidebar-content { padding: 0 !important; margin: 0 !important; height: 100% !important; }
        .section-title {
          text-transform: uppercase; font-weight: 800; font-size: 0.65rem;
          color: #008eb3 !important; margin-top: 12px; margin-bottom: 5px;
          letter-spacing: 0.5px;
        }
        .btn-go-home {
          width: 100%; font-weight: 800; text-transform: uppercase; font-size: 0.7rem;
          border-radius: 8px; transition: 0.3s; padding: 8px;
          background: #fff !important; color: #ff4d4d !important;
          border: 1.5px solid #ff4d4d !important;
        }
        .btn-go-home:hover { background: #ff4d4d !important; color: #fff !important; }
        hr { border-top: 1px solid #00d4ff !important; opacity: 0.15 !important; margin: 10px 0 !important; }
      ")))
    ),

    sidebar = sidebar(
      width = 300, padding = 0,
      div(
        style = "height: 100vh; background: linear-gradient(180deg, #ffffff 0%, #e6faff 100%); border-right: 3px solid #00d4ff; display: flex; flex-direction: column;",

        div(class = "text-center", style = "padding: 20px 0 5px 0;",
            img(src = "Rscience_logo_sticker.png", style = "width: 180px;")
        ),

        div(style = "padding: 0 1.5rem; flex-grow: 1; overflow-y: auto;",
            hr(),
            div(class = "section-title", "1. Setup Phase"),
            input_switch(ns("sw_dataset"), "Dataset Selection", value = TRUE),
            input_switch(ns("sw_tool"), "Tool & Script Engine", value = FALSE),
            hr(),
            div(class = "section-title", "2. Execution Phase"),
            input_switch(ns("sw_analysis"), "Data Analysis Studio", value = FALSE),
            hr()
        ),

        div(style = "padding: 15px 1.5rem; background: rgba(255,255,255,0.4);",
            actionButton(ns("btn_go_home"), "Exit to Launchpad", icon = icon("door-open"), class = "btn-go-home")
        )
      )
    ),

    # ÁREA PRINCIPAL: TabsetPanel INVISIBLE
    tabsetPanel(
      id = ns("engine_switcher"),
      type = "hidden", # <--- Aquí está la magia

      tabPanelBody("tab_dataset",
                   div(class="p-3", mod_import_ui(ns("demo_import")))
      ),

      tabPanelBody("tab_analysis",
                   div(class="p-4",
                       navset_card_pill(
                         title = "Data Analysis Studio",
                         nav_panel("Variable Selection",
                                   layout_column_wrap(width = 1/2,
                                                      card(card_header("Main Variables"),
                                                           selectizeInput(ns("y_v"), "Dependent Variable (Y)", choices = NULL),
                                                           selectizeInput(ns("x_v"), "Independent Variable (X)", choices = NULL)),
                                                      card(card_header("Aesthetics"),
                                                           selectInput(ns("p_col"), "Point Color", c("Cyan"="#00d4ff", "Red"="#ff4d4d")),
                                                           sliderInput(ns("p_size"), "Point Size", 1, 10, 4))
                                   )
                         ),
                         nav_panel("Output & Visualization",
                                   card(plotOutput(ns("p1"), height = "500px"))
                         )
                       )
                   )
      ),

      tabPanelBody("tab_tool",
                   div(class="p-5 text-center", icon("terminal", "fa-5x mb-3"), h4("Scripting Engine"))
      ),

      tabPanelBody("tab_welcome",
                   div(class="vh-100 d-flex align-items-center justify-content-center",
                       h4("Select a module from the sidebar", class="text-muted"))
      )
    )
  )
}

# ==============================================================================
# MÓDULO RSCIENCE (SERVER)
# ==============================================================================
# ==============================================================================
# MÓDULO RSCIENCE (SERVER) - v.0.0.1 DATA ANALYSIS RESTORED
# ==============================================================================
# ==============================================================================
# MÓDULO RSCIENCE (SERVER) - v.0.0.1 FULL TABS RESTORED
# ==============================================================================
# ==============================================================================
# MÓDULO RSCIENCE (SERVER) - v.0.0.1 FULL PERSISTENCE EDITION
# ==============================================================================
# ==============================================================================
# MÓDULO RSCIENCE (SERVER) - v.0.0.1 ORCHESTRATOR EDITION
# ==============================================================================
mod_rscience_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. INICIALIZACIÓN SILENCIOSA DEL MÓDULO DE IMPORTACIÓN
    # Esto mantiene los datos en memoria, pero no genera outputs aquí.
    res_import <- mod_import_server("demo_import")

    # 2. LÓGICA DE NAVEGACIÓN (SWITCHES -> TABS)
    # Solo gestiona qué panel está visible.
    # Al usar 'tab_welcome' como fallback, evitamos que intente renderizar
    # cosas inexistentes al arrancar el Launchpad.
    observe({
      target <- "tab_welcome"

      if (isTRUE(input$sw_dataset)) {
        target <- "tab_dataset"
      } else if (isTRUE(input$sw_analysis)) {
        target <- "tab_analysis"
      } else if (isTRUE(input$sw_tool)) {
        target <- "tab_tool"
      }

      updateTabsetPanel(session, "engine_switcher", selected = target)
    })

    # 3. EXCLUSIVIDAD DE LOS SWITCHES DEL SIDEBAR
    all_sw <- c("sw_dataset", "sw_tool", "sw_analysis")

    lapply(all_sw, function(id_sw) {
      observeEvent(input[[id_sw]], {
        if (input[[id_sw]]) {
          others <- all_sw[all_sw != id_sw]
          for (other in others) {
            updateCheckboxInput(session, other, value = FALSE)
          }
        }
      }, ignoreInit = TRUE)
    })

    # 4. STATUS MINI (SOLO TEXTO INFORMATIVO)
    # Una pequeña señal de vida en el sidebar que no rompe el código.
    output$txt_status_mini <- renderUI({
      # Usamos isTruthy para evitar el error de 'length zero' al inicio
      if (shiny::isTruthy(res_import()) && isTRUE(res_import()$is_ready)) {
        span(class = "badge bg-info", "Data Ready")
      } else {
        span(class = "badge bg-light text-dark", "System Idle")
      }
    })

    # 5. LÓGICA DEL BOTÓN HOME (REGRESO AL LAUNCHPAD)
    observeEvent(input$btn_go_home, {
      # El observer del Launchpad (input[['engine_v1-btn_go_home']])
      # detectará este click y cambiará la página.
    })

  })
}
