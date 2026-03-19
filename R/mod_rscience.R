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
      tags$style(HTML("
        /* 1. ELIMINAR SCROLL DEL SIDEBAR PADRE */
        aside.sidebar {
          height: 100vh !important;
          overflow: hidden !important;
        }

        /* Quitamos el padding que bslib le pone al contenido del sidebar */
        .sidebar-content {
          padding: 0 !important;
          margin: 0 !important;
          height: 100% !important;
        }

        /* 2. TEXTOS Y BOTONES COMPACTOS */
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
      "))
    ),

    sidebar = sidebar(
      width = 300,
      padding = 0, # <--- Importante: anula el padding de bslib

      # CONTENEDOR PRINCIPAL CON DEGRADADO
      div(
        style = "
          height: 100vh;
          background: linear-gradient(180deg, #ffffff 0%, #e6faff 100%) !important;
          border-right: 3px solid #00d4ff;
          display: flex; flex-direction: column;
          margin: 0; padding: 0;
        ",

        # Logo ajustado para que no empuje el contenido hacia abajo
        div(class = "text-center", style = "padding: 20px 0 5px 0;",
            img(src = "Rscience_logo_sticker.png", style = "width: 180px; filter: drop-shadow(0 5px 10px rgba(0,212,255,0.2));")
        ),

        div(class = "text-center", style = "margin-bottom: 10px;",
            span("v.0.0.1", class = "badge bg-dark", style = "font-family: monospace; font-size: 0.7rem;")),

        # Opciones con scroll interno SOLO si es estrictamente necesario (en pantallas muy pequeñas)
        div(style = "padding: 0 1.5rem; flex-grow: 1; overflow-y: auto;",
            hr(),
            div(class = "section-title", "1. Setup Phase"),
            input_switch(ns("sw_dataset"), "Dataset Selection", value = TRUE),
            input_switch(ns("sw_tool"), "Tool & Script Engine", value = FALSE),
            hr(),
            div(class = "section-title", "2. Execution Phase"),
            input_switch(ns("sw_theory"), "Theory & Basics", value = FALSE),
            input_switch(ns("sw_analysis"), "Data Analysis Studio", value = FALSE),
            input_switch(ns("sw_resources"), "Extra Resources", value = FALSE),
            hr(),
            uiOutput(ns("txt_status_mini"))
        ),

        # Botón de salida (fijo abajo)
        div(style = "padding: 15px 1.5rem; background: rgba(255,255,255,0.4);",
            actionButton(ns("btn_go_home"), "Exit to Launchpad",
                         icon = icon("door-open"),
                         class = "btn-go-home")
        )
      )
    ),

    # Área principal del módulo
    div(class = "p-0", style = "height: 100vh; overflow: hidden;",
        uiOutput(ns("main_content")))
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
mod_rscience_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactivo de Datos ---
    data_active <- reactive({
      if (is.null(input$f1)) return(mtcars)
      read.csv(input$f1$datapath)
    })

    # --- Lógica de exclusividad (Switches del Sidebar Principal) ---
    all_sw <- c("sw_dataset", "sw_tool", "sw_theory", "sw_analysis", "sw_resources")
    lapply(all_sw, function(id_sw) {
      observeEvent(input[[id_sw]], {
        if (input[[id_sw]]) {
          others <- all_sw[all_sw != id_sw]
          for (other in others) { updateCheckboxInput(session, other, value = FALSE) }
        } else if (sum(sapply(all_sw, function(x) isTRUE(input[[x]]))) == 0) {
          updateCheckboxInput(session, id_sw, value = TRUE)
        }
      }, ignoreInit = TRUE)
    })

    # --- Renderizado de Contenido Principal ---
    output$main_content <- renderUI({

      # 1. SECCIÓN DATASET
      if (isTRUE(input$sw_dataset)) {
        card(full_screen = TRUE,
             card_header(class="bg-primary text-white", "1. Dataset Selection"),
             div(class="p-4",
                 fileInput(ns("f1"), "Load CSV Data", accept = ".csv", width = "100%"),
                 hr(),
                 tableOutput(ns("prev_tab"))))

        # 2. SECCIÓN DATA ANALYSIS (CON PESTAÑAS INTERNAS)
      } else if (isTRUE(input$sw_analysis)) {

        navset_card_pill(
          title = "Data Analysis Studio",
          # Pestaña A: Selección de Variables
          nav_panel("Variable Selection",
                    layout_column_wrap(
                      width = 1/2,
                      card(card_header("Main Variables"),
                           selectInput(ns("y_v"), "Dependent Variable (Y)", names(data_active())),
                           selectInput(ns("x_v"), "Independent Variable (X)", names(data_active()), selected=names(data_active())[2])),
                      card(card_header("Aesthetics"),
                           selectInput(ns("p_col"), "Point Color", c("Blue"="#00d4ff", "Red"="#ff4d4d", "Green"="#2ecc71")),
                           sliderInput(ns("p_size"), "Point Size", 1, 10, 4))
                    )),

          # Pestaña B: Output Visual
          nav_panel("Output & Visualization",
                    card(full_screen = TRUE,
                         card_header(class="d-flex justify-content-between",
                                     "Dynamic Plot",
                                     checkboxInput(ns("show_reg"), "Show Trend Line", FALSE)),
                         plotOutput(ns("p1"), height = "500px"))),

          # Pestaña C: Reporting / Summary
          nav_panel("Reporting",
                    card(card_header("Statistical Summary"),
                         verbatimTextOutput(ns("st_summary"))),
                    card(card_header("Export Options"),
                         downloadButton(ns("dn_report"), "Generate PDF Report", class="btn-success")))
        )

        # 3. SECCIÓN TOOLS
      } else if (isTRUE(input$sw_tool)) {
        card(div(class="p-5 text-center", icon("terminal", "fa-5x mb-3"), h4("Scripting Engine")))

      } else {
        div(class="vh-100 d-flex align-items-center justify-content-center",
            h4("Select a module from the sidebar", class="text-muted"))
      }
    })

    # --- Outputs ---
    output$prev_tab <- renderTable({ head(data_active(), 10) }, striped = TRUE, width = "100%")

    output$st_summary <- renderPrint({
      req(input$x_v, input$y_v)
      summary(data_active()[, c(input$x_v, input$y_v)])
    })

    output$p1 <- renderPlot({
      req(input$x_v, input$y_v)
      p <- ggplot(data_active(), aes(x = .data[[input$x_v]], y = .data[[input$y_v]])) +
        geom_point(color = input$p_col, size = input$p_size, alpha = 0.7) +
        theme_minimal(base_size = 14)
      if (isTRUE(input$show_reg)) p <- p + geom_smooth(method = "lm", color = "red")
      p
    })
  })
}
