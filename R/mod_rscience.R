# ==============================================================================
# MÓDULO RSCIENCE (UI) - v.0.0.1 ENCAPSULADO & CYAN EDITION
# ==============================================================================
library("bslib")
library("shiny")


mod_rscience_ui <- function(id) {
  ns <- NS(id)

  # ID único para el contenedor raíz del motor
  wrapper_id <- ns("engine_wrapper")

  tagList(
    tags$head(
      tags$style(HTML(paste0("
        /* --- ENCAPSULAMIENTO DEL MOTOR --- */

        /* 1. Reset de visibilidad para el contenedor del Engine */
        #", wrapper_id, " {
          display: block;
          width: 100vw;
          height: 100vh;
          overflow: hidden;
        }

        /* 2. Ocultar el encabezado de las pestañas (específico del módulo) */
        #", ns("engine_switcher"), " { display: none; }

        /* 3. Fix de Sidebar (BSLIB) dentro de este módulo */
        #", wrapper_id, " aside.sidebar {
          height: 100vh !important;
          overflow: visible !important;
        }

        #", wrapper_id, " .sidebar-content {
          padding: 0 !important;
          margin: 0 !important;
          height: 100% !important;
          overflow: visible !important;
        }

        /* 4. Estilos de los títulos de sección */
        #", wrapper_id, " .section-title {
           text-transform: uppercase; font-weight: 800; font-size: 0.65rem;
           color: #008eb3 !important; margin-top: 12px; margin-bottom: 5px;
           letter-spacing: 0.5px;
        }

        /* 5. Botón de Salida (Go Home) */
        #", wrapper_id, " .btn-go-home {
          width: 100%; font-weight: 800; text-transform: uppercase; font-size: 0.7rem;
          border-radius: 8px; transition: 0.3s; padding: 8px;
          background: #fff !important; color: #ff4d4d !important;
          border: 1.5px solid #ff4d4d !important;
        }
        #", wrapper_id, " .btn-go-home:hover { background: #ff4d4d !important; color: #fff !important; }

        #", wrapper_id, " hr { border-top: 1px solid #00d4ff !important; opacity: 0.15 !important; margin: 10px 0 !important; }

        /* 6. Selectize por encima de todo */
        .selectize-dropdown { z-index: 10000 !important; }

        /* 7. TABLA PREVIEW: ZEBRA CYAN (Específica del ID de importación) */
        #", ns("demo_import-preview"), " table.dataTable thead th {
          background-color: #00d4ff !important;
          color: white !important;
        }

        #", ns("demo_import-preview"), " table.dataTable tbody tr.odd {
          background-color: #e6faff !important;
        }

        #", ns("demo_import-preview"), " table.dataTable tbody tr.even {
          background-color: #ffffff !important;
        }

        #", ns("demo_import-preview"), " table.dataTable tbody tr:hover {
          background-color: #ccf5ff !important;
        }
      ")))
    ),

    # Envolvemos el layout de bslib en el div controlador
    div(id = wrapper_id,
        page_sidebar(
          theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),
          fillable = TRUE,

          sidebar = sidebar(
            width = 300, padding = 0,
            div(
              style = "height: 100vh; background: linear-gradient(180deg, #ffffff 0%, #e6faff 100%); border-right: 3px solid #00d4ff; display: flex; flex-direction: column; overflow: visible;",

              div(class = "text-center", style = "padding: 20px 0 5px 0;",
                  img(src = "Rscience_logo_sticker.png", style = "width: 180px;")
              ),

              # Área de Switches
              div(style = "padding: 0 1.5rem; flex-grow: 1; overflow-y: visible;",
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

          # ÁREA PRINCIPAL (Tabset oculto)
          tabsetPanel(
            id = ns("engine_switcher"),
            type = "hidden",
            tabPanelBody("tab_dataset", div(class="p-3", mod_import_ui(ns("demo_import")))),
            tabPanelBody("tab_analysis", div(class="p-4", h4("Analysis Studio Content"))),
            tabPanelBody(
              "tab_tool",
              div(
                #style = "height: calc(100vh - 80px); overflow-y: auto; overflow-x: hidden;",
                mod_tools_ui(ns("my_tool"))
              )
            ),
            tabPanelBody("tab_welcome", div(class="vh-100 d-flex align-items-center justify-content-center", h4("Select a module", class="text-muted")))
          )
        )
    )
  )
}

# ==============================================================================
# MÓDULO RSCIENCE (SERVER)
# ==============================================================================
mod_rscience_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Módulos Internos
    res_import <- mod_import_server("demo_import")
    res_tool   <- mod_tools_server("my_tool") # Descomentar cuando esté listo

    # 2. Lógica de Navegación (Switches -> Tabs)
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

    # 3. Exclusividad de Switches
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

    # 4. Status Mini (Opcional)
    output$txt_status_mini <- renderUI({
      if (shiny::isTruthy(res_import()) && isTRUE(res_import()$is_ready)) {
        span(class = "badge bg-info", "Data Ready")
      } else {
        span(class = "badge bg-light text-dark", "System Idle")
      }
    })

    # 5. El botón Home no necesita lógica interna adicional
    # porque el Orquestador lo escucha mediante input[["engine_v1-btn_go_home"]]
  })
}
