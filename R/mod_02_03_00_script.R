# ==============================================================================
# MÓDULO: EXPLORADOR PRINCIPAL RScience (Full Features) - v.0.0.1
# ==============================================================================

mod_02_03_00_script_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    # Estilos CSS encapsulados para el estado de bloqueo y botones
    tags$head(
      tags$style(HTML(paste0("
        .locked-main-content {
            pointer-events: none !important;
            opacity: 0.9;
            border: 5px solid #28a745 !important;
            transition: all 0.4s ease;
            position: relative;
        }
        .locked-main-content::before {
            content: '🔒 SELECCIÓN CONFIRMADA';
            position: absolute; top: 10px; right: 10px;
            background: #28a745; color: white;
            padding: 5px 15px; border-radius: 20px;
            font-weight: 800; z-index: 999;
        }
        .path-display {
            background: #1a202c; color: #00FFFF;
            padding: 8px 15px; border-radius: 8px;
            font-family: monospace; font-size: 0.9rem;
        }
      ")))
    ),

    div(class = "container-fluid p-4",
        # 1. Cabecera con Selector y Botones
        card(
          card_header(
            div(class = "d-flex justify-content-between align-items-center",
                div(style = "width: 40%;",
                    selectInput(ns("tool_selector"), "Herramienta:", choices = NULL, width = "100%")
                ),
                div(class = "btn-group",
                    actionButton(ns("btn_confirm"), "Confirm", class = "btn btn-success", icon = icon("check")),
                    actionButton(ns("btn_edit"), "Edit", class = "btn btn-warning", icon = icon("edit")),
                    actionButton(ns("btn_reset"), "Reset", class = "btn btn-primary", icon = icon("sync"))
                )
            )
          ),

          # 2. Cuerpo con el Tabset (donde cae el contenido de la librería)
          div(id = ns("main_content_wrapper"),
              card_body(
                padding = 0,
                navset_card_underline(id = ns("main_tabs"))
              )
          ),

          # 3. Footer / Debug Panel
          card_footer(
            div(class = "d-flex justify-content-between",
                uiOutput(ns("path_info")),
                actionLink(ns("toggle_debug"), "Show/Hide Debug")
            ),
            conditionalPanel(
              condition = "input.toggle_debug % 2 != 0",
              ns = ns,
              hr(),
              listviewer::jsoneditOutput(ns("debug_view"), height = "200px")
            )
          )
        )
    )
  )
}

mod_02_03_00_script_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- A. REFERENCIAS DE LIBRERÍA ---
    pkg_path  <- system.file(package = "Rscience2027")
    base_path <- file.path(pkg_path, "shiny", "fn03_tool_script")

    # Reactivos de estado
    is_locked <- reactiveVal(FALSE)

    tools <- reactive({
      req(dir.exists(base_path))
      t <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
      t[grepl("^tool_", t)]
    })

    # --- B. CARGA DINÁMICA ---
    observe({
      req(tools())
      updateSelectInput(session, "tool_selector", choices = tools())

      lapply(tools(), function(tool_name) {
        file_path <- file.path(base_path, tool_name, "f01_script", "mod_special_script_info.R")
        if (file.exists(file_path)) {
          source(file_path, local = TRUE)
          insertTab(inputId = "main_tabs",
                    tabPanel(title = tool_name, value = tool_name,
                             div(class = "p-4", mod_special_script_info_ui(ns(tool_name))))
          )
          mod_special_script_info_server(tool_name)
        }
      })
    })

    # --- C. LÓGICA DE BOTONES Y ESTADOS ---

    # Confirmar: Bloquea UI y añade borde verde
    observeEvent(input$btn_confirm, {
      shinyjs::addClass(id = "main_content_wrapper", class = "locked-main-content")
      shinyjs::disable("tool_selector")
      is_locked(TRUE)
    })

    # Editar: Quita bloqueo
    observeEvent(input$btn_edit, {
      shinyjs::removeClass(id = "main_content_wrapper", class = "locked-main-content")
      shinyjs::enable("tool_selector")
      is_locked(FALSE)
    })

    # Reset: Limpia selección (opcionalmente podrías recargar la app)
    observeEvent(input$btn_reset, {
      updateSelectInput(session, "tool_selector", selected = tools()[1])
      shinyjs::removeClass(id = "main_content_wrapper", class = "locked-main-content")
      shinyjs::enable("tool_selector")
      is_locked(FALSE)
    })

    # Sincronización Selector <-> Tabs
    observeEvent(input$tool_selector, {
      updateTabsetPanel(session, "main_tabs", selected = input$tool_selector)
    })
    observeEvent(input$main_tabs, {
      updateSelectInput(session, "tool_selector", selected = input$main_tabs)
    })

    # --- D. DEBUG Y PATH ---
    output$path_info <- renderUI({
      div(class = "path-display", paste("Active Path:", file.path(base_path, input$tool_selector)))
    })

    output$debug_view <- listviewer::renderJsonedit({
      listviewer::jsonedit(list(
        package = "Rscience2027",
        version = "0.0.1",
        selection = input$tool_selector,
        locked = is_locked(),
        path = base_path
      ),
      mode="text")
    })
  })
}
