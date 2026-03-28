# ==============================================================================
# MÓDULO: EXPLORADOR PRINCIPAL RScience (Full Features) - v.0.0.1
# ==============================================================================

mod_02_03_00_script_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
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

          div(id = ns("main_content_wrapper"),
              card_body(
                padding = 0,
                navset_card_underline(id = ns("main_tabs"))
              )
          ),
          uiOutput(ns("debug_footer_ui"))
        )
    )
  )
}

mod_02_03_00_script_server <- function(id, vector_str_folder_tool_script = NULL, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. CONFIGURACIÓN DE RUTAS Y REACTIVOS ---
    pkg_path  <- system.file(package = "Rscience2027")
    if (pkg_path == "") pkg_path <- "inst"
    base_path <- file.path(pkg_path, "shiny", "fn03_tool_script")

    tools_env <- reactiveVal(list())
    is_done   <- reactiveVal(FALSE)
    is_locked <- reactiveVal(FALSE)

    # Definimos el estado inicial por defecto para reutilizarlo
    default_output <- list(
      description = "*** Rscience - tool_script information ***",
      is_done   = FALSE,
      is_locked = FALSE,
      selected  = NULL,
      metadata  = NULL,
      folder_path_tool_script = NULL
    )

    confirmed_output <- reactiveVal(default_output)

    internal_vector_folder_opt <- reactive({
      if (is.function(vector_str_folder_tool_script)) vector_str_folder_tool_script() else vector_str_folder_tool_script
    })

    # --- 2. PROCESO MAESTRO DE IMPORTACIÓN ---
    observeEvent(internal_vector_folder_opt(), {
      req(dir.exists(base_path))
      folders <- internal_vector_folder_opt()
      req(length(folders) > 0)

      current_tools <- names(tools_env())
      if (length(current_tools) > 0) {
        for (t in current_tools) { removeTab(inputId = "main_tabs", target = t) }
      }

      tmp_list <- list()
      for (tool_name in folders) {
        folder_path <- file.path(base_path, tool_name)
        file_path <- file.path(folder_path, "f01_script_info", "mod_special_script_info.R")

        if (file.exists(file_path)) {
          tryCatch({
            local_env <- new.env(parent = .GlobalEnv)
            source(file_path, local = local_env)

            tmp_list[[tool_name]] <- list(
              tool_script_name = tool_name,
              folder_path_tool_script = folder_path,
              file_path = file_path,
              local_env = local_env
            )

            insertTab(inputId = "main_tabs",
                      tabPanel(title = tool_name, value = tool_name,
                               div(class = "p-4", local_env$mod_special_script_info_ui(ns(tool_name)))))

            local_env$mod_special_script_info_server(tool_name)
          }, error = function(e) { message("  [!] ERROR en '", tool_name, "': ", e$message) })
        }
      }
      tools_env(tmp_list)
    }, ignoreInit = FALSE)

    # --- 3. SINCRONIZACIÓN DE INTERFAZ ---
    observeEvent(tools_env(), {
      lista <- tools_env()
      req(length(lista) > 0)
      updateSelectInput(session, "tool_selector", choices = names(lista), selected = names(lista)[1])
    })

    observeEvent(input$tool_selector, {
      req(input$tool_selector %in% names(tools_env()))
      updateTabsetPanel(session, "main_tabs", selected = input$tool_selector)
    })

    observeEvent(input$main_tabs, {
      req(input$main_tabs != "", input$main_tabs %in% names(tools_env()))
      updateSelectInput(session, "tool_selector", selected = input$main_tabs)
    })

    # --- 4. LÓGICA DE BOTONES ---

    # CONFIRM: Bloquea y emite datos
    observeEvent(input$btn_confirm, {
      req(input$tool_selector, tools_env())

      shinyjs::addClass(id = "main_content_wrapper", class = "locked-main-content")
      shinyjs::disable("tool_selector")
      is_locked(TRUE)
      is_done(TRUE)

      meta_data <- tools_env()[[input$tool_selector]]
      confirmed_output(list(
        description = "*** Rscience - tool_script information ***",
        is_done   = TRUE,
        is_locked = TRUE,
        selected  = input$tool_selector,
        metadata  = meta_data,
        folder_path_tool_script = meta_data$folder_path_tool_script
      ))
    })

    # EDIT: Desbloquea interfaz pero RESETEA el output (mantiene selección actual)
    observeEvent(input$btn_edit, {
      shinyjs::removeClass(id = "main_content_wrapper", class = "locked-main-content")
      shinyjs::enable("tool_selector")
      is_locked(FALSE)
      is_done(FALSE)

      # Volvemos a los valores por defecto (limpia lo que ve el padre)
      confirmed_output(default_output)
    })

    # RESET: Desbloquea interfaz, RESETEA el output y vuelve al INICIO de los selectores
    observeEvent(input$btn_reset, {
      shinyjs::removeClass(id = "main_content_wrapper", class = "locked-main-content")
      shinyjs::enable("tool_selector")
      is_locked(FALSE)
      is_done(FALSE)

      # Volvemos a los valores por defecto
      confirmed_output(default_output)

      if (length(tools_env()) > 0) {
        updateSelectInput(session, "tool_selector", selected = names(tools_env())[1])
      }
    })

    # --- 5. SALIDAS DE INFORMACIÓN Y DEBUG ---

    output$debug_footer_ui <- renderUI({
      req(show_debug)
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
    })

    output$path_info <- renderUI({
      req(input$tool_selector, tools_env())
      path <- tools_env()[[input$tool_selector]]$folder_path_tool_script
      div(class = "path-display", icon("folder-open"), " ", path)
    })

    output$debug_view <- listviewer::renderJsonedit({
      listviewer::jsonedit(confirmed_output(), mode = "text")
    })

    # --- 6. RETORNO DE DATOS AL PADRE ---
    return(confirmed_output)
  })
}
