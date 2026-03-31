# ==============================================================================
# MÓDULO: EXPLORADOR PRINCIPAL RScience (Full Features) - v.0.0.1
# ==============================================================================

mod_02_03_00_script_ui <- function(id) {
  ns <- NS(id)
  root_sel <- paste0(".", ns("script-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        /* --- BLOQUEO CON VELO GRIS (SIMETRÍA DATASET) --- */
        ", root_sel, " .lock-wrapper {
            position: relative !important;
            transition: all 0.3s ease !important;
        }

        ", root_sel, " .locked-disabled::after {
            content: '' !important;
            position: absolute !important;
            top: -5px !important; left: -10px !important;
            right: -10px !important; bottom: -10px !important;
            background: rgba(0, 0, 0, 0.04) !important;
            border: 1px dashed rgba(40, 167, 69, 0.3) !important;
            z-index: 100 !important;
            border-radius: 15px !important;
            cursor: not-allowed !important;
        }

        ", root_sel, " .locked-disabled {
            pointer-events: none !important;
            user-select: none !important;
            opacity: 0.7 !important;
        }

        /* --- OCULTAR TABS --- */
        ", root_sel, " .nav-tabs, ", root_sel, " .nav-underline { display: none !important; }

        /* --- HEADER DINÁMICO --- */
        ", root_sel, " .selection-header {
            padding: 15px 25px !important; display: flex !important;
            justify-content: space-between !important; align-items: center !important;
            border-radius: 12px !important; margin-bottom: 20px !important;
            box-shadow: 0 4px 12px rgba(0,0,0,0.05) !important;
        }
        ", root_sel, " .selection-header.waiting-mode { background: #f0fdff !important; border: 1px solid #00cfd4 !important; color: #008184 !important; }
        ", root_sel, " .selection-header.active-selection { background: #fff9f0 !important; border: 1px solid #ff9100 !important; color: #b36600 !important; }
        ", root_sel, " .selection-header.confirmed { background: #f6fff8 !important; border: 1px solid #28a745 !important; color: #1e7e34 !important; }

        /* --- BOTONES PILDORA XL --- */
        ", root_sel, " .btn.btn-pill-xl {
            border-radius: 50px !important; padding: 15px 35px !important;
            font-weight: 800 !important; font-size: 1.1rem !important;
            text-transform: uppercase !important; letter-spacing: 1px !important;
            display: inline-flex !important; align-items: center !important;
            justify-content: center !important; gap: 10px !important;
            transition: all 0.3s ease !important;
        }
        ", root_sel, " .btn.btn-pill-xl.btn-locked {
            background-color: #e9ecef !important; color: #1e7e34 !important;
            border-color: #1e7e34 !important; opacity: 1 !important;
        }
      ")))
    ),

    div(class = paste("container-fluid p-4", ns("script-container")),
        uiOutput(ns("script_header_ui")),

        div(class = "row g-4 align-items-end mb-4",
            # Contenedor con lock-wrapper para el velo
            div(class = "col-md-7",
                div(id = ns("script_selector_wrapper"), class = "lock-wrapper",
                    div(style = "font-weight: 800; text-transform: uppercase; margin-bottom: 10px;",
                        "Script Tool Selection", uiOutput(ns("lock_icon_ui"), inline = TRUE)),
                    selectInput(ns("tool_selector"), label = NULL, choices = NULL, width = "100%")
                )
            ),
            div(class = "col-md-5 text-end",
                div(class = "d-flex justify-content-end gap-2",
                    actionButton(ns("btn_confirm"), span(icon("lock"), "Lock"), class = "btn-success btn-pill-xl"),
                    actionButton(ns("btn_edit"),    span(icon("lock-open"), "Unlock"), class = "btn-warning btn-pill-xl"),
                    actionButton(ns("btn_reset"),   span(icon("sync"), "Reset"), class = "btn-primary btn-pill-xl")
                )
            )
        ),

        div(id = ns("main_content_wrapper"),
            navset_card_underline(id = ns("main_tabs"))
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

    # Estado inicial por defecto para retornos y resets
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

    # --- 2. RENDER: HEADER DINÁMICO (Sincronizado con is_locked) ---
    output$script_header_ui <- renderUI({
      current_sel <- input$tool_selector

      if (is_locked()) {
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" SCRIPT LOADED:", current_sel)),
            span(class = "header-id", "STATUS: LOCK"))
      } else if (!is.null(current_sel) && current_sel != "") {
        div(class = "selection-header active-selection",
            span(icon("lock-open"), paste(" SELECTED:", current_sel)),
            span(class = "header-id", "STATUS: UNLOCK"))
      } else {
        div(class = "selection-header waiting-mode",
            span(icon("bolt"), " Waiting for script selection..."),
            span(class = "header-id", "STATUS: WAITING"))
      }
    })

    # --- 3. PROCESO MAESTRO DE IMPORTACIÓN (Tabs Ocultos) ---
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

            # Insertamos el tab (el CSS del UI se encarga de que sea invisible)
            insertTab(inputId = "main_tabs",
                      tabPanel(title = tool_name, value = tool_name,
                               div(class = "p-4", local_env$mod_special_script_info_ui(ns(tool_name)))))

            local_env$mod_special_script_info_server(tool_name)
          }, error = function(e) { message("  [!] ERROR en '", tool_name, "': ", e$message) })
        }
      }
      tools_env(tmp_list)
    }, ignoreInit = FALSE)

    # --- 4. SINCRONIZACIÓN DE INTERFAZ ---
    observeEvent(tools_env(), {
      lista <- tools_env()
      vector_opts <- names(lista)
      vector_opts <- c("Select a script..." = "", vector_opts)

      req(length(lista) > 0)
      updateSelectInput(session, "tool_selector", choices = vector_opts, selected = vector_opts[1])
    })

    observeEvent(input$tool_selector, {
      req(input$tool_selector %in% names(tools_env()))
      updateTabsetPanel(session, "main_tabs", selected = input$tool_selector)
    })

    # --- 5. LÓGICA DE BOTONES (Con Velo Gris y Bloqueo XL) ---

    # ACCIÓN: CONFIRM (LOCK)
    observeEvent(input$btn_confirm, {
      req(input$tool_selector, tools_env())

      # Aplicar Velo Gris al selector y borde verde al contenido
      shinyjs::addClass(id = "script_selector_wrapper", class = "locked-disabled")
      shinyjs::addClass(id = "main_content_wrapper", class = "locked-main-content")

      # Estilo Botón Confirm (Deshabilitado y color Lock)
      shinyjs::disable("btn_confirm")
      shinyjs::addClass(id = "btn_confirm", class = "btn-locked")

      is_locked(TRUE)
      is_done(TRUE)

      # Emitir metadata al padre
      meta_data <- tools_env()[[input$tool_selector]]
      confirmed_output(list(
        description = "*** Rscience - tool_script information ***",
        is_done   = TRUE,
        is_locked = TRUE,
        selected  = input$tool_selector,
        metadata  = meta_data,
        folder_path_tool_script = meta_data$folder_path_tool_script
      ))

      showNotification(paste("Success: Script", input$tool_selector, "locked."), type = "message")
    })

    # ACCIÓN: EDIT (UNLOCK)
    observeEvent(input$btn_edit, {
      # Quitar Velo Gris y bloqueos
      shinyjs::removeClass(id = "script_selector_wrapper", class = "locked-disabled")
      shinyjs::removeClass(id = "main_content_wrapper", class = "locked-main-content")

      # Restaurar botones
      shinyjs::enable("btn_confirm")
      shinyjs::removeClass(id = "btn_confirm", class = "btn-locked")

      is_locked(FALSE)
      is_done(FALSE)

      # Limpiar salida hacia el padre (vuelve a default)
      confirmed_output(default_output)
      message("--- [SCRIPTS] Modo edición habilitado ---")
    })

    # ACCIÓN: RESET
    observeEvent(input$btn_reset, {
      # Limpiar todos los estilos de bloqueo
      shinyjs::removeClass(id = "script_selector_wrapper", class = "locked-disabled")
      shinyjs::removeClass(id = "main_content_wrapper", class = "locked-main-content")
      shinyjs::enable("btn_confirm")
      shinyjs::removeClass(id = "btn_confirm", class = "btn-locked")

      is_locked(FALSE)
      is_done(FALSE)
      confirmed_output(default_output)

      # Volver al primer elemento de la lista
      if (length(tools_env()) > 0) {
        updateSelectInput(session, "tool_selector", selected = names(tools_env())[1])
      }
      message("--- [SCRIPTS] Reset completo ---")
    })

    # --- 6. SALIDAS DE INFORMACIÓN Y DEBUG ---
    output$debug_footer_ui <- renderUI({
      req(show_debug)
      card_footer(
        div(class = "d-flex justify-content-between align-items-center",
            div(class = "path-display",
                icon("folder-open"), " ",
                if(!is.null(input$tool_selector)) tools_env()[[input$tool_selector]]$folder_path_tool_script else "No path selected"),
            actionLink(ns("toggle_debug"), "Show/Hide JSON Debug")
        ),
        conditionalPanel(
          condition = "input.toggle_debug % 2 != 0",
          ns = ns,
          hr(),
          listviewer::jsoneditOutput(ns("debug_view"), height = "250px")
        )
      )
    })

    output$debug_view <- listviewer::renderJsonedit({
      listviewer::jsonedit(confirmed_output(), mode = "text")
    })

    # --- 7. RETORNO DE DATOS ---
    return(confirmed_output)
  })
}
