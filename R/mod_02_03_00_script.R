# ==============================================================================
# MÓDULO: EXPLORADOR DE SCRIPTS RScience - v.0.0.2 (Engine Sync)
# ==============================================================================
mod_02_03_00_script_DEBUG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Este uiOutput cargará todo lo que definiste en output$show_debug_external
    uiOutput(ns("show_debug_external"))
  )
}

mod_02_03_00_script_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(id = ns("total_script_container"), class = "container-fluid",

        # 1. Cabecera Dinámica
        div(id = ns("header_section"), uiOutput(ns("script_header_ui"))),

        div(class = "row g-3 align-items-center", style="margin-top:5px;",
            # 2. Selector de Script
            div(id = ns("selector_section"), class = "col-md-7",
                div(id = ns("script_selector_wrapper"), class = "lock-wrapper",
                    div(style = "font-weight: 800; text-transform: uppercase; margin-bottom: 5px; font-size: 0.8rem; color: #00d4ff;",
                        "Script Tool Selection"),
                    radioButtons(ns("tool_selector"), label = NULL, choices = c("Buscando scripts..." = ""), width = "100%")
                )
            ),
            # 3. Control del Motor
            div(class = "col-md-5",
                div(class = "action-row-right",
                    mod_07_00_engine_control_ui(ns("script_switch"))
                )
            )
        ),

        div(style = "border-top: 4px solid rgba(0,212,255, 1); margin: 30px 0;"),

        # 4. Contenido de los Tabs
        div(id = ns("main_content_wrapper"), class = "script-content-area",
            navset_hidden(id = ns("main_tabs"))
        ),

        # 5. Panel de Debug Dinámico
        uiOutput(ns("show_debug_internal"))
    )
  )
}

mod_02_03_00_script_server <- function(id, vector_str_folder_tool_script = NULL, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)


    # --- 1. CONFIGURACIÓN INICIAL ---
    internal_vector_folder <- reactive({
      val <- if(is.function(vector_str_folder_tool_script)) vector_str_folder_tool_script() else vector_str_folder_tool_script
      val
    }) %>% shiny::debounce(150)

    engine_state <- mod_07_00_engine_control_server("script_switch", show_debug = internal_show_debug)

    get_default_data <- function() {
      list(
        description = "*** RScience - Script ***",
        my_sys_time = Sys.time(),
        click_count = 0,
        mode = "unlock",
        is_done = NULL,
        is_locked = NULL,
        selected_script_tool = NULL,
        script_tool_folder_path = NULL,
        folder_exists = NULL,
        metadata = list()
      )
    }

    reset_data_store <- function() {
      defaults <- get_default_data()

      # mapply recorre los nombres y valores de la lista de defaults
      # y los asigna uno a uno al objeto reactiveValues
      mapply(function(val, name) {
        data_store[[name]] <- val
      }, defaults, names(defaults))
    }

    data_store <- do.call(reactiveValues, get_default_data())

    # Corregimos is_done: TRUE si hay algo seleccionado
    is_done_val <- reactive({
      sel <- input$tool_selector
      !is.null(sel) && sel != ""
    })

    rlist_selected_script_folder <- reactive({
      # Buscamos la carpeta BASE de scripts, no el seleccionado aún
      pkg_path <- system.file("shiny", "fn03_tool_script", package = "Rscience2027")
      #if (pkg_path == "") pkg_path <- "inst/shiny/fn03_tool_script" # Fallback desarrollo

      target_folder_path_absolute <- normalizePath(pkg_path, mustWork = FALSE)

      list(
        target_folder_path_absolute = target_folder_path_absolute,
        target_folder_exists = dir.exists(target_folder_path_absolute)
      )
    })

    # --- 2. IMPORTACIÓN DINÁMICA ---
    # 2.1 Generamos la lista de entornos y datos
    tmp_list <- reactive({
      req(internal_vector_folder())
      base_folder <- rlist_selected_script_folder()$target_folder_path_absolute

      res_list <- list()
      for (tool_script_name in internal_vector_folder()) {

        # Construimos la ruta exacta al archivo mod_special_script_info.R
        # según tu estructura de carpetas confirmada por 'ls'
        file_path <- file.path(base_folder,
                               tool_script_name,
                               "f01_shiny_show",
                               "p00_script_info",
                               "mod_special_script_info.R")

        if (file.exists(file_path)) {
          local_env <- new.env(parent = .GlobalEnv)
          try({
            source(file = file_path, local = local_env)
            res_list[[tool_script_name]] <- list(
              tool_script_name = tool_script_name,
              folder_path_absolute = normalizePath(file.path(base_folder, tool_script_name)),
              file_path_absolute = normalizePath(file_path),
              local_env = local_env,
              names_obj = names(local_env)
            )
          })
        }
      }
      res_list
    })


    df_tmp <- reactive({
      internal_tmp_list <- tmp_list()
      req(internal_tmp_list)

      # 1. Transformamos la lista de listas en un data.frame limpio
      # Usamos lapply para extraer solo lo que queremos de cada sub-lista
      list_for_df <- lapply(internal_tmp_list, function(item) {
        data.frame(
          tool_name   = item$tool_script_name,
          check_folder_exists = item$check_folder_exists,
          check_file_file_path = item$check_file_file_path,
          folder_path_absolute = item$folder_path_absolute,
          file_path_absolute = item$file_path_absolute,
          objects     = paste(item$names_obj, collapse = ", "),
          # Convertimos el vector de nombres a un solo string para que quepa en una celda

          stringsAsFactors = FALSE
        )
      })

      # 2. Unimos todas las filas
      df_output <- do.call(rbind, list_for_df)

      # Quitamos los nombres de las filas que a veces ensucian la tabla
      rownames(df_output) <- NULL

      df_output
    })

    output$debug_df_tmp <- DT::renderDT({
      # Intentamos obtener el data frame
      internal_df_tmp <- tryCatch(df_tmp(), error = function(e) NULL)

      # Si no hay datos, creamos un DF de aviso
      if(is.null(internal_df_tmp)) {
        internal_df_tmp <- data.frame(Status = "Waiting for scripts...")
      }

      DT::datatable(
        internal_df_tmp,
        rownames = FALSE,
        extensions = 'FixedHeader',
        options = list(
          pageLength = 5,      # Pocas filas para que no rompa el layout del debug
          dom = 'ftp',         # Solo muestra Filtro, Tabla y Paginación (sin info ni length)
          scrollX = TRUE,      # Scroll horizontal por si las rutas son largas
          style = "bootstrap4" # Para que combine con tu UI de RScience
        ),
        selection = 'none'     # Evita que se seleccionen filas en el panel de debug
      )
    })


    # --- 2.2 Observador para renderizar la UI de los Tabs ---
    observeEvent(tmp_list(), {
      internal_tmp_list <- tmp_list()
      vector_names <- names(internal_tmp_list)

      for (name in vector_names) {
        target_env <- internal_tmp_list[[name]]$local_env

        insertTab(
          inputId = "main_tabs",
          tabPanel(
            title = name,
            value = name,
            div(class = "p-3", target_env$mod_special_script_info_ui(ns(name)))
          )
          # immediate = TRUE  <--- ELIMINA ESTA LÍNEA TOTALMENTE
        )

        target_env$mod_special_script_info_server(name)
      }

      choices <- names(internal_tmp_list)
      # updateSelectInput(session, "tool_selector", choices = c("Select a script..." = "", choices))
      #choices <- names(internal_tmp_list)

      # IMPORTANTE: Para radioButtons, es mejor no usar el "Select a script..."
      # como opción vacía si quieres obligar a una selección real,
      # o manejarlo con un inline = TRUE si prefieres.
      updateRadioButtons(session, "tool_selector",
                         choices = choices,
                         selected = character(0))
    })




    # --- 3. LÓGICA DE BLOQUEO (SIN ANIDAMIENTOS) ---
    observeEvent(engine_state(), {
      internal_mode <- engine_state()$mode
      internal_is_done <- is_done_val()
      internal_selected_script_tool <- input$tool_selector
      internal_rlist_selected_script_folder <-   rlist_selected_script_folder()
      internal_selected_tool_script_folder <- internal_rlist_selected_script_folder$target_folder_path_absolute
      internal_selected_folder_exists <- internal_rlist_selected_script_folder$target_folder_exists

      internal_tmp_list <- tmp_list()

      to_freeze <- c("header_section", "selector_section")

      # A. GUARDAS
      req(internal_mode, internal_tmp_list)

      # B. VALIDACIÓN DE BLOQUEO
      if (internal_mode == "lock" && internal_is_done == FALSE) {
        isolate({
          showNotification("Please select a script before locking.", type = "warning")

          # # # # ---------- # # # # ---------- # # # # ---------- # # # # ---------- # # # # ----------
          shinyjs::delay(1000, {
            shinyWidgets::updateRadioGroupButtons(session, "script_switch-engine_mode", selected = "unlock")
          })
          # # # # ---------- # # # # ---------- # # # # ---------- # # # # ---------- # # # # ----------

        })
        return()
      }

      # C. ACCIÓN UI
      if (internal_mode == "lock" && internal_is_done == TRUE) {
        isolate({
          lapply(to_freeze, shinyjs::addClass, class = "locked-disabled")
          shinyjs::addClass("main_content_wrapper", "locked-main-content")
          data_store$mode <- internal_mode
          data_store$is_locked <- TRUE
          data_store$selected_script_tool <- internal_selected_script_tool
          data_store$script_tool_folder_path <- file.path(
            rlist_selected_script_folder()$target_folder_path_absolute,
            internal_selected_script_tool
          )
          data_store$folder_exists <- dir.exists(file.path(
            rlist_selected_script_folder()$target_folder_path_absolute,
            internal_selected_script_tool
          ))

          data_store$metadata <- internal_tmp_list[[internal_selected_script_tool]]
        })
        return()
      }

      if (internal_mode == "unlock") {

        isolate({
          lapply(to_freeze, shinyjs::removeClass, class = "locked-disabled")
          shinyjs::removeClass("main_content_wrapper", "locked-main-content")
          reset_data_store()
        })
        return()
      }

      if (internal_mode == "reset"){

        reset_data_store()
        # # # # ---------- # # # # ---------- # # # # ---------- # # # # ---------- # # # # ----------
        shinyjs::delay(1000, {
          shinyWidgets::updateRadioGroupButtons(session, "script_switch-engine_mode", selected = "unlock")
        })
        # # # # ---------- # # # # ---------- # # # # ---------- # # # # ---------- # # # # ----------
      }

    })

    is_locked_val <- reactive({
      # Evita que el programa se caiga si data_store$is_locked es NULL
      res <- data_store$is_locked
      if(is.null(res)) return(FALSE)
      res
    })

    # --- 4. RENDERS ---
    output$script_header_ui <- renderUI({
      internal_selected_tool_script <- input$tool_selector
      internal_is_locked <- is_locked_val()
      internal_is_done   <- is_done_val()

      if (internal_is_locked) {
        div(class = "selection-header confirmed", span(icon("lock"), paste(" CONFIRMED:", internal_selected_tool_script)))
      } else if (internal_is_done) {
        div(class = "selection-header active-selection", span(icon("lock-open"), paste(" SELECTED:", internal_selected_tool_script)))
      } else {
        div(class = "selection-header waiting-mode", span(icon("bolt"), " Waiting selection..."))
      }
    })

    # Cambio de Tab al seleccionar
    observeEvent(input$tool_selector, {
      req(input$tool_selector != "")
      nav_select("main_tabs", selected = input$tool_selector)
    })

    # # # DEBUG
    output$debug_internal <- listviewer::renderJsonedit({
      req(internal_show_debug())
      listviewer::jsonedit(listdata = reactiveValuesToList(data_store), mode = "text")
    })

    output$show_debug_internal <- renderUI({
      req(internal_show_debug())
      div(class = "debug-section", style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 10px;",
          div(class = "section-label", style = "justify-content: flex-start !important; gap: 8px;", icon("bug"), " Internal Debug - Script"),
          listviewer::jsoneditOutput(ns("debug_internal"), height = "auto"))
    })

    output$debug_external <- listviewer::renderJsonedit({
      listviewer::jsonedit(listdata = reactiveValuesToList(data_store), mode = "text")
    })

    output$show_debug_external <- renderUI({
      div(style = "background: #1a1a1a; padding: 15px; border-radius: 8px;",
          div(class = "row",
              div(class = "col-md-4",
                  div(class = "section-label", style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 15px;", icon("bug"), " External Debug - Script"),
                  listviewer::jsoneditOutput(ns("debug_external"), height = "500px")),
              div(class = "col-md-4",
                  div(style = "border-left: 1px solid #333; padding-left: 15px; height: 100%;",
                      mod_07_00_engine_control_DEBUG_ui(id = ns("script_switch")))
                  )#,
              ),
          DT::DTOutput(ns("debug_df_tmp"))
      )


    })


    return(reactive({ reactiveValuesToList(data_store) }))


  })
}


