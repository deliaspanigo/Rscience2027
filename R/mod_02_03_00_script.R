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

        # --- CABECERA (ENGINE CONTROL) ---
        div(id = ns("the_control"),
            style = "display: flex; align-items: center; justify-content: flex-start; gap: 20px; margin-bottom: 25px;",
            div(class = "section-label",
                style = "margin: 0; white-space: nowrap; font-size: 1.2rem; font-weight: 700; color: #00d4ff;",
                icon("code"), " Script"
            ),
            mod_07_00_toggle_ui(ns("main_switch")),
            div(style = "flex-grow: 1;"),
            mod_07_00_label_ui(ns("main_switch")),
            mod_07_00_refresh_ui(ns("main_switch")),
            mod_07_00_unlock_ghost_ui(ns("main_switch"))
        ),

        div(class = "row g-3 align-items-center", style="margin-top:5px;",
            # 2. Selector de Script
            div(id = ns("selector_section"), class = "col-md-7",
                div(id = ns("script_selector_wrapper"), class = "lock-wrapper",
                    div(style = "font-weight: 800; text-transform: uppercase; margin-bottom: 5px; font-size: 0.8rem; color: #00d4ff;",
                        "Script Tool Selection"),
                    radioButtons(ns("tool_selector"), label = NULL, choices = c("Buscando scripts..." = ""), width = "100%")
                )
            )
        ),

        div(style = "border-top: 4px solid rgba(0,212,255, 1); margin: 30px 0;"),

        # 4. Contenido de los Tabs
        div(id = ns("main_content_wrapper"), class = "script-content-area",
            navset_hidden(id = ns("main_tabs"))
        ),

        # 5. Panel de Debug Dinámico
        uiOutput(ns("show_debug"))
    )
  )
}

mod_02_03_00_script_server <- function(id, vector_str_folder_tool_script = NULL, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. CONFIGURACIÓN INICIAL ---

    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)

    internal_vector_folder <- reactive({
      val <- if(is.function(vector_str_folder_tool_script)) vector_str_folder_tool_script() else vector_str_folder_tool_script
      val
    }) %>% shiny::debounce(150)

    rlist_control_btn <- mod_07_00_engine_control_server("main_switch",
                                                         show_debug = internal_show_debug,
                                                         show_ghost = FALSE)


    get_default_data <- function() {
      list(
        description = "*** RScience - Script ***",
        my_sys_time = Sys.time(),
        click_count = 0,
        mode = "unlock",
        is_done = FALSE,
        is_locked = FALSE,
        selected_script_tool = NULL,
        script_tool_folder_path = NULL,
        folder_exists = NULL,
        metadata = list()
      )
    }

    data_store <- do.call(reactiveValues, get_default_data())

    reset_data_store <- function() {
      defaults <- get_default_data()
      mapply(function(val, name) {
        data_store[[name]] <- val
      }, defaults, names(defaults))
    }

    # Validación estricta de selección
    is_done_val <- reactive({
      sel <- input$tool_selector
      !is.null(sel) && length(sel) > 0 && sel != ""
    })

    rlist_selected_script_folder <- reactive({
      pkg_path <- system.file("shiny", "fn03_tool_script", package = "Rscience2027")
      target_folder_path_absolute <- normalizePath(pkg_path, mustWork = FALSE)
      list(
        target_folder_path_absolute = target_folder_path_absolute,
        target_folder_exists = dir.exists(target_folder_path_absolute)
      )
    })

    # --- 2. IMPORTACIÓN DINÁMICA ---

    tmp_list <- reactive({
      req(internal_vector_folder())
      req(rlist_selected_script_folder())

      base_folder <- rlist_selected_script_folder()$target_folder_path_absolute

      res_list <- list()
      for (tool_script_name in internal_vector_folder()) {
        file_path <- file.path(base_folder, tool_script_name, "f01_shiny_show", "p00_script_info", "mod_special_script_info.R")

        if (file.exists(file_path)) {
          local_env <- new.env(parent = .GlobalEnv)
          try({
            source(file = file_path, local = local_env)
            res_list[[tool_script_name]] <- list(
              tool_script_name = tool_script_name,
              check_folder_exists = TRUE,
              check_file_file_path = TRUE,
              folder_path_absolute = normalizePath(file.path(base_folder, tool_script_name)),
              file_path_absolute = normalizePath(file_path),
              local_env = local_env,
              names_obj = names(local_env)
            )
          }, silent = TRUE)
        }
      }
      res_list
    })

    df_tmp <- reactive({
      internal_tmp_list <- tmp_list()
      req(internal_tmp_list)

      list_for_df <- lapply(internal_tmp_list, function(item) {
        data.frame(
          tool_name = item$tool_script_name,
          check_folder_exists = item$check_folder_exists,
          folder_path_absolute = item$folder_path_absolute,
          objects = paste(item$names_obj, collapse = ", "),
          stringsAsFactors = FALSE
        )
      })

      df_output <- do.call(rbind, list_for_df)
      rownames(df_output) <- NULL
      df_output
    })

    output$debug_df_tmp <- DT::renderDT({
      internal_df_tmp <- tryCatch(df_tmp(), error = function(e) NULL)
      if(is.null(internal_df_tmp)) internal_df_tmp <- data.frame(Status = "Waiting for scripts...")

      DT::datatable(internal_df_tmp, rownames = FALSE, options = list(pageLength = 5, dom = 'ftp', scrollX = TRUE))
    })

    # --- 3. GESTIÓN DE TABS Y UI ---

    observeEvent(tmp_list(), {
      internal_tmp_list <- tmp_list()
      choices <- names(internal_tmp_list)

      for (name in choices) {
        target_env <- internal_tmp_list[[name]]$local_env
        insertTab(
          inputId = "main_tabs",
          tabPanel(
            title = name, value = name,
            div(class = "p-3", target_env$mod_special_script_info_ui(ns(name)))
          )
        )
        target_env$mod_special_script_info_server(name)
      }

      updateRadioButtons(session, "tool_selector", choices = choices, selected = character(0))
    })

    observeEvent(input$tool_selector, {
      req(input$tool_selector != "")
      nav_select("main_tabs", selected = input$tool_selector)
      data_store$selected_script_tool <- input$tool_selector
      data_store$is_done <- TRUE
    })

    # --- 4. LÓGICA DEL MOTOR (SYNC) ---

    observeEvent(rlist_control_btn(), {
      state <- rlist_control_btn()$mode
      req(state, input$tool_selector)

      internal_rlist_selected_script_folder <- rlist_selected_script_folder()
      base_folder <- internal_rlist_selected_script_folder$target_folder_path_absolute
      the_tool <- input$tool_selector
      the_folder_relative <- file.path(base_folder, the_tool)
      the_folder_absolute <- normalizePath(the_folder_relative)
      the_folder_exists <- dir.exists(the_folder_absolute)

      done <- is_done_val()
      sel <- input$tool_selector
      to_freeze <- c("header_section", "selector_section")

      if (state == "lock") {
        if (!done) {
          showNotification("Please select a script before locking.", type = "warning")
          shinyWidgets::updateRadioGroupButtons(session, "script_switch-engine_mode", selected = "unlock")
        } else {
          lapply(to_freeze, shinyjs::addClass, class = "locked-disabled")
          shinyjs::addClass("main_content_wrapper", "locked-main-content")

          data_store$mode <- "lock"
          data_store$is_locked <- TRUE
          data_store$script_tool_folder_path <- the_folder_absolute
          data_store$folder_exists <- the_folder_exists


          data_store$metadata <- tmp_list()[[sel]]
        }
      }

      else if (state == "unlock") {
        lapply(to_freeze, shinyjs::removeClass, class = "locked-disabled")
        shinyjs::removeClass("main_content_wrapper", "locked-main-content")
        data_store$mode <- "unlock"
        data_store$is_locked <- FALSE
      }

      else if (state == "reset") {
        reset_data_store()
        updateRadioButtons(session, "tool_selector", selected = character(0))
        nav_select("main_tabs", selected = "default_none")
        shinyjs::delay(500, {
          shinyWidgets::updateRadioGroupButtons(session, "script_switch-engine_mode", selected = "unlock")
        })
      }
    }, ignoreInit = TRUE)

    # --- 5. RENDERS Y DEBUG ---



    output$debug_script <- listviewer::renderJsonedit({
      req(internal_show_debug())
      listviewer::jsonedit(listdata = reactiveValuesToList(data_store), mode = "text")
    })

    output$debug_control <- listviewer::renderJsonedit({
      req(internal_show_debug())
      flat_rlist_control_btn <- rlist_control_btn()
      listviewer::jsonedit(listdata = flat_rlist_control_btn, mode = "text")
    })

    output$show_debug <- renderUI({
      req(internal_show_debug())

      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 15px; margin-top: 20px;",

          # Título de la sección (ocupa todo el ancho)
          div(class = "section-label",
              style = "justify-content: flex-start !important; gap: 8px; margin-bottom: 15px; color: #ffab70;",
              icon("bug"), " Internal Debug - Script"),

          # Fila de columnas
          div(class = "row",

              # Columna Izquierda: Script
              div(class = "col-md-6",
                  div(style = "font-size: 0.75rem; color: #8b949e; margin-bottom: 5px; font-weight: 600;",
                      "SCRIPT STATE"),
                  listviewer::jsoneditOutput(ns("debug_script"), height = "auto")
              ),

              # Columna Derecha: Control
              div(class = "col-md-6",
                  div(style = "font-size: 0.75rem; color: #8b949e; margin-bottom: 5px; font-weight: 600;",
                      "CONTROL ENGINE"),
                  listviewer::jsoneditOutput(ns("debug_control"), height = "auto")
              )
          )
      )
    })





    # --- 6. RETORNO REACTIVO ---
    the_output <- reactive({ reactiveValuesToList(data_store) })

    return(the_output)
  })
}


