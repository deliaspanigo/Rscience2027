# ==============================================================================
# MÓDULO: EXPLORADOR DE SCRIPTS RScience - v.0.0.2 (Engine Sync)
# ==============================================================================

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
                    selectInput(ns("tool_selector"), label = NULL, choices = NULL, width = "100%")
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
        uiOutput(ns("debug_panel_ui"))
    )
  )
}

mod_02_03_00_script_server <- function(id, vector_str_folder_tool_script = NULL, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. ESTADOS Y MOTOR ---
    engine_state <- mod_07_00_engine_control_server("script_switch", show_debug = show_debug)

    is_done    <- reactiveVal(FALSE)
    is_locked  <- reactiveVal(FALSE)
    tools_env  <- reactiveVal(list())

    # --- 2. IMPORTACIÓN DINÁMICA DE TABS ---
    observeEvent(vector_str_folder_tool_script(), {
      pkg_path <- system.file(package = "Rscience2027")
      if (pkg_path == "") pkg_path <- "inst"
      base_path <- file.path(pkg_path, "shiny", "fn03_tool_script")

      folders <- if(is.function(vector_str_folder_tool_script)) vector_str_folder_tool_script() else vector_str_folder_tool_script
      req(length(folders) > 0)

      tmp_list <- list()
      for (tool_name in folders) {
        folder_path <- file.path(base_path, tool_name)
        file_path <- file.path(folder_path, "f01_shiny_show", "p00_script_info", "mod_special_script_info.R")

        if (file.exists(file_path)) {
          local_env <- new.env(parent = .GlobalEnv)
          source(file_path, local = local_env)
          tmp_list[[tool_name]] <- list(tool_script_name = tool_name, folder_path = folder_path, local_env = local_env)

          insertTab(inputId = "main_tabs",
                    tabPanel(title = tool_name, value = tool_name,
                             div(class = "p-3", local_env$mod_special_script_info_ui(ns(tool_name)))))
          local_env$mod_special_script_info_server(tool_name)
        }
      }
      tools_env(tmp_list)
      updateSelectInput(session, "tool_selector", choices = c("Select a script..." = "", names(tmp_list)))
    }, ignoreInit = FALSE)

    # --- 3. LÓGICA DE BLOQUEO (SYNC CON ENGINE) ---
    observeEvent(engine_state(), {
      state <- engine_state()$mode
      current_sel <- input$tool_selector
      to_freeze <- c("header_section", "selector_section")

      if (state == "lock") {
        if (!is.null(current_sel) && current_sel != "") {
          lapply(to_freeze, function(x) shinyjs::addClass(id = x, class = "locked-disabled"))
          shinyjs::addClass(id = "main_content_wrapper", class = "locked-main-content")
          is_locked(TRUE); is_done(TRUE)
        } else {
          showNotification("Please select a script before locking.", type = "warning")
          shinyjs::delay(200, {
            shinyWidgets::updateRadioGroupButtons(session, "script_switch-engine_mode", selected = "unlock")
          })
        }
      } else {
        lapply(to_freeze, function(x) shinyjs::removeClass(id = x, class = "locked-disabled"))
        shinyjs::removeClass(id = "main_content_wrapper", class = "locked-main-content")
        is_locked(FALSE); is_done(FALSE)
      }
    })

    # --- 4. RENDERS ---
    output$script_header_ui <- renderUI({
      current <- input$tool_selector
      if (is_locked()) {
        div(class = "selection-header confirmed", span(icon("lock"), paste(" SCRIPT CONFIRMED:", current)), span(class = "header-id", "STATUS: LOCK"))
      } else if (!is.null(current) && current != "") {
        div(class = "selection-header active-selection", span(icon("lock-open"), paste(" SELECTED:", current)), span(class = "header-id", "STATUS: UNLOCK"))
      } else {
        div(class = "selection-header waiting-mode", span(icon("bolt"), " Waiting for script selection..."), span(class = "header-id", "STATUS: WAITING"))
      }
    })

    observeEvent(input$tool_selector, {
      req(input$tool_selector != "")
      nav_select("main_tabs", selected = input$tool_selector)
    })

    # --- 5. DEBUG PANEL (NUEVO) ---
    output$debug_panel_ui <- renderUI({
      req(show_debug)
      div(style = "margin-top: 20px; border-top: 1px dashed #444; padding-top: 10px;",
          h4(icon("bug"), "Script Engine Debugger", style="color: #ff9100; font-size: 0.9rem;"),
          listviewer::jsoneditOutput(ns("debug_json"), height = "250px")
      )
    })

    output$debug_json <- listviewer::renderJsonedit({
      # Mostramos exactamente lo mismo que el return
      listviewer::jsonedit(the_output(), mode = "text")
    })

    # Objeto reactivo final
    the_output <- reactive({
      list(
        is_done = is_done(),
        is_locked = is_locked(),
        selected = input$tool_selector,
        metadata = if(is_locked()) tools_env()[[input$tool_selector]] else NULL
      )
    })

    # --- 6. RETORNO ---
    return(the_output) # Importante: Retornamos el reactivo sin paréntesis
  })
}
