# ==============================================================================
# COORDINADOR: PACK SETTINGS (VARS + LEVELS) - RScience v.0.0.1
# ==============================================================================

mod_special_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),

    # Monitor de estado (Solo se ve si show_debug = TRUE)
    uiOutput(ns("global_status_tag")),
    uiOutput(ns("debug_container")),

    card(
      full_screen = TRUE,
      card_header(
        uiOutput(ns("master_title_ui"))
      ),
      navset_pill(
        id = ns("main_tabs"),
        nav_panel(
          title = uiOutput(ns("label_p1"), inline = TRUE),
          value = "panel_vars",
          # Contenedor dinámico para evitar errores si el source tarda
          uiOutput(ns("ui_step1"))
        ),
        nav_panel(
          title = uiOutput(ns("label_p2"), inline = TRUE),
          value = "panel_levels",
          uiOutput(ns("ui_step2"))
        )
      )
    )
  )
}

mod_special_settings_server <- function(id, df_input, folder_path_tool_script, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- CARGA DINÁMICA DE SUBMÓDULOS ---
    # Esto corrige el error de "Reactive Context"
    observe({
      req(folder_path_tool_script())
      base_path <- folder_path_tool_script()

      # Definimos las rutas de los submódulos
      files_to_load <- c(
        "sm01_var_selection.R",
        "sm02_levels.R"
      )

      for(f in files_to_load) {
        # Intentamos buscar en el sistema de archivos del paquete
        f_path <- system.file(base_path, "f03_soft_opts", "f04_settings", "f03_prod", "sub_module", f, package = "Rscience2027")

        # Si no lo encuentra (dev mode), buscamos ruta relativa
        if(f_path == "") {
          f_path <- file.path(base_path, "f03_soft_opts", "f04_settings", "f03_prod", "sub_module", f)
        }

        if(file.exists(f_path)) {
          source(f_path, local = TRUE)
        } else {
          warning(paste("No se pudo encontrar el archivo:", f))
        }
      }

      # Renderizamos la UI de los submódulos una vez cargados los archivos
      output$ui_step1 <- renderUI({ SUB_mod_var_selection_ui(ns("step1")) })
      output$ui_step2 <- renderUI({ SUB_mod_levels_ui(ns("step2")) })
    })

    # --- 1. INSTANCIAS DE SERVIDORES ---
    res_vars <- SUB_mod_var_selection_server("step1", df_input = df_input)

    # Puentes reactivos seguros
    safe_rv     <- reactive({ req(res_vars()$is_locked); res_vars()$metadata$rv })
    safe_factor <- reactive({ req(res_vars()$is_locked); res_vars()$metadata$factor })

    res_levels <- SUB_mod_levels_server(
      "step2",
      df_input   = df_input,
      var_rv     = safe_rv,
      var_factor = safe_factor
    )

    # --- 2. RENDERIZADO DE INTERFAZ DINÁMICA ---

    output$master_title_ui <- renderUI({
      is_all_locked <- isTRUE(res_vars()$is_locked && res_levels()$is_locked)
      icon_tag <- if(is_all_locked) icon("lock") else icon("lock-open")
      color_tag <- if(is_all_locked) "#28a745" else "#fd7e14"

      div(class = "d-flex align-items-center",
          span(icon_tag, style = paste0("color: ", color_tag, "; margin-right: 12px; font-size: 1.2rem;")),
          span("FACTORIAL ANALYSIS SETTINGS - script001", style="font-weight:800; color: #2c3e50;")
      )
    })

    output$label_p1 <- renderUI({
      is_locked <- res_vars()$is_locked
      icon_tag <- if(isTRUE(is_locked)) icon("lock") else icon("lock-open")
      tagList("1. Variables", span(icon_tag, style = paste0("color: ", if(isTRUE(is_locked)) "#28a745" else "#fd7e14", "; margin-left: 10px;")))
    })

    output$label_p2 <- renderUI({
      is_locked <- res_levels()$is_locked
      icon_tag <- if(isTRUE(is_locked)) icon("lock") else icon("lock-open")
      tagList("2. Levels", span(icon_tag, style = paste0("color: ", if(isTRUE(is_locked)) "#28a745" else "#fd7e14", "; margin-left: 10px;")))
    })

    # --- 3. SALIDA Y DEBUG ---
    full_output <- reactive({
      list(
        "is_all_locked" = isTRUE(res_vars()$is_locked && res_levels()$is_locked),
        "metadata" = list("vars" = res_vars(), "levels" = res_levels())
      )
    })

    output$debug_container <- renderUI({
      req(show_debug)
      div(style = "margin-top: 10px; max-height: 200px; overflow-y: auto;",
          listviewer::jsoneditOutput(ns("global_debug_json"), height = "auto"))
    })

    output$global_debug_json <- listviewer::renderJsonedit({
      listviewer::jsonedit(full_output(), mode = "text")
    })

    return(full_output)
  })
}
