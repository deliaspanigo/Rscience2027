mod_special_settings_ui <- function(id) {
  ns <- NS(id)

  # Quitamos la lógica de addResourcePath de aquí, ya la hace el mod_04
  tagList(
    div(class = "rs-settings-pack settings-container",
        card(
          class = "rs-card-wrapper rs-coordinator-card",
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            uiOutput(ns("master_title_ui"))
          ),
          navset_pill(
            id = ns("main_tabs"),
            nav_panel(
              title = "Variables", # Simplificado para estabilidad
              value = "panel_vars",
              div(style = "padding-top: 15px;", uiOutput(ns("ui_step1")))
            ),
            nav_panel(
              title = "Levels",
              value = "panel_levels",
              div(style = "padding-top: 15px;", uiOutput(ns("ui_step2")))
            )
          )
        ),
        # Monitor de debug interno
        if(TRUE) uiOutput(ns("global_status_tag"))
    )
  )
}

mod_special_settings_server <- function(id, df_input, folder_path_tool_script, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_df_input <- reactive(if(is.function(df_input)) df_input() else df_input)
    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)



    # --- IMPORTANTE: YA NO HACEMOS SOURCE AQUÍ ---
    # El mod_04 ya cargó recursivamente sm01_var_selection.R y sm02_levels.R
    # en el entorno. Si los volvemos a cargar, rompemos la reactividad.

    # --- 1. RENDERIZADO DE UI ---
    output$ui_step1 <- renderUI({
      # Llamamos directamente a la función que ya existe en el entorno
      req(exists("SUB_mod_var_selection_ui"))
      SUB_mod_var_selection_ui(ns("step1"))
    })

    output$ui_step2 <- renderUI({
      req(exists("SUB_mod_levels_ui"))
      SUB_mod_levels_ui(ns("step2"))
    })

    # --- 2. INSTANCIAS DE SERVIDORES ---
    # Invocamos los servidores de los submódulos que ya están cargados
    res_vars <- SUB_mod_var_selection_server("step1", df_input = internal_df_input)

    # Reactivos de seguridad puente para el paso 2
    safe_rv <- reactive({
      req(res_vars()$is_locked)
      res_vars()$metadata$rv
    })

    safe_factor <- reactive({
      req(res_vars()$is_locked)
      res_vars()$metadata$factor
    })

    res_levels <- SUB_mod_levels_server(
      "step2",
      df_input   = df_input,
      var_rv     = safe_rv,
      var_factor = safe_factor
    )

    # --- 3. TÍTULO DINÁMICO ---
    output$master_title_ui <- renderUI({
      locked_1 <- isTRUE(res_vars()$is_locked)
      locked_2 <- isTRUE(res_levels()$is_locked)
      is_all_locked <- locked_1 && locked_2

      icon_tag  <- if(is_all_locked) icon("check-double") else icon("sliders")
      color_tag <- if(is_all_locked) "#198754" else "#2c3e50"

      div(class = "d-flex align-items-center",
          span(icon_tag, style = paste0("color: ", color_tag, "; margin-right: 12px; font-size: 1.2rem;")),
          span("FACTORIAL ANALYSIS SETUP", style="font-weight:800; color: #2c3e50;"),
          span("v.0.0.1", style="margin-left: 10px; font-size: 0.7rem; color: #adb5bd;")
      )
    })

    # --- 4. SALIDA CONSOLIDADA ---
    # Este es el retorno que leerá el mod_04 y finalmente tu App
    return(reactive({
      list(
        is_locked = isTRUE(res_vars()$is_locked && res_levels()$is_locked),
        metadata = list(
          selection = res_vars()$metadata$selection, # Para que coincida con tu print() en app.R
          vars      = res_vars(),
          levels    = res_levels()
        ),
        timestamp = Sys.time()
      )
    }))
  })
}
