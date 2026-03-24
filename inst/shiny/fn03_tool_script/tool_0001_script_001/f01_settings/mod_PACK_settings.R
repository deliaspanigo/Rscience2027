# ==============================================================================
# PACK: COORDINADOR CON DEBUG MULTI-TAB (SIN ERRORES)
# ==============================================================================

mod_PACK_settings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    navset_card_pill(
      id = ns("main_tabs"),
      title = "Factor Analysis Workflow",

      nav_panel(
        title = "1. Variables",
        value = "panel_vars",
        icon = icon("table"),
        SUB_mod_var_selection_ui(ns("step1"))
      ),

      nav_panel(
        title = "2. Levels & Style",
        value = "panel_levels",
        icon = icon("palette"),
        SUB_mod_levels_ui(ns("step2"))
      )
    )
  )
}

mod_PACK_settings_server <- function(id, df_input, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- EJECUCIÓN DEL SUBMÓDULO 1 ---
    # Este módulo ahora solo bloquea su propia UI, no manda órdenes al padre
    res_vars <- SUB_mod_var_selection_server("step1", df_input = df_input)

    # --- EJECUCIÓN DEL SUBMÓDULO 2 ---
    # Se alimenta de lo que el usuario elija en el 1, pero no lo fuerza a saltar
    res_levels <- SUB_mod_levels_server(
      "step2",
      df_input = df_input,
      var_rv = reactive({ res_vars()$rv }),
      var_factor = reactive({ res_vars()$factor })
    )

    # ==========================================================================
    # IMPORTANTE: HE ELIMINADO EL observeEvent(res_vars()$is_done, ...)
    # Ahora el cambio de pestaña es estrictamente manual por el usuario.
    # ==========================================================================

    # Panel de Debug (opcional)
    output$d_sm01 <- renderPrint({ res_vars() })
    output$d_sm02 <- renderPrint({ res_levels() })

    return(reactive({
      list(vars = res_vars(), levels = res_levels())
    }))
  })
}
