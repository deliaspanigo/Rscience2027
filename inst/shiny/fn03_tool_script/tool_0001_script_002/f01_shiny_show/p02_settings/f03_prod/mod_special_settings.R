mod_special_DEBUG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Este uiOutput cargará todo lo que definiste en output$show_debug_external
    uiOutput(ns("debug_external"))
  )
}


mod_special_settings_ui <- function(id) {
  ns <- NS(id)

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
              title = "Variables",
              value = "panel_vars",
              div(style = "padding-top: 15px;", SUB_mod_var_selection_ui(ns("step1")))
            ),
            nav_panel(
              title = "Levels",
              value = "panel_levels",
              div(style = "padding-top: 15px;", SUB_mod_levels_ui(ns("step2")))
            )
          )
        ),
        # --- NUEVO MONITOR DE DEBUG GLOBAL ---
        uiOutput(ns("global_debug_ui"))
    )
  )
}

mod_special_settings_server <- function(id, df_input, folder_path_tool_script, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_df_input <- reactive(if(is.function(df_input)) df_input() else df_input)
    internal_show_debug <- reactive(if(is.function(show_debug)) show_debug() else show_debug)

    # --- 1. INSTANCIAS DE SUB-SERVIDORES ---
    rlist_vars <- SUB_mod_var_selection_server("step1", df_input = internal_df_input, show_debug = internal_show_debug())

    safe_rv     <- reactive({ req(rlist_vars()$is_locked); rlist_vars()$metadata$rv })
    safe_factor <- reactive({ req(rlist_vars()$is_locked); rlist_vars()$metadata$factor })

    rlist_levels <- SUB_mod_levels_server(
      "step2",
      df_input   = internal_df_input,
      var_rv     = safe_rv,
      var_factor = safe_factor,
      show_debug = internal_show_debug()
    )

    # --- 2. GENERACIÓN DE LISTA LIMPIA PARA REPRODUCIBILIDAD ---
    rlist_all_inputs <- reactive({
      # Eliminado el dput que hacía print molesto

      list(
        rv = list(
          detail = "Response variable",
          name = "var_name_rv",
          R_value = rlist_vars()$metadata$rv,
          str_R = as.character(rlist_vars()$metadata$rv),
          str_quarto = "_var_name_rv_"
        ),
        factor = list(
          detail = "The Factor",
          name = "var_name_factor",
          R_value = rlist_vars()$metadata$factor,
          str_R = as.character(rlist_vars()$metadata$factor),
          str_quarto = "_var_name_factor_"

        ),
        alpha_value = list(
          detail = "Alpha value number",
          name = "alpha_value",
          R_value = rlist_vars()$metadata$alpha,
          str_R = as.character(rlist_vars()$metadata$alpha),
          str_quarto = "_alpha_value_"

        ),
        vector_new_order_levels = list(
          detail = "Vector with levels in the new order",
          name = "vector_order_levels_new",
          R_value = rlist_levels()$data$nivel,
          # Sentencia con comillas simples y sin barras de escape visibles
          str_R = chartr('"', "'", paste(deparse(rlist_levels()$data$nivel), collapse = "")),
          str_quarto = "_vector_order_levels_new_"
        ),
        vector_color = list(
          detail = "Vector with hex colors",
          name = "vector_color_levels_new",
          R_value = rlist_levels()$data$color,
          str_R = chartr('"', "'", paste(deparse(rlist_levels()$data$color), collapse = "")),
          str_quarto = "_vector_color_levels_new_"
        )
      )
    })

    # --- 3. OBJETO DE SALIDA CONSOLIDADO ---
    the_output <- reactive({
      list(
        is_locked = isTRUE(rlist_vars()$is_locked && rlist_levels()$is_locked),
        metadata = list(
          vars   = rlist_vars(),
          levels = rlist_levels()
        ),
        timestamp = Sys.time(),
        list_clean = rlist_all_inputs()
      )
    })



    # --- 5. TÍTULO DINÁMICO ---
    output$master_title_ui <- renderUI({
      locked_all <- isTRUE(the_output()$is_locked)
      icon_tag  <- if(locked_all) icon("check-double") else icon("sliders")
      color_tag <- if(locked_all) "#00bc8c" else "#00d4ff"

      div(class = "d-flex align-items-center",
          span(icon_tag, style = paste0("color: ", color_tag, "; margin-right: 12px; font-size: 1.2rem;")),
          span("FACTORIAL ANALYSIS SETUP", style="font-weight:800; color: #ffffff;"),
          span("v.0.0.1", style="margin-left: 10px; font-size: 0.7rem; color: #566b7a;")
      )
    })

    output$global_json <- listviewer::renderJsonedit({
      req(internal_show_debug())
      listviewer::jsonedit(the_output(), mode = "view")
    })

    # --- 4. DEBUG GLOBAL ---
    output$global_debug_ui <- renderUI({
      req(internal_show_debug())
      div(class = "rs-card-wrapper mt-3",
          style = "border: 1px dashed #00d4ff; background: #0b1218;",
          h6("GLOBAL OUTPUT MONITOR (mod_special_settings)", style="color: #00d4ff; font-weight:800;"),
          listviewer::jsoneditOutput(ns("global_json"), height = "auto")
      )
    })

    #######
    output$json_external <- listviewer::renderJsonedit({
      req(internal_show_debug())
      listviewer::jsonedit(the_output(), mode = "view")
    })

    # --- 4. DEBUG GLOBAL ---
    output$debug_external <- renderUI({
      req(internal_show_debug())
      div(class = "rs-card-wrapper mt-3",
          style = "border: 1px dashed #00d4ff; background: #0b1218;",
          h6("GLOBAL OUTPUT MONITOR (mod_special_settings)", style="color: #00d4ff; font-weight:800;"),
          listviewer::jsoneditOutput(ns("json_external"), height = "auto")
      )
    })


    return(the_output)
  })
}
