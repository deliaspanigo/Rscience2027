# ==============================================================================
# COORDINADOR: PACK SETTINGS (VARS + LEVELS) - RScience v.0.0.1
# ==============================================================================

mod_PACK_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),

    # --- 1. MONITOR GLOBAL (ESTILO CONSOLA) ---
    #card(
      uiOutput(ns("global_status_tag")),

      div(style = "margin-top: 10px; max-height: 200px; overflow-y: auto;",
          listviewer::jsoneditOutput(ns("global_debug_json"), height = "auto")
      ),
      # card_body(
      #   div(style = "background: #1e1e1e; border-radius: 8px; padding: 5px;",
      #       listviewer::jsoneditOutput(ns("global_debug_json"), height = "150px")
      #   )
      # )
   # ),

    # --- 2. WORKFLOW CARD CON MASTER LOCK EN TÍTULO ---
    card(
      full_screen = TRUE,
      card_header(
        uiOutput(ns("master_title_ui")) # Título con candado dinámico
      ),

      navset_pill(
        id = ns("main_tabs"),

        # TAB 1: VARIABLE SELECTION
        nav_panel(
          title = uiOutput(ns("label_p1"), inline = TRUE),
          value = "panel_vars",
          SUB_mod_var_selection_ui(ns("step1"))
        ),

        # TAB 2: LEVELS STUDIO
        nav_panel(
          title = uiOutput(ns("label_p2"), inline = TRUE),
          value = "panel_levels",
          SUB_mod_levels_ui(ns("step2"))
        )
      )
    )
  )
}

mod_PACK_settings_server <- function(id, df_input, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. INSTANCIAS DE SUBMÓDULOS ---
    res_vars <- SUB_mod_var_selection_server("step1", df_input = df_input)

    # Puentes seguros
    safe_rv     <- reactive({ req(res_vars()$is_locked); res_vars()$metadata$rv })
    safe_factor <- reactive({ req(res_vars()$is_locked); res_vars()$metadata$factor })

    res_levels <- SUB_mod_levels_server(
      "step2",
      df_input   = df_input,
      var_rv     = safe_rv,
      var_factor = safe_factor
    )

    # --- 2. LÓGICA DE ICONOS Y TÍTULOS ---

    # Título Principal (Master Lock)
    output$master_title_ui <- renderUI({
      # Bloqueado solo si AMBOS están cerrados
      is_all_locked <- isTRUE(res_vars()$is_locked && res_levels()$is_locked)

      icon_tag <- if(is_all_locked) icon("lock") else icon("lock-open")
      color_tag <- if(is_all_locked) "#28a745" else "#fd7e14"

      div(class = "d-flex align-items-center",
          span(icon_tag, style = paste0("color: ", color_tag, "; margin-right: 12px; font-size: 1.2rem;")),
          span("FACTORIAL ANALYSIS SETTINGS", style="font-weight:800; color: #2c3e50;")
      )
    })

    # Etiquetas de Tabs (Iconos a la derecha)
    make_tab_label <- function(is_locked, text) {
      icon_tag <- if(isTRUE(is_locked)) icon("lock") else icon("lock-open")
      color_tag <- if(isTRUE(is_locked)) "#28a745" else "#fd7e14"

      tagList(
        text,
        span(icon_tag, style = paste0("color: ", color_tag, "; margin-left: 10px; font-size: 0.9rem;"))
      )
    }

    output$label_p1 <- renderUI({ make_tab_label(res_vars()$is_locked, "1. Variables & Alpha") })
    output$label_p2 <- renderUI({ make_tab_label(res_levels()$is_locked, "2. Levels & Style") })

    # --- 3. MONITORING & DEBUG ---
    full_output <- reactive({
      list(
        "is_all_locked" = isTRUE(res_vars()$is_locked && res_levels()$is_locked),
        "steps" = list("p1" = res_vars(), "p2" = res_levels())
      )
    })

    output$global_debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      listviewer::jsonedit(full_output(), mode = "text")
    })

    output$global_status_tag <- renderUI({
      req(show_debug)
      ready <- isTRUE(res_vars()$is_locked && res_levels()$is_locked)
      span(class = if(ready) "badge bg-success" else "badge bg-warning",
           if(ready) "SYSTEM READY" else "AWAITING CONFIG")
    })

    return(full_output)
  })
}
