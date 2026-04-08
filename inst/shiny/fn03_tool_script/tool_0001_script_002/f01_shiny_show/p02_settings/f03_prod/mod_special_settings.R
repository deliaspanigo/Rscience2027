# ==============================================================================
# COORDINADOR: PACK SETTINGS (VARS + LEVELS) - RScience v.0.0.1
# ==============================================================================

mod_special_settings_ui <- function(id) {
  ns <- NS(id)

  # Carga de recursos (Igual que en los submódulos para asegurar redundancia)
  css_folder <- system.file("www", "css", package = "Rscience2027")
  if (css_folder == "") css_folder <- "www/css"
  try(addResourcePath("RS-STYLES", normalizePath(css_folder)), silent = TRUE)

  tagList(
    tags$head(
      useShinyjs(),
      tags$link(rel = "stylesheet", type = "text/css",
                href = paste0("RS-STYLES/style_000.css?v=", as.numeric(Sys.time())))
    ),

    # Contenedor Maestro con clase de Pack
    div(class = "rs-settings-pack settings-container",

        # Monitor de estado (Solo debug)
        uiOutput(ns("global_status_tag")),
        uiOutput(ns("debug_container")),

        # Card de bslib integrada al diseño RS
        card(
          class = "rs-card-wrapper rs-coordinator-card", # Usamos la clase del pack
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            uiOutput(ns("master_title_ui"))
          ),

          navset_pill(
            id = ns("main_tabs"),
            nav_panel(
              title = uiOutput(ns("label_p1"), inline = TRUE),
              value = "panel_vars",
              div(style = "padding-top: 15px;", uiOutput(ns("ui_step1")))
            ),
            nav_panel(
              title = uiOutput(ns("label_p2"), inline = TRUE),
              value = "panel_levels",
              div(style = "padding-top: 15px;", uiOutput(ns("ui_step2")))
            )
          )
        )
    )
  )
}

mod_special_settings_server <- function(id, df_input, folder_path_tool_script, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- CARGA DINÁMICA DE SUBMÓDULOS (Mantener igual) ---
    observe({
      req(folder_path_tool_script())
      base_path <- folder_path_tool_script()
      files_to_load <- c("sm01_var_selection.R", "sm02_levels.R")

      for(f in files_to_load) {
        f_path <- system.file(base_path, "f03_soft_opts", "f04_settings", "f03_prod", "sub_module", f, package = "Rscience2027")
        if(f_path == "") f_path <- file.path(base_path, "f03_soft_opts", "f04_settings", "f03_prod", "sub_module", f)
        if(file.exists(f_path)) source(f_path, local = TRUE)
      }

      output$ui_step1 <- renderUI({ SUB_mod_var_selection_ui(ns("step1")) })
      output$ui_step2 <- renderUI({ SUB_mod_levels_ui(ns("step2")) })
    })

    # --- 1. INSTANCIAS DE SERVIDORES ---
    res_vars <- SUB_mod_var_selection_server("step1", df_input = df_input)

    safe_rv     <- reactive({ req(res_vars()$is_locked); res_vars()$metadata$rv })
    safe_factor <- reactive({ req(res_vars()$is_locked); res_vars()$metadata$factor })

    res_levels <- SUB_mod_levels_server(
      "step2",
      df_input   = df_input,
      var_rv     = safe_rv,
      var_factor = safe_factor
    )

    # --- 2. RENDERIZADO DE INTERFAZ (Sincronizado con Pack) ---

    output$master_title_ui <- renderUI({
      is_all_locked <- isTRUE(res_vars()$is_locked && res_levels()$is_locked)

      # Colores semánticos del pack: Verde (#198754) vs Naranja/Gris (#6c757d)
      icon_tag  <- if(is_all_locked) icon("check-double") else icon("sliders")
      color_tag <- if(is_all_locked) "#198754" else "#2c3e50"

      div(class = "d-flex align-items-center",
          span(icon_tag, style = paste0("color: ", color_tag, "; margin-right: 12px; font-size: 1.2rem;")),
          span("FACTORIAL ANALYSIS SETUP", style="font-weight:800; color: #2c3e50; letter-spacing: 0.5px;"),
          span("v.0.0.1", style="margin-left: 10px; font-size: 0.7rem; color: #adb5bd; vertical-align: middle;")
      )
    })

    # Labels de las pestañas con indicadores de bloqueo
    output$label_p1 <- renderUI({
      locked <- isTRUE(res_vars()$is_locked)
      icon_tag <- if(locked) icon("lock") else icon("circle-dot")
      col_tag  <- if(locked) "#198754" else "#ffc107" # Verde vs Amarillo Activo

      tagList(
        span("1. VARIABLES", style = "font-weight: 700; font-size: 0.8rem;"),
        span(icon_tag, style = paste0("color: ", col_tag, "; margin-left: 8px;"))
      )
    })

    output$label_p2 <- renderUI({
      locked <- isTRUE(res_levels()$is_locked)
      icon_tag <- if(locked) icon("lock") else icon("circle-dot")
      col_tag  <- if(locked) "#198754" else "#ffc107"

      tagList(
        span("2. LEVELS STUDIO", style = "font-weight: 700; font-size: 0.8rem;"),
        span(icon_tag, style = paste0("color: ", col_tag, "; margin-left: 8px;"))
      )
    })

    # --- 3. SALIDA Y DEBUG (Igual) ---
    full_output <- reactive({
      list(
        "is_all_locked" = isTRUE(res_vars()$is_locked && res_levels()$is_locked),
        "metadata" = list("vars" = res_vars(), "levels" = res_levels())
      )
    })

    output$debug_container <- renderUI({
      req(show_debug)
      div(class = "rs-card-wrapper", style = "margin-bottom: 15px; padding: 10px; background: #f8f9fa;",
          listviewer::jsoneditOutput(ns("global_debug_json"), height = "auto"))
    })

    output$global_debug_json <- listviewer::renderJsonedit({
      listviewer::jsonedit(full_output(), mode = "view")
    })

    return(full_output)
  })
}
