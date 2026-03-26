library(shiny)
library(colourpicker)
library(dplyr)
library(DT)
library(plotly)
library(shinyjs)

# ==============================================================================
# UI: FACTOR LEVELS STUDIO - RScience v.0.0.1
# ==============================================================================

SUB_mod_levels_ui <- function(id) {
  ns <- NS(id)
  scope_id <- paste0("#", ns("module_container"))

  # CSS Encapsulado y corregido
  css_custom <- paste0("
    ", scope_id, " .card-container { overflow: visible !important; position: relative; transition: all 0.3s ease; }
    ", scope_id, " .row { margin-left: 0; margin-right: 0; } /* Elimina scroll horizontal */

    /* Botón Maximizar dentro de la Card */
    ", scope_id, " .btn-maximize {
        position: absolute; top: 15px; right: 15px; z-index: 5000;
        background: #f8f9fa; border: 1px solid #ddd; border-radius: 50%;
        width: 35px; height: 35px; display: flex; align-items: center;
        justify-content: center; color: #6c757d; box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    }
    ", scope_id, " .btn-maximize:hover { background: #e9ecef; color: #007bff; cursor: pointer; text-decoration: none; }

    /* Clase Full Screen */
    .full-screen-card {
        position: fixed !important; top: 0 !important; left: 0 !important;
        width: 100vw !important; height: 100vh !important; z-index: 9999 !important;
        background: white !important; padding: 40px !important; border-radius: 0 !important;
    }

    ", scope_id, " .selectize-dropdown { z-index: 10000 !important; }
    ", scope_id, " .locked-overlay {
        position: absolute; top: 0; left: 0; width: 100%; height: 100%;
        background: rgba(230, 230, 230, 0.6); z-index: 2000; border-radius: 15px;
        display: flex; justify-content: center; align-items: center;
        visibility: hidden; opacity: 0; transition: all 0.3s ease;
    }
    ", scope_id, " .is-locked .locked-overlay { visibility: visible; opacity: 1; }
    ", scope_id, " .lock-icon { font-size: 5rem; color: #28a745; filter: drop-shadow(0 0 5px white); }
    ", scope_id, " .btn-pill-xl { border-radius: 50px !important; padding: 10px 25px !important; font-weight: 700 !important; text-transform: uppercase; border: none !important; box-shadow: 0 4px 10px rgba(0,0,0,0.1); }
    ", scope_id, " .action-row-right { display: flex; gap: 10px; justify-content: flex-end; margin-bottom: 20px; align-items: center; }
    ", scope_id, " .selection-header { padding: 15px 25px; border-radius: 12px; margin-bottom: 20px; font-weight: 800; display: flex; justify-content: space-between; align-items: center; }
    ", scope_id, " .confirmed { background: #d4edda; color: #155724; border-left: 8px solid #28a745; }
    ", scope_id, " .active-selection { background: #fff3cd; color: #856404; border-left: 8px solid #ffc107; }
  ")

  tagList(
    tags$head(tags$style(HTML(css_custom))),
    useShinyjs(),
    div(id = ns("module_container"),
        uiOutput(ns("import_header")),
        uiOutput(ns("action_buttons_ui")),
        uiOutput(ns("main_content_ui")),
        div(style = "margin-top: 20px;",
            listviewer::jsoneditOutput(ns("debug_json"), height = "auto"))
    )
  )
}

# ==============================================================================
# SERVER: FACTOR LEVELS STUDIO - RScience v.0.0.1
# ==============================================================================

SUB_mod_levels_server <- function(id, df_input = reactive(NULL), var_rv = reactive(NULL), var_factor = reactive(NULL), show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. ESTADO INICIAL Y SALIDA ESTRICTA ---
    list_default <- list(
      "details"    = "*** Factor Levels Configuration - RScience ***",
      "is_done"    = FALSE,
      "is_locked"  = FALSE,
      "time_stamp" = Sys.time(),
      "data"       = NULL
    )

    status <- reactiveValues(locked = FALSE)
    output_rv <- reactiveValues(val = list_default)

    # --- 2. VALIDACIÓN DE DISPONIBILIDAD ---
    is_ready <- reactive({
      df    <- if (is.reactive(df_input)) df_input() else df_input
      v_rv  <- if (is.reactive(var_rv)) var_rv() else var_rv
      v_fac <- if (is.reactive(var_factor)) var_factor() else var_factor
      !is.null(df) && !is.null(v_rv) && v_rv != "" && !is.null(v_fac) && v_fac != ""
    })

    base_data <- reactive({
      req(is_ready())
      df    <- if (is.reactive(df_input)) df_input() else df_input
      v_rv  <- if (is.reactive(var_rv)) var_rv() else var_rv
      v_fac <- if (is.reactive(var_factor)) var_factor() else var_factor
      df_clean <- df %>% select(all_of(c(v_rv, v_fac))) %>% na.omit()
      lvls <- sort(unique(as.character(df_clean[[v_fac]])))
      list(df = df_clean, vx = v_fac, vy = v_rv, lvls = lvls)
    })

    # --- 3. LÓGICA DE DUPLICADOS ---
    check_duplicates <- reactive({
      req(is_ready())
      lvls <- base_data()$lvls
      vals <- lapply(lvls, function(l) input[[paste0("pos_", l)]])
      if (any(sapply(vals, is.null))) return(list(has_dups = FALSE, values = c()))

      ordenes <- as.numeric(unlist(vals))
      dups <- ordenes[duplicated(ordenes) | duplicated(ordenes, fromLast = TRUE)]
      list(has_dups = length(dups) > 0, values = unique(dups))
    })

    # --- 4. CONFIGURACIÓN EN TIEMPO REAL ---
    current_config <- reactive({
      req(is_ready())
      data_info <- base_data()
      lapply(seq_along(data_info$lvls), function(i) {
        lv <- data_info$lvls[i]
        data.frame(
          nivel = lv,
          orden = as.numeric(input[[paste0("pos_", lv)]] %||% i),
          color = input[[paste0("col_", lv)]] %||% "#000000",
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% arrange(orden)
    })

    # --- 5. EVENTOS (MAXIMIZAR, IMPORT, EDIT) ---

    # Maximizar Settings
    observeEvent(input$max_settings, {
      shinyjs::toggleClass(id = "card_settings_wrapper", class = "full-screen-card")
      icon_state <- if (input$max_settings %% 2 == 0) "expand" else "compress"
      updateActionLink(session, "max_settings", icon = icon(icon_state))
    })

    # Maximizar Plot
    observeEvent(input$max_plot, {
      shinyjs::toggleClass(id = "card_plot_wrapper", class = "full-screen-card")
      shinyjs::runjs("setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 300);")
      icon_state <- if (input$max_plot %% 2 == 0) "expand" else "compress"
      updateActionLink(session, "max_plot", icon = icon(icon_state))
    })

    observeEvent(input$btn_import, {
      req(!check_duplicates()$has_dups)
      status$locked <- TRUE
      output_rv$val <- list(
        "details"    = "*** Factor Levels Configuration - RScience ***",
        "is_done"    = TRUE,
        "is_locked"  = TRUE,
        "time_stamp" = Sys.time(),
        "data"       = current_config()
      )
      runjs(sprintf("$('#%s').addClass('is-locked');", ns("card_settings_wrapper")))
      runjs(sprintf("$('#%s').addClass('is-locked');", ns("card_plot_wrapper")))
    })

    observeEvent(input$btn_edit, {
      status$locked <- FALSE
      output_rv$val <- list_default
      runjs(sprintf("$('#%s').removeClass('is-locked');", ns("card_settings_wrapper")))
      runjs(sprintf("$('#%s').removeClass('is-locked');", ns("card_plot_wrapper")))
    })

    observeEvent(input$btn_reset, { session$reload() })

    # --- 6. RENDERS DE LA UI ---
    output$import_header <- renderUI({
      cls <- if(status$locked) "confirmed" else "active-selection"
      div(class = paste("selection-header", cls),
          span(icon(if(status$locked) "lock" else "palette"), " FACTOR LEVELS STUDIO"),
          span(style="font-size: 0.8rem; opacity:0.7;", format(Sys.time(), "%H:%M:%S")))
    })

    output$action_buttons_ui <- renderUI({
      req(is_ready())
      div(class = "action-row-right",
          if(check_duplicates()$has_dups) span(style="color:#dc3545; font-weight:bold; margin-right:15px;", icon("exclamation-circle"), "ORDEN DUPLICADO"),
          actionButton(ns("btn_import"), span(icon("check"), "Accept"), class = "btn-success btn-pill-xl"),
          actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-warning btn-pill-xl"),
          actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-primary btn-pill-xl")
      )
    })

    output$main_content_ui <- renderUI({
      if (!is_ready()) {
        div(style = "padding: 80px 40px; text-align: center; background: #f8f9fa; border: 2px dashed #ccc; border-radius: 20px; margin: 20px;",
            icon("layer-group", class = "fa-4x", style = "color: #adb5bd; margin-bottom: 20px;"),
            h3("Waiting for Variable Selection", style = "color: #6c757d; font-weight: 700;"))
      } else {
        fluidRow(
          column(6, div(id = ns("card_settings_wrapper"), class = "card-container",
                        actionLink(ns("max_settings"), label = NULL, icon = icon("expand"), class = "btn-maximize"),
                        div(class = "locked-overlay", div(class = "lock-icon", icon("lock"))),
                        div(style = "background: white; padding: 25px; border-radius: 15px; border: 1px solid #ddd; min-height: 500px; height: 100%;",
                            tabsetPanel(
                              tabPanel("Settings", div(style = "max-height: 400px; overflow-y: auto;", uiOutput(ns("settings_ui")))),
                              tabPanel("Summary", DTOutput(ns("summary_table")))
                            )))),
          column(6, div(id = ns("card_plot_wrapper"), class = "card-container",
                        actionLink(ns("max_plot"), label = NULL, icon = icon("expand"), class = "btn-maximize"),
                        div(class = "locked-overlay", div(class = "lock-icon", icon("lock"))),
                        div(style = "background: white; padding: 25px; border-radius: 15px; border: 1px solid #ddd; min-height: 500px; height: 100%;",
                            plotlyOutput(ns("preview_plot"), height = "450px"))))
        )
      }
    })

    # --- 7. RENDERS DE CONTENIDO ---
    output$settings_ui <- renderUI({
      req(base_data())
      data <- base_data()
      lapply(seq_along(data$lvls), function(i) {
        lv <- data$lvls[i]
        div(style = "padding:10px; border-bottom: 1px solid #eee;",
            fluidRow(
              column(4, strong(lv)),
              column(4, selectInput(ns(paste0("pos_", lv)), NULL, choices = 1:length(data$lvls), selected = i)),
              column(4, colourInput(ns(paste0("col_", lv)), NULL, value = rainbow(length(data$lvls))[i]))
            ))
      })
    })

    output$preview_plot <- renderPlotly({
      req(is_ready(), current_config())
      conf <- current_config()
      data_info <- base_data()
      plot_df <- data_info$df
      plot_df[[data_info$vx]] <- factor(plot_df[[data_info$vx]], levels = conf$nivel)
      plot_ly(plot_df, x = ~get(data_info$vx), y = ~get(data_info$vy), color = ~get(data_info$vx),
              colors = setNames(conf$color, conf$nivel), type = "box")
    })

    output$summary_table <- renderDT({
      req(current_config())
      datatable(current_config(), rownames = FALSE, options = list(dom = 't', ordering = FALSE))
    })

    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      listviewer::jsonedit(listdata = output_rv$val, mode = "view")
    })

    return(reactive({ output_rv$val }))
  })
}
