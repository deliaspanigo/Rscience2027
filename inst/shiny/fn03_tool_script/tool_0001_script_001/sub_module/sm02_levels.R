library(shiny)
library(colourpicker)
library(dplyr)
library(DT)
library(plotly)
library(shinyjs)

library(shiny)
library(colourpicker)
library(dplyr)
library(DT)
library(plotly)
library(shinyjs)

SUB_mod_levels_ui <- function(id) {
  ns <- NS(id)
  scope_id <- paste0("#", ns("module_container"))

  # CSS Encapsulado para el módulo
  css_custom <- paste0("
    ", scope_id, " .card-container { overflow: visible !important; position: relative; }
    ", scope_id, " .selectize-dropdown { z-index: 10000 !important; }
    ", scope_id, " .debug-panel-clean { background: #f8f9fa; color: #333; padding: 15px; border: 1px solid #ddd; border-radius: 8px; margin-bottom: 20px; font-size: 0.9rem; }
    ", scope_id, " .locked-overlay {
        position: absolute; top: 0; left: 0; width: 100%; height: 100%;
        background: rgba(220, 220, 220, 0.5); z-index: 2000; border-radius: 15px;
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
        uiOutput(ns("debug_ui")),

        # UI Dinámica para Botones y Contenido Principal
        uiOutput(ns("action_buttons_ui")),
        uiOutput(ns("main_content_ui"))
    )
  )
}

SUB_mod_levels_server <- function(id, df_input = reactive(NULL), var_rv = reactive(NULL), var_factor = reactive(NULL), show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status <- reactiveValues(locked = FALSE)

    # --- VALIDACIÓN DE DISPONIBILIDAD ---
    is_ready <- reactive({
      df    <- if (is.reactive(df_input)) df_input() else df_input
      v_rv  <- if (is.reactive(var_rv)) var_rv() else var_rv
      v_fac <- if (is.reactive(var_factor)) var_factor() else var_factor
      !is.null(df) && !is.null(v_rv) && v_rv != "" && !is.null(v_fac) && v_fac != ""
    })

    # --- BASE DE DATOS ---
    base_data <- reactive({
      req(is_ready())
      df    <- if (is.reactive(df_input)) df_input() else df_input
      v_rv  <- if (is.reactive(var_rv)) var_rv() else var_rv
      v_fac <- if (is.reactive(var_factor)) var_factor() else var_factor

      df_clean <- df %>% select(all_of(c(v_rv, v_fac))) %>% na.omit()
      lvls <- sort(unique(as.character(df_clean[[v_fac]])))
      list(df = df_clean, vx = v_fac, vy = v_rv, lvls = lvls)
    })

    # --- LÓGICA DE DUPLICADOS ---
    check_duplicates <- reactive({
      req(is_ready())
      lvls <- base_data()$lvls
      ordenes <- sapply(lvls, function(l) as.numeric(input[[paste0("pos_", l)]] %||% 0))
      dups <- ordenes[duplicated(ordenes) | duplicated(ordenes, fromLast = TRUE)]
      list(has_dups = length(dups) > 0, values = unique(dups))
    })

    # --- OBJETO DE SALIDA ---
    output_object <- reactive({
      req(is_ready())
      data_info <- base_data()
      df_config <- lapply(data_info$lvls, function(lv) {
        data.frame(
          nivel = lv,
          orden = as.numeric(input[[paste0("pos_", lv)]] %||% which(data_info$lvls == lv)),
          color = input[[paste0("col_", lv)]] %||% "#000000",
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% arrange(orden)

      list(data = df_config, is_done = status$locked, timestamp = Sys.time())
    })

    # --- UI: HEADER & BOTONES ---
    output$import_header <- renderUI({
      cls <- if(status$locked) "confirmed" else "active-selection"
      div(class = paste("selection-header", cls),
          span(icon(if(status$locked) "lock" else "cog"), " FACTOR ANALYZER STUDIO"),
          span(style="font-size: 0.8rem;", paste("Update:", format(Sys.time(), "%H:%M:%S"))))
    })

    output$action_buttons_ui <- renderUI({
      req(is_ready())
      div(class = "action-row-right",
          if(check_duplicates()$has_dups) span(style="color:red; font-weight:bold; margin-right:10px;", icon("exclamation-triangle"), "ORDEN DUPLICADO"),
          actionButton(ns("btn_import"), span(icon("check"), "Import"), class = "btn-success btn-pill-xl"),
          actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-warning btn-pill-xl"),
          actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-primary btn-pill-xl")
      )
    })

    # --- UI: CONTENIDO PRINCIPAL (CARTEL O EDITOR) ---
    output$main_content_ui <- renderUI({
      if (!is_ready()) {
        div(style = "padding: 100px 50px; text-align: center; background: #f8f9fa; border: 2px dashed #ccc; border-radius: 20px; margin: 20px;",
            icon("layer-group", class = "fa-4x", style = "color: #adb5bd; margin-bottom: 20px;"),
            h3("Esperando configuración", style = "color: #6c757d; font-weight: 700;"),
            p("Seleccione las variables en el paso anterior para activar este panel.", style = "color: #999;")
        )
      } else {
        fluidRow(
          column(5, div(id = ns("card_settings_wrapper"), class = "card-container",
                        div(class = "locked-overlay", div(class = "lock-icon", icon("lock"))),
                        div(style = "background: #ffffff; padding: 25px; border-radius: 15px; border: 1px solid #e0e0e0; min-height: 500px;",
                            tabsetPanel(
                              tabPanel("Settings", div(style = "padding: 15px; max-height: 400px; overflow-y: auto;", uiOutput(ns("settings_ui")))),
                              tabPanel("Summary", div(style = "padding: 15px;", DTOutput(ns("summary_table"))))
                            )))),
          column(7, div(id = ns("card_plot_wrapper"), class = "card-container",
                        div(class = "locked-overlay", div(class = "lock-icon", icon("lock"))),
                        div(style = "background: #ffffff; padding: 25px; border-radius: 15px; border: 1px solid #e0e0e0; min-height: 500px;",
                            plotlyOutput(ns("preview_plot"), height = "450px"))))
        )
      }
    })

    # --- RENDERS & OBSERVERS ---
    output$settings_ui <- renderUI({
      req(base_data())
      data <- base_data()
      dup_info <- check_duplicates()
      lapply(seq_along(data$lvls), function(i) {
        lv <- data$lvls[i]
        curr_pos <- as.numeric(input[[paste0("pos_", lv)]] %||% i)
        is_err <- curr_pos %in% dup_info$values
        div(class = if(is_err) "row-error" else "", style = "padding:10px; border-bottom: 1px solid #eee;",
            fluidRow(
              column(4, strong(lv)),
              column(4, selectInput(ns(paste0("pos_", lv)), NULL, choices = 1:length(data$lvls), selected = curr_pos)),
              column(4, colourInput(ns(paste0("col_", lv)), NULL, value = rainbow(length(data$lvls))[i]))
            ))
      })
    })

    output$preview_plot <- renderPlotly({
      req(is_ready(), output_object())
      conf <- output_object()$data
      data_info <- base_data()
      plot_df <- data_info$df
      plot_df[[data_info$vx]] <- factor(plot_df[[data_info$vx]], levels = conf$nivel)

      plot_ly(plot_df, x = ~get(data_info$vx), y = ~get(data_info$vy), color = ~get(data_info$vx),
              colors = setNames(conf$color, conf$nivel), type = "box")
    })

    output$summary_table <- renderDT({
      req(is_ready())
      datatable(output_object()$data, rownames = FALSE, options = list(dom = 't'))
    })

    observeEvent(input$btn_import, {
      status$locked <- TRUE
      runjs(sprintf("$('#%s').addClass('is-locked');", ns("card_settings_wrapper")))
      runjs(sprintf("$('#%s').addClass('is-locked');", ns("card_plot_wrapper")))
    })

    observeEvent(input$btn_edit, {
      status$locked <- FALSE
      runjs(sprintf("$('#%s').removeClass('is-locked');", ns("card_settings_wrapper")))
      runjs(sprintf("$('#%s').removeClass('is-locked');", ns("card_plot_wrapper")))
    })

    observeEvent(input$btn_reset, { session$reload() })

    return(output_object)
  })
}
