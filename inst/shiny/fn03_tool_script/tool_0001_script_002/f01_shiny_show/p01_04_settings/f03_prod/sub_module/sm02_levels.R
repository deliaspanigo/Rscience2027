library(shiny)
library(colourpicker)
library(dplyr)
library(DT)
library(plotly)
library(shinyjs)
library(listviewer)

# ==============================================================================
# UI: FACTOR LEVELS STUDIO - RScience v.0.0.1 (SELF-CONTAINED CSS)
# ==============================================================================

SUB_mod_levels_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # CSS INYECTADO DIRECTAMENTE
    tags$head(
      tags$style(HTML(paste0("
        /* Contenedor principal estilo RScience Dark */
        .rs-settings-pack { background: #0b1218; padding: 15px; border-radius: 10px; font-family: 'Inter', sans-serif; color: white; }

        /* Cards para configuración y gráfico */
        .rs-card-wrapper {
          position: relative; background: #1a262f; border: 1px solid #2a3b47;
          border-radius: 8px; padding: 15px; margin-bottom: 15px;
        }

        /* Headers de estado */
        .rs-status-header { padding: 12px; border-radius: 6px; font-weight: bold; display: flex; justify-content: space-between; margin-bottom: 10px; }
        .status-active { background: rgba(0, 212, 255, 0.1); color: #00d4ff; border-left: 4px solid #00d4ff; }
        .status-confirmed { background: rgba(0, 188, 140, 0.1); color: #00bc8c; border-left: 4px solid #00bc8c; }

        /* Botonera estilizada */
        .btn-rs-pack { border-radius: 4px; font-weight: 600; text-transform: uppercase; font-size: 0.75rem; letter-spacing: 1px; margin-left: 5px; }

        /* Sistema de Bloqueo Visual (Overlay) */
        .rs-locked-overlay {
          display: none; position: absolute; top:0; left:0; width:100%; height:100%;
          background: rgba(11, 18, 24, 0.8); z-index: 100; border-radius: 8px;
          justify-content: center; align-items: center; cursor: not-allowed;
        }
        .is-rs-locked .rs-locked-overlay { display: flex; }
        .rs-lock-icon { color: #00d4ff; font-size: 3rem; text-shadow: 0 0 15px #00d4ff; }

        /* Filas de niveles */
        .level-row { border-bottom: 1px solid #2a3b47; padding: 8px 0; }
        .level-row:last-child { border-bottom: none; }

        /* Contenedor blanco para tablas DT (mejor legibilidad) */
        .rs-table-container { background: white; border-radius: 4px; padding: 5px; color: #333; }

        /* Forzar visibilidad del label de colourpicker en dark mode */
        .level-row span { font-size: 0.9rem; }
      ")))
    ),

    div(id = ns("module_container"), class = "rs-settings-pack",
        uiOutput(ns("import_header")),
        br(),
        uiOutput(ns("action_buttons_ui")),
        br(),
        uiOutput(ns("main_content_ui")),
        uiOutput(ns("debug_container"))
    )
  )
}

# ==============================================================================
# SERVER: FACTOR LEVELS STUDIO - RScience v.0.0.1
# ==============================================================================

SUB_mod_levels_server <- function(id, df_input, var_rv, var_factor, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_show_debug     <- reactive( if(is.function(show_debug)) show_debug() else show_debug)

    # --- 1. ESTADO INICIAL ---
    list_default <- list(
      "details"    = "*** Factor Levels Configuration - RScience ***",
      "is_done"    = FALSE,
      "is_locked"  = FALSE,
      "time_stamp" = Sys.time(),
      "data"       = NULL
    )

    status <- reactiveValues(locked = FALSE)
    output_rv <- reactiveValues(val = list_default)

    # --- 2. VALIDACIÓN ---
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

    # --- 3. CONFIGURACIÓN REACTIVA ---
    current_config <- reactive({
      req(is_ready())
      data_info <- base_data()
      lapply(seq_along(data_info$lvls), function(i) {
        lv <- data_info$lvls[i]
        data.frame(
          nivel = lv,
          orden = as.numeric(input[[paste0("pos_", lv)]] %||% i),
          color = input[[paste0("col_", lv)]] %||% "#00d4ff",
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% arrange(orden)
    })

    # --- 4. EVENTOS ---
    observeEvent(input$btn_import, {
      status$locked <- TRUE
      output_rv$val <- list(
        "details"    = "*** Factor Levels Configuration ***",
        "is_done"    = TRUE,
        "is_locked"  = TRUE,
        "time_stamp" = Sys.time(),
        "data"       = current_config()
      )
      shinyjs::addClass(id = "card_settings_wrapper", class = "is-rs-locked")
      shinyjs::addClass(id = "card_plot_wrapper", class = "is-rs-locked")
    })

    observeEvent(input$btn_edit, {
      status$locked <- FALSE
      output_rv$val <- list_default
      shinyjs::removeClass(id = "card_settings_wrapper", class = "is-rs-locked")
      shinyjs::removeClass(id = "card_plot_wrapper", class = "is-rs-locked")
    })

    # --- 5. RENDERS ---
    output$import_header <- renderUI({
      cls <- if(status$locked) "status-confirmed" else "status-active"
      div(class = paste("rs-status-header", cls),
          span(icon(if(status$locked) "lock" else "palette"), " 2. FACTOR LEVELS STUDIO"),
          span(style="font-size: 0.7rem; opacity:0.6;", format(Sys.time(), "%H:%M:%S")))
    })

    output$action_buttons_ui <- renderUI({
      req(is_ready())
      div(class = "d-flex justify-content-end",
          actionButton(ns("btn_import"), span(icon("check"), "Accept"), class = "btn-rs-pack btn-success"),
          actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-rs-pack btn-warning"),
          actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-rs-pack btn-primary")
      )
    })

    output$main_content_ui <- renderUI({
      if (!is_ready()) {
        div(class = "rs-card-wrapper", style = "text-align: center; padding: 60px;",
            icon("layer-group", class = "fa-3x", style = "color: #2a3b47; margin-bottom: 15px;"),
            h4("Waiting for Variable selection...", style = "color: #566b7a; font-weight: 700;"))
      } else {
        fluidRow(
          column(6, div(id = ns("card_settings_wrapper"), class = "rs-card-wrapper",
                        div(class = "rs-locked-overlay", div(class = "rs-lock-icon", icon("lock"))),
                        tabsetPanel(
                          tabPanel("Config", div(style = "padding-top: 15px; max-height: 400px; overflow-y: auto;", uiOutput(ns("settings_ui")))),
                          tabPanel("Table", div(class = "rs-table-container mt-3", DTOutput(ns("summary_table"))))
                        ))),
          column(6, div(id = ns("card_plot_wrapper"), class = "rs-card-wrapper",
                        div(class = "rs-locked-overlay", div(class = "rs-lock-icon", icon("lock"))),
                        plotlyOutput(ns("preview_plot"), height = "450px")))
        )
      }
    })

    output$settings_ui <- renderUI({
      req(base_data())
      data <- base_data()
      lapply(seq_along(data$lvls), function(i) {
        lv <- data$lvls[i]
        div(class = "level-row",
            fluidRow(
              column(4, span(lv, style="font-weight:700; color: #00d4ff;")),
              column(4, selectInput(ns(paste0("pos_", lv)), NULL, choices = 1:length(data$lvls), selected = i)),
              column(4, colourpicker::colourInput(ns(paste0("col_", lv)), NULL, value = rainbow(length(data$lvls))[i]))
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
              colors = setNames(conf$color, conf$nivel), type = "box") %>%
        layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
               font = list(color = '#ffffff'),
               xaxis = list(gridcolor = '#2a3b47', title = data_info$vx),
               yaxis = list(gridcolor = '#2a3b47', title = data_info$vy))
    })

    output$summary_table <- renderDT({
      req(current_config())
      datatable(current_config(), rownames = FALSE,
                options = list(dom = 't', pageLength = -1, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })

    output$debug_container <- renderUI({
      req(internal_show_debug())
      div(class = "rs-card-wrapper", style = "margin-top:20px;",
          h6("DEBUG: output_rv$val", style="color:#566b7a"),
          listviewer::jsoneditOutput(ns("debug_json"), height = "auto"))
    })

    output$debug_json <- listviewer::renderJsonedit({
      listviewer::jsonedit(output_rv$val, mode = "view")
    })

    return(reactive({ output_rv$val }))
  })
}
