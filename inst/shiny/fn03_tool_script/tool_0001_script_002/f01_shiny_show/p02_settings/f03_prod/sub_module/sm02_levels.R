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

  tagList(
    useShinyjs(),
    # Todo envuelto en el contenedor del pack para heredar los estilos dark
    div(id = ns("module_container"), class = "rs-settings-pack",
        uiOutput(ns("import_header")),
        br(),

        uiOutput(ns("action_buttons_ui")),
        br(),
        uiOutput(ns("main_content_ui")),

        # Debug con el estilo del pack
        uiOutput(ns("debug_container"))
    )
  )
}

# ==============================================================================
# SERVER: FACTOR LEVELS STUDIO - RScience v.0.0.1
# ==============================================================================

SUB_mod_levels_server <- function(id, df_input = reactive(NULL), var_rv = reactive(NULL), var_factor = reactive(NULL), show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    # --- 2. VALIDACIÓN (Igual a tu original) ---
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

    # --- 3. CONFIGURACIÓN ---
    current_config <- reactive({
      req(is_ready())
      data_info <- base_data()
      lapply(seq_along(data_info$lvls), function(i) {
        lv <- data_info$lvls[i]
        data.frame(
          nivel = lv,
          orden = as.numeric(input[[paste0("pos_", lv)]] %||% i),
          color = input[[paste0("col_", lv)]] %||% "#00d4ff", # Default Cian
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows() %>% arrange(orden)
    })

    # --- 4. EVENTOS (Sincronizados con Pack v.1.5) ---
    observeEvent(input$btn_import, {
      status$locked <- TRUE
      output_rv$val <- list(
        "details"    = "*** Factor Levels Configuration ***",
        "is_done"    = TRUE, "is_locked"  = TRUE,
        "time_stamp" = Sys.time(), "data" = current_config()
      )
      # Usamos las clases del pack para el bloqueo visual
      shinyjs::addClass(id = "card_settings_wrapper", class = "is-rs-locked")
      shinyjs::addClass(id = "card_plot_wrapper", class = "is-rs-locked")
    })

    observeEvent(input$btn_edit, {
      status$locked <- FALSE
      output_rv$val <- list_default
      shinyjs::removeClass(id = "card_settings_wrapper", class = "is-rs-locked")
      shinyjs::removeClass(id = "card_plot_wrapper", class = "is-rs-locked")
    })

    # --- 5. RENDERS DINÁMICOS ---
    output$import_header <- renderUI({
      # Mapeo a clases de estado del Pack
      cls <- if(status$locked) "status-confirmed" else "status-active"
      div(class = paste("rs-status-header", cls),
          span(icon(if(status$locked) "lock" else "palette"), " 2. FACTOR LEVELS STUDIO"),
          span(style="font-size: 0.7rem; opacity:0.6;", format(Sys.time(), "%H:%M:%S")))
    })

    output$action_buttons_ui <- renderUI({
      req(is_ready())
      div(class = "rs-action-row d-flex justify-content-end",
          actionButton(ns("btn_import"), span(icon("check"), "Accept"), class = "btn-rs-pack btn-success"),
          actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-rs-pack btn-warning"),
          actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-rs-pack btn-primary")
      )
    })

    output$main_content_ui <- renderUI({
      if (!is_ready()) {
        div(class = "rs-card-wrapper", style = "text-align: center; padding: 60px;",
            icon("layer-group", class = "fa-3x", style = "color: #444; margin-bottom: 15px;"),
            h4("Waiting for Variable selection...", style = "color: #666; font-weight: 700;"))
      } else {
        fluidRow(
          column(6, div(id = ns("card_settings_wrapper"), class = "rs-card-wrapper",
                        div(class = "rs-locked-overlay", div(class = "rs-lock-icon", icon("lock"))),
                        tabsetPanel(
                          tabPanel("Config", div(style = "padding: 15px; max-height: 400px; overflow-y: auto;", uiOutput(ns("settings_ui")))),
                          tabPanel("Table", div(class = "rs-table-container", DTOutput(ns("summary_table"))))
                        ))),
          column(6, div(id = ns("card_plot_wrapper"), class = "rs-card-wrapper",
                        div(class = "rs-locked-overlay", div(class = "rs-lock-icon", icon("lock"))),
                        plotlyOutput(ns("preview_plot"), height = "450px")))
        )
      }
    })

    # --- 6. CONTENIDOS (Inputs Dark) ---
    output$settings_ui <- renderUI({
      req(base_data())
      data <- base_data()
      lapply(seq_along(data$lvls), function(i) {
        lv <- data$lvls[i]
        div(class = "level-row", style = "padding:10px; border-bottom: 1px solid rgba(255,255,255,0.05); color: white;",
            fluidRow(
              column(4, span(lv, style="font-weight:700; color: var(--rs-primary);")),
              column(4, selectInput(ns(paste0("pos_", lv)), NULL, choices = 1:length(data$lvls), selected = i)),
              column(4, colourpicker::colourInput(ns(paste0("col_", lv)), NULL, value = rainbow(length(data$lvls))[i], showColour = "both"))
            ))
      })
    })

    # Render de Plotly (Transparente para que pegue con el Dark Mode)
    output$preview_plot <- renderPlotly({
      req(is_ready(), current_config())
      conf <- current_config()
      data_info <- base_data()
      plot_df <- data_info$df
      plot_df[[data_info$vx]] <- factor(plot_df[[data_info$vx]], levels = conf$nivel)

      p <- plot_ly(plot_df, x = ~get(data_info$vx), y = ~get(data_info$vy), color = ~get(data_info$vx),
                   colors = setNames(conf$color, conf$nivel), type = "box") %>%
        layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
               font = list(color = '#ffffff'),
               xaxis = list(gridcolor = '#333'), yaxis = list(gridcolor = '#333'))
      p
    })

    output$summary_table <- renderDT({
      req(current_config())
      datatable(current_config(), rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = -1))
    })

    output$debug_container <- renderUI({
      req(show_debug)
      div(class = "rs-card-wrapper", style = "margin-top:20px;",
          listviewer::jsoneditOutput(ns("debug_json"), height = "auto"))
    })

    output$debug_json <- listviewer::renderJsonedit({
      listviewer::jsonedit(listdata = output_rv$val, mode = "view")
    })

    return(reactive({ output_rv$val }))
  })
}
