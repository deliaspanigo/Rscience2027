library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(DT)
library(openxlsx)

# ==============================================================================
# MÓDULO: SELECTOR DE VARIABLES (RV Y FACTOR) - VERSIÓN ESTÁTICA
# ==============================================================================

SUB_mod_var_selection_ui <- function(id) {
  ns <- NS(id)
  scope_id <- paste0("#", ns("var_selector_container"))

  # CSS ENCAPSULADO (Overflow corregido y Z-Index alto para selectores)
  css_custom <- paste0("
    ", scope_id, " .card-container { overflow: visible !important; position: relative; }
    ", scope_id, " .row, ", scope_id, " .col-sm-6 { overflow: visible !important; }
    .selectize-dropdown { z-index: 999999 !important; position: absolute !important; }

    ", scope_id, " .locked-overlay {
      position: absolute; top: 0; left: 0; width: 100%; height: 100%;
      background: rgba(230, 230, 230, 0.5); z-index: 2000; border-radius: 15px;
      display: flex; justify-content: center; align-items: center;
      visibility: hidden; opacity: 0; transition: all 0.3s ease;
    }
    ", scope_id, " .is-locked .locked-overlay { visibility: visible; opacity: 1; }
    ", scope_id, " .lock-icon { font-size: 5rem; color: #28a745; filter: drop-shadow(0 0 5px white); }

    ", scope_id, " .selection-header { padding: 15px 25px; border-radius: 12px; margin-bottom: 20px; font-weight: 800; display: flex; justify-content: space-between; align-items: center; }
    ", scope_id, " .confirmed { background: #d4edda; color: #155724; border-left: 8px solid #28a745; }
    ", scope_id, " .waiting { background: #e9ecef; color: #495057; border-left: 8px solid #adb5bd; }
    ", scope_id, " .active-selection { background: #fff3cd; color: #856404; border-left: 8px solid #ffc107; }
    ", scope_id, " .error-selection { background: #f8d7da; color: #721c24; border-left: 8px solid #dc3545; }

    ", scope_id, " .btn-pill-xl { border-radius: 50px !important; padding: 10px 25px !important; font-weight: 700 !important; text-transform: uppercase; border: none !important; box-shadow: 0 4px 10px rgba(0,0,0,0.1); }
    ", scope_id, " .action-row-right { display: flex; gap: 10px; justify-content: flex-end; margin-bottom: 20px; align-items: center; }
  ")

  tagList(
    tags$head(tags$style(HTML(css_custom))),
    useShinyjs(),

    div(id = ns("var_selector_container"),
        uiOutput(ns("status_header")),

        div(class = "action-row-right",
            uiOutput(ns("validation_warning")),
            actionButton(ns("btn_import"), span(icon("check"), "Aceptar Selección"), class = "btn-success btn-pill-xl"),
            actionButton(ns("btn_edit"),   span(icon("edit"), "Editar"),   class = "btn-warning btn-pill-xl"),
            actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),    class = "btn-primary btn-pill-xl")
        ),

        div(id = ns("card_inputs_wrapper"), class = "card-container",
            div(class = "locked-overlay", div(class = "lock-icon", icon("lock"))),
            div(style = "background: #ffffff; padding: 25px; border-radius: 15px; border: 1px solid #e0e0e0; box-shadow: 0 4px 15px rgba(0,0,0,0.08);",
                fluidRow(
                  column(6,
                         selectizeInput(ns("var_rv"), "Variable Respuesta (RV)", choices = NULL,
                                        options = list(placeholder = 'Seleccione RV...', dropdownParent = 'body'))
                  ),
                  column(6,
                         selectizeInput(ns("var_factor"), "Variable Factor (Factor)", choices = NULL,
                                        options = list(placeholder = 'Seleccione Factor...', dropdownParent = 'body'))
                  )
                ),
                hr(),
                DTOutput(ns("info_table"))
            )
        )
    )
  )
}

SUB_mod_var_selection_server <- function(id, df_input = mtcars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status <- reactiveValues(locked = FALSE)

    data_r <- reactive({ if (is.reactive(df_input)) df_input() else df_input })

    observe({
      df <- data_r(); req(df); cols <- names(df)
      updateSelectizeInput(session, "var_rv", choices = c("", cols), server = TRUE)
      updateSelectizeInput(session, "var_factor", choices = c("", cols), server = TRUE)
    })

    check_logic <- reactive({
      v_fac <- input$var_factor %||% ""
      v_rv  <- input$var_rv %||% ""
      if (v_fac == "" || v_rv == "") return(list(valid = FALSE, is_error = FALSE, msg = "Esperando selección..."))
      if (v_fac == v_rv) return(list(valid = FALSE, is_error = TRUE, msg = "RV y Factor duplicados"))
      list(valid = TRUE, is_error = FALSE, msg = "Selección lista")
    })

    table_data <- reactive({
      df <- data_r()
      base <- data.frame(Rol=character(), Variable=character(), `No. Columna`=integer(), `Excel Col`=character(), stringsAsFactors=F, check.names=F)
      if (is.null(df)) return(base)

      cols <- names(df)
      get_row <- function(var, role) {
        if (is.null(var) || var == "") return(NULL)
        idx <- which(cols == var)
        if(length(idx) == 0) return(NULL)
        data.frame(Rol=role, Variable=var, `No. Columna`=idx, `Excel Col`=openxlsx::int2col(idx), stringsAsFactors=F, check.names=F)
      }
      res <- bind_rows(get_row(input$var_rv, "Respuesta (RV)"), get_row(input$var_factor, "Factor"))
      if (nrow(res) == 0) return(base) else res
    })

    output$info_table <- renderDT({
      d <- table_data()
      dt <- datatable(d, rownames=F, selection='none', options=list(dom='t', ordering=F, language=list(emptyTable="Sin variables")))
      if (nrow(d) > 0) {
        dt <- dt %>% formatStyle('Rol', fontWeight='bold', color='#2c3e50') %>%
          formatStyle('Variable', color='#007bff', fontWeight='600')
      }
      dt
    })

    output$status_header <- renderUI({
      logic <- check_logic()
      if (status$locked) { cls <- "confirmed"; msg <- "BLOQUEADO"; ico <- "lock" }
      else if (logic$is_error) { cls <- "error-selection"; msg <- toupper(logic$msg); ico <- "exclamation-circle" }
      else if (!logic$valid) { cls <- "waiting"; msg <- toupper(logic$msg); ico <- "clock" }
      else { cls <- "active-selection"; msg <- "LISTO PARA ACEPTAR"; ico <- "check-circle" }

      div(class = paste("selection-header", cls), span(icon(ico), msg),
          span(style="font-size: 0.8rem; opacity: 0.7;", format(Sys.time(), "%H:%M:%S")))
    })

    output$validation_warning <- renderUI({
      logic <- check_logic()
      if(logic$is_error && !status$locked) span(style="color:#dc3545; font-weight:800; margin-right:15px;", icon("times-triangle"), logic$msg)
    })

    observe({
      logic <- check_logic()
      if(!logic$valid || status$locked) disable("btn_import") else enable("btn_import")
    })

    observeEvent(input$btn_import, {
      status$locked <- TRUE
      runjs(sprintf("$('#%s').addClass('is-locked');", ns("card_inputs_wrapper")))
    })

    observeEvent(input$btn_edit, {
      status$locked <- FALSE
      runjs(sprintf("$('#%s').removeClass('is-locked');", ns("card_inputs_wrapper")))
    })

    observeEvent(input$btn_reset, { session$reload() })

    return(reactive({ list(rv = input$var_rv, factor = input$var_factor, is_done = status$locked) }))
  })
}
