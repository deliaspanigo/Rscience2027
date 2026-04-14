library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(DT)
library(openxlsx)
library(stringr)
library(listviewer)

# ==============================================================================
# MODULE: VARIABLE SELECTOR (FIXED RENDER-UI) - RScience v.0.0.1
# ==============================================================================

SUB_mod_var_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(), # Indispensable para el bloqueo
    tags$head(
      tags$style(HTML(paste0("
        .rs-settings-pack { background: #0b1218; padding: 20px; border-radius: 10px; font-family: 'Inter', sans-serif; color: white; }
        .rs-card-wrapper {
          position: relative; padding: 20px; border: 1px solid #2a3b47;
          border-radius: 8px; background: #1a262f; margin-bottom: 15px;
        }
        /* Fix del Overlay */
        .rs-locked-overlay {
          display: none; position: absolute; top:0; left:0; width:100%; height:100%;
          background: rgba(11, 18, 24, 0.85); z-index:1000;
          justify-content: center; align-items: center; border-radius: 8px;
        }
        .is-locked .rs-locked-overlay { display: flex !important; }
        .rs-lock-icon { color: #00d4ff; text-shadow: 0 0 15px #00d4ff; }

        .selection-header {
          padding: 15px; border-radius: 5px; margin-bottom: 15px;
          font-weight: bold; display: flex; justify-content: space-between;
        }
        .waiting { background: rgba(255, 243, 205, 0.1); color: #ffeeba; border-left: 5px solid #ffeeba; }
        .success { background: rgba(0, 188, 140, 0.1); color: #00bc8c; border-left: 5px solid #00bc8c; }
        .confirmed { background: rgba(0, 212, 255, 0.1); color: #00d4ff; border-left: 5px solid #00d4ff; }
        .error { background: rgba(231, 76, 60, 0.1); color: #e74c3c; border-left: 5px solid #e74c3c; }

        .selectize-input { background: #0b1218 !important; color: white !important; border: 1px solid #2a3b47 !important; }
        .selectize-dropdown { background: #1a262f !important; color: white !important; }
        .rs-table-dark-container { background: white; border-radius: 4px; padding: 5px; margin-top: 15px; color: #333; }
      ")))
    ),

    div(class = "rs-settings-pack",
        uiOutput(ns("status_header")),

        div(class = "d-flex justify-content-end gap-2 mb-3",
            actionButton(ns("btn_import"), span(icon("check"), "Accept"), class = "btn-success"),
            actionButton(ns("btn_edit"), span(icon("edit"), "Edit"), class = "btn-warning"),
            actionButton(ns("btn_reset"), span(icon("sync"), "Reset"), class = "btn-primary")
        ),

        div(id = ns("card_inputs_wrapper"), class = "rs-card-wrapper",
            # Overlay de bloqueo visible mediante CSS class
            div(class = "rs-locked-overlay",
                div(class = "rs-lock-icon", icon("lock", class = "fa-4x"))
            ),

            uiOutput(ns("dynamic_selectors")),

            div(class = "rs-table-dark-container",
                DTOutput(ns("info_table"))
            ),
            br(),
            uiOutput(ns("debug_panel"))
        )
    )
  )
}

SUB_mod_var_selection_server <- function(id, df_input, show_debug = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    the_dataset <- reactive({ if (is.reactive(df_input)) df_input() else df_input })
    internal_show_debug     <- reactive( if(is.function(show_debug)) show_debug() else show_debug)

    # 1. ESTADO DEL MÓDULO
    data_store <- reactiveValues(
      details = "Waiting for Data",
      is_locked = FALSE,
      time_stamp = Sys.time(),
      metadata = list(rv = NULL, factor = NULL, alpha = 0.05, controls_passed = FALSE)
    )

    # 2. RENDER DE SELECTORES (Aislado para evitar loops)
    output$dynamic_selectors <- renderUI({
      df <- the_dataset()
      req(df)
      cols <- names(df)

      v_let <- openxlsx::int2col(seq_along(cols))
      choices_list <- cols
      names(choices_list) <- paste0(sprintf("%02d", seq_along(cols)), " - ", v_let, " - ", cols)

      # Importante: isolate() para que no se re-renderice al cambiar valores internos
      isolate({
        fluidRow(
          column(4, selectizeInput(ns("var_rv"), "Response Variable (RV)",
                                   choices = c("Select..." = "", choices_list),
                                   selected = data_store$metadata$rv)),
          column(4, selectizeInput(ns("var_factor"), "Factor Variable",
                                   choices = c("Select..." = "", choices_list),
                                   selected = data_store$metadata$factor)),
          column(4, selectInput(ns("alpha_value"), "Alpha Level",
                                choices = c("1%" = 0.01, "5%" = 0.05, "10%" = 0.10),
                                selected = data_store$metadata$alpha))
        )
      })
    })

    # 3. VALIDACIÓN
    check_status <- reactive({
      req(input$var_rv, input$var_factor)
      v_rv  <- input$var_rv
      v_fac <- input$var_factor

      if (v_rv == "" && v_fac == "") return(list(valid = FALSE, msg = "Select variables", type = "waiting"))
      if (v_rv != "" && v_fac != "" && v_rv == v_fac) return(list(valid = FALSE, msg = "Variables must be different", type = "error"))
      if (v_rv == "" || v_fac == "") return(list(valid = FALSE, msg = "Incomplete Selection", type = "waiting"))

      df <- the_dataset()
      if (!is.numeric(df[[v_rv]])) return(list(valid = FALSE, msg = "RV must be Numeric", type = "error"))

      return(list(valid = TRUE, msg = "Ready to Accept", type = "success"))
    })

    # 4. SINCRONIZACIÓN
    observe({
      req(input$var_rv, input$var_factor)
      status <- check_status()

      if(!data_store$is_locked){
        data_store$metadata$rv <- input$var_rv
        data_store$metadata$factor <- input$var_factor
        data_store$metadata$alpha <- as.numeric(input$alpha_value %||% 0.05)
        data_store$metadata$controls_passed <- status$valid
        data_store$details <- status$msg
      }
      data_store$time_stamp <- Sys.time()
    })

    # 5. EVENTOS (Bloqueo corregido)
    observeEvent(input$btn_import, {
      req(check_status()$valid)
      data_store$is_locked <- TRUE
      shinyjs::addClass(id = "card_inputs_wrapper", class = "is-locked")
    })

    observeEvent(input$btn_edit, {
      data_store$is_locked <- FALSE
      shinyjs::removeClass(id = "card_inputs_wrapper", class = "is-locked")
    })

    observeEvent(input$btn_reset, {
      data_store$is_locked <- FALSE
      data_store$metadata$rv <- ""
      data_store$metadata$factor <- ""
      shinyjs::removeClass(id = "card_inputs_wrapper", class = "is-locked")
      # Forzamos re-render de los selectores limpios
      shinyjs::refresh()
    })

    # 6. RENDERS DE UI
    output$status_header <- renderUI({
      if(is.null(input$var_rv)) return(div(class = "selection-header waiting", "Initializing..."))

      status <- check_status()
      locked <- isTRUE(data_store$is_locked)

      cls <- if(locked) "confirmed" else status$type
      msg <- if(locked) "SYSTEM LOCKED" else status$msg

      div(class = paste("selection-header", cls),
          span(icon(if(locked) "lock" else "info-circle"), msg),
          span(format(data_store$time_stamp, "%H:%M:%S")))
    })

    output$info_table <- renderDT({
      datatable(
        data.frame(
          Parameter = c("RV", "Factor", "Alpha"),
          Value = c(data_store$metadata$rv %||% "---",
                    data_store$metadata$factor %||% "---",
                    data_store$metadata$alpha)
        ),
        options = list(dom = 't', ordering = FALSE), rownames = FALSE
      )
    })

    output$debug_json <- listviewer::renderJsonedit({
      listviewer::jsonedit(reactiveValuesToList(data_store))
    })

    output$debug_panel <- renderUI({
      req(internal_show_debug())
      listviewer::jsoneditOutput(ns("debug_json"), height = "200px")
    })

    return(reactive({ reactiveValuesToList(data_store) }))
  })
}
