library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(DT)
library(openxlsx)
library(jsonlite)
library(listviewer)
library(stringr)

# ==============================================================================
# MODULE: VARIABLE SELECTOR (RV AND FACTOR) - RScience v.0.0.1
# ==============================================================================

SUB_mod_var_selection_ui <- function(id) {
  ns <- NS(id)
  scope_id <- paste0("#", ns("var_selector_container"))

  css_custom <- paste0("
    ", scope_id, " .card-container { overflow: visible !important; position: relative; }
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
            actionButton(ns("btn_import"), span(icon("check"), "Accept Selection"), class = "btn-success btn-pill-xl"),
            actionButton(ns("btn_edit"),   span(icon("edit"), "Edit"),   class = "btn-warning btn-pill-xl"),
            actionButton(ns("btn_reset"),  span(icon("sync"), "Reset"),  class = "btn-primary btn-pill-xl")
        ),

        div(id = ns("card_inputs_wrapper"), class = "card-container",
            div(class = "locked-overlay", div(class = "lock-icon", icon("lock"))),
            div(style = "background: #ffffff; padding: 25px; border-radius: 15px; border: 1px solid #e0e0e0; box-shadow: 0 4px 15px rgba(0,0,0,0.08);",
                fluidRow(
                  column(6,
                         selectizeInput(ns("var_rv"), "Response Variable (RV)",
                                        choices = NULL,
                                        options = list(placeholder = 'Select RV...',
                                                       onInitialize = I('function() { this.setValue(""); }')))
                  ),
                  column(6,
                         selectizeInput(ns("var_factor"), "Factor Variable (Factor)",
                                        choices = NULL,
                                        options = list(placeholder = 'Select Factor...',
                                                       onInitialize = I('function() { this.setValue(""); }')))
                  )
                ),
                hr(),
                DTOutput(ns("info_table"))
            )
        ),

        div(style = "margin-top: 20px;",
            listviewer::jsoneditOutput(ns("debug_json"), height = "auto")
        )
    )
  )
}

# ==============================================================================
# SERVER: CONTROL LOGIC AND OUTPUT
# ==============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

SUB_mod_var_selection_server <- function(id, df_input = mtcars, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. INITIAL STATE (Reintegrando timestamp)
    list_default <- list(
      "details"    = "*** Variable Selection - RScience ***",
      "is_done"    = FALSE,
      "is_locked"  = FALSE,
      "time_stamp" = Sys.time(),
      "metadata"   = list(rv = NULL, factor = NULL, controls_passed = FALSE)
    )

    data_store <- do.call(reactiveValues, list_default)
    the_dataset <- reactive({ if (is.reactive(df_input)) df_input() else df_input })

    # 2. INPUT UPDATES
    observe({
      df <- the_dataset(); req(df)
      cols <- names(df)

      n_digits <- nchar(length(cols))
      vector_pos_mod <- sprintf(paste0("%0", n_digits, "d"), 1:length(cols))
      vector_letter <- openxlsx::int2col(1:length(cols))
      max_width <- max(nchar(vector_letter))
      vector_letter_mod <- stringr::str_pad(vector_letter, width = max_width, side = "left")

      choices_list <- cols
      names(choices_list) <- paste0(vector_pos_mod, " - ", vector_letter_mod, " - ", cols)

      updateSelectizeInput(session, "var_rv", choices = choices_list, selected = "", server = TRUE)
      updateSelectizeInput(session, "var_factor", choices = choices_list, selected = "", server = TRUE)
    })

    # 3. STATISTICAL VALIDATION (Reactive)
    check_analysis_validity <- reactive({
      v_rv  <- input$var_rv %||% ""
      v_fac <- input$var_factor %||% ""

      if (v_rv == "" || v_fac == "") return(list(valid = TRUE, msg = "Waiting for User Selection"))
      if (v_rv == v_fac) return(list(valid = FALSE, msg = "Error: Duplicate Variables Selected"))

      df_raw <- the_dataset()
      mini_db <- df_raw[, c(v_rv, v_fac), drop = FALSE] %>% na.omit()

      if (!is.numeric(mini_db[[v_rv]])) return(list(valid = FALSE, msg = "Error: RV must be numeric"))

      fac_vec <- as.factor(mini_db[[v_fac]])
      if (nlevels(fac_vec) < 2) return(list(valid = FALSE, msg = "Error: Factor needs >= 2 levels (post-NA)"))

      if (var(mini_db[[v_rv]]) == 0) return(list(valid = FALSE, msg = "Error: RV total variance is zero"))

      var_by_level <- tapply(mini_db[[v_rv]], fac_vec, var)
      if (any(is.na(var_by_level)) || any(var_by_level == 0)) {
        return(list(valid = FALSE, msg = "Error: Level variance is zero (test impossible)"))
      }

      return(list(valid = TRUE, msg = "Ready to Accept Selection"))
    })

    # 4. ACTION BUTTONS
    is_valid_ui <- reactive({
      check <- check_analysis_validity()
      return(check$valid && input$var_rv != "" && input$var_factor != "")
    })

    observe({
      if (is_valid_ui() && !isTRUE(data_store$is_locked)) shinyjs::enable("btn_import") else shinyjs::disable("btn_import")
    })

    observeEvent(input$btn_import, {
      req(is_valid_ui())
      check <- check_analysis_validity()

      data_store$is_locked <- TRUE
      data_store$is_done <- TRUE
      data_store$time_stamp <- Sys.time() # Update timestamp on lock

      data_store$metadata <- list(
        rv = input$var_rv,
        factor = input$var_factor,
        controls_passed = check$valid # Guardamos el estado de los controles
      )

      shinyjs::addClass(id = "card_inputs_wrapper", class = "is-locked")
      shinyjs::disable("var_rv"); shinyjs::disable("var_factor")
    })

    observeEvent(input$btn_edit, {
      data_store$is_locked <- FALSE; data_store$is_done <- FALSE
      data_store$time_stamp <- Sys.time()
      shinyjs::removeClass(id = "card_inputs_wrapper", class = "is-locked")
      shinyjs::enable("var_rv"); shinyjs::enable("var_factor")
    })

    observeEvent(input$btn_reset, {
      for (name in names(list_default)) { data_store[[name]] <- list_default[[name]] }
      data_store$time_stamp <- Sys.time()
      updateSelectizeInput(session, "var_rv", selected = "")
      updateSelectizeInput(session, "var_factor", selected = "")
      shinyjs::removeClass(id = "card_inputs_wrapper", class = "is-locked")
      shinyjs::enable("var_rv"); shinyjs::enable("var_factor")
    })

    # 5. RENDERS
    output$status_header <- renderUI({
      locked <- isTRUE(data_store$is_locked)
      check  <- check_analysis_validity()

      if (locked) {
        cls <- "confirmed"; msg <- "Selection Locked"; ico <- "lock"
      } else if (!check$valid) {
        cls <- "error-selection"; msg <- check$msg; ico <- "times-circle"
      } else if (input$var_rv != "" && input$var_factor != "") {
        cls <- "active-selection"; msg <- check$msg; ico <- "hand-point-up"
      } else {
        cls <- "waiting"; msg <- check$msg; ico <- "hourglass-half"
      }

      div(class = paste("selection-header", cls),
          span(icon(ico), msg),
          span(style="font-size: 0.8rem; opacity: 0.7;", format(data_store$time_stamp, "%H:%M:%S")))
    })

    output$info_table <- renderDT({
      df <- the_dataset(); req(df)
      cols <- names(df)
      n_digits <- nchar(length(cols))
      v_pos_mod <- sprintf(paste0("%0", n_digits, "d"), 1:length(cols))
      v_let_mod <- format(openxlsx::int2col(1:length(cols)), justify = "right")

      res <- data.frame(
        Role = c("Response (RV)", "Factor"),
        Abbr = c("RV", "FAC"),
        `Var. Name` = c("---", "---"),
        `Col. #` = c("---", "---"),
        `Letter` = c("---", "---"),
        check.names = FALSE, stringsAsFactors = FALSE
      )

      v_rv <- input$var_rv %||% ""; v_fac <- input$var_factor %||% ""
      if (v_rv != "") {
        idx <- which(cols == v_rv)
        res$`Var. Name`[1] <- v_rv; res$`Col. #`[1] <- v_pos_mod[idx]; res$Letter[1] <- v_let_mod[idx]
      }
      if (v_fac != "") {
        idx <- which(cols == v_fac)
        res$`Var. Name`[2] <- v_fac; res$`Col. #`[2] <- v_pos_mod[idx]; res$Letter[2] <- v_let_mod[idx]
      }

      datatable(res, rownames = FALSE, selection = 'none',
                options = list(dom = 't', ordering = FALSE,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })

    the_output <- reactive({ reactiveValuesToList(data_store) })
    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug); listviewer::jsonedit(listdata = the_output(), mode = "text")
    })

    return(the_output)
  })
}
