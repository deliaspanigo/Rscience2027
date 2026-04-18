library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# MÓDULOS UI
# ==============================================================================

mod_11_B_shiny_output_DEBUG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("debug_external"))
  )
}

mod_11_B_shiny_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_hidden(
      id = ns("proccessing_switcher"),
      nav_panel_hidden(
        value = "state_waiting",
        uiOutput(ns("ui_waiting_state"))
      ),
      nav_panel_hidden(
        value = "state_loading",
        div(style = "padding: 80px; text-align: center;",
            icon("sync", class = "fa-spin fa-3x", style = "color: #00d4ff;"),
            h4("Sincronizando...", style = "color: #00d4ff;"))
      ),
      nav_panel_hidden(
        value = "state_ready",
        uiOutput(ns("placeholder_dinamico"))
      )
    ),
    uiOutput(ns("debug_internal"))
  )
}

# ==============================================================================
# MÓDULO SERVER
# ==============================================================================

# ==============================================================================
# MÓDULO SERVER (mod_11_B_shiny_output_server) - ACTUALIZADO
# ==============================================================================

mod_11_B_shiny_output_server <- function(id,
                                                module_shiny_output_file_path, # Path al .R del hijo
                                                temp_folder_tool_script,
                                                show_debug = TRUE,
                                                show_file = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. CONFIGURACIÓN DE RECURSOS ---
    www_folder <- system.file("www", package = "Rscience2027")
    if (www_folder == "") www_folder <- "www"
    try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)

    # --- 2. REACTIVOS DE ENTRADA ---
    internal_path <- reactive({
      if (is.function(module_shiny_output_file_path)) module_shiny_output_file_path() else module_shiny_output_file_path
    })

    # El hijo ahora solo necesita la carpeta temporal para buscar sus archivos hardcodeados
    internal_temp_folder <- reactive({
      if (is.function(temp_folder_tool_script)) temp_folder_tool_script() else temp_folder_tool_script
    })

    local_env  <- reactiveVal(NULL)
    data_store <- reactiveValues(
      details = "*** RScience - Module Processing ***",
      is_done = FALSE
    )
    rv <- reactiveValues(ready = FALSE, special_module = NULL)

    # --- 3. METADATOS Y VALIDACIÓN ---
    internal_meta <- reactive({
      p <- internal_path()
      if (is.null(p) || is.na(p) || p == "") return(list(status = "WAITING_PATH", exists = FALSE))
      list(status = "PATH_PROVIDED", target_file = p, exists = file.exists(p))
    })

    # --- 4. CARGA DINÁMICA (SOURCE) ---
    observeEvent(internal_meta(), {
      info <- internal_meta()
      if (info$status == "PATH_PROVIDED" && isTRUE(info$exists)) {
        new_env <- new.env(parent = .GlobalEnv)
        tryCatch({
          source(info$target_file, local = new_env)
          local_env(new_env)
          rv$ready <- TRUE
        }, error = function(e) {
          rv$ready <- FALSE
          warning("--- Error en source del módulo hijo: ", e$message)
        })
      } else {
        rv$ready <- FALSE
      }
    })

    # --- 5. EJECUCIÓN DEL HIJO (CONEXIÓN SIMPLIFICADA) ---
    observe({
      req(rv$ready)
      env <- local_env()
      req(!is.null(env$mod_special_shiny_output_server))

      # Invocamos al hijo pasándole solo la carpeta base
      rv$special_module <- env$mod_special_shiny_output_server(
        id                      = "sub_proc",
        temp_folder_tool_script = internal_temp_folder,
        show_file               = show_file,
        show_debug              = show_debug
      )
    })

    # --- 6. AGREGADOR DE SALIDA ---
    the_output <- reactive({
      out <- reactiveValuesToList(data_store)

      if (rv$ready && !is.null(rv$special_module)) {
        child_data <- if(is.reactive(rv$special_module)) rv$special_module() else rv$special_module
        out$module_special <- child_data

        # Sincronizamos si el proceso está terminado basándonos en el hijo
        if (!is.null(child_data$is_done)) {
          out$is_done <- child_data$is_done
        }
      }
      return(out)
    })

    # --- 7. RENDERS UI ---
    output$ui_waiting_state <- renderUI({
      div(style = "padding: 80px 20px; text-align: center; border: 2px dashed #444; border-radius: 20px; background: #1a1a1a;",
          div(class = "text-center rs-logo-animated",
              img(src = "WWW-FOLDER/Rscience_logo_sticker.png", style = "width: 150px;")),
          h3("Action Required", style = "color: #00bc8c; margin-top: 15px; font-weight: 600;"),
          p("Select a valid analysis tool to visualize results.", style = "color: #aaaaaa;")
      )
    })

    output$placeholder_dinamico <- renderUI({
      req(rv$ready)
      env <- local_env()
      req(env$mod_special_shiny_output_ui)
      env$mod_special_shiny_output_ui(ns("sub_proc"))
    })

    # --- 8. ESTADOS Y DEBUG ---
    observe({
      info <- internal_meta()
      state <- if (info$status == "WAITING_PATH") "state_waiting" else if (!rv$ready) "state_loading" else "state_ready"
      nav_select("proccessing_switcher", state)
    })

    output$json_int <- listviewer::renderJsonedit({ listviewer::jsonedit(the_output()) })
    output$json_ext <- listviewer::renderJsonedit({ listviewer::jsonedit(the_output()) })

    output$debug_internal <- renderUI({
      req(show_debug)
      div(style = "margin-top: 20px; padding: 15px; background: #1a1a1a; border-radius: 8px; border: 1px solid #333;",
          h4(icon("terminal"), "Internal Debug - Parent", style = "color: #00bc8c; font-size: 0.8rem;"),
          listviewer::jsoneditOutput(ns("json_int"), height = "250px"))
    })

    output$debug_external <- renderUI({
      div(style = "margin-top: 10px; padding: 15px; background: #1a1a1a; border: 1px solid #333;",
          h4(icon("terminal"), "External Debug - Parent", style = "color: #00bc8c; font-size: 0.8rem;"),
          listviewer::jsoneditOutput(ns("json_ext"), height = "250px"))
    })

    return(the_output)
  })
}

