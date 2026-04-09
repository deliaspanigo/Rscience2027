# ==========================================================
# MOD_07_00_ENGINE_CONTROL.R
# ==========================================================



mod_07_00_engine_control_DEBUG_ui <-  function(id) {

  ns <- NS(id)

  tagList(
    uiOutput(ns("show_debug_external"))
  )

}



# UI del Módulo
mod_07_00_engine_control_ui <- function(id) {
  ns <- NS(id)

  # Carga de CSS
  lib_www_path <- system.file("www", "css", package = "Rscience2027")
  if (lib_www_path == "") lib_www_path <- "www"
  if (dir.exists(lib_www_path)) {
    addResourcePath("lib_www", normalizePath(lib_www_path))
    path_to_css <- "lib_www/style_000.css" # Cambia al nombre de tu CSS de motor
  } else {
    path_to_css <- NULL
  }

  tagList(
    tags$head(
      useShinyjs(),
      if (!is.null(path_to_css)) tags$link(rel = "stylesheet", type = "text/css", href = path_to_css)
    ),

    div(class = "rs-control-card",
        uiOutput(ns("internal_status_ui")),
        div(class = "rs-btn-group-container rs-engine-selector",
            shinyWidgets::radioGroupButtons(
              inputId = ns("engine_mode"),
              label = NULL,
              choiceNames = list(
                HTML("<i class='fas fa-lock-open'></i>&nbsp; UNLOCK"),
                HTML("<i class='fas fa-lock'></i>&nbsp; LOCK"),
                HTML("<i class='fas fa-sync-alt'></i>&nbsp; RESET")
              ),
              choiceValues = list("unlock", "lock", "reset"),
              selected = "unlock",
              justified = FALSE, # IMPORTANTE: FALSE para permitir el gap de las cápsulas
              status = "engine-custom"
            )
        ),
        uiOutput(ns("show_debug_internal"))
    )
  )
}


# Server del Módulo
mod_07_00_engine_control_server <- function(id, show_debug = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_show_debug     <- reactive( if(is.function(show_debug)) show_debug() else show_debug)


    www_folder <- system.file("www", package = "Rscience2027")
    if (www_folder == "") www_folder <- "www"
    try(addResourcePath("WWW-FOLDER", normalizePath(www_folder)), silent = TRUE)

    get_default_data <- function() {
      list(
          description = "*** RScience - Control Engine Multipropose ***",
          my_sys_time = Sys.time(),
          click_count = 0,
          mode = "unlock",
          try_lock = FALSE
      )
    }
    reset_data_store <- function() {
      defaults <- get_default_data()

      # mapply recorre los nombres y valores de la lista de defaults
      # y los asigna uno a uno al objeto reactiveValues
      mapply(function(val, name) {
        data_store[[name]] <- val
      }, defaults, names(defaults))
    }

    data_store <- do.call(reactiveValues, get_default_data())


    # --- LÓGICA DE RESET CON BLOQUEO TOTAL ---
    observeEvent(input$engine_mode, {
      req(input$engine_mode == "reset")

      # 1. BLOQUEO INMEDIATO (Visual y Lógico)
      # Usamos la clase que definimos antes para que el usuario no toque nada
      # rs_engine_status(c("the_menu", "the_summary"), action = "clean")

      # Marcamos un estado de "tránsito"
      data_store$is_locked <- TRUE

      # 2. PROCESO INTERNO (Aislado si es necesario)
      # Si reset_data_store activa muchos observers, podrías envolverlo,
      # pero al ser una función de asignación directa, el problema suele ser la cadena reactiva.
      isolate({
        reset_data_store()
        data_store$mode <- "reset"
      })

      # 3. EL DELAY (La "Cámara de Silencio")
      shinyjs::delay(2000, {

        # Solo al final de los 2 segundos actualizamos los valores que disparan UI
        isolate({
          data_store$my_sys_time <- Sys.time()
          data_store$mode <- "unlock"
          data_store$is_done <- FALSE
        })

        # 4. LIBERACIÓN
        shinyWidgets::updateRadioGroupButtons(session, "engine_mode", selected = "unlock")
        # rs_engine_status(c("the_menu", "the_summary"), action = "unlock")

        showNotification("Engine Ready", type = "message")
      })
    }, ignoreInit = TRUE)

    # --- ACTUALIZACIÓN DE ESTADO (LOCK / UNLOCK) ---
    observeEvent(input$engine_mode, {
      req(input$engine_mode %in% c("unlock", "lock"))
      data_store$mode <- input$engine_mode
      data_store$my_sys_time = Sys.time()
      data_store$click_count <- data_store$click_count + 1


      if(input$engine_mode == "lock")  data_store$try_lock <- TRUE else data_store$try_lock <- TRUE
    }, ignoreInit = T)

    # --- RENDERS VISUALES ---
    output$internal_status_ui <- renderUI({
      mode <- data_store$mode
      config <- switch(mode,
                       "unlock" = list(txt = "ENGINE UNLOCKED",  cl = "status-unlock"),
                       "lock"   = list(txt = "ENGINE LOCKED",    cl = "status-lock"),
                       "reset"  = list(txt = "RESETTING...",     cl = "status-reset"),
                       list(txt = "WAITING...", cl = "status-waiting")
      )
      div(class = paste("rs-status-display", config$cl), span(config$txt))
    })

    # --- LÓGICA DE DEBUG (PROTEGIDA) ---
    render_debug_json <- function() {
      listviewer::jsonedit(
        listdata = reactiveValuesToList(data_store),
        mode = "text"
      )
    }

    output$debug_internal <- listviewer::renderJsonedit({
      req(internal_show_debug())
      render_debug_json()
    })

    output$show_debug_internal <- renderUI({
      req(internal_show_debug())
      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 10px;",
          div(class = "section-label", style = "justify-content: flex-start !important; gap: 8px;", icon("bug"), " Internal Debug - Control Engine"),
          listviewer::jsoneditOutput(ns("debug_internal"), height = "auto")
      )
    })

    output$debug_external <- listviewer::renderJsonedit({
      render_debug_json()
    })

    output$show_debug_external <- renderUI({
      div(class = "debug-section",
          style = "background: rgba(0,0,0,0.2); border-radius: 8px; padding: 10px;",
          div(class = "section-label", style = "justify-content: flex-start !important; gap: 8px;", icon("bug"), " External Debug - Control Engine"),
          listviewer::jsoneditOutput(ns("debug_external"), height = "auto")
      )
    })



    # --- RETORNO ---
    return(reactive({ reactiveValuesToList(data_store) }))
  })
}



