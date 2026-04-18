# ==============================================================================
# SUB-MÓDULO: mod_06_00_show_file (Sin CSS personalizado)
# ==============================================================================

# ==============================================================================
# SUB-MÓDULO: mod_99_show_one_file_ui (ACTUALIZADO)
# ==============================================================================

mod_99_show_one_file_ui <- function(id) {
  ns <- NS(id)
  div(id = ns("container"), class = "mb-4",
      uiOutput(ns("header_ui")),
      uiOutput(ns("display_zone")),

      # --- ESTA ES LA PIEZA QUE FALTABA ---
      uiOutput(ns("show_debug_internal"))
  )
}

# ==============================================================================
# SUB-MÓDULO: mod_99_show_one_file_server
# ==============================================================================

# ==============================================================================
# SUB-MÓDULO: mod_99_show_one_file_server
# ==============================================================================

# ==============================================================================
# SUB-MÓDULO: mod_99_show_one_file_server (Debug & JSON Sync)
# ==============================================================================

mod_99_show_one_file_server <- function(id,
                                        super_label = "Visualizador de Archivo",
                                        file_path,
                                        show_file = TRUE,
                                        show_label = TRUE,
                                        label_bg_color = "#3333fa",
                                        show_debug = T) { # <--- Nuevo Argumento
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. PROCESAMIENTO DE ARGUMENTOS ---
    internal_show_debug   <- reactive({ if (is.function(show_debug)) show_debug() else show_debug })
    internal_super_label  <- reactive({ if (is.function(super_label)) super_label() else super_label })
    internal_file_path    <- reactive({ if (is.function(file_path)) file_path() else file_path })
    internal_show_file    <- reactive({ if (is.function(show_file)) show_file() else show_file })
    internal_show_label   <- reactive({ if (is.function(show_label)) show_label() else show_label })
    internal_label_bg     <- reactive({ if (is.function(label_bg_color)) label_bg_color() else label_bg_color })

    # Prefijo único para recursos web
    res_prefix <- paste0("res_show_", id)

    # --- 2. GESTIÓN DE METADATOS (DATA STORE) ---
    # Centralizamos la información para el retorno y para el debug JSON
    data_store <- reactive({
      path <- internal_file_path()
      exists <- !is.null(path) && file.exists(path)

      list(
        module_id    = id,
        is_done      = exists,
        file_exists  = exists,
        path_full    = if(exists) normalizePath(path) else NA,
        file_name    = if(!is.null(path)) basename(path) else NA,
        file_size    = if(exists) file.size(path) else 0,
        file_ext     = if(!is.null(path)) tools::file_ext(path) else NA,
        label_used   = internal_super_label(),
        bg_color     = internal_label_bg(),
        timestamp    = timestamp()
      )
    })

    # --- 3. RENDERS UI ---

    # Cabecera
    output$header_ui <- renderUI({
      req(internal_show_label())
      info <- data_store()

      header_style <- sprintf(
        "padding: 15px; border: 1px solid #dee2e6; border-radius: 8px; margin-bottom: 12px; background-color: %s; color: white;",
        info$bg_color
      )

      div(style = header_style,
          fluidRow(class = "align-items-center",
                   column(6, tags$strong(info$label_used)),
                   column(6, div(class = "text-end",
                                 if(info$file_exists) {
                                   tagList(
                                     actionButton(ns("open"), " Abrir", icon = icon("external-link-alt"), class = "btn-sm btn-info"),
                                     downloadButton(ns("download"), " Descargar", class = "btn-sm btn-secondary")
                                   )
                                 } else {
                                   span(class = "badge bg-warning text-dark", "Archivo no encontrado")
                                 }
                   ))
          )
      )
    })

    # Zona de visualización
    output$display_zone <- renderUI({
      req(internal_show_file())
      info <- data_store()

      if (!info$file_exists) {
        return(div(class = "alert alert-warning", "El archivo no existe o la ruta es inválida."))
      }

      out_dir <- normalizePath(dirname(info$path_full), mustWork = FALSE)
      addResourcePath(prefix = res_prefix, directoryPath = out_dir)

      if (info$file_ext == "r") {
        lineas <- tryCatch(readLines(info$path_full, warn = FALSE), error = function(e) "Error al leer.")
        return(div(class = "p-3 border rounded bg-white", tags$pre(tags$code(paste(lineas, collapse = "\n")))))
      }

      if (info$file_ext %in% c("html", "pdf")) {
        url <- paste0(res_prefix, "/", info$file_name, "?t=", as.numeric(Sys.time()))
        return(tags$iframe(src = url, style = "width:100%; height:800px; border:1px solid #ccc;"))
      }

      div(class = "alert alert-info", sprintf("Vista previa no disponible para .%s", info$file_ext))
    })

    # --- 4. DEBUG PANEL (Estilo Engine Sync) ---

    output$debug_internal <- listviewer::renderJsonedit({
      req(internal_show_debug())
      listviewer::jsonedit(listdata = data_store(), mode = "text")
    })

    output$show_debug_internal <- renderUI({
      req(internal_show_debug())
      div(class = "debug-section", style = "background: rgba(0,0,0,0.05); padding: 10px; border-left: 4px solid #3333fa; margin-top: 10px;",
          div(style = "font-weight: bold; margin-bottom: 5px;", icon("bug"), " Internal Debug - Show File"),
          listviewer::jsoneditOutput(ns("debug_internal"), height = "auto"))
    })

    # --- 5. ACCIONES ---
    observeEvent(input$open, {
      info <- data_store()
      req(info$file_exists)
      url <- paste0(res_prefix, "/", info$file_name, "?t=", as.numeric(Sys.time()))
      shinyjs::runjs(sprintf("window.open('%s', '_blank');", url))
    })

    output$download <- downloadHandler(
      filename = function() { data_store()$file_name },
      content = function(file) { file.copy(data_store()$path_full, file) }
    )

    # --- 6. RETORNO ---
    return(data_store)
  })
}
