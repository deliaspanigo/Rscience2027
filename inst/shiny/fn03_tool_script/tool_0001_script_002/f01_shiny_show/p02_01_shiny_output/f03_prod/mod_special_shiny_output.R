# ==============================================================================
# SUB-MÓDULO: mod_06_00_show_file (Sin CSS personalizado)
# ==============================================================================

mod_special_shiny_output_ui <- function(id) {
  ns <- NS(id)
  div(id = ns("container"), class = "mb-4",
      uiOutput(ns("header_ui")),
      uiOutput(ns("display_zone"))
  )
}

mod_special_shiny_output_server <- function(id,
                                       super_label = "Visualizador de Archivo",
                                       file_path,
                                       show_file = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    internal_super_label  <- reactive({ if (is.function(super_label)) super_label() else super_label })
    internal_file_path  <- reactive({ if (is.function(file_path)) file_path() else file_path })
    internal_show_file  <- reactive({ if (is.function(show_file)) show_file() else show_file })


    res_prefix <- paste0("res_show_", id)

    # 1. CABECERA
    output$header_ui <- renderUI({
      path <- internal_file_path()
      exists <- !is.null(path) && file.exists(path)

      div(class = "p-3 border rounded bg-light mb-2",
          fluidRow(class = "align-items-center",
                   column(6, tags$strong(internal_super_label())),
                   column(6, div(class = "text-end",
                                 if(exists) {
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

    # 2. VISUALIZACIÓN
    output$display_zone <- renderUI({
      req(internal_show_file())
      path <- internal_file_path()

      if (is.null(path) || !file.exists(path)) {
        return(div(class = "alert alert-warning", "El archivo no existe."))
      }

      ext <- tolower(tools::file_ext(path))
      out_dir <- normalizePath(dirname(path), mustWork = FALSE)
      addResourcePath(prefix = res_prefix, directoryPath = out_dir)

      # Caso: R
      if (ext == "r") {
        lineas <- tryCatch(readLines(path, warn = FALSE), error = function(e) "Error de lectura.")
        return(div(class = "p-3 border rounded bg-white",
                   tags$pre(tags$code(paste(lineas, collapse = "\n")))))
      }

      # Caso: HTML / PDF
      if (ext %in% c("html", "pdf")) {
        url <- paste0(res_prefix, "/", basename(path), "?t=", as.numeric(Sys.time()))
        return(tags$iframe(src = url, style = "width:100%; height:750px; border:1px solid #ccc;"))
      }

      # Caso: No soportado
      div(class = "alert alert-info",
          sprintf("Vista previa no disponible para formato .%s", ext))
    })

    # 3. ACCIONES
    observeEvent(input$open, {
      path <- internal_file_path()
      req(path, file.exists(path))
      url <- paste0(res_prefix, "/", basename(path), "?t=", as.numeric(Sys.time()))
      runjs(sprintf("window.open('%s', '_blank');", url))
    })

    output$download <- downloadHandler(
      filename = function() { basename(internal_file_path()) },
      content = function(file) { file.copy(internal_file_path(), file) }
    )


    # Al final del server del hijo
    return(
      reactive({
        list(
          is_done = TRUE,
          current_file = internal_file_path()
        )
      })
    )
  })
}
