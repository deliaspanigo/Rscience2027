# ==============================================================================
# 1. SUB-MÓDULO ATÓMICO: mod_run_and_show
# ==============================================================================

mod_06_00_render_and_show_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        #", ns("container"), " .btn-r-normal { background-color: #0d6efd !important; color: white !important; border: none; }
        #", ns("container"), " .btn-r-success { background-color: #198754 !important; color: white !important; border: none; }
        #", ns("container"), " .btn-r-error { background-color: #dc3545 !important; color: white !important; border: none; }
        #", ns("container"), " iframe { background-color: white; width: 100%; height: 750px; border: 1px solid #ddd; border-radius: 8px; margin-top: 15px; }
        #", ns("container"), " .debug-box { background: #f8f9fa; border: 1px solid #dee2e6; border-left: 4px solid #0d6efd; padding: 10px; margin-top: 10px; font-family: monospace; font-size: 0.85em; }
        #", ns("container"), " .header-row-custom { padding: 10px 15px; border-radius: 8px; transition: background-color 0.3s ease; }
        #", ns("container"), " .header-label-text { margin: 0; font-weight: 600; color: #212529; }
      ")))
    ),
    div(id = ns("container"), class = "mb-4",
        uiOutput(ns("header_ui")),
        uiOutput(ns("debug_zone")),
        uiOutput(ns("display_zone"))
    )
  )
}

mod_06_00_render_and_show_server <- function(id,
                                             super_label = "Procesamiento",
                                             bg_color = "#f8f9fa",
                                             input_file_path_qmd,
                                             output_file_path,
                                             stamp_time = reactive(Sys.time()),
                                             show_debug = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prefijo único para los recursos de este módulo
    res_prefix <- paste0("res_run_", id)

    # 1. RENDERIZAR LA CABECERA (Botones y Label)
    output$header_ui <- renderUI({
      div(class = "header-row-custom", style = paste0("background-color: ", bg_color, ";"),
          fluidRow(class = "align-items-center",
                   column(6, tags$h5(super_label, class = "header-label-text")),
                   column(6, div(class = "d-flex justify-content-end gap-2",
                                 actionButton(ns("run"), " Renderizar", icon = icon("play"), class = "btn-r-normal"),
                                 disabled(actionButton(ns("open"), " Abrir", icon = icon("external-link-alt"), class = "btn-info")),
                                 disabled(downloadButton(ns("download"), " Descargar", class = "btn-outline-secondary"))
                   ))
          )
      )
    })

    # 2. REGISTRO DE RECURSOS (Versión reforzada)
    res_prefix <- paste0("res_run_", id)

    # Registramos la ruta apenas inicia el módulo si la carpeta ya existe
    observe({
      o_path <- output_file_path()
      req(o_path)
      out_dir <- normalizePath(dirname(o_path), mustWork = FALSE)

      if (dir.exists(out_dir)) {
        # El parámetro 'warn' evita mensajes molestos si se registra varias veces
        addResourcePath(prefix = res_prefix, directoryPath = out_dir)
      }
    })

    # 3. DEBUG ZONE (Restaurada con tus indicadores OK/X)
    output$debug_zone <- renderUI({
      if (!show_debug) return(NULL)

      # Reacciona cuando se presiona ejecutar para actualizar estados
      input$run

      q_path <- input_file_path_qmd()
      o_path <- output_file_path()

      q_exists <- if(!is.null(q_path)) file.exists(q_path) else FALSE
      o_exists <- if(!is.null(o_path)) file.exists(o_path) else FALSE

      div(class = "debug-box",
          tags$b("QMD: "), tags$code(q_path),
          if(q_exists) span(" [OK]", style="color:green") else span(" [X]", style="color:red"),
          br(),
          tags$b("OUT: "), tags$code(o_path),
          if(o_exists) span(" [OK]", style="color:green") else span(" [X]", style="color:red")
      )
    })

    # 4. LÓGICA DE RENDERIZADO
    observeEvent(input$run, {
      q_path <- input_file_path_qmd()
      o_path <- output_file_path()

      if (is.null(q_path) || !file.exists(q_path)) {
        runjs(sprintf("$('#%s').addClass('btn-r-error').removeClass('btn-r-normal')", ns("run")))
        showNotification("Error: Archivo fuente no encontrado.", type = "error")
        return()
      }

      showModal(modalDialog(
        title = "Quarto Engine",
        div(class = "text-center", icon("sync", class = "fa-spin fa-2x mb-2", style="color:#0d6efd"),
            p("Generando el documento...")),
        footer = NULL, easyClose = FALSE
      ))

      tryCatch({
        quarto::quarto_render(input = q_path, output_format = "all")

        # Re-confirmamos el acceso a la carpeta tras crear el archivo
        addResourcePath(prefix = res_prefix, directoryPath = normalizePath(dirname(o_path)))

        if(file.exists(o_path)) {
          runjs(sprintf("$('#%s').addClass('btn-r-success').removeClass('btn-r-normal btn-r-error')", ns("run")))
          disable("run"); enable("open"); enable("download")
        }
      }, error = function(e) {
        runjs(sprintf("$('#%s').addClass('btn-r-error').removeClass('btn-r-normal')", ns("run")))
        showNotification(e$message, type = "error")
      })
      removeModal()
    })

    # 5. VISUALIZACIÓN E INTERACCIÓN
    output$display_zone <- renderUI({

      # 1. Agregamos esto para que el UI se refresque cuando hagas clic en Run
      # Pero NO usamos req(input$run) porque eso bloquearía la carga si el archivo ya existe.
      force_refresh <- input$run

      o_path <- output_file_path()

      # Verificamos si el archivo existe físicamente
      if (is.null(o_path) || !file.exists(o_path)) return(NULL)

      ext <- tolower(tools::file_ext(o_path))

      # Registro preventivo: Refrescamos la ruta justo antes de mostrar
      out_dir <- normalizePath(dirname(o_path), mustWork = FALSE)
      addResourcePath(prefix = res_prefix, directoryPath = out_dir)

      # Caso archivos de código R
      if (ext == "r") {
        lineas_codigo <- readLines(o_path, warn = FALSE)
        return(
          div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #ddd; margin-top: 15px; max-height: 750px; overflow: auto;",
            tags$pre(tags$code(paste(lineas_codigo, collapse = "\n")))
          )
        )
      }

      # Caso HTML o PDF (vía iFrame)
      if (ext %in% c("html", "pdf")) {
        # Usamos Sys.time() para que el navegador ignore cualquier 404 previo
        url <- paste0(res_prefix, "/", basename(o_path), "?t=", as.numeric(Sys.time()))
        tags$iframe(src = url)
      }
    })


    # 6. ACCIONES ADICIONALES
    observeEvent(input$open, {
      url <- paste0(res_prefix, "/", basename(output_file_path()), "?t=", as.numeric(Sys.time()))
      runjs(sprintf("window.open('%s', '_blank');", url))
    })

    output$download <- downloadHandler(
      filename = function() { basename(output_file_path()) },
      content = function(file) { file.copy(output_file_path(), file) }
    )
  })
}
