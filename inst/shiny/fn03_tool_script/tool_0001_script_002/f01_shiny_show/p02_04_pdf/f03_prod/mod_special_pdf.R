# ==============================================================================
# SUB-MÓDULO: mod_special_shiny_output (Hijo con archivos Hardcodeados)
# ==============================================================================

mod_special_pdf_ui <- function(id) {
  ns <- NS(id)
  div(id = ns("container"), class = "mb-4",
      uiOutput(ns("tabs_area")),
      uiOutput(ns("show_debug_internal"))
  )
}

mod_special_pdf_server <- function(id,
                                                   temp_folder_tool_script, # Carpeta base
                                                   show_file = TRUE,
                                                   show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. PROCESAMIENTO DE LA CARPETA BASE ---
    internal_folder_temp <- reactive({
      if (is.function(temp_folder_tool_script)) temp_folder_tool_script() else temp_folder_tool_script
    })

    internal_show_file <- reactive({
      if (is.function(show_file)) show_file() else show_file
    })

    internal_show_debug <- reactive({
      if (is.function(show_debug)) show_debug() else show_debug
    })

    # --- 2. HARDCODE DE ARCHIVOS ---
    # Definimos aquí la estructura fija de lo que este módulo debe mostrar
    data_config <- reactive({
      base_path <- internal_folder_temp()
      req(base_path)

      # Construimos la ruta hacia la subcarpeta donde Quarto dejó los resultados
      target_dir <- file.path(base_path, "f02_quarto_proc", "f08_pdf")

      # Lista fija de archivos y sus etiquetas
      list(
        list(file = "report_pdf.pdf", label = "Summary Anova", super = "Summary Anova")
      ) |>
        lapply(function(x) {
          x$full_path <- file.path(target_dir, x$file)
          x$exists    <- file.exists(x$full_path)
          x
        })
    })

    res_prefix <- paste0("res_show_", id)

    # --- 3. GENERACIÓN DE TABS ---
    output$tabs_area <- renderUI({
      items <- data_config()
      req(length(items) > 0)

      tab_panels <- lapply(seq_along(items), function(i) {
        it <- items[[i]]

        bslib::nav_panel(
          title = it$label,
          div(class = "p-3 border rounded bg-white text-dark",
              div(class = "p-2 mb-3 bg-light border-bottom",
                  fluidRow(
                    column(6, tags$strong(it$super)),
                    column(6, div(class = "text-end",
                                  if(it$exists) {
                                    tagList(
                                      actionButton(ns(paste0("open_", i)), " Abrir",
                                                   icon = icon("external-link-alt"), class = "btn-sm btn-info"),
                                      downloadButton(ns(paste0("dl_", i)), " Descargar", class = "btn-sm btn-secondary")
                                    )
                                  } else {
                                    span(class = "badge bg-danger", "No generado")
                                  }
                    ))
                  )
              ),
              # Renderizado del Iframe
              if(it$exists && isTRUE(internal_show_file())) {
                resource_name <- paste0(res_prefix, "_", i)
                addResourcePath(prefix = resource_name, directoryPath = normalizePath(dirname(it$full_path)))

                url <- paste0(resource_name, "/", it$file, "?t=", as.numeric(Sys.time()))
                tags$iframe(src = url, style = "width:100%; height:750px; border:none;")
              } else if (!it$exists) {
                div(class = "alert alert-warning", "Aún no se ha generado este reporte.")
              }
          )
        )
      })

      do.call(bslib::navset_card_pill, c(tab_panels, list(id = ns("main_nav"))))
    })

    # --- 4. MANEJO DE ACCIONES ---
    observe({
      items <- data_config()
      lapply(seq_along(items), function(i) {
        it <- items[[i]]

        output[[paste0("dl_", i)]] <- downloadHandler(
          filename = function() { it$file },
          content = function(file) { file.copy(it$full_path, file) }
        )

        observeEvent(input[[paste0("open_", i)]], {
          resource_name <- paste0(res_prefix, "_", i)
          url <- paste0(resource_name, "/", it$file, "?t=", as.numeric(Sys.time()))
          shinyjs::runjs(sprintf("window.open('%s', '_blank');", url))
        })
      })
    })

    # --- 5. DEBUG ---
    output$debug_json <- listviewer::renderJsonedit({
      listviewer::jsonedit(data_config())
    })

    output$show_debug_internal <- renderUI({
      req(show_debug)
      div(class = "mt-4 p-2 border-top bg-dark text-light",
          tags$small(icon("bug"), " Debug Hijo: Archivos Hardcodeados"),
          listviewer::jsoneditOutput(ns("debug_json"), height = "250px"))
    })

    return(reactive({
      items <- data_config()
      list(
        files = sapply(items, function(x) x$full_path),
        is_done = all(sapply(items, function(x) x$exists))
      )
    }))
  })
}
