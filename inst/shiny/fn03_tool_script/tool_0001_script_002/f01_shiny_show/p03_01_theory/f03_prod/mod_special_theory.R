library(shiny)
library(bslib)
library(listviewer)

mod_special_theory_DEBUG_ui <-  function(id) {

  ns <- NS(id)

  tagList(
    uiOutput(ns("show_debug_external"))
  )

}


mod_special_theory_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # CSS para responsividad del Iframe
    tags$style(HTML("
      .rs-iframe-container {
        position: relative;
        width: 100%;
        height: 750px;
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 4px 15px rgba(0,0,0,0.3);
      }
    ")),

    navset_card_tab(
      full_screen = TRUE,
      title = span(icon("graduation-cap"), "RScience Theory Engine"),

      # --- PANEL PRINCIPAL ---
      nav_panel(
        title = "Contenido",
        card_body(
          uiOutput(ns("iframe_handler"))
        )
      ),

      # --- PANEL DE DEBUG (Solo visible si show_debug = T) ---
      nav_panel(
        title = span("Help"),
        card_body(
          h5("Detalles del help"),
        )
      )
    )
  )
}

mod_special_theory_server <- function(id, show_debug) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. METADATOS: Corregido nombres de variables
    theory_meta <- reactive({
      rel_path  <- file.path("quarto", "tool_0001_anova_1_way", "f01_theory", "file01")
      file_name <- "theory_anova.html"

      full_path <- system.file(rel_path, file_name, package = "Rscience2027")

      if (full_path == "") {
        # Fallback para desarrollo local
        full_path <- normalizePath(file.path(rel_path, file_name), mustWork = FALSE)
      }

      # IMPORTANTE: Coherencia en los nombres (usamos 'exists')
      list(
        "description" =  "Submodulo especial Theory",
        "help" =         "Check tool!!!",
        file_name       = file_name,
        full_path       = full_path,
        exists          = file.exists(full_path), # Antes era file_exists
        dir_path        = dirname(full_path),
        resource_prefix = paste0("theory_core_", id), # Faltaba en la lista
        timestamp       = Sys.time(),
        status          = if(file.exists(full_path)) "SUCCESS" else "FILE_NOT_FOUND"
      )
    })

    # 2. RECURSO: Registro seguro
    observe({
      meta <- theory_meta()
      # Corregido: meta$exists ahora existe
      if (isTRUE(meta$exists)) {
        addResourcePath(
          prefix = meta$resource_prefix,
          directoryPath = meta$dir_path
        )
      }
    })

    # 3. RENDER: Iframe dinámico
    output$iframe_handler <- renderUI({
      meta <- theory_meta()
      req(meta)

      if (isTRUE(meta$exists)) {
        div(class = "rs-iframe-container",
            tags$iframe(
              src = paste0(meta$resource_prefix, "/", meta$file_name),
              width = "100%",
              height = "100%",
              style = "border:none;",
              allow = "fullscreen; chalkboard"
            )
        )
      } else {
        div(style = "padding: 40px; text-align: center; border: 2px solid #ff4b4b;",
            icon("circle-exclamation", style = "font-size: 3em; color: #ff4b4b;"),
            h4("Error de Localización"),
            p("No se encontró el archivo .html"),
            code(meta$full_path))
      }
    })

    # 4. DEBUG RENDERS
    output$debug_internal <- listviewer::renderJsonedit({
      listviewer::jsonedit(theory_meta(), mode = "text")
    })

    output$debug_external <- listviewer::renderJsonedit({
      listviewer::jsonedit(theory_meta(), mode = "text")
    })

    output$show_debug_external <- renderUI({
      div(style = "background: #1a1a1a; padding: 15px; border-radius: 8px;",
          h5("DEBUG - Special Theory", style="color: #00d4ff;"),
          listviewer::jsoneditOutput(ns("debug_external"), height = "500px")
      )
    })

    return(theory_meta)
  })
}
