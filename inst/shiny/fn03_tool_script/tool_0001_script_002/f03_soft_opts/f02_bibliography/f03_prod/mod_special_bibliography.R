library(shiny)
library(bslib)


mod_special_bibliography_ui <- function(id) {
  ns <- NS(id)
  container_id <- ns("bibliography_container")

  tagList(
    tags$head(
      tags$style(HTML(paste0("
        #", container_id, " .iframe-wrapper {
          position: relative;
          width: 100%;
          height: 82vh;
          border: 2px solid #adb5bd;
          border-radius: 0 0 12px 12px; /* Redondeado solo abajo para unir con el tab */
          overflow: hidden;
          background: #ffffff;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        #", container_id, " iframe {
          width: 100%;
          height: 100%;
          border: none;
          display: block;
        }
        /* Eliminamos el padding del body del card para que el iframe ocupe todo */
        #", container_id, " .card-body { padding: 0 !important; }
      ")))
    ),

    div(id = container_id,
        navset_card_tab(
          full_screen = TRUE,
          title = span(icon("graduation-cap"), "RScience Learning Center"),

          # Pestaña Principal de Teoría
          nav_panel(
            title = "Bibliography Content",
            card_body(
              div(class = "iframe-wrapper",
                  uiOutput(ns("iframe_handler")))
            )
          ),

          # Pestaña de Notas o Instrucciones (Espacio para el futuro)
          nav_panel(
            title = "Quick Help",
            card_body(
              div(style = "padding: 20px;",
                  h4("Navegación en RevealJS"),
                  p("Utiliza el clic del mouse o las flechas para avanzar."),
                  tags$ul(
                    tags$li(strong("M:"), " Ver menú de diapositivas."),
                    tags$li(strong("F:"), " Pantalla completa."),
                    tags$li(strong("C:"), " Activar pizarra (Chalkboard).")
                  )
              )
            )
          )
        )
    )
  )
}



mod_special_bibliography_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. RUTA MULTIPLATAFORMA ---
    # Construcción robusta de la ruta interna del paquete
    rel_path <- file.path("quarto", "tool_0001_anova_1_way", "f02_bibliography", "file01")
    file_name <- "bibliography.html"

    full_path <- system.file(rel_path, file_name, package = "Rscience2027")

    # --- 2. RECURSO SEGURO ---
    resource_prefix <- paste0("bibliography_core_", id)

    if (full_path != "") {
      # Forzamos barras '/' para compatibilidad con el navegador (web-standard)
      dir_to_share <- normalizePath(dirname(full_path), winslash = "/", mustWork = FALSE)
      addResourcePath(prefix = resource_prefix, directoryPath = dir_to_share)
    }

    # --- 3. RENDER ---
    output$iframe_handler <- renderUI({
      if (full_path != "") {
        tags$iframe(
          src = paste0(resource_prefix, "/", file_name),
          title = "RScience Theory Content",
          allow = "fullscreen; chalkboard"
        )
      } else {
        div(style = "padding: 40px; text-align: center;",
            icon("exclamation-triangle", style = "color: #dc3545; font-size: 3rem;"),
            h3("Teoría no encontrada"),
            p("Verifica la instalación del paquete Rscience2027."))
      }
    })

    message("Bibliography Module [", id, "] initialized. Path: ", full_path)
  })
}
