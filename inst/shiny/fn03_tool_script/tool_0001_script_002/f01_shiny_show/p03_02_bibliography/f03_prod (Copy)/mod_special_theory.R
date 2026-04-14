library(shiny)
library(bslib)


mod_special_theory_ui <- function(id) {
  ns <- NS(id)
  container_id <- ns("theory_container")

  # 1. LOCALIZACIÓN DE RECURSOS (CSS, Imágenes, JS)
  # Buscamos la carpeta www dentro del paquete instalado o en el directorio local
  lib_www_path_relative <- system.file("www", package = "Rscience2027")

  # Si estamos en desarrollo y el paquete no está instalado, usamos "www"
  if (lib_www_path_relative == "") lib_www_path_relative <- "www"

  lib_www_path_absolute <- normalizePath(lib_www_path_relative, mustWork = TRUE)

  # Creamos el alias 'lib_www' para que el navegador acceda a las imágenes
  addResourcePath("lib_www", lib_www_path_absolute)

  # Ruta física para que R lea el CSS e inyecte el código
  path_to_css <- file.path(lib_www_path_absolute, "styles.css")

  tagList(
    tags$head(
      useShinyjs(),
      # Inyectamos el CSS directamente desde el archivo físico
      if (file.exists(path_to_css)) includeCSS(path_to_css)
    ),

    div(id = container_id,
        navset_card_tab(
          full_screen = TRUE,
          title = span(icon("graduation-cap"), "RScience Learning Center"),

          # Pestaña Principal de Teoría
          nav_panel(
            title = "Theory Content",
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



mod_special_theory_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. RUTA MULTIPLATAFORMA ---
    # Construcción robusta de la ruta interna del paquete
    rel_path <- file.path("quarto", "tool_0001_anova_1_way", "f01_theory", "file01")
    file_name <- "theory_anova.html"

    full_path <- system.file(rel_path, file_name, package = "Rscience2027")

    # --- 2. RECURSO SEGURO ---
    resource_prefix <- paste0("theory_core_", id)

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
          allow = "fullscreen; chalkboard",
          # --- ESTO ES LO IMPORTANTE ---
          style = "width: 100%; height: 800px; border: none; border-radius: 8px;"
        )
      } else {
        div(style = "padding: 40px; text-align: center;",
            icon("exclamation-triangle", style = "color: #dc3545; font-size: 3rem;"),
            h3("Teoría no encontrada"),
            p("Verifica la instalación del paquete Rscience2027."))
      }
    })

    message("Theory Module [", id, "] initialized. Path: ", full_path)
  })
}
