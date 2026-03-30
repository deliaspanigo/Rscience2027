library(shiny)

# --- Módulo UI 1 ---
library(shiny)
library(bslib)

library(shiny)
library(bslib)

library(shiny)
library(bslib)

mod_special_cite_ui <- function(id) {
  ns <- NS(id)
  container_id <- ns("cite_container")

  tagList(
    tags$head(
      tags$style(HTML(paste0("
        #", container_id, " { margin-bottom: 25px; }

        #", container_id, " .section-title {
          font-size: 1.1rem;
          font-weight: 700;
          color: #00838f;
          border-bottom: 2px solid #00acc1;
          padding-bottom: 5px;
          margin-bottom: 15px;
          display: flex;
          align-items: center;
          gap: 10px;
        }

        #", container_id, " .method-box {
          background-color: #f8f9fa;
          border-left: 5px solid #495057;
          padding: 20px;
          border-radius: 4px;
          font-size: 0.95rem;
          line-height: 1.6;
          color: #212529;
          margin-bottom: 30px;
          box-shadow: inset 0 0 10px rgba(0,0,0,0.02);
        }

        #", container_id, " .format-container {
          background-color: #ffffff;
          border: 1px solid #dee2e6;
          border-radius: 8px;
          padding: 15px;
        }

        #", container_id, " .nav-pills .nav-link.active {
          background-color: #00acc1 !important;
          color: white !important;
        }

        #", container_id, " .cite-display {
          margin-top: 15px;
          padding: 15px;
          background-color: #212529;
          color: #00e5ff;
          font-family: 'Monaco', 'Consolas', monospace;
          font-size: 0.85rem;
          border-radius: 6px;
          min-height: 80px;
        }

        #", container_id, " .vancouver-text, #", container_id, " .apa-text {
          color: #e9ecef; /* Color claro para texto en fondo oscuro */
          font-family: inherit;
        }
      ")))
    ),

    div(id = container_id,
        card(
          card_body(
            # --- SECCIÓN 1: MATERIALES Y MÉTODOS (Directo) ---
            div(class = "section-title", icon("pen-fancy"), "Materiales y Métodos"),
            p("Puedes copiar y adaptar el siguiente párrafo para tu manuscrito:"),
            div(class = "method-box",
                "El análisis de varianza (ANOVA) se realizó utilizando el módulo estadístico ",
                strong("'Script 0001-ANOVA-1-WAY'"), " del ecosistema ",
                strong("RScience (v.0.0.1; Panigo, 2026)"),
                ". El procesamiento automatizado aseguró la integridad de los datos y la estandarización de los reportes técnicos en el entorno R."),

            # --- SECCIÓN 2: BIBLIOGRAFÍA (Tabs para Formatos) ---
            div(class = "section-title", icon("bookmark"), "Formatos de Referencia"),
            div(class = "format-container",
                navset_pill(
                  id = ns("format_tabs"),
                  nav_panel(
                    title = "APA 7th",
                    div(class = "cite-display",
                        div(class = "apa-text",
                            "Panigo, D. E. (2026). RScience (v.0.0.1): One-way ANOVA Statistical Tool [Software]. Script 0001-ANOVA-1-WAY. Trento, Italy."))
                  ),
                  nav_panel(
                    title = "Vancouver",
                    div(class = "cite-display",
                        div(class = "vancouver-text",
                            "Panigo DE. RScience (v.0.0.1): One-way ANOVA Statistical Tool [Software]. Script 0001-ANOVA-1-WAY. Trento, Italy; 2026."))
                  ),
                  nav_panel(
                    title = "BibTeX",
                    div(class = "cite-display",
                        tags$pre(style = "color: inherit; margin-bottom: 0;",
                                 "@manual{rscience_anova_2026,", tags$br(),
                                 "  title  = {RScience (v.0.0.1): One-way ANOVA Statistical Tool},", tags$br(),
                                 "  author = {Panigo, David Elias},", tags$br(),
                                 "  year   = {2026},", tags$br(),
                                 "  note   = {Script 0001-ANOVA-1-WAY},", tags$br(),
                                 "  address= {Trento, Italy}", tags$br(),
                                 "}"
                        )
                    )
                  )
                )
            )
          )
        )
    )
  )
}



# --- Módulo Server Único ---
# Este servidor orquestará la lógica (aunque en este caso sea mínima)
mod_special_cite_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Log para trazabilidad de la versión
    message("Citation Module [", id, "] initialized - USC: 0001-ANOVA-1-WAY")

  })
}





