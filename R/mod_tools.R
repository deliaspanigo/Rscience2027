

#devtools::load_all()

mod_tools_ui <- function(id) {
  ns <- NS(id)
  root_sel <- paste0(".", ns("tools-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        /* 1. CONTENEDOR RAIZ */
        ", root_sel, " {
            display: flex;
            flex-direction: column;
            width: 100%;
            height: calc(100vh - 50px); /* Altura dinámica basada en el viewport */
            min-height: 700px;           /* Altura mínima de seguridad */
            padding: 20px 20px 40px 20px !important;
            background: #f8f9fa;
            box-sizing: border-box;
        }

        /* 2. HEADER Y CONTROLES (ESTÁTICOS) */
        ", root_sel, " .flex-shrink-0 { flex-shrink: 0; }

        /* 3. SECCIÓN DEL MAPA (ELÁSTICA) */
        ", root_sel, " .map-section {
            display: flex;
            flex-direction: column;
            flex-grow: 1;      /* Esto empuja al mapa a ocupar todo el resto */
            margin-top: 20px;
            min-height: 450px; /* <--- CRÍTICO: Si esto es 0, el tree no se dibuja */
        }

        ", root_sel, " .map-wrapper {
            background: #000;
            border-radius: 20px;
            border: 4px solid #1a202c;
            flex-grow: 1;      /* El wrapper llena la sección */
            position: relative;
            overflow: hidden;
            width: 100%;
            min-height: 400px; /* <--- CRÍTICO: Espacio mínimo para el SVG de D3 */
        }

        /* Estilos visuales de tus botones y labels */
        ", root_sel, " .selection-header { padding: 15px; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); }
        ", root_sel, " .path-display-area { background: #1a202c; border-radius: 12px; padding: 12px; min-height: 50px; display: flex; align-items: center; gap: 8px; }
        ", root_sel, " .path-chip { background: #28a745; color: white; padding: 5px 12px; border-radius: 50px; font-weight: 800; font-size: 0.8rem; }
        .custom-hr { border-top: 3px solid #00d4ff; margin: 20px 0; }
        .section-label { font-weight: 800; text-transform: uppercase; margin-bottom: 10px; color: #1a1a1a; }
      ")))
    ),

    div(class = paste("container-fluid", ns("tools-container")),

        # HEADER
        div(class = "row flex-shrink-0",
            div(class = "col-12", uiOutput(ns("tools_header")))
        ),

        # CONTROLES
        div(class = "row g-3 align-items-center flex-shrink-0", style="margin-top:10px;",
            div(class = "col-md-7",
                div(class = "section-label", icon("route"), " Current Tool Path"),
                div(class = "path-display-area", uiOutput(ns("path_chips_ui")))
            ),
            div(class = "col-md-5 text-end",
                actionButton(ns("btn_confirm"), "Confirm", class = "btn-success btn-pill-xl", icon = icon("check")),
                actionButton(ns("btn_edit"), "Edit", class = "btn-warning btn-pill-xl", icon = icon("edit")),
                actionButton(ns("btn_reset"), "Reset", class = "btn-primary btn-pill-xl", icon = icon("sync"))
            )
        ),

        div(class = "custom-hr flex-shrink-0"),

        # INFO BANNER
        div(class = "flex-shrink-0", uiOutput(ns("scripts_info_banner"))),

        # MAPA (Aquí es donde estaba el fallo de altura)
        div(class = "map-section",
            div(class = "section-label", icon("sitemap"), " Tool Selection Tree"),
            div(class = "map-wrapper",
                # IMPORTANTE: El tree necesita que su contenedor tenga altura
                mod_tree_ui(ns("inner_tree"))
            )
        )
    )
  )
}


mod_tools_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Motor D3 (Hijo)
    arbol_res <- mod_tree_server("inner_tree", show_debug = FALSE)
    is_locked <- reactiveVal(FALSE)

    # Lógica de bloqueo UI
    toggle_ui <- function(lock) {
      if(lock) {
        shinyjs::disable("btn_confirm")
        shinyjs::addClass(id = "main_tools_col", class = "locked-disabled")
      } else {
        shinyjs::enable("btn_confirm")
        shinyjs::removeClass(id = "main_tools_col", class = "locked-disabled")
      }
      is_locked(lock)
    }

    # Header Dinámico (Estilo Import)
    output$tools_header <- renderUI({
      res <- arbol_res()
      if (is_locked()) {
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" TOOL LOCKED:", res$node_name)),
            span(class = "header-id", "STATUS: READY"))
      } else if (res$node_name != "Rscience") {
        div(class = "selection-header active-selection",
            span(icon("file-import"), paste(" SELECTED:", res$node_name)),
            span(class = "header-id", "STATUS: PENDING"))
      } else {
        div(class = "selection-header waiting-mode",
            span(icon("bolt"), " Waiting for tool selection..."),
            span(class = "header-id", "STATUS: WAITING"))
      }
    })

    # Chips de Path
    output$path_chips_ui <- renderUI({
      path_string <- arbol_res()$path
      parts <- unlist(strsplit(path_string, " / "))
      if(length(parts) <= 1) return(span("Navigate tree to select...", style="color:#718096; italic;"))
      lapply(parts, function(p) span(class = "path-chip", p))
    })

    # Banner de Scripts (Estilo File Info)
    output$scripts_info_banner <- renderUI({
      res <- arbol_res()
      req(length(res$scripts) > 0)
      div(class = "info-banner-blue", fluidRow(
        column(6, span(style="font-weight:900; color:#00d4ff; font-size:1.1rem;", "TOOL: "), span(style="font-size:1.1rem; font-weight:600;", res$node_name)),
        column(3, span(style="font-weight:900; color:#00d4ff;", "SCRIPTS: "), span(length(res$scripts))),
        column(3, span(style="font-weight:900; color:#00d4ff;", "READY: "), span(if(is_locked()) "YES" else "NO"))
      ))
    })

    # Eventos de Botones
    observeEvent(input$btn_confirm, {
      if(arbol_res()$node_name != "Rscience") toggle_ui(TRUE)
    })

    observeEvent(input$btn_edit, toggle_ui(FALSE))

    observeEvent(input$btn_reset, {
      toggle_ui(FALSE)
      # Aquí podrías añadir un mensaje a mod_tree para resetear el zoom
    })

    return(reactive({
      list(confirmado = is_locked(), datos = arbol_res())
    }))
  })
}


# ui <- fluidPage(
#   mod_tools_ui("my_selector")
# )
#
# server <- function(input, output, session) {
#   # Resultado final de todo el flujo
#   resultado_final <- mod_tools_server("my_selector")
#
#   observe({
#     req(resultado_final()$confirmado)
#     print("--- SELECCIÓN FINAL RECIBIDA EN APP ---")
#     print(resultado_final()$datos$path)
#   })
# }
#
# shinyApp(ui, server)
