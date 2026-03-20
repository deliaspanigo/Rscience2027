# ==============================================================================
# RSCIENCE 2026 - TREE ENGINE (CENTER vs RESET) - v.0.0.1
# ==============================================================================
library(shiny)
library(bslib)
library(shinyjs)
library(collapsibleTree)
library(readxl)
library(dplyr)

# --- CARGA Y NORMALIZACIÓN ---
file_path <- "arbol_estadistico.xlsx"
tree_data <- tryCatch({
  df <- readxl::read_excel(path = file_path, sheet = 1)
  names(df) <- stringr::str_to_lower(names(df))
  df
}, error = function(e) {
  data.frame(nivel1="Error", nivel2="Error", nivel3="Error", nivel4="Error", nivel5="Error", size=25, script_id="s000")
})

if(!"size" %in% names(tree_data)) tree_data$size <- 25

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; }
      .engine-title { color: #00d4ff; font-weight: 900; }
      .map-container { background: white; border-radius: 25px; padding: 20px; border: 1px solid #eee; position: relative; height: 750px; }

      /* BOTONES FLOTANTES */
      .map-controls { position: absolute; top: 20px; right: 20px; z-index: 100; display: flex; flex-direction: column; gap: 10px; }
      .btn-map-tool { width: 45px; height: 45px; border-radius: 50% !important; display: flex; align-items: center; justify-content: center; box-shadow: 0 4px 10px rgba(0,0,0,0.1); border: none; transition: 0.2s; }
      .btn-map-tool:hover { transform: scale(1.1); }
      .btn-center-view { background-color: #00d4ff; color: white; }
      .btn-reset-map { background-color: #ffc107; color: white; }

      .side-panel { background: white; border-radius: 25px; padding: 30px; border: 1px solid #eee; min-height: 750px; }
      .path-chip { background: #f0fdf4; color: #28a745; padding: 12px; border-radius: 12px; margin-bottom: 8px; font-weight: 800; border: 1px solid #bbf7d0; }
      .debug-box { background: #121212; color: #00ff00; padding: 20px; border-radius: 12px; font-family: 'Courier New', monospace; margin-top: 20px; border-left: 6px solid #00d4ff; }
      .script-val { color: #ffffff; font-size: 1.4rem; font-weight: 900; display: block; margin-top: 10px; }
    "))
  ),

  div(class = "container-fluid", style = "padding: 30px;",
      h1(class = "engine-title", "STATISTICAL TREE ENGINE"),
      p("v.0.0.1 - Navegación Diferenciada"),

      fluidRow(
        column(8,
               div(class = "map-container",
                   div(class = "map-controls",
                       actionButton("btn_center", icon("compress-arrows-alt"), class = "btn-map-tool btn-center-view", title = "Centrar Visual (Mantiene selección)"),
                       actionButton("btn_clear_map", icon("sync-alt"), class = "btn-map-tool btn-reset-map", title = "Resetear Mapa (Colapsar todo)")
                   ),
                   uiOutput("tree_wrapper")
               )
        ),

        column(4,
               div(class = "side-panel",
                   h5("RUTA: GENERAL -> PARTICULAR", style="font-weight:900; color:#cbd5e0;"),
                   uiOutput("tree_selection_ui"),
                   hr(),
                   h6("SISTEMA DE IDENTIFICACIÓN", style="font-weight:800; color:#555;"),
                   uiOutput("debug_ui"),
                   div(style="margin-top:30px;",
                       actionButton("btn_select", "Confirmar Script", class = "btn-success", style="width:100%; border-radius:50px; font-weight:800; padding: 15px;"),
                       actionButton("btn_reset_all", "Reiniciar App", class = "btn-danger", style="width:100%; border-radius:50px; font-weight:800; margin-top:10px;")
                   )
               )
        )
      )
  )
)

server <- function(input, output, session) {

  # Trigger para el renderizado
  tree_refresh <- reactiveVal(0)

  # --- LÓGICA DE LOS BOTONES ---

  # 1. BOTÓN CENTRAR: Solo refresca el UI para volver al centro
  # pero NO limpia el input de Shiny si es posible
  observeEvent(input$btn_center, {
    tree_refresh(tree_refresh() + 1)
  })

  # 2. BOTÓN RESET MAPA: Recarga la sesión ligera para limpiar estados
  observeEvent(input$btn_clear_map, {
    shinyjs::runjs("history.go(0);") # Un refresh rápido que limpia el árbol
  })

  output$tree_wrapper <- renderUI({
    tree_refresh()
    collapsibleTreeOutput("stat_tree", height = "700px")
  })

  output$stat_tree <- renderCollapsibleTree({
    collapsibleTree(
      tree_data,
      hierarchy = c("nivel1", "nivel2", "nivel3", "nivel4", "nivel5"),
      root = "START",
      inputId = "selected_node",
      linkLength = 150,
      fill = "#00d4ff",
      fillClosed = "#28a745",
      nodeSize = "size",
      zoomable = TRUE
    )
  })

  # --- UI DE RUTA Y DEBUG (General -> Particular) ---

  output$tree_selection_ui <- renderUI({
    path_raw <- input$selected_node
    if (is.null(path_raw) || length(path_raw) == 0) return(p("Seleccione nodos..."))

    path_ordered <- rev(path_raw) # Invertimos para mostrar General -> Particular

    lapply(seq_along(path_ordered), function(i) {
      div(class = "path-chip", span(style="color:#aaa; font-size:0.8rem; margin-right:10px;", paste0("LVL ", i)), path_ordered[i])
    })
  })

  output$debug_ui <- renderUI({
    path_raw <- input$selected_node
    if (is.null(path_raw) || length(path_raw) == 0) return(div(class="debug-box", "IDLE..."))

    path <- rev(path_raw)
    res <- tree_data
    try({
      if(length(path) >= 1) res <- res %>% filter(nivel1 == path[1])
      if(length(path) >= 2) res <- res %>% filter(nivel2 == path[2])
      if(length(path) >= 3) res <- res %>% filter(nivel3 == path[3])
      if(length(path) >= 4) res <- res %>% filter(nivel4 == path[4])
      if(length(path) >= 5) res <- res %>% filter(nivel5 == path[5])
    }, silent = TRUE)

    sid <- if(nrow(res) > 0) res$script_id[1] else "NO ENCONTRADO"

    div(class="debug-box",
        div(">> TARGET_NODE: ", path[length(path)]),
        div(style="color:#fff; font-weight:bold; margin-top:10px;", ">> SCRIPT_ID DETECTED:"),
        span(class="script-val", sid)
    )
  })

  observeEvent(input$btn_reset_all, { session$reload() })
}

shinyApp(ui, server)
