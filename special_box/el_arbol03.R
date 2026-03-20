# ==============================================================================
# RSCIENCE 2026 - MODULAR TREE ENGINE (FIXED LAYOUT & VISUAL LOCK) - v.0.0.1
# ==============================================================================
library(shiny)
library(bslib)
library(shinyjs)
library(collapsibleTree)
library(readxl)
library(dplyr)

# --- DATOS ---
file_path <- "arbol_estadistico.xlsx"
tree_data <- tryCatch({
  df <- readxl::read_excel(path = file_path, sheet = 1)
  names(df) <- tolower(names(df))
  df
}, error = function(e) {
  data.frame(nivel1="Error", nivel2="Error", nivel3="Error", nivel4="Error", nivel5="Error", size=25, script_id="s000")
})

# ==============================================================================
# MÓDULO UI
# ==============================================================================
treeAppUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$head(tags$style(HTML(paste0("
      .map-container { background: white; border-radius: 25px; padding: 20px; border: 1px solid #eee; position: relative; height: 750px; overflow: hidden; }
      .map-controls { position: absolute; top: 20px; right: 20px; z-index: 101; display: flex; flex-direction: column; gap: 10px; }
      .btn-map-tool { width: 45px; height: 45px; border-radius: 50% !important; border: none; display: flex; align-items: center; justify-content: center; box-shadow: 0 4px 10px rgba(0,0,0,0.1); color: white; }
      .btn-center-view { background-color: #00d4ff; }
      .btn-reset-map { background-color: #ffc107; }
      .side-panel { background: white; border-radius: 25px; padding: 30px; border: 1px solid #eee; min-height: 750px; }
      .path-chip { background: #f0fdf4; color: #28a745; padding: 12px; border-radius: 12px; margin-bottom: 8px; font-weight: 800; border: 1px solid #bbf7d0; display: block; }
      .debug-box { background: #121212; color: #00ff00; padding: 20px; border-radius: 12px; font-family: monospace; margin-top: 20px; border-left: 6px solid #00d4ff; }
      .script-val { color: #ffffff; font-size: 1.4rem; font-weight: 900; display: block; margin-top: 10px; }

      /* Header de selección - Fijo para evitar saltos */
      .selection-header { min-height: 60px; background: #f8f9fa; color: #a0aec0; padding: 15px 25px; border-radius: 15px; margin-bottom: 15px; font-weight: 700; display: flex; justify-content: space-between; align-items: center; border: 2px dashed #edf2f7; transition: all 0.3s ease; }
      .selection-header.active { background: #00d4ff; color: white; border: 2px solid #00d4ff; box-shadow: 0 4px 15px rgba(0,212,255,0.2); }
      .header-id { background: #edf2f7; color: #718096; padding: 5px 15px; border-radius: 50px; font-size: 0.9rem; }
      .active .header-id { background: white; color: #00d4ff; }

      /* Capa de bloqueo visual */
      #", ns("map_lock"), " {
        position:absolute; top:0; left:0; width:100%; height:100%;
        background: rgba(44, 62, 80, 0.5); /* Color oscuro con opacidad */
        backdrop-filter: blur(2px); /* Ligero desenfoque */
        z-index:100; cursor:not-allowed; border-radius:25px; display:none;
      }
    ")))),

    fluidRow(
      column(8,
             uiOutput(ns("selection_header")),
             div(class = "map-container",
                 div(class = "map-controls",
                     actionButton(ns("btn_center"), icon("compress-arrows-alt"), class = "btn-map-tool btn-center-view"),
                     actionButton(ns("btn_clear_map"), icon("sync-alt"), class = "btn-map-tool btn-reset-map")
                 ),
                 div(id = ns("map_lock")),
                 collapsibleTreeOutput(ns("stat_tree"), height = "700px")
             )
      ),
      column(4,
             div(class = "side-panel",
                 h5("RUTA: GENERAL -> PARTICULAR", style="font-weight:900; color:#cbd5e0; margin-bottom:20px;"),
                 uiOutput(ns("tree_selection_ui")),
                 hr(),
                 h6("SISTEMA DE IDENTIFICACIÓN", style="font-weight:800; color:#555;"),
                 uiOutput(ns("debug_ui")),
                 div(style="margin-top:30px;",
                     actionButton(ns("btn_select"), "Confirmar Script", class = "btn-success", style="width:100%; border-radius:50px; font-weight:800; padding: 15px; margin-bottom:10px;"),
                     actionButton(ns("btn_edit"), "Editar / Desbloquear", class = "btn-warning", style="width:100%; border-radius:50px; font-weight:800; padding: 15px; margin-bottom:10px;"),
                     actionButton(ns("btn_reset_all"), "Reiniciar App", class = "btn-danger", style="width:100%; border-radius:50px; font-weight:800; padding: 15px;")
                 )
             )
      )
    )
  )
}

# ==============================================================================
# MÓDULO SERVER
# ==============================================================================
treeAppServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_confirmed <- reactiveVal(FALSE)

    # 1. Árbol
    output$stat_tree <- renderCollapsibleTree({
      collapsibleTree(data, hierarchy = c("nivel1", "nivel2", "nivel3", "nivel4", "nivel5"),
                      root = "START", inputId = ns("node_click"), linkLength = 150,
                      fill = "#00d4ff", fillClosed = "#28a745", nodeSize = "size", zoomable = TRUE)
    })

    # 2. Lógica Script ID
    current_script_id <- reactive({
      path <- input$node_click
      if (is.null(path) || length(path) == 0) return(NA)
      p <- rev(path)
      res <- data
      try({
        if(length(p) >= 1) res <- res %>% filter(nivel1 == p[1])
        if(length(p) >= 2) res <- res %>% filter(nivel2 == p[2])
        if(length(p) >= 3) res <- res %>% filter(nivel3 == p[3])
        if(length(p) >= 4) res <- res %>% filter(nivel4 == p[4])
        if(length(p) >= 5) res <- res %>% filter(nivel5 == p[5])
      }, silent = TRUE)
      if(nrow(res) > 0) res$script_id[1] else "..."
    })

    # 3. HEADER PERSISTENTE (Evita saltos de página)
    output$selection_header <- renderUI({
      if (!is_confirmed()) {
        div(class = "selection-header",
            span(icon("hourglass-start"), " Esperando elección del usuario..."),
            span(class = "header-id", "ID: ---")
        )
      } else {
        path <- rev(input$node_click)
        div(class = "selection-header active",
            span(icon("check-circle"), paste(" CAMINO:", paste(path, collapse = " > "))),
            span(class = "header-id", paste("ID:", current_script_id()))
        )
      }
    })

    # 4. RUTA DERECHA
    output$tree_selection_ui <- renderUI({
      path <- input$node_click
      if (is.null(path) || length(path) == 0) return(p("Seleccione un nodo..."))
      path_ordered <- rev(path)
      lapply(seq_along(path_ordered), function(i) {
        div(class = "path-chip", span(style="color:#aaa; font-size:0.8rem; margin-right:10px;", paste0("LVL ", i)), path_ordered[i])
      })
    })

    # 5. DEBUG
    output$debug_ui <- renderUI({
      path <- input$node_click
      if (is.null(path) || length(path) == 0) return(div(class="debug-box", "SISTEMA IDLE..."))
      div(class="debug-box",
          div(">> TARGET: ", rev(path)[length(path)]),
          div(style="color:#fff; font-weight:bold; margin-top:10px;", ">> SCRIPT_ID:"),
          span(class="script-val", current_script_id()))
    })

    # 6. ACCIONES
    observeEvent(input$btn_select, {
      if(!is.null(input$node_click)) {
        is_confirmed(TRUE)
        shinyjs::show("map_lock") # Capa de color con opacidad
        shinyjs::disable("btn_select")
        showNotification("Script Confirmado Correctamente.", type = "message")
      } else {
        showNotification("Debe seleccionar una ruta primero.", type = "error")
      }
    })

    observeEvent(input$btn_edit, {
      is_confirmed(FALSE)
      shinyjs::hide("map_lock")
      shinyjs::enable("btn_select")
      showNotification("Mapa liberado para cambios.", type = "warning")
    })

    observeEvent(input$btn_reset_all, { session$reload() })
    observeEvent(input$btn_center, { session$reload() })
  })
}

# ==============================================================================
# APP
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),
  useShinyjs(),
  div(class = "container-fluid", style = "padding: 30px;",
      h1(style="color:#00d4ff; font-weight:900; margin-bottom:10px;", "STATISTICAL TREE ENGINE"),
      p("v.0.0.1 - Módulo de Navegación Jerárquica", style="color:#94a3b8; margin-bottom:25px;"),
      treeAppUI("main_tree")
  )
)

server <- function(input, output, session) {
  treeAppServer("main_tree", tree_data)
}

shinyApp(ui, server)
