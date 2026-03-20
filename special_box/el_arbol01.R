# ==============================================================================
# RSCIENCE 2026 - AUTO-COLLAPSE ENGINE (SINGLE PATH) - v.0.0.1
# ==============================================================================
library(shiny)
library(bslib)
library(shinyjs)
library(collapsibleTree)
library(readxl)
library(dplyr)

# --- DATOS ---
file_path <- "arbol_estadistico.xlsx"
tree_full <- if (file.exists(file_path)) {
  readxl::read_excel(path = file_path, sheet = 1)
} else {
  data.frame(
    Nivel1 = c("Desc", "Desc", "LM", "LM"),
    Nivel2 = c("1 Var", "2 Var", "Fijos", "Mixtos"),
    Nivel3 = c("Cuali", "Cuanti", "Anova", "LMM"),
    Nivel4 = c("Prop", "T-Test", "1-Factor", "Int-Aleat"),
    Size = 25
  )
}

# Limpieza inicial de la columna Size
tree_full$Size <- as.numeric(as.character(tree_full$Size))
tree_full$Size[is.na(tree_full$Size)] <- 25

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#00d4ff"),
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; }
      .engine-title { color: #00d4ff; font-weight: 900; }
      .map-container {
        background: white; border-radius: 25px; padding: 20px;
        border: 1px solid #e0e0e0; position: relative; height: 750px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.03);
      }
      .side-panel {
        background: white; border-radius: 25px; padding: 30px;
        border: 1px solid #e0e0e0; min-height: 750px;
      }
      .path-chip {
        background: #f0fdf4; color: #28a745; padding: 12px;
        border-radius: 12px; font-weight: 700; margin-bottom: 10px;
        border: 1px solid #bbf7d0; display: flex; align-items: center;
      }
      .btn-pill-xl { border-radius: 50px !important; padding: 15px !important; font-weight: 800; width: 100%; margin-bottom: 15px; }
      .locked-overlay {
        position: absolute; top: 0; left: 0; width: 100%; height: 100%;
        background: rgba(255, 255, 255, 0.85); display: none;
        align-items: center; justify-content: center; z-index: 1000; border-radius: 25px;
      }
      .locked-overlay.active { display: flex; }
    "))
  ),

  div(class = "container-fluid", style = "padding: 30px;",
      h1(class = "engine-title", "STATISTICAL TREE ENGINE"),
      p("v.0.0.1 - Auto-Collapse Mode"),

      fluidRow(
        # COLUMNA 8: EL MAPA
        column(8,
               div(class = "map-container",
                   actionButton("btn_center", icon("expand"), class = "btn-outline-primary",
                                style="position:absolute; top:20px; right:20px; z-index:10; border-radius:50%; width:45px; height:45px;"),

                   div(id = "tree_container_rel", style="position:relative;",
                       collapsibleTreeOutput("stat_tree", height = "700px"),
                       div(id = "locked_overlay", class = "locked-overlay",
                           div(style="text-align:center;", icon("lock", "fa-5x", style="color:#dc3545;"), h2("BLOQUEADO"))
                       )
                   )
               )
        ),

        # COLUMNA 4: DETALLE
        column(4,
               div(class = "side-panel",
                   h5("RUTA ACTUAL", style="font-weight:900; color:#ccc;"),
                   uiOutput("tree_selection_ui"),
                   hr(),
                   actionButton("btn_select", tagList(icon("bolt"), "Confirmar"), class = "btn-pill-xl btn-success"),
                   actionButton("btn_edit", tagList(icon("edit"), "Modificar"), class = "btn-pill-xl btn-warning"),
                   actionButton("btn_reset", tagList(icon("sync"), "Reiniciar"), class = "btn-pill-xl btn-danger")
               )
        )
      )
  )
)

server <- function(input, output, session) {

  # Variable reactiva para el zoom/reset
  zoom_trigger <- reactiveVal(0)
  observeEvent(input$btn_center, { zoom_trigger(zoom_trigger() + 1) })

  # RENDER DEL ÁRBOL
  output$stat_tree <- renderCollapsibleTree({
    # Forzamos re-render si se pulsa centrar
    zoom_trigger()

    collapsibleTree(
      tree_full,
      hierarchy = c("Nivel1", "Nivel2", "Nivel3", "Nivel4"),
      root = "START",
      inputId = "selected_node",
      linkLength = 160,
      fill = "#28a745",
      fillClosed = "#00d4ff",
      nodeSize = "Size",
      fontSize = 15,
      zoomable = TRUE
    )
  })

  # RUTA CRÍTICA Y LÓGICA DE AUTO-COLLAPSE VISUAL
  output$tree_selection_ui <- renderUI({
    path <- input$selected_node
    if (is.null(path) || length(path) == 0) return(p("Seleccione un nodo..."))

    # Mostramos los chips
    chips <- lapply(seq_along(path), function(i) {
      div(class = "path-chip", span(style="margin-right:10px; opacity:0.5;", i), path[i])
    })
    tagList(chips)
  })

  # EVENTOS
  observeEvent(input$btn_select, {
    req(input$selected_node)
    shinyjs::addClass("locked_overlay", "active")
    shinyjs::disable("btn_select")
    showNotification("Ruta confirmada.", type = "message")
  })

  observeEvent(input$btn_edit, {
    shinyjs::removeClass("locked_overlay", "active")
    shinyjs::enable("btn_select")
  })

  observeEvent(input$btn_reset, {
    session$reload()
  })
}

shinyApp(ui, server)
