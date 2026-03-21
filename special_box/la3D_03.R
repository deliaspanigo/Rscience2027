library(shiny)
library(visNetwork)
library(readxl)
library(dplyr)
library(shinyjs)

# --- 1. PROCESAMIENTO DE DATOS (Transformación de Excel a Red) ---
# Esta función toma tu Excel jerárquico y crea las tablas de Nodos y Edges
transformar_excel_a_red <- function(ruta_excel) {

  # Leer Excel
  df <- read_xlsx(ruta_excel)

  # Crear una columna de Ruta Única para evitar problemas con nombres repetidos
  # Ej: "Estadística > 1 Var" vs "Pruebas > 1 Var"
  df_path <- df %>%
    mutate(
      path0 = "Rscience",
      path1 = paste(path0, Nivel1, sep = "|"),
      path2 = paste(path1, Nivel2, sep = "|"),
      path3 = paste(path2, Nivel3, sep = "|"),
      path4 = paste(path3, Nivel4, sep = "|"),
      path5 = paste(path4, Nivel5, sep = "|")
    )

  # Generar la tabla de EDGES (Relaciones Padre-Hijo)
  edges1 <- df_path %>% select(from = path0, to = path1) %>% distinct()
  edges2 <- df_path %>% select(from = path1, to = path2) %>% distinct()
  edges3 <- df_path %>% select(from = path2, to = path3) %>% distinct()
  edges4 <- df_path %>% select(from = path3, to = path4) %>% distinct()
  edges5 <- df_path %>% select(from = path4, to = path5) %>% distinct()

  all_edges_raw <- bind_rows(edges1, edges2, edges3, edges4, edges5) %>%
    filter(!is.na(to) & !grepl("\\|NA$", to)) # Limpiar NAs

  # Generar la tabla de NODOS ÚNICOS
  all_nodes_paths <- unique(c(all_edges_raw$from, all_edges_raw$to))

  nodes <- data.frame(path = all_nodes_paths, stringsAsFactors = FALSE) %>%
    mutate(
      id = 1:n(), # ID numérico único para visNetwork
      # El label es solo el último nombre de la ruta (lo que ve el usuario)
      label = sapply(strsplit(path, "\\|"), tail, 1),
      # Nivel jerárquico para el layout
      level = sapply(strsplit(path, "\\|"), length)
    )

  # Mapear los nombres de ruta en los edges a los IDs numéricos de los nodos
  edges <- all_edges_raw %>%
    left_join(nodes %>% select(id, path), by = c("from" = "path")) %>%
    rename(from_id = id) %>%
    left_join(nodes %>% select(id, path), by = c("to" = "path")) %>%
    rename(to_id = id) %>%
    select(from = from_id, to = to_id) %>%
    mutate(id = 1:n()) # ID único para cada CABLE

  # Limpiar la tabla de nodos para visNetwork
  nodes <- nodes %>% select(id, label, level, path)

  return(list(nodes = nodes, edges = edges))
}


# --- 2. INTERFAZ DE USUARIO (UI) ---
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),

  tags$head(tags$style(HTML("
    .path-header {
      background: #111827; border: 1px solid #374151; padding: 15px;
      border-radius: 12px; margin-bottom: 15px; min-height: 70px;
    }
    .node-chip {
      background: #ff9100; color: white; padding: 5px 12px;
      border-radius: 6px; font-weight: bold; margin-right: 5px;
      border: 1px solid #b36600; font-size: 0.9rem;
    }
    .sep { color: #6b7280; }
    .network-box { background: white; border-radius: 15px; padding: 10px; box-shadow: inset 0 0 10px rgba(0,0,0,0.1); }
  "))),

  titlePanel("Rscience: Explorador Interactivo (visNetwork)"),

  div(class = "path-header",
      span(style="color:#9ca3af; font-size:0.8rem; font-weight:bold; display:block; margin-bottom:5px;", "RUTA ACTIVA:"),
      uiOutput("breadcrumb_ui")
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Interacción"),
      p("Haz clic en cualquier nodo para aislar su ruta hacia la raíz."),
      p("visNetwork permite arrastrar nodos y hacer zoom."),
      actionButton("reset", "Ver Árbol Completo", class="btn-warning w-100"),
      width = 3
    ),

    mainPanel(
      div(id = "net-wrapper", class = "network-box",
          visNetworkOutput("network_plot", height = "750px")
      )
    )
  )
)

# --- 3. SERVIDOR (SERVER) ---
server <- function(input, output, session) {

  # Cargar y transformar datos una sola vez al inicio
  red_data <- reactive({
    tryCatch({
      transformar_excel_a_red("arbol_estadistico.xlsx")
    }, error = function(e) {
      stop("No se pudo encontrar o leer 'arbol_estadistico.xlsx'. Asegúrate de que esté en la misma carpeta.")
    })
  })

  # Objeto reactivo para guardar la selección actual
  v <- reactiveValues(selected_node = NULL, ancestor_ids = NULL, edge_ids = NULL)

  # Renderizado inicial del mapa
  output$network_plot <- renderVisNetwork({
    data <- red_data()

    visNetwork(data$nodes, data$edges) %>%
      # Layout jerárquico (de izquierda a derecha)
      visHierarchicalLayout(direction = "LR", sortMethod = "directed", levelSeparation = 250) %>%
      # Configuración de Nodos
      visNodes(
        shape = "dot",
        size = 20,
        color = list(background = "lightsteelblue", border = "#2b7ce9", highlight = "#ff9100"),
        font = list(color = "black", size = 14)
      ) %>%
      # Configuración de Cables (Edges)
      visEdges(
        arrows = "to",
        color = list(color = "#cccccc", highlight = "#ff9100"),
        width = 2,
        smooth = TRUE # Curvas suaves
      ) %>%
      # Interacción: habilitar selección de nodos
      visInteraction(hover = TRUE, navigationButtons = TRUE) %>%
      # Capturar el evento de clic en un nodo
      visEvents(click = "function(nodes) { Shiny.onInputChange('click_node', nodes.nodes[0]); }")
  })

  # Lógica de Selección y Aislamiento (al hacer clic)
  observeEvent(input$click_node, {
    req(input$click_node)
    data <- red_data()
    node_id <- input$click_node

    # 1. Encontrar ancestros por ID (Identidad Única)
    ancestor_ids <- c(node_id)
    curr_id <- node_id

    while(TRUE) {
      parent_edge <- data$edges %>% filter(to == curr_id)
      if(nrow(parent_edge) == 0) break
      curr_id <- parent_edge$from[1]
      ancestor_ids <- c(ancestor_ids, curr_id)
    }

    # 2. Encontrar los cables (edges) que unen estos nodos
    path_edges <- data$edges %>%
      filter(from %in% ancestor_ids & to %in% ancestor_ids)

    edge_ids <- path_edges$id

    # Guardar en reactiveValues
    v$selected_node <- node_id
    v$ancestor_ids <- ancestor_ids
    v$edge_ids <- edge_ids

    # 3. Actualizar el estilo visual (Aislamiento)
    # Nodos: Naranja los activos, gris transparente el resto
    new_nodes <- data$nodes %>%
      mutate(
        color.background = ifelse(id %in% ancestor_ids, "#ff9100", "rgba(200,200,200,0.1)"),
        color.border = ifelse(id %in% ancestor_ids, "white", "rgba(200,200,200,0.1)"),
        font.color = ifelse(id %in% ancestor_ids, "black", "rgba(150,150,150,0.2)")
      )

    # Cables: Naranja los activos, gris transparente el resto
    new_edges <- data$edges %>%
      mutate(
        color.color = ifelse(id %in% edge_ids, "#ff9100", "rgba(200,200,200,0.1)"),
        width = ifelse(id %in% edge_ids, 5, 1)
      )

    # Enviar actualización a visNetwork (sin redibujar todo el layout)
    visNetworkProxy("network_plot") %>%
      visUpdateNodes(new_nodes) %>%
      visUpdateEdges(new_edges)
  })

  # Renderizado de los Chips Superiores (Breadcrumbs)
  output$breadcrumb_ui <- renderUI({
    req(v$ancestor_ids)
    data <- red_data()

    # Obtener nombres en orden Raíz -> Hijo
    path_names <- data$nodes %>%
      filter(id %in% v$ancestor_ids) %>%
      arrange(level) %>%
      pull(label)

    tagList(
      lapply(seq_along(path_names), function(i) {
        if(i == length(path_names)) {
          span(class="node-chip", path_names[i])
        } else {
          span(tagList(span(class="node-chip", path_names[i]), span(class="sep", ">")))
        }
      })
    )
  })

  # Botón Reset: Restaurar colores originales
  observeEvent(input$reset, {
    data <- red_data()
    v$selected_node <- NULL
    v$ancestor_ids <- NULL
    v$edge_ids <- NULL

    visNetworkProxy("network_plot") %>%
      visUpdateNodes(data$nodes) %>%
      visUpdateEdges(data$edges)
  })
}

shinyApp(ui, server)
