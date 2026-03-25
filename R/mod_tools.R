# ==============================================================================
# MÓDULO: EXPLORADOR DE HERRAMIENTAS RScience - v.0.0.1
# ==============================================================================
library(jsonlite)
library(listviewer)
library(shiny)
library(shinyjs)

mod_tools_ui <- function(id) {
  ns <- NS(id)
  root_sel <- paste0(".", ns("tools-container"))

  tagList(
    # CSS Específico del Módulo
    tags$head(
      tags$style(HTML(paste0("
        ", root_sel, " {
            display: flex; flex-direction: column;
            width: 100%; height: calc(100vh - 80px);
            padding: 20px !important; background: #f8f9fa;
            overflow: hidden;
        }
        ", root_sel, " .map-section {
            display: flex; flex-direction: column; flex: 1;
            margin-top: 15px; position: relative;
        }
        ", root_sel, " .map-wrapper {
            background: #000; border-radius: 20px;
            border: 4px solid #1a202c; position: relative;
            flex: 1; width: 100%; overflow: hidden;
            transition: all 0.3s ease;
        }

        /* Bloqueo Visual Verde */
        .locked-tree {
            pointer-events: none !important;
            opacity: 0.85;
            position: relative;
            border: 4px solid #28a745 !important;
            box-shadow: 0 0 20px rgba(40, 167, 69, 0.3);
        }

        .locked-tree::after {
            content: '🔒 SELECCIÓN CONFIRMADA';
            position: absolute;
            top: 20px;
            left: 20px;
            background: #28a745;
            color: #ffffff;
            padding: 10px 18px;
            border-radius: 8px;
            font-weight: 800;
            font-size: 1.2rem;
            z-index: 2000;
            box-shadow: 0 0 15px rgba(40, 167, 69, 0.5);
        }

        .path-display-area {
            background: #1a202c; border-radius: 12px; padding: 10px;
            min-height: 50px; display: flex; align-items: center; gap: 8px;
        }
        .path-chip {
            background: #00FFFF; color: #000; padding: 4px 12px;
            border-radius: 50px; font-weight: 800; font-size: 0.8rem;
        }
        .info-banner-blue {
            background: #e6f7ff; border-left: 5px solid #1890ff;
            padding: 10px 20px; margin: 10px 0; border-radius: 4px;
        }
      ")))
    ),

    div(class = paste("container-fluid", ns("tools-container")),
        # 1. Cabecera Dinámica
        div(class = "flex-shrink-0", uiOutput(ns("tools_header"))),

        # 2. Barra de Control y Path
        div(class = "row g-3 align-items-center flex-shrink-0", style="margin-top:5px;",
            div(class = "col-md-7",
                div(class = "path-display-area", uiOutput(ns("path_chips_ui")))
            ),
            div(class = "col-md-5 text-end",
                actionButton(ns("btn_confirm"), "Confirm", class = "btn btn-success", icon = icon("check")),
                actionButton(ns("btn_edit"), "Edit", class = "btn btn-warning", icon = icon("edit")),
                actionButton(ns("btn_reset"), "Reset", class = "btn btn-primary", icon = icon("sync"))
            )
        ),

        # 3. Banner de Información
        uiOutput(ns("scripts_info_banner")),

        # 4. El Árbol (Motor de Selección)
        div(class = "map-section",
            div(id = ns("tree_wrapper"), class = "map-wrapper",
                mod_tree_ui(ns("inner_tree"))
            )
        ),

        # 5. Debug Panel (Opcional)
        div(style = "margin-top: 10px; max-height: 200px; overflow-y: auto;",
            listviewer::jsoneditOutput(ns("debug_json"), height = "auto")
        )
    )
  )
}

mod_tools_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- ESTADOS REACTIVOS ---
    is_done   <- reactiveVal(FALSE)
    is_locked <- reactiveVal(FALSE)

    # Importamos el servidor del árbol
    # rlist_tree ahora contiene la lista de info_nodo() del árbol
    rlist_tree <- mod_tree_server("inner_tree")

    # --- LÓGICA: BOTÓN CONFIRMAR ---
    observeEvent(input$btn_confirm, {
      # Accedemos a la data actual del árbol
      tree_data <- rlist_tree()

      # FIX CRÍTICO: El árbol usa 'selected_node_name', no 'node_name'
      current_node <- tree_data$selected_node_name

      # Debug para verificar en consola qué está llegando
      message("--- [TOOLS] Intentando confirmar nodo: ", current_node, " ---")

      req(current_node)

      # Si el nodo es válido y no es la raíz vacía
      if(current_node != "Rscience") {

        # 1. Bloqueo Visual (Borde Verde y Cartel)
        shinyjs::addClass(id = "tree_wrapper", class = "locked-tree")

        # 2. Deshabilitar botones (Usando ns() por seguridad)
        shinyjs::disable("btn_confirm")

        # 3. Actualizar estados para el Sidebar y lógica interna
        is_locked(TRUE)
        is_done(TRUE)

        message("--- [TOOLS] SELECCIÓN BLOQUEADA ---")

      } else {
        # Feedback si intentan confirmar sin elegir una herramienta real
        shinyjs::runjs("alert('Por favor, navega en el árbol y selecciona una herramienta específica antes de confirmar.');")
      }
    })

    # --- LÓGICA: BOTÓN EDITAR (DESBLOQUEAR) ---
    observeEvent(input$btn_edit, {
      shinyjs::removeClass(id = "tree_wrapper", class = "locked-tree")
      shinyjs::enable("btn_confirm")

      is_locked(FALSE)
      is_done(FALSE) # Esto hará que el candado vuelva a NARANJA TITILANTE
      message("--- [TOOLS] Edición habilitada ---")
    })

    # --- RENDERIZADO DE CABECERA ---
    output$tools_header <- renderUI({
      tree_data <- rlist_tree()
      # Usamos el color Naranja o Verde según el estado de is_done
      color_header <- if(is_done()) "#28a745" else "#ff9100"

      tags$h2(paste("Herramienta:", tree_data$selected_node_name_mod),
              style = paste0("color:", color_header, "; font-weight:900; transition: color 0.4s;"))
    })

    # --- RENDERIZADO DE PATH (CHIPS) ---
    output$path_chips_ui <- renderUI({
      req(rlist_tree()$path_mod)
      # Limpiamos el path y lo separamos para crear los chips Cyan
      partes <- unlist(strsplit(rlist_tree()$path_mod, " / "))
      lapply(partes, function(p) {
        span(class = "path-chip", p)
      })
    })

    # --- BANNER DE SCRIPTS ---
    # --- METRICS BANNER (THREE LINES) ---
    # --- METRICS BANNER (UNIFIED STYLE) ---
    # --- METRICS BANNER (GRAMMAR & LOGIC CORRECTED) ---
    output$scripts_info_banner <- renderUI({
      tree_data <- rlist_tree()
      req(tree_data$real_total_tools > 0)

      # 1. Logic for Tools (Line 1)
      is_are_tools <- if(tree_data$n_tools == 1) "is" else "are"
      label_tools  <- if(tree_data$n_tools == 1) "tool" else "tools"

      # 2. Logic for Global Scripts (Line 2)
      is_are_scripts_total <- if(tree_data$n_script == 1) "is" else "are"
      label_scripts_total  <- if(tree_data$n_script == 1) "script" else "scripts"

      # 3. Logic for Current Selection (Line 3)
      # We use n_script here because it's the scripts associated with the chosen node
      is_are_current <- if(tree_data$n_script == 1) "is" else "are"
      label_scripts_current <- if(tree_data$n_script == 1) "script" else "scripts"

      status_color <- if(is_done()) "#28a745" else "#ff9100"

      div(class = "info-banner-blue",
          style = "line-height: 1.8; padding: 15px; border-radius: 8px; font-size: 0.95rem;",

          # Line 1: RScience System Coverage
          div(
            icon("sitemap", style = "color: #1890ff; width: 25px;"),
            span("RScience System: ", style = "font-weight: 800; color: #1a202c;"),
            span(sprintf("There %s %d %s available in this branch out of %d in the entire system.",
                         is_are_tools, tree_data$n_tools, label_tools, tree_data$real_total_tools))
          ),

          # Line 2: Global Repository (X selected out of Y possible)
          div(
            icon("database", style = "color: #1890ff; width: 25px;"),
            span("Global Repository: ", style = "font-weight: 800; color: #1a202c;"),
            span(sprintf("There %s %d %s selected out of %d possible in the database.",
                         is_are_scripts_total, tree_data$n_script, label_scripts_total, tree_data$real_total_scripts))
          ),

          # Line 3: Selected Tool Specifics
          div(style = paste0("margin-top: 5px; padding-top: 5px; border-top: 1px solid rgba(24, 144, 255, 0.2); color:", status_color, ";"),
              icon("microchip", style = paste0("color:", status_color, "; width: 25px;")),
              span("Current Selection: ", style = "font-weight: 800;"),
              span(sprintf("There %s %d %s associated with the selected tool: '%s'.",
                           is_are_current, tree_data$n_script, label_scripts_current, tree_data$selected_node_name_mod),
                   style = "font-weight: 600;")
          )
      )
    })

    # --- OBJETO DE SALIDA (Reactivo) ---
    # Este es el objeto que el mod_rscience_server recibirá
    the_output <- reactive({
      # Capturamos la info actual del árbol
      tree_info <- rlist_tree()

      # Construimos la lista final solicitada
      list(
        "description" = "*** Rscience - Tool selector ***",
        "is_done"     = is_done(),   # TRUE/FALSE para el candado del sidebar
        "is_locked"   = is_locked(), # TRUE/FALSE para el estado de la UI
        "tree"        = tree_info    # Aquí va TODO: selected_node_name, path, script_id, etc.
      )
    })





    # Debug JSON
    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      listviewer::jsonedit(listdata = the_output(), mode = "view")
    })

    return(the_output)
  })
}
