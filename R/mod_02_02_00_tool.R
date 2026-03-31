# ==============================================================================
# MÓDULO: EXPLORADOR DE HERRAMIENTAS RScience - v.0.0.1
# ==============================================================================
library(jsonlite)
library(listviewer)
library(shiny)
library(shinyjs)

mod_02_02_00_tool_ui <- function(id) {
  ns <- NS(id)
  root_sel <- paste0(".", ns("tools-container"))

  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        ", root_sel, " {
            display: flex; flex-direction: column;
            width: 100%; height: calc(100vh - 80px);
            padding: 20px !important; background: #f8f9fa;
            overflow: hidden;
        }

        /* --- BOTONES PILDORA XL (IDÉNTICOS A DATASET) --- */
        ", root_sel, " .btn.btn-pill-xl {
            border-radius: 50px !important; padding: 15px 35px !important;
            font-weight: 800 !important; font-size: 1.1rem !important;
            text-transform: uppercase !important; letter-spacing: 1px !important;
            display: inline-flex !important; align-items: center !important;
            justify-content: center !important; gap: 10px !important;
            transition: all 0.3s ease !important;
        }

        ", root_sel, " .btn.btn-pill-xl:hover { transform: translateY(-4px) !important; filter: brightness(1.1) !important; }

        ", root_sel, " .btn.btn-pill-xl.btn-locked {
            background-color: #e9ecef !important;
            color: #1e7e34 !important;
            border-color: #1e7e34 !important;
            opacity: 1 !important;
            box-shadow: none !important;
            transform: none !important;
        }

        /* --- CONTENEDOR DE ACCIONES --- */
        ", root_sel, " .action-row-right {
            display: flex !important; flex-direction: row !important; justify-content: flex-end !important;
            align-items: center !important; gap: 12px !important; height: 100% !important;
        }

        /* --- MAPA Y BLOQUEO VERDE --- */
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

        .locked-tree {
            pointer-events: none !important;
            opacity: 0.85;
            position: relative;
            border: 4px solid #28a745 !important;
            box-shadow: 0 0 20px rgba(40, 167, 69, 0.3);
        }

        .locked-tree::after {
            content: '🔒 SELECCIÓN CONFIRMADA' !important;
            position: absolute !important;
            top: 20px !important; left: 20px !important;
            background: #28a745 !important; color: #ffffff !important;
            padding: 10px 18px !important; border-radius: 8px !important;
            font-weight: 800 !important; font-size: 1.2rem !important;
            z-index: 2000 !important;
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

        /* --- SELECTION HEADER (MARQUESINA DINÁMICA) --- */
        ", root_sel, " .selection-header {
            padding: 15px 25px 15px 25px !important;
            display: flex !important;
            justify-content: space-between !important;
            align-items: center !important;
            border-radius: 12px !important;
            transition: all 0.4s ease !important;
            box-shadow: 0 4px 12px rgba(0,0,0,0.05) !important;
        }

        ", root_sel, " .selection-header.waiting-mode { background: #f0fdff !important; border: 1px solid #00cfd4 !important; color: #008184 !important; }
        ", root_sel, " .selection-header.active-selection { background: #fff9f0 !important; border: 1px solid #ff9100 !important; color: #b36600 !important; }
        ", root_sel, " .selection-header.confirmed { background: #f6fff8 !important; border: 1px solid #28a745 !important; color: #1e7e34 !important; }

        ", root_sel, " .header-id {
            font-family: 'Monaco', 'Courier New', monospace !important; font-weight: 700 !important;
            font-size: 0.85rem !important; background: rgba(0,0,0,0.08) !important; padding: 4px 15px !important; border-radius: 20px !important;
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
            div(class = "col-md-5",
                div(class = "action-row-right",
                    actionButton(inputId = ns("btn_confirm"), label = span(icon("lock"), "Lock"), class = "btn-success btn-pill-xl"),
                    actionButton(inputId = ns("btn_edit"),    span(icon("lock-open"), "Unlock"), class = "btn-warning btn-pill-xl"),
                    actionButton(inputId = ns("btn_reset"),   span(icon("sync"), "Reset"),    class = "btn-primary btn-pill-xl")
                )
            )
        ),

        # 3. Banner de Información
        uiOutput(ns("scripts_info_banner")),

                div(style = "border-top: 4px solid rgba(0,212,255, 1); margin: 35px 0;"),
        # 4. El Árbol (Motor de Selección)
        div(class = "map-section",
            div(id = ns("tree_wrapper"), class = "map-wrapper",
                mod_02_02_01_tree_ui(ns("inner_tree"))
            )
        ),

        # 5. Debug Panel
        div(style = "margin-top: 10px; max-height: 200px; overflow-y: auto;",
            listviewer::jsoneditOutput(ns("debug_json"), height = "auto")
        )
    )
  )
}

mod_02_02_00_tool_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- ESTADOS REACTIVOS ---
    is_done   <- reactiveVal(FALSE)
    is_locked <- reactiveVal(FALSE)

    # Importamos el servidor del árbol
    # rlist_tree ahora contiene la lista de info_nodo() del árbol
    rlist_tree <- mod_02_02_01_tree_server("inner_tree")

    # --- LÓGICA: BOTÓN CONFIRMAR ---
    # --- DENTRO DEL SERVER ---

    # LÓGICA: BOTÓN CONFIRMAR
    observeEvent(input$btn_confirm, {
      tree_data <- rlist_tree()
      current_node <- tree_data$selected_node_name
      req(current_node)

      if(current_node != "Rscience") {
        # 1. Bloqueo Visual del Árbol
        shinyjs::addClass(id = "tree_wrapper", class = "locked-tree")

        # 2. Estilo Bloqueado para el Botón (Igual a Dataset)
        shinyjs::disable("btn_confirm")
        shinyjs::addClass(id = "btn_confirm", class = "btn-locked")

        is_locked(TRUE)
        is_done(TRUE)
        message("--- [TOOLS] SELECCIÓN BLOQUEADA ---")
      } else {
        shinyjs::runjs("alert('Por favor, selecciona una herramienta específica.');")
      }
    })

    # LÓGICA: BOTÓN EDITAR
    observeEvent(input$btn_edit, {
      # 1. Quitar bloqueos visuales
      shinyjs::removeClass(id = "tree_wrapper", class = "locked-tree")

      # 2. Restaurar botón Lock
      shinyjs::enable("btn_confirm")
      shinyjs::removeClass(id = "btn_confirm", class = "btn-locked")

      is_locked(FALSE)
      is_done(FALSE)
    })

    # LÓGICA: BOTÓN RESET
    observeEvent(input$btn_reset, {
      shinyjs::removeClass(id = "tree_wrapper", class = "locked-tree")
      shinyjs::enable("btn_confirm")
      shinyjs::removeClass(id = "btn_confirm", class = "btn-locked")

      # Aquí podrías añadir el reset del árbol si el mod_02_02_01 lo permite
      is_locked(FALSE)
      is_done(FALSE)
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
      # Extraemos la info del árbol
      tree_data <- rlist_tree()

      # El nombre actual es el nombre modificado del nodo seleccionado
      # Si el nodo es "Rscience" (raíz), lo tratamos como NULL para el estado WAITING
      current_name <- if(!is.null(tree_data$selected_node_name_mod) && tree_data$selected_node_name != "Rscience") {
        tree_data$selected_node_name_mod
      } else {
        NULL
      }

      if (is_locked()) {
        # ESTADO: BLOQUEADO (Verde)
        div(class = "selection-header confirmed",
            span(icon("lock"), paste(" TOOL CONFIRMED:", current_name)),
            span(class = "header-id", "STATUS: LOCK"))

      } else if (!is.null(current_name)) {
        # ESTADO: SELECCIONADO PERO NO BLOQUEADO (Naranja)
        div(class = "selection-header active-selection",
            span(icon("lock-open"), paste(" SELECTED TOOL:", current_name)),
            span(class = "header-id", "STATUS: UNLOCK"))

      } else {
        # ESTADO: ESPERANDO (Azul)
        div(class = "selection-header waiting-mode",
            span(icon("bolt"), " Waiting for tool selection..."),
            span(class = "header-id", "STATUS: WAITING"))
      }
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
      listviewer::jsonedit(listdata = the_output(), mode = "text")
    })

    return(the_output)
  })
}
