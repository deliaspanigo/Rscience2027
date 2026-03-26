# ==========================================
# MÓDULO: EXPLORADOR DE ÁRBOL RScience
# ==========================================
library(shiny)
library(dplyr)
library(jsonlite)
library(listviewer)

# ==========================================
# MÓDULO: EXPLORADOR DE ÁRBOL RScience (UI)
# ==========================================

mod_02_02_01_tree_ui <- function(id) {
  ns <- NS(id)
  module_id <- ns("module-wrapper")

  tagList(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/d3@6/dist/d3.min.js"),
      tags$style(HTML(paste0("
        #", module_id, " {
          height: 100%; width: 100%;
          background-color: #000;
          display: flex;
          overflow: hidden;
          font-family: 'Segoe UI', sans-serif;
          position: relative;
        }

        #", module_id, " .viewport-area {
          flex-grow: 1;
          position: relative;
          background: radial-gradient(circle, #0a1015 0%, #000 100%);
        }

        /* SIDEBAR UNIFICADA: Ahora por defecto está colapsada */
        #", module_id, " .sidebar-ctrl {
          width: 280px;
          height: 100%;
          background: rgba(10, 20, 25, 0.95);
          border-left: 2px solid #00FFFF;
          padding: 25px 20px;
          display: flex;
          flex-direction: column;
          gap: 20px;
          z-index: 1001;
          box-shadow: -10px 0 30px rgba(0,0,0,0.5);

          /* ESTADO INICIAL: Escondida a la derecha */
          position: absolute;
          right: 0;
          transform: translateX(100%);
          transition: transform 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        }

        /* Clase que quitaremos/pondremos para mostrarla */
        #", module_id, " .sidebar-ctrl.active {
          transform: translateX(0);
        }

        #", module_id, " .toggle-sidebar-btn {
          position: absolute;
          right: 10px;
          top: 10px;
          z-index: 1002;
          background: #00FFFF;
          color: #000;
          border: none;
          padding: 8px 12px;
          border-radius: 5px;
          font-weight: bold;
          cursor: pointer;
          box-shadow: 0 0 10px #00FFFF;
        }

        .sidebar-section { margin-bottom: 10px; }
        .panel-title {
          color: #00FFFF; font-weight: 900; font-size: 0.9rem;
          border-bottom: 1px solid rgba(0,255,255,0.3); padding-bottom: 8px; margin-bottom: 15px;
          letter-spacing: 2px;
        }

        .btn-cmd {
          background: transparent; color: #00FFFF; border: 1px solid #00FFFF;
          padding: 12px; width: 100%; font-weight: bold; cursor: pointer;
          border-radius: 6px; text-transform: uppercase; transition: all 0.3s;
          margin-bottom: 10px;
        }
        .btn-cmd:hover { background: #00FFFF; color: #000; box-shadow: 0 0 15px #00FFFF; }

        .toggle-row {
          display: flex; justify-content: space-between; align-items: center;
          color: #fff; font-size: 0.85rem; padding: 5px 0;
        }

        .node text { pointer-events: none; fill: #fff; font-size: 18px; font-weight: 700; text-shadow: 2px 2px 4px #000; }
        .link { fill: none; stroke-opacity: 0.6; }
      ")))
    ),

    div(style = "max-height: 500px; overflow-y: auto;",
        listviewer::jsoneditOutput(ns("debug_json"), height = "auto")
    ),
    div(id = module_id,
        tags$button("☰ MENU", class = "toggle-sidebar-btn", onclick = "toggleSidebar()"),

        div(class = "viewport-area",
            div(id = "tree-container", style="width:100%; height:100%;")
        ),

        # La sidebar nace sin la clase 'active', por lo tanto está oculta
        div(id = "sidebar", class = "sidebar-ctrl",
            div(class = "sidebar-section",
                div(class = "panel-title", "⚡ SYSTEM ACTIONS"),
                tags$button("↺ Reset View", class = "btn-cmd", onclick = "resetMap()"),
                tags$button("🎯 Focus Branch", class = "btn-cmd", style="border-color:#ADFF2F; color:#ADFF2F;", onclick = "runCollapseOthers()"),
                tags$button("⇱ Expand All", class = "btn-cmd", onclick = "fullExpand()")
            ),

            div(class = "sidebar-section",
                div(class = "panel-title", "🛠️ VIEW SETTINGS"),
                div(class = "toggle-row", "GHOST MODE",
                    tags$input(type="checkbox", onclick="toggleGhost()", style="width:18px;height:18px;")),
                div(class = "toggle-row", "TOP ALIGN",
                    tags$input(type="checkbox", onclick="toggleTopAlign()", style="width:18px;height:18px;"))
            ),

            div(class = "sidebar-section", style="margin-top: auto;",
                div(class = "panel-title", "📤 EXPORT"),
                tags$button("📸 Capture PNG", class = "btn-cmd",
                            style="border-color:#FF00FF; color:#FF00FF;", onclick = "captureTree()")
            )
        )
    ),
    uiOutput(ns("js_injector"))
  )
}

mod_02_02_01_tree_server <- function(id, show_debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cleaning_jumps <- function(text) {
      # gsub busca todas las ocurrencias de /n y las reemplaza por nada
      str_clean <- gsub("/n", "", text)
      return(str_clean)
    }

    # 1. CARGA DE DATOS
    data_full <- tryCatch({
      Rscience2027::tree_data
    }, error = function(e) {
      data.frame(nivel1 = "Error", nivel2 = "No Data", script_id = "0")
    })

    # 2. OBJETO REACTIVO: MÉTRICAS (v.0.1.1)
    info_nodo <- reactive({
      selected_node_name <- if (is.null(input$selected_node) ||
                                input$selected_node == "" ||
                                input$selected_node == "Rscience") {
        "Rscience"
      } else {
        input$selected_node
      }

      # Totales Globales
      real_total_tools <- data_full %>% select(starts_with("nivel")) %>% distinct() %>% nrow()
      real_total_scripts <- length(unique(data_full$script_id))

      if (selected_node_name == "Rscience") {
        path_final <- "Rscience (Root)"
        scripts_ids <- unique(data_full$script_id)
        n_tools <- real_total_tools
        n_script <- real_total_scripts
        is_last_node <- FALSE # El root nunca es el último
      } else {
        row_match <- data_full %>% filter(if_any(starts_with("nivel"), ~ .x == selected_node_name))

        if (nrow(row_match) > 0) {
          sample_row <- row_match %>% slice(1)
          niveles_all <- sample_row %>% select(starts_with("nivel")) %>% as.character()
          niveles_clean <- niveles_all[!is.na(niveles_all) & niveles_all != "" & niveles_all != "NA"]

          idx_actual <- which(niveles_clean == selected_node_name)
          path_parts <- niveles_clean[1:idx_actual]
          path_final <- paste(c("Rscience", path_parts), collapse = " / ")

          # --- LÓGICA DE ÚLTIMO NODO ---
          # Un nodo es el último si en ninguna fila de row_match hay algo en el nivel inmediatamente superior
          # col_siguiente <- paste0("nivel", idx_actual + 1)
          #
          # if (col_siguiente %in% names(data_full)) {
          #   # Verificamos si hay hijos en la columna siguiente
          #   hijos <- row_match %>%
          #     pull(!!sym(col_siguiente)) %>%
          #     unique()
          #
          #   # Es último si todos los hijos son NA o vacíos
          #   is_last_node <- all(is.na(hijos) | hijos == "" | hijos == "NA")
          # } else {
          #   # Si ni siquiera existe la columna siguiente en el DF, es el último nivel posible
          #   is_last_node <- TRUE
          # }
          # ----------------------------

          # Filtrado para métricas de la rama seleccionada
          query_data <- data_full
          for (i in seq_along(path_parts)) {
            col_name <- paste0("nivel", i)
            if (col_name %in% names(query_data)) {
              query_data <- query_data %>% filter(!!sym(col_name) == path_parts[i])
            }
          }

          scripts_ids <- query_data %>% pull(script_id) %>% unique()
          n_tools <- query_data %>% select(starts_with("nivel")) %>% distinct() %>% nrow()
          n_script <- length(scripts_ids)
          is_last_node <- n_tools == 1

        } else {
          path_final <- "Rscience (Root)"
          scripts_ids <- unique(data_full$script_id)
          n_tools <- real_total_tools
          n_script <- real_total_scripts
          is_last_node <- FALSE
        }
      }

      list(
        description = "*** Rscience - Tree - Tool Selector ***",
        selected_node_name = selected_node_name,
        selected_node_name_mod = cleaning_jumps(text = selected_node_name),
        path = path_final,
        path_mod = cleaning_jumps(text = path_final),
        is_last_node = is_last_node, # <--- Ahora dinámico
        script_id = scripts_ids,
        script_id_folder_path = NA,
        check_script_id_folder = rep(TRUE, length(scripts_ids)),
        n_tools = n_tools,
        n_script = n_script,
        real_total_tools = real_total_tools,
        real_total_scripts = real_total_scripts,
        time = format(Sys.time(), "%H:%M:%S")
      )
    })

    # 3. HELPER JERARQUÍA
    df_a_jerarquia <- function(data, name = "Rscience") {
      node <- list(name = as.character(name))
      if (ncol(data) > 0) {
        level_col <- names(data)[1]
        hijos <- unique(data[[level_col]])
        hijos <- hijos[!is.na(hijos) & hijos != "" & hijos != "NA"]
        if (length(hijos) > 0) {
          node$children <- lapply(hijos, function(h) {
            sub_data <- data[data[[level_col]] == h, -1, drop = FALSE]
            df_a_jerarquia(sub_data, h)
          })
        }
      }
      node
    }

    # 4. INYECTOR JS (RESTAURADO COMPLETO)
    # 4. INYECTOR JS (IGUALACIÓN DE RENDERIZADO NARANJA = CYAN)
    output$js_injector <- renderUI({
      req(data_full)
      df_tree <- data_full %>% select(starts_with("nivel"))
      tree_json <- jsonlite::toJSON(df_a_jerarquia(df_tree), auto_unbox = TRUE)

      tags$script(HTML(paste0(
        sprintf("var treeData = %s;", tree_json),
        "
    var root, svg, g, treemap, zoomHandler, i = 0, duration = 600;
    var activeNode = null, ghostMode = false, topAlignMode = false;
    var currentScale = 1;

    function initTree() {
      const container = d3.select('#tree-container');
      if (container.empty()) return;
      container.selectAll('*').remove();
      const rect = container.node().getBoundingClientRect();

      svg = container.append('svg').attr('width', '100%').attr('height', '100%');

      // Mantenemos el defs por si los nodos lo usan, pero los links serán sólidos
      const defs = svg.append('defs');
      const filter = defs.append('filter').attr('id', 'glow');
      filter.append('feGaussianBlur').attr('stdDeviation', '4').attr('result', 'coloredBlur');
      const feMerge = filter.append('feMerge');
      feMerge.append('feMergeNode').attr('in', 'coloredBlur');
      feMerge.append('feMergeNode').attr('in', 'SourceGraphic');

      g = svg.append('g');
      zoomHandler = d3.zoom().scaleExtent([0.01, 8])
        .on('zoom', (event) => {
           currentScale = event.transform.k;
           g.attr('transform', event.transform);
           updateTextSizes();
        });
      svg.call(zoomHandler);

      treemap = d3.tree().nodeSize([180, 950]);
      root = d3.hierarchy(treeData, d => d.children);
      root.descendants().forEach(d => { d.id = ++i; });
      root.x0 = rect.height / 2; root.y0 = 0;
      activeNode = root;

      Shiny.setInputValue('", ns("selected_node"), "', 'Rscience');
      if (root.children) root.children.forEach(window.collapse);
      update(root, 0);
    }

    function updateTextSizes() {
      let fontSize = Math.max(18, 16 / currentScale);
      g.selectAll('g.node text').style('font-size', fontSize + 'px');
    }

    window.isPathActive = function(d) {
      if (!activeNode) return false;
      let curr = activeNode;
      while (curr) {
        if (curr.id === d.id) return true;
        curr = curr.parent;
      }
      return false;
    };

    function update(source, customDuration) {
      const currentDuration = (customDuration !== undefined) ? customDuration : duration;
      if (activeNode) reorderPathToTop(activeNode, !topAlignMode);

      const nodes = treemap(root).descendants();
      const links = nodes.slice(1);
      nodes.forEach(d => d.y = d.depth * 950);
      fitToLeftAnchor(nodes, currentDuration);

      const node = g.selectAll('g.node').data(nodes, d => d.id);
      const nodeEnter = node.enter().append('g').attr('class', 'node')
        .attr('transform', d => (currentDuration === 0) ? `translate(${d.y},${d.x})` : `translate(${source.y0 || 0},${source.x0 || 0})`)
        .on('click', (event, d) => {
          activeNode = d;
          Shiny.setInputValue('", ns("selected_node"), "', d.data.name, {priority: 'event'});
          if (d.children) { d._children = d.children; d.children = null; }
          else { d.children = d._children; d._children = null; }
          update(d);
        });

      nodeEnter.append('circle').attr('r', 28);
      const textElement = nodeEnter.append('text')
        .attr('x', d => d.children || d._children ? -50 : 50)
        .attr('text-anchor', d => d.children || d._children ? 'end' : 'start');

      textElement.each(function(d) {
        const lines = (d.data.name || '').split('/n');
        const el = d3.select(this);
        const vOffset = (lines.length - 1) * 0.5;
        lines.forEach((line, index) => {
          el.append('tspan').attr('x', d.children || d._children ? -50 : 50)
            .attr('dy', index === 0 ? `-${vOffset}em` : '1.1em').text(line.trim());
        });
      });

      const nodeUpdate = nodeEnter.merge(node);
      if (currentDuration === 0) { nodeUpdate.attr('transform', d => `translate(${d.y},${d.x})`); }
      else { nodeUpdate.transition().duration(currentDuration).attr('transform', d => `translate(${d.y},${d.x})`); }

      nodeUpdate.select('circle')
        .style('fill', d => window.isPathActive(d) ? '#ff9100' : (d.children || d._children ? '#00FFFF' : '#00FF00'))
        .style('stroke', d => (d.id === (activeNode ? activeNode.id : null)) ? '#00FF00' : '#fff')
        .style('stroke-width', d => (d.id === (activeNode ? activeNode.id : null)) ? '8px' : '6px')
        .style('filter', d => window.isPathActive(d) ? 'url(#glow)' : 'none')
        .style('opacity', d => (ghostMode && !window.isPathActive(d)) ? 0.15 : 1);

      node.exit().remove();

      const link = g.selectAll('path.link').data(links, d => d.id);
      const linkEnter = link.enter().insert('path', 'g').attr('class', 'link')
        .attr('d', d => { const o = {y: source.y0 || 0, x: source.x0 || 0}; return window.diagonal(o, o); });

      const linkUpdate = linkEnter.merge(link);

      // ELIMINAMOS FILTROS COMPLEJOS: Solo color y grosor sólido como el cyan
      linkUpdate
        .attr('fill', 'none')
        .attr('stroke', d => window.isPathActive(d) ? '#ff9100' : '#00FFFF')
        .attr('stroke-width', d => window.isPathActive(d) ? 22 : 10)
        .style('filter', 'none') // <--- Quitamos el glow de los links
        .style('opacity', d => window.isPathActive(d) ? 1 : (ghostMode ? 0.08 : 0.4));

      linkUpdate.transition().duration(currentDuration)
        .attr('d', d => window.diagonal(d, d.parent));

      link.exit().remove();
      nodes.forEach(d => { d.x0 = d.x; d.y0 = d.y; });
    }

    window.diagonal = function(s, d) {
      if (!s || !d) return '';
      let sy = s.y, sx = s.x, dy = d.y, dx = d.x;
      // Offset de 1 píxel para asegurar que el navegador renderice la línea
      if (Math.abs(sx - dx) < 1) { sx += 1; }
      return `M ${sy} ${sx} C ${(sy+dy)/2} ${sx}, ${(sy+dy)/2} ${dx}, ${dy} ${dx}`;
    }

    function fitToLeftAnchor(targetNodes, customDuration) {
      const container = document.getElementById('tree-container');
      if (!container || !targetNodes || targetNodes.length === 0) return;
      const rect = container.getBoundingClientRect();
      const minX = d3.min(targetNodes, d => d.x); const maxX = d3.max(targetNodes, d => d.x);
      const minY = d3.min(targetNodes, d => d.y); const maxY = d3.max(targetNodes, d => d.y);
      let scale = Math.min(rect.width / (maxY - minY + 950), rect.height / (maxX - minX + 200));
      scale = Math.min(Math.max(scale, 0.05), 0.7);
      const transform = d3.zoomIdentity.translate(150 * scale, rect.height / 2).scale(scale).translate(0, -(minX + maxX) / 2);
      svg.transition().duration(customDuration).call(zoomHandler.transform, transform);
    }

    window.toggleSidebar = function() {
      const sidebar = document.getElementById('sidebar');
      if(sidebar) sidebar.classList.toggle('active');
      setTimeout(() => { if(treemap && root) fitToLeftAnchor(treemap(root).descendants(), 600); }, 450);
    }

    function reorderPathToTop(d, revert = false) {
      let curr = d;
      while (curr && curr.parent) {
        let p = curr.parent; let children = p.children || p._children;
        if (children) {
          if (revert) children.sort((a, b) => a.id - b.id);
          else { let idx = children.findIndex(c => c.id === curr.id); if (idx > -1) children.unshift(children.splice(idx, 1)[0]); }
        }
        curr = p;
      }
    }

    window.collapse = function(d) { if(d.children) { d._children = d.children; d._children.forEach(window.collapse); d.children = null; } }
    window.toggleGhost = function() { ghostMode = !ghostMode; update(activeNode); }
    window.toggleTopAlign = function() { topAlignMode = !topAlignMode; update(activeNode); }
    window.resetMap = function() { activeNode = root; if(root.children) root.children.forEach(window.collapse); update(root); }
    window.fullExpand = function() { (function ex(d){ if(d._children){d.children=d._children; d._children=null;} if(d.children)d.children.forEach(ex); })(root); update(root); }
    window.runCollapseOthers = function() {
      if (!activeNode || activeNode === root) return;
      var ancestors = []; var curr = activeNode; while (curr) { ancestors.push(curr.id); curr = curr.parent; }
      root.descendants().forEach(d => { if (ancestors.indexOf(d.id) === -1 && d.children) { d._children = d.children; d.children = null; } });
      update(activeNode);
    }

    window.captureTree = function() {
       const svgNode = document.querySelector('#tree-container svg');
       const rect = svgNode.getBoundingClientRect();
       const clonedSvg = svgNode.cloneNode(true);
       const serializer = new XMLSerializer();
       let source = serializer.serializeToString(clonedSvg);
       const img = new Image();
       const svgBlob = new Blob([source], {type: 'image/svg+xml;charset=utf-8'});
       const url = URL.createObjectURL(svgBlob);
       img.onload = function() {
         const canvas = document.createElement('canvas');
         canvas.width = rect.width*2; canvas.height = rect.height*2;
         const ctx = canvas.getContext('2d'); ctx.fillStyle = '#000'; ctx.fillRect(0,0,canvas.width,canvas.height);
         ctx.scale(2,2); ctx.drawImage(img,0,0);
         const link = document.createElement('a'); link.download = 'RScience_Map.png'; link.href = canvas.toDataURL(); link.click();
       };
       img.src = url;
    }

    setTimeout(initTree, 300);
    window.addEventListener('resize', () => { if(treemap && root) fitToLeftAnchor(treemap(root).descendants(), 0); });
    "
      )))
    })

    output$debug_json <- listviewer::renderJsonedit({
      req(show_debug)
      listviewer::jsonedit(listdata = info_nodo(), mode = "text")
    })

    return(info_nodo)
  })
}

