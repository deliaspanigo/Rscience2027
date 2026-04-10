library(shiny)
library(bslib)
library(shinyjs)

# ==============================================================================
# UI MÓDULO
# ==============================================================================
mod_pipeline_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    # Estilos encapsulados para el módulo
    tags$head(
      tags$style(HTML(paste0("
        .matrix-container { background: #0f171e; border: 1px solid #2a3b47; border-radius: 8px; overflow: hidden; }
        .matrix-header-row {
          background: #1a262f; color: #00d4ff; font-weight: 800;
          text-transform: uppercase; font-size: 0.7rem; letter-spacing: 1.5px;
          border-bottom: 2px solid #00d4ff;
        }
        .matrix-row { border-bottom: 1px solid #1a262f; transition: all 0.5s ease; height: 55px; }
        .matrix-cell { padding: 12px 10px; font-family: 'JetBrains Mono', monospace; font-size: 0.8rem; color: #fff; }
        .check-icon { color: #22303a; font-size: 1rem; transition: all 0.4s; }
        .status-on { color: #00bc8c; text-shadow: 0 0 10px #00bc8c; }
        .status-proc { color: #00d4ff; animation: blink 0.8s infinite; }
        @keyframes blink { 0% { opacity: 0.3; } 50% { opacity: 1; } 100% { opacity: 0.3; } }
        .row-active { background: rgba(0, 212, 255, 0.08); }
        .step-id { color: #00d4ff; font-weight: bold; text-align: center; }
        .time-code { color: #566b7a; }
        .time-active { color: #adb5bd; }
        .total-time-card { background: #1a262f; border: 1px solid #00d4ff; border-radius: 10px; padding: 15px; }
        .main-btn { border-radius: 50px; font-weight: 800; padding: 12px 40px; letter-spacing: 2px; }
      ")))
    ),

    uiOutput(ns("item01_folder_target")),
    uiOutput(ns("item02_folder_quarto_render")),
    uiOutput(ns("item03_qmd_files")),
    uiOutput(ns("item04_temp_folder_Rscience")),
    uiOutput(ns("item05_copy_files")),
    uiOutput(ns("item06_qmd_files_temp")),

    # Controles Superiores
    div(class = "container-fluid py-3",
        div(class = "row align-items-center",
            div(class = "col-md-6",
                actionButton(ns("run_process"), "INITIALIZE PIPELINE",
                             class = "btn-primary main-btn shadow-lg", icon = icon("bolt")),
                actionButton(ns("reset_process"), "RESET",
                             class = "btn-outline-secondary ms-2", style="border-radius:50px;")
            ),
            div(class = "col-md-6",
                div(class = "total-time-card d-flex justify-content-between align-items-center",
                    span("TOTAL ELAPSED TIME:", style="color: #00d4ff; font-weight: 700; font-size: 0.7rem;"),
                    span(id = ns("total_clock"), "00:00:00.000",
                         style="font-family: 'JetBrains Mono'; font-size: 1.5rem; color: #fff;")
                )
            )
        )
    ),

    # Tabla Matrix
    div(class = "container-fluid",
        div(class = "matrix-container shadow-lg",
            div(class = "row g-0 matrix-header-row",
                div(class = "col-1 matrix-cell text-center", "Step"),
                div(class = "col-2 matrix-cell", "Task"),
                div(class = "col-3 matrix-cell", "Technical Details"),
                div(class = "col-2 matrix-cell text-center", "Status (R | S | E)"),
                div(class = "col-2 matrix-cell", "Start Time"),
                div(class = "col-2 matrix-cell", "End Time")
            ),
            uiOutput(ns("matrix_static_body"))
        )
    )
  )
}

# ==============================================================================
# SERVER MÓDULO
# ==============================================================================
mod_pipeline_server <- function(id, folder_target, list_settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Normalización de la ruta de entrada
    internal_folder_target <- reactive({
      if(is.function(folder_target)) folder_target() else folder_target
    })

    ##########################################################################################################
    # 1. Steo 01 - Procesamiento de datos y estados
    rlist_item01_folder_target <- reactive({
      req(internal_folder_target())

      path_val <- internal_folder_target()
      exists_val <- dir.exists(path_val)

      # Definición de colores y textos RScience
      color_hex <- if(exists_val) "#00bc8c" else "#ff4b5c" # Verde Esmeralda / Rojo Pink

      list(
        path       = path_val,
        exists     = exists_val,
        text       = if(exists_val) "FOLDER VERIFIED" else "PATH NOT FOUND",
        color      = color_hex,
        icon_name  = if(exists_val) "check-circle" else "exclamation-triangle",
        shadow     = paste0("0 0 12px ", color_hex),
        is_done    = exists_val
      )
    })

    # 3. Renderizado con estilos in-line detallados
    output$item01_folder_target <- renderUI({
      req(rlist_item01_folder_target())
      res <- rlist_item01_folder_target()

      aver <- "Item 01 - Folder Tool Script - "

      div(
        style = "display: flex; align-items: center; padding: 12px; background: #1a262f;
             border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",

        # 1. Luz de Estado
        div(
          style = paste0(
            "width: 12px; height: 12px; border-radius: 50%; margin-right: 15px;
         flex-shrink: 0; background-color: ", res$color, "; box-shadow: ", res$shadow, ";"
          )
        ),

        # 2. Contenedor de Texto (Corregido con display block)
        div(
          style = "width: 100%; overflow: hidden;",

          # Título y Estado (Icono + Texto)
          div(
            style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color, "; margin-bottom: 4px;"),
            icon(res$icon_name, class = "me-2"), # Espaciador de Bootstrap 'me-2'
            paste0(aver, res$text)
          ),

          # Path en una línea nueva y con estilo de código
          div(
            res$path,
            style = "font-family: 'JetBrains Mono', monospace; font-size: 0.72rem;
                 color: #adb5bd; background: #0b1218; padding: 4px 8px;
                 border-radius: 4px; white-space: nowrap; overflow: hidden;
                 text-overflow: ellipsis; display: block;"
          )
        )
      )
    })

    ###########################################################################################################

    # 2. Procesamiento de datos y estados
    rlist_item02_quarto_proc <- reactive({
      req(internal_folder_target())
      # Asegúrate de que el item anterior use 'is_done' o la lógica que corresponda
      req(rlist_item01_folder_target())

      path_val <- internal_folder_target()
      path_folder_relative <- file.path(path_val, "f02_quarto_proc")

      # Usamos mustWork = FALSE para que no explote si la carpeta no existe aún
      path_folder_absolute <- normalizePath(path_folder_relative, mustWork = FALSE)

      check_folder_exists <- dir.exists(path_folder_absolute)

      # Definición de colores RScience
      color_hex <- if(check_folder_exists) "#00bc8c" else "#ff4b5c"

      list(
        path       = path_folder_absolute,
        exists     = check_folder_exists,
        # CORREGIDO: Usamos check_folder_exists
        text       = if(check_folder_exists) "FOLDER VERIFIED" else "PATH NOT FOUND",
        color      = color_hex,
        # CORREGIDO: Usamos check_folder_exists
        icon_name  = if(check_folder_exists) "check-circle" else "exclamation-triangle",
        shadow     = paste0("0 0 12px ", color_hex),
        is_done    = check_folder_exists
      )
    })

    # 3. Renderizado con estilos in-line detallados
    output$item02_folder_quarto_render <- renderUI({
      req(rlist_item02_quarto_proc())
      res <- rlist_item02_quarto_proc()

      # Prefijo para identificar el item en el debug visual
      prefix_text <- "Item 02 - Quarto Render: "

      div(
        style = "display: flex; align-items: center; padding: 12px; background: #1a262f;
                 border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",

        # 1. Luz de Estado con Neón
        div(
          style = paste0(
            "width: 12px; height: 12px; border-radius: 50%; margin-right: 15px;
             flex-shrink: 0; background-color: ", res$color, "; box-shadow: ", res$shadow, ";"
          )
        ),

        # 2. Contenedor de Texto
        div(
          style = "width: 100%; overflow: hidden;",

          # Título y Estado
          div(
            style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color, "; margin-bottom: 4px;"),
            icon(res$icon_name, class = "me-2"),
            span(prefix_text),
            span(res$text)
          ),

          # Path (Caja de código)
          div(
            res$path,
            style = "font-family: 'JetBrains Mono', monospace; font-size: 0.72rem;
                     color: #adb5bd; background: #0b1218; padding: 4px 8px;
                     border-radius: 4px; white-space: nowrap; overflow: hidden;
                     text-overflow: ellipsis; display: block;"
          )
        )
      )
    })

    ###########################################################################################################
    # 2. Procesamiento de datos y estados
    rlist_item03_qmd_files <- reactive({
      req(rlist_item02_quarto_proc())

      # 1. Base path desde el item anterior
      path_val <- rlist_item02_quarto_proc()$path

      # 2. Definición de la lista de archivos
      list_render_qmd_file <- list(
        "pack01" = list(qmd_file_path_relative = normalizePath(file.path("g02_quarto_mod", "AAA_01_RUNNER_g02_quarto_mod.qmd"), mustWork = F)),
        "pack02" = list(qmd_file_path_relative = normalizePath(file.path("g04_script_external", "AAA_01_RUNNER_g04_script_external.qmd"), mustWork = F)),
        "pack03" = list(qmd_file_path_relative = normalizePath(file.path("g05_shiny_output", "AAA_01_RUNNER_g05_shiny_output.qmd"), mustWork = F)),
        "pack04" = list(qmd_file_path_relative = normalizePath(file.path("g06_asa", "AAA_01_RUNNER_g06_asa.qmd"), mustWork = F))
      )

      # 3. Procesamiento individual con lapply
      list_processed <- lapply(list_render_qmd_file, function(item) {
        item$qmd_file_path_abs_local <- normalizePath(file.path(path_val, item$qmd_file_path_relative))
        item$exists_local <- file.exists(item$qmd_file_path_abs_local)

        item$status_color <- if(item$exists_local) "#00bc8c" else "#ff4b5c"
        item$status_text  <- if(item$exists_local) "READY" else "MISSING"
        return(item)
      })



      # 4. CÁLCULO GLOBAL (¿Están todos?)
      all_exist <- all(sapply(list_processed, function(x) x$exists_local))
      color_hex <- if(all_exist) "#00bc8c" else "#ff4b5c"

      # 5. Salida consolidada
      list(
        details     = "qmd files",
        is_done     = all_exist,
        # Metadata para el render principal
        text        = if(all_exist) "ALL RUNNERS VERIFIED" else "SOME RUNNERS MISSING",
        color       = color_hex,
        icon_name   = if(all_exist) "check-double" else "file-circle-exclamation",
        shadow      = paste0("0 0 12px ", color_hex),
        list_qmd    = list_processed
      )
    })

    # 3. Renderizado con estilos in-line detallados
    output$item03_qmd_files <- renderUI({
      req(rlist_item03_qmd_files())
      res <- rlist_item03_qmd_files()

      # Prefijo para identificar el item en el debug visual
      prefix_text <- "Item 03 - Quarto files local: "

      div(
        style = "display: flex; align-items: center; padding: 12px; background: #1a262f;
                 border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",

        # 1. Luz de Estado con Neón
        div(
          style = paste0(
            "width: 12px; height: 12px; border-radius: 50%; margin-right: 15px;
             flex-shrink: 0; background-color: ", res$color, "; box-shadow: ", res$shadow, ";"
          )
        ),

        # 2. Contenedor de Texto
        div(
          style = "width: 100%; overflow: hidden;",

          # Título y Estado
          div(
            style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color, "; margin-bottom: 4px;"),
            icon(res$icon_name, class = "me-2"),
            span(prefix_text),
            span(res$text)
          ),

          # Path (Caja de código)
          div(
            res$path,
            style = "font-family: 'JetBrains Mono', monospace; font-size: 0.72rem;
                     color: #adb5bd; background: #0b1218; padding: 4px 8px;
                     border-radius: 4px; white-space: nowrap; overflow: hidden;
                     text-overflow: ellipsis; display: block;"
          )
        )
      )
    })

    ###########################################################################################################

    rlist_item04_temp_folder_Rscience <- reactive({
      # Dependencia del paso anterior
      req(rlist_item03_qmd_files()$is_done)

      # 1. Generar el nombre con el timestamp: Año_Mes_Dia_Hora_Minuto_Segundo
      # Usamos %Y_%m_%d_%H_%M_%S para los guiones bajos
      timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
      folder_name <- paste0("Rscience_", timestamp)

      # 2. Definir la ruta (usando el directorio temporal del sistema o uno específico)
      # Si quieres que sea en el temp del SO:
      base_temp <- tempdir()
      full_path_temp <- file.path(base_temp, folder_name)

      # 3. Intentar crear la carpeta
      # showWarnings = FALSE evita mensajes molestos si por algún azar ya existiera
      folder_created <- dir.create(full_path_temp, showWarnings = FALSE, recursive = TRUE)

      # 4. Definir estados de RScience
      color_hex <- if(folder_created) "#00bc8c" else "#ff4b5c"

      # 5. Salida consolidada
      list(
        path        = full_path_temp,
        folder_name = folder_name,
        is_done     = folder_created,
        # Metadata para el render
        text        = if(folder_created) "TEMP REPOSITORY CREATED" else "FAILED TO CREATE TEMP FOLDER",
        color       = color_hex,
        icon_name   = if(folder_created) "folder-plus" else "folder-minus",
        shadow      = paste0("0 0 12px ", color_hex),
        timestamp = timestamp
      )
    })
    output$item04_temp_folder_Rscience <- renderUI({
      req(rlist_item04_temp_folder_Rscience())
      res <- rlist_item04_temp_folder_Rscience()

      # Prefijo para identificar el item en el debug visual
      prefix_text <- "Item 04 - Create Temp Environment: "

      div(
        style = "display: flex; align-items: center; padding: 12px; background: #1a262f;
                 border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",

        # 1. Luz de Estado con Neón
        div(
          style = paste0(
            "width: 12px; height: 12px; border-radius: 50%; margin-right: 15px;
             flex-shrink: 0; background-color: ", res$color, "; box-shadow: ", res$shadow, ";"
          )
        ),

        # 2. Contenedor de Texto
        div(
          style = "width: 100%; overflow: hidden;",

          # Título y Estado
          div(
            style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color, "; margin-bottom: 4px;"),
            icon(res$icon_name, class = "me-2"),
            span(prefix_text, style = "color: #566b7a;"), # Color más tenue para el prefijo
            span(res$text)
          ),

          # Visualización del Nombre de Carpeta (Badge dinámico)
          div(
            style = "display: flex; align-items: center; gap: 8px; margin-top: 4px;",

            # Nombre de la carpeta con estilo de "ID"
            span(
              res$folder_name,
              style = "font-family: 'JetBrains Mono', monospace; font-size: 0.72rem;
                       color: #00d4ff; background: #0b1218; padding: 2px 8px;
                       border-radius: 4px; border: 1px solid #1a262f;"
            ),

            # El path completo truncado
            span(
              res$path,
              style = "font-family: 'JetBrains Mono', monospace; font-size: 0.65rem;
                       color: #566b7a; white-space: nowrap; overflow: hidden;
                       text-overflow: ellipsis;"
            )
          )
        )
      )
    })

    ###########################################################################################################


    rlist_item05_copy_files <- reactive({
      # Dependencias: Necesitamos los archivos verificados y la carpeta temporal creada
      req(rlist_item03_qmd_files()$is_done)
      req(rlist_item04_temp_folder_Rscience()$is_done)

      # 1. Rutas de origen y destino
      path_origin <- rlist_item02_quarto_proc()$path  # f02_quarto_proc
      path_dest   <- rlist_item04_temp_folder_Rscience()$path # Carpeta Rscience_...

      # 2. Ejecutar la copia recursiva
      # file.copy copiará la carpeta 'f02_quarto_proc' dentro de 'Rscience_...'
      copy_status <- file.copy(from = path_origin,
                               to = path_dest,
                               recursive = TRUE,
                               overwrite = TRUE)

      # 3. Definir estados de RScience
      color_hex <- if(copy_status) "#00bc8c" else "#ff4b5c"

      # 4. Salida consolidada
      list(
        origin      = path_origin,
        dest        = path_dest,
        is_done     = copy_status,
        text        = if(copy_status) "REPOSITORY CLONED SUCCESSFULLY" else "ERROR DURING CLONING",
        color       = color_hex,
        icon_name   = if(copy_status) "clone" else "triangle-exclamation",
        shadow      = paste0("0 0 12px ", color_hex)
      )
    })


    output$item05_copy_files <- renderUI({
      req(rlist_item05_copy_files())
      res <- rlist_item05_copy_files()

      prefix_text <- "Item 05 - File System Sync: "

      div(
        style = "display: flex; align-items: center; padding: 12px; background: #1a262f;
                 border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",

        # 1. Luz de Estado con Neón
        div(
          style = paste0(
            "width: 12px; height: 12px; border-radius: 50%; margin-right: 15px;
             flex-shrink: 0; background-color: ", res$color, "; box-shadow: ", res$shadow, ";"
          )
        ),

        # 2. Contenedor de Texto
        div(
          style = "width: 100%; overflow: hidden;",

          # Título y Estado
          div(
            style = paste0("font-weight: 800; font-size: 0.75rem; color: ", res$color, "; margin-bottom: 4px;"),
            icon(res$icon_name, class = "me-2"),
            span(prefix_text, style = "color: #566b7a;"),
            span(res$text)
          ),

          # Detalle del flujo de copia
          div(
            style = "display: flex; align-items: center; gap: 6px; font-family: 'JetBrains Mono', monospace; font-size: 0.65rem;",
            span("FROM:", style = "color: #00d4ff;"),
            span(basename(res$origin), style = "color: #adb5bd;"),
            icon("arrow-right", style = "color: #566b7a; font-size: 0.6rem;"),
            span("TO:", style = "color: #00d4ff;"),
            span(basename(res$dest), style = "color: #adb5bd;")
          )
        )
      )
    })

    ###########################################################################################################

    rlist_item06_qmd_files_temp <- reactive({
      # Dependencia: El clonado del paso 05 debe estar terminado
      req(rlist_item05_copy_files()$is_done)

      # 1. Recuperamos la base temporal y la estructura original
      path_temp_base <- rlist_item05_copy_files()$dest # Carpeta Rscience_...
      folder_cloned  <- basename(rlist_item02_quarto_proc()$path) # "f02_quarto_proc"

      # 2. Re-mapeamos la lista de archivos a la nueva ubicación temporal
      # Usamos la estructura de nombres del Item 03 para ser consistentes
      list_qmd_origin <- rlist_item03_qmd_files()$list_qmd

      list_processed_temp <- lapply(names(list_qmd_origin), function(pkg_name) {
        item <- list_qmd_origin[[pkg_name]]

        # Construimos el nuevo path: Temp / f02_quarto_proc / carpeta_pkg / runner.qmd
        new_path <- normalizePath(
          file.path(path_temp_base, folder_cloned, item$qmd_file_path_relative),
          mustWork = FALSE
        )

        list(
          qmd_file_path_relative = item$qmd_file_path_relative,
          qmd_file_path_abs_temp = new_path,
          exists_temp            = file.exists(new_path),
          status_color           = if(file.exists(new_path)) "#00bc8c" else "#ff4b5c"
        )
      })
      names(list_processed_temp) <- names(list_qmd_origin)

      # 3. Cálculo Global
      all_exist_temp <- all(sapply(list_processed_temp, function(x) x$exists_temp))
      color_hex      <- if(all_exist_temp) "#00bc8c" else "#ff4b5c"

      # 4. Salida consolidada
      list(
        details   = list_processed_temp,
        is_done   = all_exist_temp,
        text      = if(all_exist_temp) "SANDBOX RUNNERS VERIFIED" else "SANDBOX CORRUPTED",
        color     = color_hex,
        icon_name = if(all_exist_temp) "vial-circle-check" else "bug",
        shadow    = paste0("0 0 12px ", color_hex)
      )
    })
    output$item06_qmd_files_temp <- renderUI({
      req(rlist_item06_qmd_files_temp())
      res <- rlist_item06_qmd_files_temp()

      prefix_text <- "Item 06 - Sandbox Environment Verify: "

      div(
        style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px; margin-bottom: 10px;",

        # Encabezado Principal
        div(style = "display: flex; align-items: center; margin-bottom: 10px;",
            # Luz de Estado
            div(style = paste0("width: 12px; height: 12px; border-radius: 50%; margin-right: 15px;
                               background-color: ", res$color, "; box-shadow: ", res$shadow, ";")),

            # Icono y Texto (Estructura corregida)
            span(
              style = "display: flex; align-items: center; font-size: 0.75rem; font-weight: 800;",
              icon(res$icon_name, style = "margin-right: 8px;"), # Icono separado
              span(prefix_text, style = "color: #566b7a;"),
              span(res$text, style = paste0("color: ", res$color, "; margin-left: 5px;"))
            )
        ),

        # Mini indicadores de archivos
        div(
          style = "display: flex; flex-wrap: wrap; gap: 10px; background: #0b1218; padding: 8px; border-radius: 4px;",
          lapply(names(res$details), function(pkg) {
            item <- res$details[[pkg]]
            div(
              style = "display: flex; align-items: center; font-size: 0.6rem; font-family: 'JetBrains Mono';",
              div(style = paste0("width: 5px; height: 5px; border-radius: 50%; margin-right: 5px; background: ", item$status_color, ";")),
              span(pkg, style = "color: #adb5bd;")
            )
          })
        )
      )
    })
    ###########################################################################################################

    # Estados: "pending" (azul), "processing" (azul parpadeo), "done" (verde), "error" (rojo)
    render_status <- reactiveValues(
      pack01 = "pending",
      pack02 = "pending",
      pack03 = "pending",
      pack04 = "pending"
    )
    observeEvent(rlist_item06_qmd_files_temp()$is_done, {
      req(rlist_item06_qmd_files_temp()$is_done)

      details <- rlist_item06_qmd_files_temp()$details

      for(pkg_name in names(details)) {
        # 1. Cambiar a estado procesando (puedes añadir lógica de parpadeo aquí)
        render_status[[pkg_name]] <- "processing"

        tryCatch({
          # 2. Ejecutar Quarto Render
          quarto::quarto_render(input = details[[pkg_name]]$qmd_file_path_abs_temp)

          # 3. Éxito -> Verde
          render_status[[pkg_name]] <- "done"
        }, error = function(e) {
          # Error -> Rojo
          render_status[[pkg_name]] <- "error"
        })
      }
    })
output$item07_quarto_exec <- renderUI({
  # Colores RScience para ejecución
  get_color <- function(status) {
    switch(status,
           "pending"    = "#007bff", # Azul
           "processing" = "#00d4ff", # Celeste brillante
           "done"       = "#00bc8c", # Verde
           "error"      = "#ff4b5c") # Rojo
  }

  get_icon <- function(status) {
    switch(status,
           "pending"    = "hourglass-start",
           "processing" = "spinner fa-spin",
           "done"       = "check-double",
           "error"      = "times-circle")
  }

  div(
    style = "padding: 12px; background: #1a262f; border: 1px solid #2a3b47; border-radius: 8px;",

    # Título del Proceso
    div(style = "margin-bottom: 10px; font-weight: 800; font-size: 0.75rem; color: #adb5bd;",
        icon("microchip"), " ITEM 07 - CLOUD ENGINE EXECUTION"),

    # Grid de Packs
    div(
      style = "display: grid; grid-template-columns: 1fr; gap: 8px;",
      lapply(names(render_status), function(pkg) {
        status <- render_status[[pkg]]
        color  <- get_color(status)

        div(
          style = paste0("display: flex; align-items: center; padding: 8px; background: #0b1218;
                          border-radius: 4px; border-left: 3px solid ", color, ";"),

          # Luz con sombra dinámica (Neón)
          div(style = paste0("width: 10px; height: 10px; border-radius: 50%; margin-right: 12px;
                             background: ", color, "; box-shadow: 0 0 8px ", color, ";")),

          # Nombre del Pack
          span(pkg, style = "font-family: 'JetBrains Mono'; font-size: 0.7rem; color: #fff; flex-grow: 1;"),

          # Estado e Icono
          span(
            style = paste0("font-size: 0.65rem; font-weight: bold; color: ", color, ";"),
            toupper(status),
            icon(get_icon(status), style = "margin-left: 8px;")
          )
        )
      })
    )
  )
})
    # list_render <- list()

    # list_render$pack01 <- list(
    #   description = "Run qmd files",
    #   qmd_file_mini_path = file.path(""
    # )

    steps_data <- data.frame(
      id = c("01", "02", "03", "04", "05", "06"),
      task = c("Dar play", "Tomar fecha", "Conformar carpeta", "Copiar carpeta", "Ejecutar 01", "Ejecutar 02"),
      detail = c("Signal Activation", "ISO Timestamp Sync", "FS: Directory Creation", "Binary Stream Transfer", "Primary Logic Script", "Validation Loop")
    )

    # Render estático inicial
    output$matrix_static_body <- renderUI({
      lapply(1:nrow(steps_data), function(i) {
        div(id = ns(paste0("row_", i)), class = "row g-0 matrix-row align-items-center",
            div(class = "col-1 matrix-cell step-id", steps_data$id[i]),
            div(class = "col-2 matrix-cell", span(steps_data$task[i], style="font-weight: 600;")),
            div(class = "col-3 matrix-cell text-muted", steps_data$detail[i]),
            div(class = "col-2 matrix-cell",
                div(style="display:flex; justify-content:space-around;",
                    icon("check-circle", id = ns(paste0("r_", i)), class = "check-icon"),
                    icon("play-circle",  id = ns(paste0("s_", i)), class = "check-icon"),
                    icon("stop-circle",  id = ns(paste0("e_", i)), class = "check-icon")
                )
            ),
            div(class = "col-2 matrix-cell time-code", id = ns(paste0("t_start_", i)), "--:--:--.---"),
            div(class = "col-2 matrix-cell time-code", id = ns(paste0("t_end_", i)), "--:--:--.---")
        )
      })
    })

    is_running <- reactiveVal(FALSE)
    start_time <- reactiveVal(NULL)

    # Lógica de Ejecución
    observeEvent(input$run_process, {
      if(is_running()) return()
      is_running(TRUE)
      start_time(Sys.time())

      for(i in 1:6) {
        local({
          idx <- i
          # INICIO DEL PASO
          delay(idx * 900, {
            addClass(paste0("row_", idx), "row-active")
            addClass(paste0("r_", idx), "status-on")
            addClass(paste0("s_", idx), "status-proc")
            html(paste0("t_start_", idx), format(Sys.time(), "%H:%M:%OS3"))
            addClass(paste0("t_start_", idx), "time-active")
          })
          # FIN DEL PASO
          delay(idx * 900 + 800, {
            removeClass(paste0("s_", idx), "status-proc")
            addClass(paste0("s_", idx), "status-on")
            addClass(paste0("e_", idx), "status-on")
            html(paste0("t_end_", idx), format(Sys.time(), "%H:%M:%OS3"))
            addClass(paste0("t_end_", idx), "time-active")
            removeClass(paste0("row_", idx), "row-active")
            if(idx == 6) {
              is_running(FALSE)
              runjs(sprintf("document.getElementById('%s').style.color = '#00bc8c';", ns("total_clock")))
            }
          })
        })
      }
    })

    # Reloj Global
    observe({
      if (!is_running()) return()
      invalidateLater(60)
      diff <- difftime(Sys.time(), start_time(), units = "secs")
      ms <- sprintf("%03d", round((as.numeric(diff) %% 1) * 1000))
      txt <- paste0("00:", format(as.POSIXct(as.numeric(diff), origin="1970-01-01", tz="UTC"), "%M:%S"), ".", ms)
      html("total_clock", txt)
    })

    observeEvent(input$reset_process, {
      shinyjs::runjs("history.go(0);") # Alternativa a reload completo
    })
  })
}

# ==============================================================================
# APP DE PRUEBA (SOLO PARA TEST)
# ==============================================================================

folder_target <- system.file("shiny", "fn03_tool_script", "tool_0001_script_002", package = "Rscience2027")


ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),
  mod_pipeline_ui("pipeline_1")
)
server <- function(input, output, session) {
  mod_pipeline_server("pipeline_1", folder_target = folder_target, list_settings = NULL)
}
shinyApp(ui, server)
