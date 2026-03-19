# R/mod_import.R
#' Módulo UI para importar datos
#'
#' @param id ID del módulo
#' @export
library("shinyjs")
mod_import_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          ns("fuente"),
          "Origen de los datos:",
          choices = c("Archivo local" = "archivo", "Ejemplo de R" = "ejemplo"),
          selected = "archivo"
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'archivo'", ns("fuente")),
          fileInput(
            ns("archivo"),
            "Archivo:",
            accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx"),
            buttonLabel = "Seleccionar...",
            placeholder = "Ningún archivo seleccionado"
          ),
          uiOutput(ns("opciones_ui"))
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'ejemplo'", ns("fuente")),
          selectInput(
            ns("dataset_ejemplo"),
            "Dataset de ejemplo:",
            choices = c("(elige uno)" = "", "mtcars", "iris", "airquality", "diamonds")
          )
        ),

        hr(),

        actionButton(ns("btn_importar"),   "📥 Importar y bloquear controles",   class = "btn-success", width = "100%"),
        br(), br(),
        actionButton(ns("btn_habilitar"),  "✏️ Habilitar edición",              class = "btn-warning", width = "100%"),
        br(), br(),
        actionButton(ns("btn_reset"),      "🗑️ Resetear todo",                  class = "btn-danger",  width = "100%")
      ),

      mainPanel(
        h4("Estado actual"),
        verbatimTextOutput(ns("estado_txt")),

        hr(),

        h4("Previsualización (primeras 15 filas)"),
        DTOutput(ns("preview")),

        hr(),

        h4("Debug: Información completa de la selección del usuario"),
        verbatimTextOutput(ns("debug_seleccion"))
      )
    )
  )
}


#' Módulo Server para importar datos
#'
#' @param id ID del módulo
#' @return Lista reactiva con toda la selección del usuario
#' @export
mod_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    datos <- reactiveValues(df = NULL, nombre = NULL)
    bloqueado <- reactiveVal(FALSE)

    controles_seleccion <- c(
      "fuente", "archivo", "dataset_ejemplo",
      "sep", "dec", "header", "hoja_excel"
    )

    botones <- c("btn_importar", "btn_habilitar", "btn_reset")

    toggle_controles_seleccion <- function(bloq) {
      bloqueado(bloq)

      # Controles fijos (siempre existen)
      fixed_ids <- c("fuente", "archivo", "dataset_ejemplo")
      lapply(fixed_ids, function(id) {
        if (bloq) shinyjs::disable(id) else shinyjs::enable(id)
      })

      # Botones SIEMPRE habilitados
      lapply(botones, function(id) shinyjs::enable(id))

      # Dinámicos: intento inmediato (puede fallar si no existen aún)
      dynamic_ids <- c("sep", "dec", "header", "hoja_excel")
      lapply(dynamic_ids, function(id) {
        try({
          if (bloq) shinyjs::disable(id) else shinyjs::enable(id)
        }, silent = TRUE)
      })
    }

    # Observador separado para bloquear dinámicos cuando aparecen (clave para que funcione)
    observe({
      if (bloqueado() && !is.null(input$archivo)) {
        # Esperamos un poco para que renderUI inserte los elementos
        invalidateLater(300, session)
        lapply(c("sep", "dec", "header", "hoja_excel"), function(id) {
          try(shinyjs::disable(id), silent = TRUE)
        })
      }
    })

    # Habilitar dinámicos al desbloquear
    observeEvent(bloqueado(), {
      if (!bloqueado()) {
        lapply(c("sep", "dec", "header", "hoja_excel"), function(id) {
          try(shinyjs::enable(id), silent = TRUE)
        })
      }
    }, ignoreInit = TRUE)

    limpiar_si_cambia <- function() {
      if (!is.null(datos$df)) {
        datos$df <- NULL
        datos$nombre <- NULL
        toggle_controles_seleccion(FALSE)
        showNotification(
          "Opciones modificadas → datos anteriores eliminados automáticamente",
          type = "warning",
          duration = 5
        )
      }
    }

    lapply(
      c("fuente", "archivo", "dataset_ejemplo", "sep", "dec", "header", "hoja_excel"),
      function(inp) observeEvent(input[[inp]], limpiar_si_cambia(), ignoreInit = TRUE, ignoreNULL = TRUE)
    )

    observeEvent(input$fuente, {
      datos$df <- NULL
      datos$nombre <- NULL
      toggle_controles_seleccion(FALSE)
    }, ignoreInit = TRUE)

    output$opciones_ui <- renderUI({
      if (input$fuente != "archivo" || is.null(input$archivo)) return(NULL)

      ext <- tolower(tools::file_ext(input$archivo$name))

      if (ext %in% c("csv", "tsv", "txt")) {
        fluidRow(
          column(4, selectInput(ns("sep"), "Separador", choices = c("," = ",", ";" = ";", "\t" = "\t"))),
          column(4, selectInput(ns("dec"), "Decimal", choices = c("." = ".", "," = ","), selected = ".")),
          column(4, checkboxInput(ns("header"), "Encabezados", TRUE))
        )
      } else if (ext %in% c("xls", "xlsx")) {
        hojas <- tryCatch(
          readxl::excel_sheets(input$archivo$datapath),
          error = function(e) "Hoja1"
        )
        selectInput(ns("hoja_excel"), "Hoja:", choices = hojas)
      } else {
        tags$p("Formato no soportado", style = "color: red;")
      }
    })

    observeEvent(input$btn_importar, {
      if (is.null(input$fuente)) {
        showNotification("Selecciona una fuente primero", type = "warning")
        return()
      }

      tryCatch({
        if (input$fuente == "archivo") {
          if (is.null(input$archivo)) {
            showNotification("Selecciona un archivo primero", type = "warning")
            return()
          }

          ruta <- input$archivo$datapath
          ext <- tolower(tools::file_ext(input$archivo$name))
          nombre <- input$archivo$name

          if (ext %in% c("csv", "tsv", "txt")) {
            if (is.null(input$sep)) {
              showNotification("Define el separador", type = "warning")
              return()
            }
            df <- vroom::vroom(
              ruta,
              delim = input$sep,
              locale = vroom::locale(decimal_mark = input$dec %||% "."),
              col_names = isTRUE(input$header),
              show_col_types = FALSE
            )
          } else if (ext %in% c("xls", "xlsx")) {
            if (is.null(input$hoja_excel)) {
              showNotification("Selecciona una hoja del Excel", type = "warning")
              return()
            }
            df <- readxl::read_excel(ruta, sheet = input$hoja_excel)
          } else {
            stop("Formato de archivo no soportado")
          }
        } else {
          if (input$dataset_ejemplo == "") {
            showNotification("Selecciona un dataset de ejemplo", type = "warning")
            return()
          }
          df <- get(input$dataset_ejemplo, envir = as.environment("package:datasets"))
          nombre <- paste0("ejemplo: ", input$dataset_ejemplo)
        }

        datos$df <- df
        datos$nombre <- nombre

        toggle_controles_seleccion(TRUE)

        showNotification(
          paste("Datos importados correctamente:", nrow(df), "filas"),
          type = "message",
          duration = 6
        )

      }, error = function(e) {
        showNotification(
          paste("Error al importar:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    observeEvent(input$btn_habilitar, {
      toggle_controles_seleccion(FALSE)
      showNotification("Edición habilitada", type = "message", duration = 4)
    })

    observeEvent(input$btn_reset, {
      datos$df <- NULL
      datos$nombre <- NULL

      shinyjs::reset(ns("archivo"))
      updateSelectInput(session, ns("dataset_ejemplo"), selected = "")
      updateRadioButtons(session, ns("fuente"), selected = "archivo")

      updateSelectInput(session, ns("sep"), selected = ",")
      updateSelectInput(session, ns("dec"), selected = ".")
      updateCheckboxInput(session, ns("header"), value = TRUE)

      toggle_controles_seleccion(FALSE)

      showNotification("Todo reseteado completamente", type = "warning", duration = 5)
    })

    output$estado_txt <- renderText({
      if (is.null(datos$df)) {
        "No hay datos cargados"
      } else {
        sprintf(
          "Cargado: %s\nFilas × Columnas: %d × %d\nControles de selección: %s",
          datos$nombre, nrow(datos$df), ncol(datos$df),
          if(bloqueado()) "bloqueados" else "habilitados"
        )
      }
    })

    output$preview <- renderDT({
      req(datos$df)
      datatable(head(datos$df, 15), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$debug_seleccion <- renderPrint({
      str(seleccion_usuario(), max.level = 1)
    })

    seleccion_usuario <- reactive({
      list(
        fuente             = input$fuente,
        archivo_nombre     = if (input$fuente == "archivo" && !is.null(input$archivo)) input$archivo$name else NULL,
        archivo_path       = if (input$fuente == "archivo" && !is.null(input$archivo)) input$archivo$datapath else NULL,
        dataset_ejemplo    = if (input$fuente == "ejemplo") input$dataset_ejemplo else NULL,
        sep                = if (input$fuente == "archivo") input$sep else NULL,
        dec                = if (input$fuente == "archivo") input$dec else NULL,
        header             = if (input$fuente == "archivo") input$header else NULL,
        hoja_excel         = if (input$fuente == "archivo" && !is.null(input$archivo) &&
                                 tolower(tools::file_ext(input$archivo$name)) %in% c("xls", "xlsx"))
          input$hoja_excel else NULL,
        datos_importados   = datos$df,
        nombre_datos       = datos$nombre,
        bloqueado          = bloqueado(),
        is_ready           = !is.null(datos$df)
      )
    })

    return(seleccion_usuario)
  })
}
