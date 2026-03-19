# ==============================================================================
# MÓDULO IMPORTACIÓN - v.0.0.1 STABLE & PROTECTED
# ==============================================================================
library("shinyjs")
library("DT")

mod_import_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .container-fluid { overflow-x: hidden !important; padding-top: 20px !important; }
        .btn-pill-xl {
          border-radius: 50px !important; padding: 12px 28px !important;
          font-weight: 800; font-size: 0.8rem; text-transform: uppercase;
          letter-spacing: 1px; transition: all 0.2s ease;
          display: inline-flex; align-items: center; justify-content: center;
          gap: 8px; border: 2px solid transparent; white-space: nowrap;
        }
        .action-row-right {
          display: flex; flex-direction: row; justify-content: flex-end;
          align-items: center; gap: 12px; height: 100%; padding-top: 25px;
        }
        .section-label {
          font-weight: 800; font-size: 0.75rem; color: #00d4ff;
          text-transform: uppercase; margin-bottom: 8px; letter-spacing: 1.2px;
        }
        .engine-divider { border-top: 1px solid rgba(0,212,255,0.2); margin: 30px 0; }
      "))
    ),

    div(class = "container-fluid",
        div(class = "row g-0 align-items-center",
            div(class = "col-md-8",
                div(class = "row g-3",
                    div(class = "col-md-4",
                        div(class = "section-label", "Source Type"),
                        # CAMBIADO A selectInput PARA ESTABILIDAD
                        selectInput(
                          ns("fuente"), NULL,
                          choices = c("Local File" = "archivo", "R Example" = "ejemplo"),
                          selected = "archivo", width = "100%"
                        )
                    ),
                    div(class = "col-md-8",
                        div(class = "section-label", "Data Selection"),
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'archivo'", ns("fuente")),
                          fileInput(ns("archivo"), NULL,
                                    accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx"),
                                    buttonLabel = "Browse...", width = "100%")
                        ),
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'ejemplo'", ns("fuente")),
                          # CAMBIADO A selectInput PARA ESTABILIDAD
                          selectInput(
                            ns("dataset_ejemplo"), NULL,
                            choices = c("(Select Dataset)" = "", "mtcars", "iris", "airquality", "diamonds"),
                            width = "100%"
                          )
                        )
                    )
                ),
                uiOutput(ns("opciones_ui"))
            ),

            div(class = "col-md-4",
                div(class = "action-row-right",
                    actionButton(ns("btn_importar"), span(icon("check"), "Import"), class = "btn-success btn-pill-xl"),
                    actionButton(ns("btn_habilitar"), span(icon("edit"), "Edit"), class = "btn-warning btn-pill-xl"),
                    actionButton(ns("btn_reset"), span(icon("trash"), "Reset"), class = "btn-danger btn-pill-xl")
                )
            )
        ),
        div(class = "engine-divider"),
        div(class = "row g-0",
            div(class = "col-12",
                div(class = "section-label mb-3", icon("table"), " Data Preview"),
                DTOutput(ns("preview")),
                div(class = "engine-divider"),
                div(class = "row g-4",
                    div(class = "col-md-6",
                        div(class = "section-label", "System Status"),
                        verbatimTextOutput(ns("estado_txt"))
                    ),
                    div(class = "col-md-6",
                        div(class = "section-label", "Selection Debug"),
                        verbatimTextOutput(ns("debug_seleccion"))
                    )
                )
            )
        )
    )
  )
}

mod_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    datos <- reactiveValues(df = NULL, nombre = NULL)
    bloqueado <- reactiveVal(FALSE)

    # --- Lógica de Opciones Dinámicas (CSV/Excel) ---
    output$opciones_ui <- renderUI({
      req(input$fuente)
      if (input$fuente != "archivo" || is.null(input$archivo)) return(NULL)
      ext <- tolower(tools::file_ext(input$archivo$name))

      if (ext %in% c("csv", "tsv", "txt")) {
        fluidRow(
          column(4, selectInput(ns("sep"), "Separador", choices = c("," = ",", ";" = ";", "\t" = "\t"))),
          column(4, selectInput(ns("dec"), "Decimal", choices = c("." = ".", "," = ","), selected = ".")),
          column(4, checkboxInput(ns("header"), "Encabezados", TRUE))
        )
      } else if (ext %in% c("xls", "xlsx")) {
        hojas <- tryCatch(readxl::excel_sheets(input$archivo$datapath), error = function(e) "Sheet1")
        selectInput(ns("hoja_excel"), "Hoja:", choices = hojas)
      }
    })

    # --- Importar ---
    observeEvent(input$btn_importar, {
      req(input$fuente)
      # ... (Tu lógica de importación actual está bien, solo asegúrate de usar req() al inicio)
      tryCatch({
        if (input$fuente == "archivo") {
          req(input$archivo)
          ruta <- input$archivo$datapath
          ext <- tolower(tools::file_ext(input$archivo$name))
          if (ext %in% c("csv", "tsv", "txt")) {
            req(input$sep)
            df <- vroom::vroom(ruta, delim = input$sep, show_col_types = FALSE)
          } else {
            req(input$hoja_excel)
            df <- readxl::read_excel(ruta, sheet = input$hoja_excel)
          }
        } else {
          req(input$dataset_ejemplo != "")
          df <- get(input$dataset_ejemplo, "package:datasets")
        }
        datos$df <- df
        datos$nombre <- if(input$fuente=="archivo") input$archivo$name else input$dataset_ejemplo
        bloqueado(TRUE)
      }, error = function(e) showNotification(e$message, type="error"))
    })

    # --- Reset y Habilitar ---
    observeEvent(input$btn_reset, {
      datos$df <- NULL
      bloqueado(FALSE)
      shinyjs::reset("archivo")
      updateSelectInput(session, "fuente", selected = "archivo")
    })

    observeEvent(input$btn_habilitar, { bloqueado(FALSE) })

    # --- RENDER OUTPUTS ---
    output$preview <- renderDT({
      req(datos$df)
      datatable(head(datos$df, 15), options = list(scrollX = TRUE))
    })

    # --- EL REACTIVO CRÍTICO (CORREGIDO LÍNEA 370) ---
    seleccion_usuario <- reactive({
      # PROTECCIÓN: Si input$fuente no existe aún, devolvemos una lista vacía o NULL
      # Esto evita el error "argument is of length zero" en el Launchpad
      req(input$fuente)

      list(
        fuente           = input$fuente,
        datos_importados = datos$df,
        nombre_datos     = datos$nombre,
        is_ready         = !is.null(datos$df),
        bloqueado        = bloqueado()
      )
    })

    return(seleccion_usuario)
  })
}
