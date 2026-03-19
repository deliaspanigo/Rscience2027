library(shiny)
library(bslib)
library(ggplot2)

# ==============================================================================
# INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#00d4ff"
  ),

  header = tags$head(
    tags$style(HTML("
      /* HACHAZO PARA ELIMINAR ESPACIO SUPERIOR */
      .sidebar-content, .bslib-sidebar-content, aside.sidebar, section.sidebar {
        padding-top: 0 !important;
        margin-top: 0 !important;
      }
      .logo-container { margin: 0 !important; padding: 0 !important; line-height: 0; }
      .sidebar { background-color: #f8f9fa !important; border-right: 1px solid #dee2e6; }

      /* EFECTO CALCOMANÍA */
      .logo-container img {
        transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
        filter: drop-shadow(0 4px 8px rgba(0,0,0,0.15));
        cursor: pointer;
        display: block;
        margin: 0 auto;
      }
      .logo-container img:hover {
        transform: scale(1.08) rotate(2deg);
        filter: drop-shadow(0 8px 16px rgba(0,212,255,0.4));
      }

      .section-title {
        text-transform: uppercase;
        letter-spacing: 1.2px;
        font-weight: 800;
        font-size: 0.7rem;
        color: #95a5a6;
        margin-top: 15px;
      }
    "))
  ),

  sidebar = sidebar(
    width = 300,
    padding = 0,

    # IDENTIDAD Rscience
    div(class = "logo-container text-center",
        img(src = "Rscience_logo_sticker.png", width = "165px")),

    div(class = "text-center",
        style = "margin-top: -15px; margin-bottom: 25px;",
        span("v.0.0.1", class = "badge bg-dark shadow-sm",
             style = "font-size: 0.9rem; padding: 6px 18px; font-family: monospace; border: 1px solid #444;")),

    # NAVEGACIÓN PRINCIPAL
    div(style = "padding: 0 1rem;",
        hr(style = "opacity: 0.1; margin-top: 0;"),
        div(class = "section-title mb-3", "1. Setup Phase"),
        input_switch("sw_dataset", "Dataset Selection", value = TRUE),
        input_switch("sw_tool", "Tool & Script Engine", value = FALSE),
        hr(style = "opacity: 0.1;"),
        div(class = "section-title mb-3", "2. Execution Phase"),
        input_switch("sw_theory", "Theory & Basics", value = FALSE),
        input_switch("sw_analysis", "Data Analysis Studio", value = FALSE),
        input_switch("sw_resources", "Extra Resources", value = FALSE),
        hr(style = "opacity: 0.1;"),
        div(class = "p-3 bg-white border rounded shadow-sm text-center",
            span(strong("Active Engine:"), style = "font-size: 0.7rem; color: #adb5bd; display: block;"),
            uiOutput("txt_status_mini"))
    )
  ),

  uiOutput("main_content")
)

# ==============================================================================
# SERVIDOR (SERVER)
# ==============================================================================
server <- function(input, output, session) {

  # Data reactiva (mtcars por defecto)
  data_active <- reactive({
    if (is.null(input$f1)) return(mtcars)
    read.csv(input$f1$datapath)
  })

  # Lógica de exclusividad de switches
  all_switches <- c("sw_dataset", "sw_tool", "sw_theory", "sw_analysis", "sw_resources")
  observe_toggle <- function(id) {
    observeEvent(input[[id]], {
      if (input[[id]]) {
        others <- all_switches[all_switches != id]
        for (other in others) { updateCheckboxInput(session, other, value = FALSE) }
      } else if (sum(sapply(all_switches, function(x) input[[x]])) == 0) {
        updateCheckboxInput(session, id, value = TRUE)
      }
    }, ignoreInit = TRUE)
  }
  lapply(all_switches, observe_toggle)

  # Status badge
  output$txt_status_mini <- renderUI({
    div(style="margin-top: 5px;",
        span(icon("microchip"), " ANOVA ", class = "badge", style="background-color: #00d4ff; color: white;"),
        span(icon("chart-line"), " GGPLOT2 ", class = "badge bg-secondary ms-1")
    )
  })

  # Renderizado de contenido
  output$main_content <- renderUI({

    if (input$sw_dataset) {
      card(
        card_header(class = "bg-primary text-white", "Phase 1: Dataset Selection"),
        navset_card_tab(
          nav_panel(icon("upload"), "Upload", div(class="p-4", fileInput("f1", "Load CSV"))),
          nav_panel(icon("table"), "Preview", div(class="p-4", tableOutput("prev_tab")))
        )
      )

    } else if (input$sw_analysis) {
      # --- EL NÚCLEO DE ANÁLISIS REESTRUCTURADO ---
      card(
        full_screen = TRUE,
        card_header(class = "bg-primary text-white", "Phase 2: Data Analysis Studio"),
        navset_card_pill(
          # 1. Variable Selection
          nav_panel("1. Variable Selection",
                    div(class="p-4",
                        layout_columns(
                          selectInput("y_var", "Dependent Variable (Y)", choices = names(data_active())),
                          selectInput("x_var", "Independent Variable (X)", choices = names(data_active()), selected = names(data_active())[2])
                        ),
                        p(class="text-muted", "Define los ejes para el análisis estadístico.")
                    )
          ),
          # 2. Settings
          nav_panel("2. Settings",
                    div(class="p-4",
                        checkboxInput("show_line", "Add Trend Line (LM)", value = TRUE),
                        sliderInput("point_size", "Point Size", min=1, max=10, value=3),
                        selectInput("theme_plot", "Plot Theme", c("Minimal", "Classic", "Dark", "Light"))
                    )
          ),
          # 3. Outputs
          nav_panel("3. Outputs",
                    div(class="p-4",
                        plotOutput("main_plot", height = "450px"),
                        hr(),
                        verbatimTextOutput("model_summary")
                    )
          ),
          # 4. Reporting
          nav_panel("4. Reporting",
                    div(class="p-5 text-center",
                        icon("file-pdf", class="fa-4x mb-3 text-danger"),
                        h4("Generate Technical Report"),
                        p("Se exportarán los resultados y gráficos en formato profesional."),
                        downloadButton("btn_report", "Export PDF Report", class="btn-primary")
                    )
          )
        )
      )

    } else {
      card(class="p-5 text-center border-0", h4("Module Workspace Active", class="text-muted"))
    }
  })

  # Outputs de la fase de análisis
  output$prev_tab <- renderTable({ head(data_active(), 5) })

  output$main_plot <- renderPlot({
    req(input$x_var, input$y_var)
    p <- ggplot(data_active(), aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(color = "#00d4ff", size = input$point_size, alpha = 0.7)

    if(input$show_line) p <- p + geom_smooth(method = "lm", color = "#2c3e50")

    # Cambio de temas dinámico
    p <- p + switch(input$theme_plot,
                    "Minimal" = theme_minimal(),
                    "Classic" = theme_classic(),
                    "Dark" = theme_dark(),
                    "Light" = theme_light())
    p
  })

  output$model_summary <- renderPrint({
    req(input$x_var, input$y_var)
    fit <- lm(as.formula(paste(input$y_var, "~", input$x_var)), data = data_active())
    summary(fit)
  })
}

shinyApp(ui, server)
