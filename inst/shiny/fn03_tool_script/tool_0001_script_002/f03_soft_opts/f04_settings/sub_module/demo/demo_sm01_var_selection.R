library(shiny)
library(shinyjs)
library(bslib)


source(file = "../sm01_var_selection.R")

# ==============================================================================
# APP PRINCIPAL (EJECUCIÓN)
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),

  div(class = "container", style = "margin-top: 50px; max-width: 800px;",
      # h2(style = "font-weight: 900; color: #2c3e50; text-align: center; margin-bottom: 30px;",
      #    "Data Variable Selector"),

      # Llamada al módulo único
      SUB_mod_var_selection_ui("mi_selector_unico")

      # br(), hr(),
      #
      # # Panel de resultados para ver qué captura el servidor
      # div(class = "card", style = "border: none; background: #f8f9fa; padding: 20px;",
      #     h5("Output del Servidor (Reactivo):"),
      #     verbatimTextOutput("servidor_debug")
      # )
  )
)

server <- function(input, output, session) {

  # Inicializamos el módulo pasándole mtcars (o lo que gustes)
  seleccion <- SUB_mod_var_selection_server("mi_selector_unico", df_input = iris, show_debug = T)

  # Mostramos en pantalla el resultado en tiempo real
  # output$servidor_debug <- renderPrint({
  #   seleccion()
  # })
}

shinyApp(ui, server)
