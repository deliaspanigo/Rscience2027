library(shiny)
library(DT)
library(bslib)
library(listviewer)
library(shinyjs)
library(shinyWidgets) # Necesario para el radioGroupButtons

devtools::load_all()


# 2. UI DE LA APP
ui <- fluidPage(
  theme = bs_theme(version = 5, bg = "#0b1218", fg = "#ffffff", primary = "#00d4ff"),


  # El h2 y el módulo
  #div(style = "height: 100vh; display: flex; flex-direction: column;",
      div(style = "padding: 10px 0; flex-shrink: 0;",
          h2("RScience Engine v.0.7.2", style="color: #00d4ff; text-align: center; margin:0;")
      ),
      mod_02_01_dataset_ui("demo_dataset"),
      mod_02_01_dataset_DEBUG_ui("demo_dataset")
  #)
)

# 3. SERVER DE LA APP
server <- function(input, output, session) {

  # Llamamos al server del módulo
  # Guardamos el retorno en un reactivo por si queremos usar los datos en la app
  datos_importados <- mod_02_01_dataset_server(id = "demo_dataset", show_debug = F)

  # Ejemplo de cómo acceder a los datos desde fuera del módulo
  observe({
    req(datos_importados()$is_done)
    message("App principal detectó carga de: ", datos_importados()$metadata$name_mod)
  })
}

shinyApp(ui, server)
