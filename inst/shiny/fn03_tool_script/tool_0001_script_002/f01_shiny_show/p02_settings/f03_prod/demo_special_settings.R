library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(DT)
library(plotly)
library(colourpicker)
library(openxlsx)
library(listviewer)

# 1. CARGA DE COMPONENTES

devtools::load_all()

source("sub_module/sm01_var_selection.R")
source("sub_module/sm02_levels.R")

source("mod_special_settings.R")

# 2. PREPARACIÓN DE DATOS DE PRUEBA
# Convertimos 'am' a factor con etiquetas para probar el Levels Studio mejor
# data_test <- mtcars

# 3. INTERFAZ DE USUARIO (UI)
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # Requerido para los bloqueos de interfaz
  useShinyjs(),

  # Contenedor con padding estético
  mod_special_settings_ui("analysis_pack")

)

# 4. LÓGICA DEL SERVIDOR
server <- function(input, output, session) {

  folder_prueba <- reactive({

    pkg_path  <- system.file(package = "Rscience2027")
    if (pkg_path == "") pkg_path <- "inst"
    base_path <- file.path(pkg_path, "shiny", "fn03_tool_script", 'tool_0001_script_002')

  })

  # Ejecutamos el PACK pasando el dataset reactivo
  # show_debug = TRUE nos permite ver el JSON consolidado al final
  resultados_finales <-mod_special_settings_server(
    id = "analysis_pack",
    df_input = reactive(mtcars),
    folder_path_tool_script = folder_prueba,
    show_debug = TRUE
  )

  # Opcional: Observar en la consola de R cuando el usuario bloquea TODO el pack
  observe({
    req(resultados_finales()$is_locked)
    message("--- [SISTEMA BLOQUEADO] Ready for Statistical Calculation ---")
    print(resultados_finales()$metadata$selection)
  })
}

# 5. LANZAR APP
shinyApp(ui, server)
