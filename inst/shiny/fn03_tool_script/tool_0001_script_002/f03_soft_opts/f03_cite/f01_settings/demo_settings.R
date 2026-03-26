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
# Asegúrate de que los nombres de archivo coincidan con los que tienes en tu carpeta
source("sub_module/sm01_var_selection.R")
source("sub_module/sm02_levels.R")
source("mod_PACK_settings.R")

# 2. PREPARACIÓN DE DATOS DE PRUEBA
# Convertimos 'am' a factor con etiquetas para probar el Levels Studio mejor
data_test <- mtcars %>%
  mutate(
    Transmission = factor(am, levels = c(0, 1), labels = c("Automatic", "Manual")),
    Cylinders = factor(cyl)
  )

# 3. INTERFAZ DE USUARIO (UI)
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # Requerido para los bloqueos de interfaz
  useShinyjs(),

  # Contenedor con padding estético
  mod_PACK_settings_ui("analysis_pack")

)

# 4. LÓGICA DEL SERVIDOR
server <- function(input, output, session) {

  # Ejecutamos el PACK pasando el dataset reactivo
  # show_debug = TRUE nos permite ver el JSON consolidado al final
  resultados_finales <- mod_PACK_settings_server(
    id = "analysis_pack",
    df_input = reactive(data_test),
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
