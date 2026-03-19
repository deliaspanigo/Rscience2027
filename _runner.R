# 1. Detener cualquier instancia de Shiny que esté corriendo en esta sesión
try(shiny::stopApp(), silent = TRUE)

# 2. Limpieza total del Environment (incluyendo archivos ocultos)
rm(list = ls(all.names = TRUE))

# 3. Forzar Garbage Collection y esperar un instante a que el sistema procese
gc(); Sys.sleep(0.5)

# 4. Limpiar la consola para que los mensajes de carga sean nuevos
cat("\014")

# 5. Cargar el código fresco
message("--- Cargando Rscience3 desde cero ---")
devtools::load_all()

# 6. Ejecutar la aplicación
shiny::runApp(
  system.file("shiny/app002_Rscience/app.R", package = "Rscience3"),
  display.mode = "normal"
)
