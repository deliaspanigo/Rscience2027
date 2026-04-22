# --- .Rprofile: FORZADO ABSOLUTO CON VERIFICACIÓN ---

# 1. Bloqueo de Cache
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
Sys.setenv(RENV_CONFIG_CACHE_ENABLED = FALSE)

# 2. DEFINICIÓN DE RUTAS (Ruta corregida para Pandoc)
base_path         <- "C:/Users/Legion/bulk/MyInstallers/Rscience2027_installer/App"
mi_ruta_libreria  <- file.path(base_path, "R-Portable/library")
mi_ruta_quarto_exe <- file.path(base_path, "Quarto/bin/quarto.exe")
# Corregido: Agregamos la subcarpeta 'tools'
mi_ruta_pandoc    <- file.path(base_path, "Quarto/bin/tools/pandoc.exe")

# Función auxiliar para chequear existencia
check_file <- function(path) {
  if (file.exists(path)) return(" [OK]") else return(" [NOT FOUND! ❌]")
}

# Aplicar configuraciones
.libPaths(mi_ruta_libreria)
Sys.setenv(QUARTO_PATH = mi_ruta_quarto_exe)
Sys.setenv(QUARTO_BIN_PATH = mi_ruta_quarto_exe)
Sys.setenv(QUARTO_PANDOC = mi_ruta_pandoc)
Sys.setenv(RSTUDIO_PANDOC = dirname(mi_ruta_pandoc))

# 3. Mensaje de confirmación con Verificación de Existencia
cat("\n====================================================")
cat("\n* RScience: MODO TEST ABSOLUTO ACTIVADO           *")
cat("\n* ----------------------------------------------  *")
cat("\n* Quarto :", Sys.getenv("QUARTO_PATH"), check_file(mi_ruta_quarto_exe))
cat("\n* Pandoc :", Sys.getenv("QUARTO_PANDOC"), check_file(mi_ruta_pandoc))
cat("\n* Libs   :", .libPaths()[1], check_file(mi_ruta_libreria))
cat("\n====================================================\n")

# 4. Cargar renv (solo si el archivo existe)
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}
