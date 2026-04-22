# --- .Rprofile: FORZADO ABSOLUTO CON VERIFICACIÓN ---

# 1. Bloqueo de Cache
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
Sys.setenv(RENV_CONFIG_CACHE_ENABLED = FALSE)

# 2. DEFINICIÓN DE RUTAS (Ruta corregida para Pandoc)
#base_path         <- "C:/Users/Legion/bulk/MyInstallers/Rscience2027_installer/App"
base_path   <- dirname(here::here())
base_path   <- file.path(base_path, "/App")
mi_ruta_libreria  <- file.path(base_path, "R-Portable/library")
mi_ruta_quarto_exe <- file.path(base_path, "Quarto/bin/quarto.exe")
# Corregido: Agregamos la subcarpeta 'tools'
mi_ruta_pandoc    <- file.path(base_path, "Quarto/bin/tools/pandoc.exe")

# --- RUTA DE CHROME PORTABLE (NUEVO) ---
mi_ruta_chrome    <- file.path(base_path, "Chrome/GoogleChromePortable/App/Chrome-bin/chrome.exe")
mi_ruta_chrome_alt <- file.path(base_path, "Chrome/GoogleChromePortable/App/Chrome/chrome.exe")
mi_ruta_chrome_legacy <- file.path(base_path, "Chrome/chrome.exe")

# Función auxiliar para chequear existencia
check_file <- function(path) {
  if (file.exists(path)) return(" [OK]") else return(" [NOT FOUND! ❌]")
}

# Función para encontrar Chrome portable automáticamente
find_chrome_portable <- function() {
  posibles_rutas <- c(mi_ruta_chrome, mi_ruta_chrome_alt, mi_ruta_chrome_legacy)

  for (ruta in posibles_rutas) {
    if (file.exists(ruta)) {
      return(ruta)
    }
  }
  return(NULL)
}

# Aplicar configuraciones
.libPaths(mi_ruta_libreria)
Sys.setenv(QUARTO_PATH = mi_ruta_quarto_exe)
Sys.setenv(QUARTO_BIN_PATH = mi_ruta_quarto_exe)
Sys.setenv(QUARTO_PANDOC = mi_ruta_pandoc)
Sys.setenv(RSTUDIO_PANDOC = dirname(mi_ruta_pandoc))

# --- CONFIGURACIÓN DE CHROME PORTABLE (NUEVO) ---
chrome_path <- find_chrome_portable()
if (!is.null(chrome_path)) {
  Sys.setenv(CHROMOTE_CHROME = chrome_path)
  options(chromote.chrome = chrome_path)
  options(webshot2.chrome = chrome_path)
  cat("\n✅ Chrome portable configurado:", chrome_path, check_file(chrome_path))
} else {
  cat("\n⚠️ Chrome portable no encontrado. Usando Chrome del sistema si está disponible.")
}

# 3. Mensaje de confirmación con Verificación de Existencia
cat("\n====================================================")
cat("\n* RScience: MODO TEST ABSOLUTO ACTIVADO           *")
cat("\n* ----------------------------------------------  *")
cat("\n* Quarto :", Sys.getenv("QUARTO_PATH"), check_file(mi_ruta_quarto_exe))
cat("\n* Pandoc :", Sys.getenv("QUARTO_PANDOC"), check_file(mi_ruta_pandoc))
cat("\n* Libs   :", .libPaths()[1], check_file(mi_ruta_libreria))
if (!is.null(chrome_path)) {
  cat("\n* Chrome :", chrome_path, check_file(chrome_path))
}
cat("\n====================================================\n")

# 4. Cargar renv (solo si el archivo existe)
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# 5. Limpiar variables temporales
rm(find_chrome_portable, chrome_path, check_file)
