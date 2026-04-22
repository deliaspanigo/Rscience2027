# setup_rscience.R
# Este script configura las rutas de Rscience antes del renderizado
# SOLO INFORMATIVO - No ejecuta ningún binario

# ============================================================
# 1. DEFINICIÓN DE RUTAS ABSOLUTAS
# ============================================================

#base_app <- "C:/Users/Legion/bulk/MyInstallers/Rscience2027_installer/App"
base_app   <- dirname(here::here())
base_app   <- file.path(base_app, "/App")
# Rutas de Quarto y Pandoc
quarto_exe <- file.path(base_app, "Quarto/bin/quarto.exe")
pandoc_dir <- file.path(base_app, "Quarto/bin/tools")
pandoc_exe <- file.path(pandoc_dir, "pandoc.exe")

# Rutas de Chrome portable
chrome_paths <- list(
  primary = file.path(base_app, "Chrome/GoogleChromePortable/App/Chrome-bin/chrome.exe"),
  alt = file.path(base_app, "Chrome/GoogleChromePortable/App/Chrome/chrome.exe"),
  legacy = file.path(base_app, "Chrome/chrome.exe")
)

# Ruta de la librería de R
r_library <- file.path(base_app, "R-Portable/library")

# ============================================================
# 2. FUNCIONES AUXILIARES (SOLO VERIFICACIÓN, NO EJECUCIÓN)
# ============================================================

# Función para verificar existencia de archivos
check_file <- function(path) {
  if (is.null(path) || path == "") {
    return(" [NOT PROVIDED]")
  }
  if (file.exists(path)) {
    return(" [✅ EXISTE]")
  } else {
    return(" [❌ NO EXISTE]")
  }
}

# Función para encontrar Chrome portable
find_chrome_portable <- function() {
  for (chrome_path in chrome_paths) {
    if (file.exists(chrome_path)) {
      return(chrome_path)
    }
  }
  return(NULL)
}

# Función para obtener versión de R (solo información)
get_r_version <- function() {
  paste0(R.version$major, ".", R.version$minor, " (", R.version$nickname, ")")
}

# ============================================================
# 3. CONFIGURACIÓN DE VARIABLES DE ENTORNO
# ============================================================

# Quarto y Pandoc
Sys.setenv(QUARTO_PATH = quarto_exe)
Sys.setenv(QUARTO_PANDOC = pandoc_exe)
Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)

# Chrome
chrome_path <- find_chrome_portable()
if (!is.null(chrome_path)) {
  Sys.setenv(CHROMOTE_CHROME = chrome_path)
  options(chromote.chrome = chrome_path)
  options(webshot2.chrome = chrome_path)
}

# Librerías
.libPaths(c(r_library, .libPaths()))

# ============================================================
# 4. VERIFICACIONES (SOLO EXISTENCIA, NO EJECUCIÓN)
# ============================================================

quarto_exists <- file.exists(quarto_exe)
pandoc_exists <- file.exists(pandoc_exe)
chrome_exists <- !is.null(chrome_path) && file.exists(chrome_path)
r_library_exists <- dir.exists(r_library)

# ============================================================
# 5. MENSAJE DE CONFIRMACIÓN INFORMATIVO
# ============================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                         RScience - Configuración de Entorno                  ║\n")
cat("╠══════════════════════════════════════════════════════════════════════════════╣\n")

# R Version
cat("║                                                                              ║\n")
cat("║  📦 R PORTABLE                                                               ║\n")
cat("║  ├─ Versión        : ", get_r_version(), "\n")
cat("║  ├─ Librerías      : ", r_library, check_file(r_library), "\n")
cat("║  └─ Path activo    : ", .libPaths()[1], "\n")

# Quarto
cat("║                                                                              ║\n")
cat("║  📄 QUARTO                                                                   ║\n")
cat("║  ├─ Ruta          : ", quarto_exe, check_file(quarto_exe), "\n")
cat("║  └─ Variable ENV  : QUARTO_PATH = ", Sys.getenv("QUARTO_PATH"), "\n")

# Pandoc
cat("║                                                                              ║\n")
cat("║  📑 PANDOC                                                                   ║\n")
cat("║  ├─ Ruta          : ", pandoc_exe, check_file(pandoc_exe), "\n")
cat("║  └─ Variable ENV  : QUARTO_PANDOC = ", Sys.getenv("QUARTO_PANDOC"), "\n")

# Chrome
cat("║                                                                              ║\n")
cat("║  🌐 CHROME PORTABLE                                                          ║\n")
if (!is.null(chrome_path) && chrome_exists) {
  cat("║  ├─ Ruta          : ", chrome_path, " [✅ EXISTE]\n")
} else {
  cat("║  ├─ Ruta          : No encontrado [❌ NO EXISTE]\n")
}
cat("║  └─ Variable ENV  : CHROMOTE_CHROME = ", Sys.getenv("CHROMOTE_CHROME"), "\n")

# Resumen
cat("║                                                                              ║\n")
cat("╠══════════════════════════════════════════════════════════════════════════════╣\n")
cat("║  ✅ RESUMEN                                                                   ║\n")

total <- 4
ok <- 0
if(quarto_exists) ok <- ok + 1
if(pandoc_exists) ok <- ok + 1
if(chrome_exists) ok <- ok + 1
if(r_library_exists) ok <- ok + 1

cat("║  ├─ Componentes OK : ", ok, "/", total, "\n")

if(ok == total) {
  cat("║  └─ 🎉 Todo está correctamente ubicado.                                    ║\n")
} else {
  cat("║  └─ ⚠️  Revisar rutas de componentes faltantes.                           ║\n")
}

cat("╚══════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================
# 6. LIMPIEZA
# ============================================================

rm(chrome_paths, check_file, find_chrome_portable,
   quarto_exists, pandoc_exists, chrome_exists, r_library_exists,
   chrome_path)
