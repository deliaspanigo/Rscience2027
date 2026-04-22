# setup_rscience.R
# Este script configura las rutas de Rscience antes del renderizado

# 1. Definimos la ruta base a tu carpeta App
# (La mantenemos absoluta por ahora para asegurar que Shiny la encuentre)
base_app <- "C:/Users/Legion/bulk/MyInstallers/Rscience2027_installer/App"

# 2. Construimos las rutas a los binarios
quarto_exe <- file.path(base_app, "Quarto/bin/quarto.exe")
pandoc_dir <- file.path(base_app, "Quarto/bin/tools")
pandoc_exe <- file.path(pandoc_dir, "pandoc.exe")

# 3. Seteamos las variables de entorno de sistema
# Esto es lo que leerá el proceso de Quarto y Pandoc al arrancar
Sys.setenv(QUARTO_PATH = quarto_exe)
Sys.setenv(QUARTO_PANDOC = pandoc_exe)
Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)

# Mensaje de confirmación en la consola
cat("\n[Rscience Setup] Entorno configurado correctamente.")
cat("\n[Rscience Setup] Usando Quarto en:", quarto_exe, "\n")
