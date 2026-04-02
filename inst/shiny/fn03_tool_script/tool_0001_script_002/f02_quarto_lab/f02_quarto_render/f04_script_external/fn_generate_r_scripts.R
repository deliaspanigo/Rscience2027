# ================================================
# FUNCIÓN PARA COPIAR CONTENIDO .qmd → .R
# ================================================

fn_generate_r_scripts <- function() {

  # Ruta de los archivos .qmd
  folder_qmd <- file.path("..", "f02_quarto_mod")

  # Listar todos los archivos .qmd
  list_path_qmd <- list.files(
    path = folder_qmd,
    pattern = "\\.qmd$",
    full.names = TRUE
  )

  if (length(list_path_qmd) == 0) {
    stop("No se encontraron archivos .qmd en la carpeta: ", folder_qmd)
  }

  print(list_path_qmd)

  # Crear carpeta de salida
  dir_output <- file.path("output_script")
  if (!dir.exists(dir_output)) {
    dir.create(dir_output, recursive = TRUE)
  }

  cat("Iniciando copia de", length(list_path_qmd), "archivos .qmd a .R...\n\n")

  for (qmd_file in list_path_qmd) {

    # Nombre del archivo .R de salida
    r_file <- file.path(
      dir_output,
      gsub("\\.qmd$", ".R", basename(qmd_file))
    )

    tryCatch({
      # Leer todo el contenido del archivo .qmd
      content <- readLines(qmd_file, warn = FALSE, encoding = "UTF-8")

      # Escribir todo el contenido en el nuevo archivo .R
      writeLines(content, con = r_file, useBytes = TRUE)

      cat("✓ Éxito:", basename(r_file), "\n")

    }, error = function(e) {
      cat("✗ Error en", basename(qmd_file), "→", e$message, "\n")
    })
  }

  cat("\n¡Proceso terminado!\n")
  cat("Archivos .R guardados en:\n", normalizePath(dir_output), "\n")
}


