fn_local_delete_file_SECURITY <- function(file_path, verbose = TRUE) {

  # 1. Normalizar rutas para comparar
  # Usamos tryCatch porque si el archivo no existe, normalizePath puede fallar según la versión
  target_file <- tryCatch(normalizePath(file_path, mustWork = FALSE), error = function(e) return(NULL))
  current_wd  <- normalizePath(getwd(), mustWork = TRUE)

  if (is.null(target_file)) {
    if(verbose) cat("Error: No se pudo procesar la ruta del archivo.\n")
    return(FALSE)
  }

  final_return <- FALSE

  # --- ESCUDO DE SEGURIDAD ---
  # Verificamos que el archivo esté DENTRO del WD y que no sea el WD mismo
  is_inside <- startsWith(target_file, current_wd) && (target_file != current_wd)

  if (!is_inside) {
    cat("!!! ACCESO DENEGADO (SEGURIDAD) !!!\n")
    cat("Intento de borrar un archivo fuera del directorio de trabajo.\n")
    cat("Archivo solicitado:", target_file, "\n")
    cat("Ruta permitida (WD):", current_wd, "\n")
    return(FALSE)
  }
  # ---------------------------

  # Verificar si el archivo existe
  if (!file.exists(target_file)) {
    if(verbose) cat("El archivo no existe o ya fue eliminado:", basename(target_file), "\n")
    return(TRUE) # Retornamos TRUE porque el objetivo (que no esté) se cumple
  }

  # Verificar que no sea una carpeta (por seguridad extra)
  if (dir.exists(target_file)) {
    cat("!!! ERROR !!!: La ruta apunta a una CARPETA, no a un archivo.\n")
    cat("Use la función de limpieza de carpetas para este propósito.\n")
    return(FALSE)
  }

  # Proceder con la eliminación
  check_delete <- file.remove(target_file)

  if (check_delete) {
    if(verbose) cat("Archivo eliminado con éxito:", basename(target_file), "\n")
    final_return <- TRUE
  } else {
    cat("! Advertencia: El archivo no pudo ser eliminado (puede estar en uso).\n")
    final_return <- FALSE
  }

  return(final_return)
}
