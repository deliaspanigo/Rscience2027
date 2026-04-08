fn_local_clean_folder_SECURITY <- function(folder_path, verbose = TRUE) {

  # 1. Normalizar rutas para comparar manzanas con manzanas
  target_path <- normalizePath(folder_path, mustWork = TRUE)
  current_wd  <- normalizePath(getwd(), mustWork = TRUE)

  final_return <- FALSE

  # --- ESCUDO DE SEGURIDAD ---
  # Verificamos si target_path contiene al inicio la ruta del getwd()
  # Además, nos aseguramos de que no sean exactamente iguales (para no borrar el propio WD)
  is_subfolder <- startsWith(target_path, current_wd) && (target_path != current_wd)

  if (!is_subfolder) {
    cat("!!! ACCESO DENEGADO !!!\n")
    cat("Intento de borrar fuera del directorio de trabajo o en la raíz del mismo.\n")
    cat("Path solicitado:", target_path, "\n")
    cat("Path permitido (WD):", current_wd, "\n")
    return(FALSE) # Se cancela la función inmediatamente
  } else {
    if(verbose) cat("Seguridad validada: La carpeta es una subdirectorio del proyecto.\n")
  }
  # ---------------------------

  check_folder <- dir.exists(target_path)

  if(!check_folder){
    cat("La carpeta no existe! PROBLEMS!!!\n")
    return(FALSE)
  }

  if(check_folder){
    vector_local_files <- list.files(path = target_path, full.names = TRUE, recursive = FALSE)
    total_files <- length(vector_local_files)

    if (total_files == 0) {
      if(verbose) cat("La carpeta está vacía!.\n")
      final_return <- TRUE
    } else {
      if(verbose) cat("La carpeta contiene", total_files, "archivos.\n Eliminando...\n")

      check_delete_each <- file.remove(vector_local_files)
      check_delete_all  <- all(check_delete_each)

      if(check_delete_all){
        if(verbose) cat("Limpieza finalizada con éxito.\n")

        # Verificación recursiva (llamando a la misma función)
        # Nota: asegúrate de que el nombre sea consistente con la definición
        doble_check <- fn_local_clean_folder_SECURITY(target_path, verbose = FALSE)
        if(doble_check) final_return <- TRUE
      } else {
        cat("! Advertencia: Algunos archivos no pudieron ser eliminados.\n")
      }
    }
  }

  return(final_return)
}
