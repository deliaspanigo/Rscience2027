fn_local_create_new_folder <- function(new_folder_path) {

  # 1. Normalizar la ruta (solo la primera vez para evitar cálculos extra)
  new_folder_path <- normalizePath(new_folder_path, mustWork = FALSE)
  check_return <- FALSE

  # --- CASO BASE (Condición de salida) ---
  check01 <- dir.exists(new_folder_path)
  if (check01) {
    # Si ya existe, devolvemos TRUE y cortamos la recursión
    check_return <- TRUE
    cat("Confirmed, folder exists!")
  }

  # --- ACCIÓN ---
  # Si llegamos aquí, es que NO existe. Intentamos crearla.
  if(!check01){
    dir.create(new_folder_path, recursive = TRUE)
    cat("Folder created!")

    # --- LLAMADA RECURSIVA ---
    # Nos llamamos a nosotros mismos para verificar el resultado.
    # Pasamos recursive_call = TRUE para saber que ya normalizamos la ruta.
    check02 <- fn_local_create_new_folder(new_folder_path)
    if(check02) check_return <- TRUE else cat("PROBLEM!")
  }

  return(check_return)
}
