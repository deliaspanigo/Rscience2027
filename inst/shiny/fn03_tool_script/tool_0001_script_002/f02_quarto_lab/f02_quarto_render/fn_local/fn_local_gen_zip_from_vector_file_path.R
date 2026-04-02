
fn_local_gen_zip_from_vector_file_path <- function(vector_file_path, str_output_zip_file_path) {

  # Asegurarnos de que la ruta de entrada sea absoluta
  vector_file_path <- normalizePath(vector_file_path, mustWork = TRUE)
  str_output_zip_file_path <- normalizePath(str_output_zip_file_path, mustWork = FALSE)

  # zip_pre_exists <- file.exists(str_output_zip_file_path)
  # if(zip_pre_exists) unlink(str_output_zip_file_path)

  if (length(vector_file_path) == 0) {
    warning("No hay detallados archivos para comprimir.")
    return(NULL)
  }

  # Output folder
  zip_folder_path <- dirname(str_output_zip_file_path)
  zip_file_name   <- basename(str_output_zip_file_path)

  # 2. Crear carpeta de salida 'output' si no existe (en el directorio actual)
  if (!dir.exists(zip_folder_path)) {
    dir.create(zip_folder_path, recursive = TRUE)
  }

  cat("Comprimiendo", length(vector_file_path), "archivos en:", zip_file_name, "\n")

  # 3. Crear el archivo ZIP
  # Usamos zip() de la base de R o del paquete zip si lo tienes
  # zip(archivo_destino, archivos_a_incluir, extras = '-j' para no incluir carpetas)
  utils::zip(
    zipfile = str_output_zip_file_path,
    files = vector_file_path,
    extras = "-j" # El flag '-j' (junk paths) guarda solo los archivos, sin la estructura de carpetas
  )

  cat("¡ZIP generado con éxito!\n")
}
