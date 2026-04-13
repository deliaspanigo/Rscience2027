#' Buscar path de módulo de forma recursiva
#' @param root_path Carpeta base donde empezar a buscar (ej. local_folder_path)
#' @param file_name Nombre exacto del archivo .R
fn_obtanin_file_path <- function(root_path, file_name) {

  print(root_path)
  # Buscamos el archivo de forma recursiva
  result <- list.files(
    path       = root_path,
    pattern    = paste0("^", file_name, "$"), # Buscamos coincidencia exacta
    recursive  = TRUE,
    full.names = TRUE
  )

  print(result)
  # Validamos si encontró algo
  if (length(result) == 0) {
    warning(paste("Archivo no encontrado:", file_name))
    return(NA)
  }

  # Si hay más de uno (por error), avisamos y tomamos el primero
  if (length(result) > 1) {
    message(paste("Ojo: Se encontró más de un archivo con el nombre", file_name, "- Usando el primero."))
  }

  return(result[1])
}
