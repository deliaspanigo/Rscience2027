fn_load_env_RData_hidden <- function(path_rdata){
  # 1. Definir la ruta del archivo

  # 2. Crear un entorno temporal
  temp_env <- new.env()

  # 3. Cargar el archivo dentro de ese entorno específico
  load(path_rdata, envir = temp_env)

  # 4. Convertir el entorno a una lista
  list_obj_script01 <- as.list(temp_env)

  # (Opcional) Eliminar el entorno temporal para liberar memoria
  rm(temp_env)

  return(list_obj_script01)
}
