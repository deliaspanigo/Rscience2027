fn_get_nested_value_from_list <- function(the_list, key) {
  if (is.list(the_list)) {
    if (key %in% names(the_list)) {
      return(the_list[[key]])
    } else {
      # Si no está en este nivel, busca en los sub-niveles
      for (child in the_list) {
        result <- fn_get_nested_value_from_list(child, key)
        if (!is.null(result)) return(result)
      }
    }
  }
  return(NULL)
}



fn_extract_and_concatenate_code_ORIGINAL <- function(vector_files,
                                            str_output_path = "output_code_extracted.R",
                                            marker_string = "code_internal\\s*=\\s*TRUE") {

  all_extracted_code <- c()

  # Iniciamos el archivo de salida vacío (o lo sobrescribimos)
  cat("", file = str_output_path)

  for (str_file in vector_files) {

    if (!file.exists(str_file)) {
      message("Skipping: Archivo no encontrado -> ", str_file)
      next
    }

    message("Procesando: ", str_file, "...")

    # 1. Leer líneas
    vector_lines <- readLines(str_file, warn = FALSE)

    # 2. Identificar aperturas y cierres
    positions_open_R <- grep("^```\\{r.*\\}\\s*$", vector_lines)
    positions_close_R <- grep("^```\\s*$", vector_lines)

    if (length(positions_open_R) == 0) {
      message("  - Sin chunks de R encontrados.")
      next
    }

    # 3. Emparejar
    matched_closes <- vapply(positions_open_R, function(open_pos) {
      possible_closes <- positions_close_R[positions_close_R > open_pos]
      if (length(possible_closes) > 0) return(possible_closes[1])
      return(NA_real_)
    }, numeric(1))

    df_chunks <- data.frame(start = positions_open_R, end = matched_closes)
    df_chunks <- df_chunks[!is.na(df_chunks$end), ]

    # 4. Filtrar por el marcador
    header_lines <- vector_lines[df_chunks$start]
    keep_indices <- grepl(marker_string, header_lines)
    df_chunks_filtered <- df_chunks[keep_indices, ]

    message("  - Chunks detectados con el marcador: ", nrow(df_chunks_filtered))

    if (nrow(df_chunks_filtered) > 0) {
      # Construir el bloque de este archivo
      file_header <- c(
        paste0("# ", paste(rep("-", 50), collapse = "")),
        paste0("# FROM FILE: ", basename(str_file)),
        paste0("# ", paste(rep("-", 50), collapse = "")),
        ""
      )

      # Extraer bloques
      list_of_blocks <- lapply(1:nrow(df_chunks_filtered), function(i) {
        block_code <- vector_lines[(df_chunks_filtered$start[i] + 1):(df_chunks_filtered$end[i] - 1)]
        block_code <- block_code[!grepl("```", block_code)]
        block_code <- block_code[!grepl("^#\\|", block_code)]
        return(c(block_code, "")) # Espacio entre chunks
      })

      # CONCATENACIÓN CRUCIAL:
      # Sumamos el header y los bloques al acumulador global
      all_extracted_code <- c(all_extracted_code, file_header, unlist(list_of_blocks), "")
    }
  }

  # 6. Guardar el gran acumulador
  if (length(all_extracted_code) > 0) {
    writeLines(all_extracted_code, str_output_path)
    message("\n>>> ÉXITO: Todos los archivos procesados en: ", str_output_path)
  } else {
    message("\n>>> ERROR: No se extrajo nada de ningún archivo.")
  }

  return(str_output_path)
}


fn_extract_and_concatenate_code <- function(vector_files,
                                            str_output_path = "output_code_extracted.R",
                                            marker_string = "code_internal\\s*=\\s*TRUE") {
  library("fs")

  # Iniciamos acumulador y bandera de estado
  all_extracted_code <- c()
  status_ok <- TRUE

  # 1. Validación de seguridad inicial
  if (length(vector_files) == 0) {
    message("Error: El vector de archivos de entrada está vacío.")
    return(FALSE)
  }

  tryCatch({
    # Limpiar/Crear archivo de salida
    cat("", file = str_output_path)

    for (str_file in vector_files) {
      # Usamos fs para normalizar la ruta
      str_file <- fs::path_tidy(str_file)

      if (!fs::file_exists(str_file)) {
        message("Skipping: Archivo no encontrado -> ", str_file)
        status_ok <- FALSE # Marcamos que algo no salió perfecto
        next
      }

      message("Procesando: ", basename(str_file), "...")

      # Leer líneas
      vector_lines <- readLines(str_file, warn = FALSE)

      # Identificar aperturas y cierres de chunks
      positions_open_R <- grep("^```\\{r.*\\}\\s*$", vector_lines)
      positions_close_R <- grep("^```\\s*$", vector_lines)

      if (length(positions_open_R) == 0) next

      # Emparejar chunks
      matched_closes <- vapply(positions_open_R, function(open_pos) {
        possible_closes <- positions_close_R[positions_close_R > open_pos]
        if (length(possible_closes) > 0) return(possible_closes[1])
        return(NA_real_)
      }, numeric(1))

      df_chunks <- data.frame(start = positions_open_R, end = matched_closes)
      df_chunks <- df_chunks[!is.na(df_chunks$end), ]

      # Filtrar por el marcador (ej: code_internal = TRUE)
      header_lines <- vector_lines[df_chunks$start]
      keep_indices <- grepl(marker_string, header_lines)
      df_chunks_filtered <- df_chunks[keep_indices, ]

      if (nrow(df_chunks_filtered) > 0) {
        # Header estético para el archivo .R
        file_header <- c(
          paste0("# ", paste(rep("-", 50), collapse = "")),
          paste0("# FROM FILE: ", basename(str_file)),
          paste0("# ", paste(rep("-", 50), collapse = "")),
          ""
        )

        # Extraer bloques y limpiar sintaxis de Quarto (#|)
        list_of_blocks <- lapply(1:nrow(df_chunks_filtered), function(i) {
          block_code <- vector_lines[(df_chunks_filtered$start[i] + 1):(df_chunks_filtered$end[i] - 1)]
          block_code <- block_code[!grepl("^#\\|", block_code)] # Quitar opciones de chunk
          return(c(block_code, ""))
        })

        all_extracted_code <- c(all_extracted_code, file_header, unlist(list_of_blocks), "")
      }
    }

    # 2. Guardar resultado final
    if (length(all_extracted_code) > 0) {
      writeLines(all_extracted_code, str_output_path)
      message("\n>>> ÉXITO: Código extraído en: ", str_output_path)
      # Si todo se procesó y hay código, devolvemos el estado final
      return(status_ok)
    } else {
      message("\n>>> AVISO: No se encontró código con el marcador especificado.")
      return(FALSE)
    }

  }, error = function(e) {
    message("\n>>> ERROR CRÍTICO durante la extracción: ", e$message)
    return(FALSE)
  })
}


