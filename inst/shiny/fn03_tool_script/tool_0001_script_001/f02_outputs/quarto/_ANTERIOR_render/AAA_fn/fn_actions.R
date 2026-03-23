
# Single Files
fn_delete_single_file <- function(str_file_path) {
  library("fs")

  # 1. MEGA-PROTECTION: Catch malformed inputs before anything else
  init_check <- tryCatch({
    if (is.null(str_file_path) || length(str_file_path) != 1 || str_file_path == "") {
      stop("Invalid input: Path must be a single non-empty string.")
    }

    # 2. SECURITY CHECK: No parent directory navigation allowed
    if (base::grepl("\\.\\./", str_file_path) || base::grepl("\\./", str_file_path)) {
      stop("Security Alert: Relative path navigation ('../' or './') is forbidden.")
    }

    # 3. SECURITY CHECK: Must be inside Working Directory
    abs_target_path <- fs::path_abs(str_file_path)
    abs_work_dir    <- fs::path_abs(base::getwd())

    if (!base::startsWith(as.character(abs_target_path), as.character(abs_work_dir))) {
      stop("Security Alert: Target file is outside the Working Directory.")
    }

    # Tidy path early to catch encoding or syntax errors
    str_file_path <- fs::path_tidy(str_file_path)
    TRUE
  }, error = function(e) {
    cat("<span style='color: #dc3545;'>✘ <b>Critical Security/Input Error:</b> ", e$message, "</span><br><br>")
    return(FALSE)
  })

  if (isFALSE(init_check)) return(FALSE)

  # Standardize for the rest of the function
  str_file_path <- fs::path_tidy(str_file_path)
  str_file_name <- fs::path_file(str_file_path)

  cat(paste0("<b>Processing single file:</b> <code>", str_file_path, "</code><br>"))
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Delete single file within Sandbox.</small><br>"))

  # Check if it's actually a directory instead of a file
  if (fs::dir_exists(str_file_path)) {
    cat("<span style='color: #fd7e14;'>⚠ <b>Warning:</b> Target is a directory, not a file. Skipping.</span><br><br>")
    return(FALSE)
  }

  if (fs::file_exists(str_file_path)) {
    check_delete <- tryCatch({
      fs::file_delete(str_file_path)

      if (!fs::file_exists(str_file_path)) {
        cat("<span style='color: #28a745;'>✔ <b>Success:</b> File deleted and verified within Sandbox.</span><br><br>")
        TRUE
      } else {
        cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Delete command issued, but file still exists.</span><br><br>")
        FALSE
      }
    }, error = function(e) {
      cat("<span style='color: #dc3545;'>✘ <b>Error:</b> File could not be deleted (it might be in use or protected).</span><br>")
      cat(paste0("<small style='color: #b02a37;'>Technical Detail: ", e$message, "</small><br><br>"))
      FALSE
    })
    return(check_delete)

  } else {
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> File does not exist. Clean state confirmed.</span><br><br>")
    return(TRUE)
  }
}   # Upgraded!


fn_check_single_file <- function(str_file_path) {
  library("fs")

  # 1. INITIAL PROTECTION: Basic Input Validation
  if (base::is.null(str_file_path) || base::length(str_file_path) != 1 || str_file_path == "") {
    base::cat("<span style='color: #dc3545;'>✘ <b>Critical Error:</b> Provided file path is NULL or empty.</span><br><br>")
    return(FALSE)
  }

  # 2. PATH NORMALIZATION
  str_file_path_tidy <- fs::path_tidy(str_file_path)
  abs_target_path    <- fs::path_abs(str_file_path)

  # 3. STANDARDIZED HEADER
  base::cat(base::paste0("<b>Verifying file:</b> <code>", str_file_path_tidy, "</code><br>"))
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Existence, type, and metadata audit.</small><br>"))

  # 4. VERIFICATION LOGIC
  # Check existence AND ensure it is a file, not a directory
  if (fs::file_exists(str_file_path) && !fs::dir_exists(str_file_path)) {

    # Audit details
    audit_details <- base::tryCatch({
      f_info   <- fs::file_info(str_file_path)
      f_size   <- base::format(f_info$size, units = "auto")
      f_mod    <- base::format(f_info$modification_time, "%Y-%m-%d %H:%M:%S")
      f_perms  <- f_info$permissions
      base::paste0("Size: ", f_size, " | Modified: ", f_mod, " | Perms: ", f_perms)
    }, error = function(e) "Metadata access restricted.")

    # SUCCESS CASE
    base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> The file exists and is accessible.</span><br>")
    base::cat(base::paste0("<small style='color: #6c757d;'><b>Audit details:</b> ", audit_details, "</small><br><br>"))

    return(TRUE)

  } else {
    # ERROR CASE (Does not exist or is a directory)
    is_dir <- fs::dir_exists(str_file_path)
    err_msg <- if (is_dir) "Target is a directory, not a file." else "File not found."

    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Error:</b> ", err_msg, "</span><br>"))

    # Stylized Error Block
    base::cat(base::paste0(
      "<div style='background-color: #fff5f5; border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0;'>",
      "<small style='color: #b02a37;'><b>Target Path Attempted:</b> <code>", abs_target_path, "</code></small><br>",
      "<small style='color: #6c757d;'><b>Note:</b> Verify the file extension and that the source path is correctly mounted.</small>",
      "</div><br>"
    ))
    return(FALSE)
  }
}  # Upgraded!

fn_rename_single_file <- function(str_file_path_from, str_file_path_to) {
  library("fs")

  # 1. PATH NORMALIZATION
  str_file_path_from <- fs::path_tidy(str_file_path_from)
  str_file_path_to   <- fs::path_tidy(str_file_path_to)

  # 2. SECURITY CHECK: Sandbox Enforcement (Solo dentro del proyecto)
  abs_path_to   <- fs::path_abs(str_file_path_to)
  abs_work_dir  <- fs::path_abs(base::getwd())

  if (!base::startsWith(as.character(abs_path_to), as.character(abs_work_dir))) {
    base::cat("<span style='color: #dc3545;'>✘ <b>Security Alert:</b> Target path is outside the Sandbox. Rename operation blocked.</span><br><br>")
    return(FALSE)
  }

  # 3. HEADER & ACTION DETAIL
  base::cat(base::paste0("<b>Renaming & Relocating File</b><br>"))
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Safe move within Sandbox audit.</small><br>"))
  base::cat(base::paste0("<small style='color: #6c757d;'><b>FROM:</b> <code>", str_file_path_from, "</code></small><br>"))
  base::cat(base::paste0("<small style='color: #6c757d;'><b>TO:</b>&nbsp;&nbsp;&nbsp;<code>", str_file_path_to, "</code></small><br>"))

  # 4. EXECUTION LOGIC
  check_rename <- FALSE
  audit_details <- "No metadata available"

  if (fs::file_exists(str_file_path_from)) {
    base::tryCatch({
      # Asegurar que la carpeta destino existe (dentro del sandbox)
      dst_dir <- fs::path_dir(str_file_path_to)
      if (!fs::dir_exists(dst_dir)) fs::dir_create(dst_dir, recurse = TRUE)

      # Ejecutar el movimiento/renombrado
      fs::file_move(path = str_file_path_from, new_path = str_file_path_to)

      # Verificación e integridad
      if (fs::file_exists(str_file_path_to)) {
        check_rename <- TRUE
        f_info <- fs::file_info(str_file_path_to)
        audit_details <- base::paste0("Size: ", base::format(f_info$size, units = "auto"),
                                      " | New Path Verified: YES")
      }
    }, error = function(e) {
      audit_details <- base::paste0("Error during move: ", e$message)
    })
  } else {
    audit_details <- "Source file not found."
  }

  # 5. VISUAL OUTPUT & VERDICT
  if (check_rename) {
    base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> File renamed and verified.</span><br>")
    base::cat(base::paste0("<small style='color: #6c757d;'><b>Audit:</b> ", audit_details, "</small><br><br>"))
    return(TRUE)
  } else {
    base::cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Could not rename file.</span><br>")

    # Error Audit Block
    base::cat(base::paste0(
      "<div style='background-color: #fff5f5; border-left: 3px solid #dc3545; padding: 10px; margin: 10px 0;'>",
      "<small style='color: #b02a37;'><b>Technical Detail:</b> ", audit_details, "</small><br>",
      "<small style='color: #6c757d;'><b>Note:</b> Ensure the source file isn't open in another program.</small>",
      "</div><br>"
    ))
    return(FALSE)
  }
} # Upgraded! - No usada

fn_copy_verify_single_file <- function(str_file_path_from, str_file_path_to) {
  library("fs")

  # 1. Path normalization
  str_from_path <- fs::path_tidy(str_file_path_from)
  str_to_path   <- fs::path_tidy(str_file_path_to)
  file_name <- fs::path_file(str_from_path)

  # 2. Standardized Header
  base::cat(base::paste0("<b>Copying & Verifying:</b> <code>", file_name, "</code><br>"))
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Individual file transfer and metadata integrity check.</small><br>"))
  base::cat(base::paste0("<small style='color: #6c757d;'><b>FROM:</b>&nbsp;<code>", str_from_path, "</code></small><br>"))
  base::cat(base::paste0("<small style='color: #6c757d;'><b>TO:</b>&nbsp;&nbsp;&nbsp;<code>", str_to_path, "</code></small><br>"))

  # 3. Execution Logic with Metadata capture
  copy_status <- FALSE
  audit_info  <- "No data"

  if (fs::file_exists(str_from_path)) {
    base::tryCatch({
      # Asegurar que la carpeta destino existe (especialmente para rutas externas)
      dst_dir <- fs::path_dir(str_to_path)
      if (!fs::dir_exists(dst_dir)) fs::dir_create(dst_dir, recurse = TRUE)

      # Ejecutar copia
      fs::file_copy(str_from_path, str_to_path, overwrite = TRUE)

      # Verificar y extraer metadatos finales
      if (fs::file_exists(str_to_path)) {
        copy_status <- TRUE
        f_info      <- fs::file_info(str_to_path)
        f_size      <- base::format(f_info$size, units = "auto")
        f_mod       <- base::format(f_info$modification_time, "%Y-%m-%d %H:%M:%S")
        audit_info  <- base::paste0("Verified Size: ", f_size, " | Timestamp: ", f_mod)
      }
    }, error = function(e) {
      audit_info <- base::paste0("Error: ", e$message)
    })
  } else {
    audit_info <- "Source file not found at origin."
  }

  # 4. Visual Audit (Badges)
  status_from <- base::ifelse(fs::file_exists(str_from_path),
                              "<span style='background:#28a745; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>ORIGIN_OK</span>",
                              "<span style='background:#dc3545; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>ORIGIN_MISSING</span>")

  status_to <- base::ifelse(copy_status,
                            "<span style='background:#28a745; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>COPIED_OK</span>",
                            "<span style='background:#dc3545; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>COPY_FAILED</span>")

  base::cat(base::paste0("<div style='margin-top:8px; margin-bottom:5px;'>", status_from, " &nbsp; ⮕ &nbsp; ", status_to, "</div>"))
  base::cat(base::paste0("<small style='color: #6c757d;'><b>Audit:</b> ", audit_info, "</small><br>"))

  # 5. Final Verdict
  if (copy_status) {
    base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> File synchronized and integrity verified.</span><br><br>")
    return(TRUE)
  } else {
    base::cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Copy failed. Check permissions or network path.</span><br><br>")
    return(FALSE)
  }
}  # Upgraded!
################################################################################



# Single folders
fn_check_single_folder <- function(str_folder_path) {
  library("fs")

  # 1. INITIAL PROTECTION: Basic Input Validation
  if (base::is.null(str_folder_path) || base::length(str_folder_path) != 1 || str_folder_path == "") {
    base::cat("<span style='color: #dc3545;'>✘ <b>Critical Error:</b> Provided folder path is NULL or empty.</span><br><br>")
    return(FALSE)
  }

  # 2. PATH NORMALIZATION
  # Standardize slashes and resolve to absolute path for the report
  str_folder_path_tidy <- fs::path_tidy(str_folder_path)
  abs_target_path      <- fs::path_abs(str_folder_path)

  # 3. STANDARDIZED HEADER
  base::cat(base::paste0("<b>Verifying folder:</b> <code>", str_folder_path_tidy, "</code><br>"))
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Existence and accessibility audit (External & Internal).</small><br>"))

  # 4. VERIFICATION LOGIC
  if (fs::dir_exists(str_folder_path)) {

    # Audit details (Using try to avoid crashing if folder is restricted)
    audit_details <- base::tryCatch({
      # Counting files and subdirectories
      file_count <- base::length(fs::dir_ls(str_folder_path, fail = FALSE))
      # Getting file system permissions
      folder_perms <- fs::file_info(str_folder_path)$permissions
      base::paste0("Contains ", file_count, " items | Permissions: ", folder_perms)
    }, error = function(e) "Access restricted for deep metadata audit")

    # SUCCESS CASE
    base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> Directory exists and is accessible.</span><br>")
    base::cat(base::paste0("<small style='color: #6c757d;'><b>Audit details:</b> ", audit_details, "</small><br>"))
    # base::cat(base::paste0("<small style='color: #6c757d;'><b>Full Path:</b> <code>", abs_target_path, "</code></small><br><br>"))

    return(TRUE)

  } else {
    # ERROR CASE
    base::cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Directory not found or inaccessible.</span><br>")

    # Stylized Error Block
    base::cat(base::paste0(
      "<div style='background-color: #fff5f5; border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0;'>",
      "<small style='color: #b02a37;'><b>Target Path Attempted:</b> <code>", abs_target_path, "</code></small><br>",
      "<small style='color: #6c757d;'><b>System Note:</b> Check if the drive is mounted or network permissions are correct.</small>",
      "</div><br>"
    ))
    return(FALSE)
  }
} # Upgraded!

fn_list_files_from_single_folder <- function(str_folder_path) {
  library("fs")
  library("dplyr")
  library("knitr")
  library("kableExtra")

  # 1. Basic Input Validation
  if (base::is.null(str_folder_path) || base::length(str_folder_path) != 1 || str_folder_path == "") {
    base::cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Cannot list files. Path is NULL or empty.</span><br><br>")
    return(FALSE)
  }

  str_folder_path <- fs::path_tidy(str_folder_path)

  # 2. Check if folder exists before listing
  if (!fs::dir_exists(str_folder_path)) {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Error:</b> Cannot list files. Directory does not exist:</span> <code>", str_folder_path, "</code><br><br>"))
    return(FALSE)
  }

  # 3. Header
  base::cat(base::paste0("<b>Folder Content Explorer:</b> <code>", str_folder_path, "</code><br>"))

  # 4. Get Data and Count
  df_files <- base::tryCatch({
    info <- fs::dir_info(str_folder_path, all = TRUE)

    total_items <- base::nrow(info)

    # Message about item count
    if (total_items > 0) {
      base::cat(base::paste0("<small style='color: #333;'>Items found in directory: <b>", total_items, "</b></small><br><br>"))
    }

    if (total_items == 0) {
      NULL
    } else {
      info %>%
        dplyr::mutate(
          File = fs::path_file(path),
          Size = base::format(size, units = "auto"),
          Modified = base::format(modification_time, "%Y-%m-%d %H:%M:%S"),
          Type = fs::file_info(path)$type
        ) %>%
        dplyr::arrange(dplyr::desc(Type), File) %>%
        dplyr::mutate(`#` = dplyr::row_number()) %>% # Numeración de archivos
        dplyr::select(`#`, File, Type, Size, Modified)
    }
  }, error = function(e) {
    base::cat(base::paste0("<small style='color: #dc3545;'>Technical error accessing files: ", e$message, "</small><br>"))
    return(NULL)
  })

  # 5. Visual Output
  if (base::is.null(df_files)) {
    base::cat("<span style='color: #6c757d;'>ℹ <i>The directory is empty.</i></span><br><br>")
    return(TRUE)
  }

  # Generate Table
  html_table <- df_files %>%
    knitr::kable(format = "html", escape = FALSE, align = "cllll") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE,
                              position = "left") %>%
    kableExtra::column_spec(1, color = "#6c757d", width = "40px") %>% # Estilo para el índice
    kableExtra::column_spec(2, bold = TRUE, width = "300px") %>%
    kableExtra::column_spec(3, color = "#6c757d", width = "100px") %>%
    kableExtra::column_spec(4, width = "100px") %>%
    kableExtra::column_spec(5, width = "150px", color = "#6c757d")

  base::cat(html_table)
  base::cat("<br>")

  return(TRUE)
}# Upgraded!


fn_check_and_create_and_clean_single_folder <- function(str_folder_path) {
  library("fs")

  # 1. Basic Input Validation
  if (base::is.null(str_folder_path) || base::length(str_folder_path) != 1 || str_folder_path == "") {
    base::cat("<span style='color: #dc3545;'>✘ <b>Critical Error:</b> Invalid folder path provided.</span><br><br>")
    return(FALSE)
  }

  # 2. SECURITY CHECK: No parent directory navigation allowed
  if (base::grepl("\\.\\./", str_folder_path) || base::grepl("\\./", str_folder_path)) {
    base::cat("<span style='color: #dc3545;'>✘ <b>Security Alert:</b> Relative path navigation ('../' or './') is forbidden.</span><br><br>")
    return(FALSE)
  }

  # 3. SECURITY CHECK: Must be inside Working Directory
  abs_target_path <- fs::path_abs(str_folder_path)
  abs_work_dir    <- fs::path_abs(base::getwd())

  if (!base::startsWith(as.character(abs_target_path), as.character(abs_work_dir))) {
    base::cat("<span style='color: #dc3545;'>✘ <b>Security Alert:</b> Attempted to access a folder outside the Working Directory. Operation blocked.</span><br><br>")
    return(FALSE)
  }

  # Standardize path for logging
  str_folder_path <- fs::path_tidy(str_folder_path)

  # HTML Header for Quarto Output
  base::cat(base::paste0("<b>Managing folder lifecycle:</b> <code>", str_folder_path, "</code><br>"))
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Ensure existence, empty content, and audit metadata.</small><br>"))

  # Execute logic
  result <- base::tryCatch({

    if (fs::dir_exists(str_folder_path)) {
      # --- CASE A: EXISTS -> EMPTY CONTENT ---
      base::cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> Directory exists. Cleaning content...</span><br>")

      content <- fs::dir_ls(str_folder_path, all = TRUE)

      if (base::length(content) > 0) {
        fs::file_delete(content)
        base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> Directory emptied.</span><br>")
      } else {
        base::cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> Directory was already empty.</span><br>")
      }

    } else {
      # --- CASE B: MISSING -> CREATE ---
      base::cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> Directory not found. Creating...</span><br>")
      fs::dir_create(str_folder_path, recurse = TRUE)
      base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> Directory created.</span><br>")
    }

    # --- FINAL VERIFICATION & METADATA AUDIT ---
    if (fs::dir_exists(str_folder_path)) {
      # Extracting metadata
      folder_info <- fs::file_info(str_folder_path)
      folder_perms <- folder_info$permissions
      file_count <- base::length(fs::dir_ls(str_folder_path, all = TRUE))

      base::cat("<span style='color: #28a745;'>✔ <b>Verification:</b> Sandbox path is ready.</span><br>")
      base::cat(base::paste0("<small style='color: #6c757d;'><b>Audit:</b> Permissions: <code>", folder_perms,
                             "</code> | Final Item Count: <b>", file_count, "</b></small><br><br>"))
      TRUE
    } else {
      base::stop("Post-condition failed: Directory not detected after operation.")
    }

  }, error = function(e) {
    base::cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Operation failed.</span><br>")
    base::cat(base::paste0("<small style='color: #b02a37;'>Technical Detail: ", e$message, "</small><br><br>"))
    FALSE
  })

  return(result)
} # Upgraded!

fn_check_and_create_and_clean_single_folder_SUPER_POWER <- function(str_folder_path) {
  # 1. Security Configuration
  # Usamos un regex para permitir cualquiera de las dos palabras clave
  str_security_regex <- "zzz_zzz_output|f00_03_MOD_code"

  # 2. Basic Input Validation
  if (base::is.null(str_folder_path) || base::length(str_folder_path) != 1 || str_folder_path == "") {
    base::cat("<span style='color: #dc3545;'>✘ <b>Critical Error:</b> Invalid folder path.</span><br><br>")
    return(FALSE)
  }

  str_folder_path_tidy <- fs::path_tidy(str_folder_path)

  # 3. SECURITY CHECK
  if (!base::grepl(str_security_regex, str_folder_path_tidy)) {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Security Alert:</b> Unauthorized path.</span><br>"))
    base::cat(base::paste0("<small style='color: #6c757d;'>Path must contain authorized keywords.</small><br><br>"))
    return(FALSE)
  }

  base::cat(base::paste0("<b>Managing folder lifecycle:</b> <code>", str_folder_path_tidy, "</code><br>"))

  # 4. Execution Logic
  result <- base::tryCatch({
    if (fs::dir_exists(str_folder_path_tidy)) {
      base::cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> Path exists. Purging content...</span><br>")

      # Borrar todo el contenido (archivos y carpetas) de forma recursiva
      content <- fs::dir_ls(str_folder_path_tidy, all = TRUE)
      if (base::length(content) > 0) {
        # 'force = TRUE' ayuda con archivos protegidos si el SO lo permite
        fs::dir_delete(content)
        base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> Content purged.</span><br>")
      }
    } else {
      base::cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> Path missing. Creating...</span><br>")
      fs::dir_create(str_folder_path_tidy, recurse = TRUE)
    }

    # Final Audit
    folder_info <- fs::file_info(str_folder_path_tidy)
    base::cat(base::paste0("<small style='color: #6c757d;'><b>Audit:</b> Perms: <code>", folder_info$permissions,
                           "</code> | Status: <b>Ready</b></small><br><br>"))
    TRUE

  }, error = function(e) {
    base::cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Operation failed.</span><br>")
    base::cat(base::paste0("<small style='color: #b02a37;'>Detail: ", e$message, "</small><br><br>"))
    FALSE
  })

  return(result)
}

fn_cleaning_single_folder <- function(str_folder_path) {
  library("fs")

  # 1. PATH NORMALIZATION
  str_folder_target <- fs::path_tidy(str_folder_path)

  # 2. SECURITY CHECK: Sandbox Enforcement
  # Solo permite borrar contenido si la carpeta está dentro del Working Directory
  abs_target_path <- fs::path_abs(str_folder_target)
  abs_work_dir    <- fs::path_abs(base::getwd())

  if (!base::startsWith(as.character(abs_target_path), as.character(abs_work_dir))) {
    base::cat("<span style='color: #dc3545;'>✘ <b>Security Alert:</b> Attempted to purge a folder outside the Sandbox. Operation blocked.</span><br><br>")
    return(FALSE)
  }

  # 3. STANDARDIZED HEADER
  base::cat(base::paste0("<b>Purging folder content:</b> <code>", str_folder_target, "</code><br>"))
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Internal Sandbox cleanup (Recursive).</small><br>"))

  # 4. EXECUTION LOGIC
  if (fs::dir_exists(str_folder_target)) {

    # Listamos todo el contenido (all = TRUE incluye archivos ocultos como .gitignore)
    content_to_delete <- fs::dir_ls(str_folder_target, all = TRUE)
    total_items <- base::length(content_to_delete)

    if (total_items > 0) {
      check_purge <- base::tryCatch({

        # Ejecución del borrado
        # Usamos dir_delete para carpetas y file_delete para archivos
        # fs maneja vectores, lo cual es más eficiente
        fs::dir_delete(content_to_delete[fs::is_dir(content_to_delete)])
        fs::file_delete(content_to_delete[fs::is_file(content_to_delete)])

        # Reconfirmación post-borrado (Auditoría final)
        remaining_content <- fs::dir_ls(str_folder_target, all = TRUE)
        remaining_count   <- base::length(remaining_content)

        if (remaining_count == 0) {
          base::cat(base::paste0("<span style='color: #28a745;'>✔ <b>Success:</b> All content removed (", total_items, " items deleted).</span><br>"))
          base::cat("<small style='color: #6c757d;'><b>Audit:</b> Folder integrity verified (0 items remaining).</small><br><br>")
          TRUE
        } else {
          base::cat(base::paste0("<span style='color: #ffc107;'>⚠ <b>Warning:</b> Purge incomplete. ", remaining_count, " items still remain.</span><br>"))
          base::cat("<small style='color: #6c757d;'><b>Note:</b> Some files might be locked by another process.</small><br><br>")
          FALSE
        }

      }, error = function(e) {
        base::cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Critical failure during purge.</span><br>")
        base::cat(base::paste0("<div style='background-color: #fff5f5; border-left: 3px solid #dc3545; padding: 8px; margin-top: 5px;'>",
                               "<small style='color: #b02a37;'><b>Technical Detail:</b> ", e$message, "</small></div><br>"))
        FALSE
      })

      return(check_purge)

    } else {
      base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> Folder is already empty.</span><br><br>")
      return(TRUE)
    }

  } else {
    base::cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Target directory does not exist. Verify path.</span><br><br>")
    return(FALSE)
  }
} # Upgraded! - No usada

################################################################################


# Multi files
fn_check_multi_files <- function(vector_file_path) {
  library("fs")
  library("knitr")
  library("kableExtra")
  library("dplyr")

  # 1. Validación inicial
  if (base::length(vector_file_path) == 0) {
    base::cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> No paths provided for checking.</span><br><br>")
    return(TRUE)
  }

  # Normalización de rutas
  vector_paths <- fs::path_tidy(vector_file_path)

  # 2. Encabezado del proceso
  base::cat("<b>Batch Existence & Metadata Audit:</b><br>")
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Verifying ", base::length(vector_paths), " items across global paths.</small><br><br>"))

  # 3. Construcción de Dataframe de Auditoría
  results_list <- base::lapply(vector_paths, function(path) {
    exists <- fs::file_exists(path)

    if (exists) {
      info <- fs::file_info(path)
      status <- "EXISTS"
      size   <- base::format(info$size, units = "auto")
      mtime  <- base::format(info$modification_time, "%Y-%m-%d %H:%M:%S")
    } else {
      status <- "MISSING"
      size   <- "---"
      mtime  <- "---"
    }

    base::data.frame(
      File     = fs::path_file(path),
      Status   = status,
      Size     = size,
      Modified = mtime,
      Path     = path,
      stringsAsFactors = FALSE
    )
  })

  df_results <- base::do.call(base::rbind, results_list) %>%
    dplyr::mutate(`#` = dplyr::row_number())

  # 4. Generar Tabla Estilizada
  tabla_html <- df_results %>%
    dplyr::mutate(
      Status = kableExtra::cell_spec(Status, color = "white", bold = TRUE,
                                     background = base::ifelse(Status == "EXISTS", "#28a745", "#dc3545"))
    ) %>%
    dplyr::select(`#`, File, Status, Size, Modified, Path) %>%
    knitr::kable(format = "html", escape = FALSE, align = "clllll") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = F, position = "left") %>%
    kableExtra::column_spec(1, color = "#6c757d", width = "40px") %>%
    kableExtra::column_spec(2, bold = TRUE, width = "250px") %>%
    kableExtra::column_spec(4:5, width = "120px", color = "#6c757d") %>%
    kableExtra::column_spec(6, color = "#6c757d", extra_css = "font-size: 9px; opacity: 0.7;")

  # Imprimir la tabla
  base::cat(tabla_html)
  base::cat("<br>")

  # 5. Resumen final con veredicto
  total_files  <- base::length(vector_paths)
  total_exists <- base::sum(df_results$Status == "EXISTS")
  total_missing <- total_files - total_exists

  if (total_missing == 0) {
    base::cat(base::paste0("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> All files verified and present (", total_exists, "/", total_files, ").</span><br><br>"))
    return(TRUE)
  } else {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Audit failed! ", total_missing, " item(s) are missing.</span><br><br>"))
    return(FALSE)
  }
} # Upgraded! - No usada



fn_delete_multi_files <- function(vector_file_paths) {
  library("fs")
  library("knitr")
  library("kableExtra")
  library("dplyr")

  # 1. Guard Clause: Check if the vector has any content
  if (base::is.null(vector_file_paths) || base::length(vector_file_paths) == 0) {
    base::cat("<b>Batch Purge Process:</b><br>")
    base::cat("<small style='color: #6c757d;'>Files processed: 0</small><br>")
    base::cat("<span style='color: #28a745;'>✔ <b>Clean:</b> No files provided for deletion. Everything is in order.</span><br><br>")
    return(TRUE)
  }

  # Report Header
  base::cat("<b>Batch Purge Process:</b><br>")
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Attempting to purge ", base::length(vector_file_paths), " items within Sandbox.</small><br>"))

  # 2. Security & Execution Loop
  abs_work_dir <- fs::path_abs(base::getwd())

  results_list <- base::lapply(vector_file_paths, function(path) {

    status_item <- "UNKNOWN"
    detail_msg  <- "Ready"

    # --- SANDBOX SECURITY CHECK ---
    is_safe <- base::tryCatch({
      # Block parent navigation
      if (base::grepl("\\.\\./", path) || base::grepl("\\./", path)) {
        stop("Navigation Forbidden")
      }
      # Block outside working directory
      abs_path <- fs::path_abs(path)
      if (!base::startsWith(as.character(abs_path), as.character(abs_work_dir))) {
        stop("Outside Sandbox")
      }
      TRUE
    }, error = function(e) {
      FALSE
    })

    if (!is_safe) {
      status_item <- "SECURITY BLOCK"
      detail_msg  <- "Path outside allowed jail"
    } else if (fs::dir_exists(path)) {
      status_item <- "SKIPPED"
      detail_msg  <- "Target is a directory"
    } else if (!fs::file_exists(path)) {
      status_item <- "ABSENT"
      detail_msg  <- "File already gone"
    } else {
      # --- ACTUAL DELETION ---
      base::tryCatch({
        fs::file_delete(path)
        status_item <- "DELETED"
        detail_msg  <- "Verified removal"
      }, error = function(e) {
        status_item <- "LOCKED/ERROR"
        detail_msg  <- base::substr(e$message, 1, 40)
      })
    }

    return(base::data.frame(
      File   = fs::path_file(path),
      Result = status_item,
      Detail = detail_msg,
      Path   = path,
      stringsAsFactors = FALSE
    ))
  })

  df_results <- base::do.call(base::rbind, results_list)

  # 3. Summary Stats
  deleted_count <- base::sum(df_results$Result == "DELETED")
  base::cat(base::paste0("<small style='color: #333;'>Files successfully deleted: <b>", deleted_count, "</b></small><br><br>"))

  # 4. Generate Stylized HTML Table
  html_table <- df_results %>%
    dplyr::mutate(
      Result = kableExtra::cell_spec(Result, color = "white", bold = TRUE,
                                     background = dplyr::case_when(
                                       Result == "DELETED"        ~ "#28a745", # Green
                                       Result == "ABSENT"         ~ "#6c757d", # Gray
                                       Result == "SECURITY BLOCK" ~ "#dc3545", # Red
                                       Result == "LOCKED/ERROR"   ~ "#dc3545", # Red
                                       Result == "SKIPPED"        ~ "#ffc107", # Yellow
                                       TRUE                       ~ "#17a2b8"  # Blue/Info
                                     ))
    ) %>%
    knitr::kable(format = "html", escape = FALSE, align = "llll") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE,
                              position = "left") %>%
    kableExtra::column_spec(1, bold = TRUE, width = "200px") %>%
    kableExtra::column_spec(3, italic = TRUE, width = "150px") %>%
    kableExtra::column_spec(4, color = "#6c757d", extra_css = "font-size: 9px;")

  base::cat(html_table)
  base::cat("<br>")

  # 5. Final Verdict
  # Check if any "DELETABLE" files still exist (ignoring directories or blocked paths)
  remaining_files <- base::sum(base::sapply(vector_file_paths, function(p) fs::file_exists(p) && !fs::dir_exists(p)))

  if (remaining_files == 0) {
    base::cat("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> Workspace cleared. No target files remain.</span><br><br>")
    return(TRUE)
  } else {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Clean-up incomplete. ", remaining_files, " file(s) still present.</span><br><br>"))
    return(FALSE)
  }
} # Upgraded!

fn_delete_multi_files_SUPER_POWER <- function(vector_file_paths) {
  library("fs")
  library("knitr")
  library("kableExtra")
  library("dplyr")

  # 1. Guard Clause
  if (base::is.null(vector_file_paths) || base::length(vector_file_paths) == 0) {
    base::cat("<b>Super Power Batch Purge:</b><br>")
    base::cat("<span style='color: #28a745;'>✔ <b>Clean:</b> No files provided.</span><br><br>")
    return(TRUE)
  }

  # Report Header
  base::cat("<b>Super Power Batch Purge Process:</b><br>")
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Purging files containing 'zzz_zzz_output' in path.</small><br>"))

  # 2. Execution Loop
  results_list <- base::lapply(vector_file_paths, function(path) {

    status_item <- "UNKNOWN"
    detail_msg  <- "Ready"
    path <- fs::path_tidy(path)

    # --- SUPER POWER SECURITY CHECK ---
    # The path MUST contain "zzz_zzz_output" as a directory
    is_power_authorized <- base::grepl("zzz_zzz_output", path)

    if (!is_power_authorized) {
      status_item <- "SECURITY BLOCK"
      detail_msg  <- "Path lacks 'zzz_zzz_output' marker"
    } else if (fs::dir_exists(path)) {
      status_item <- "SKIPPED"
      detail_msg  <- "Target is a directory"
    } else if (!fs::file_exists(path)) {
      status_item <- "ABSENT"
      detail_msg  <- "File already gone"
    } else {
      # --- ACTUAL DELETION ---
      base::tryCatch({
        fs::file_delete(path)
        status_item <- "DELETED"
        detail_msg  <- "Verified removal"
      }, error = function(e) {
        status_item <- "LOCKED/ERROR"
        detail_msg  <- base::substr(e$message, 1, 40)
      })
    }

    return(base::data.frame(
      File   = fs::path_file(path),
      Result = status_item,
      Detail = detail_msg,
      Path   = path,
      stringsAsFactors = FALSE
    ))
  })

  df_results <- base::do.call(base::rbind, results_list) %>%
    dplyr::mutate(`#` = dplyr::row_number())

  # 3. Summary Stats
  deleted_count <- base::sum(df_results$Result == "DELETED")
  base::cat(base::paste0("<small style='color: #333;'>Files successfully purged: <b>", deleted_count, "</b></small><br><br>"))

  # 4. Generate Stylized HTML Table
  html_table <- df_results %>%
    dplyr::mutate(
      Result = kableExtra::cell_spec(Result, color = "white", bold = TRUE,
                                     background = dplyr::case_when(
                                       Result == "DELETED"        ~ "#28a745", # Green
                                       Result == "ABSENT"         ~ "#6c757d", # Gray
                                       Result == "SECURITY BLOCK" ~ "#dc3545", # Red
                                       Result == "LOCKED/ERROR"   ~ "#dc3545", # Red
                                       Result == "SKIPPED"        ~ "#ffc107", # Yellow
                                       TRUE                       ~ "#17a2b8"  # Blue/Info
                                     ))
    ) %>%
    dplyr::select(`#`, File, Result, Detail, Path) %>%
    knitr::kable(format = "html", escape = FALSE, align = "cllll") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE,
                              position = "left") %>%
    kableExtra::column_spec(1, color = "#6c757d", width = "40px") %>%
    kableExtra::column_spec(2, bold = TRUE, width = "250px") %>%
    kableExtra::column_spec(4, italic = TRUE, width = "150px") %>%
    kableExtra::column_spec(5, color = "#6c757d", extra_css = "font-size: 9px;")

  base::cat(html_table)
  base::cat("<br>")

  # 5. Final Verdict
  # Count files that were supposed to be deleted but still exist
  failures <- base::sum(df_results$Result %in% c("SECURITY BLOCK", "LOCKED/ERROR"))

  if (failures == 0) {
    base::cat("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> Super Power purge complete. No unauthorized files were touched.</span><br><br>")
    return(TRUE)
  } else {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Purge incomplete. ", failures, " item(s) failed or were blocked.</span><br><br>"))
    return(FALSE)
  }
}


fn_copy_verify_multi_files <- function(vector_from_file_path, vector_to_file_path) {
  library("fs")
  library("knitr")
  library("kableExtra")
  library("dplyr")

  # 1. Validación de Consistencia de Vectores
  if (base::length(vector_from_file_path) != base::length(vector_to_file_path)) {
    base::cat("<span style='color: #dc3545;'>✘ <b>Critical Error:</b> Input vectors have different lengths.</span><br><br>")
    return(FALSE)
  }

  if (base::length(vector_from_file_path) == 0) {
    base::cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> No files provided for copying.</span><br><br>")
    return(TRUE)
  }

  # Normalización de rutas (Soporta rutas relativas y absolutas fuera del proyecto)
  vector_from <- fs::path_tidy(vector_from_file_path)
  vector_to   <- fs::path_tidy(vector_to_file_path)

  # 2. Encabezado de Auditoría (Detectando orígenes y destinos)
  dir_from <- fs::path_dir(vector_from[1])
  dir_to   <- fs::path_dir(vector_to[1])

  base::cat("<b>Batch Copy & Verify Process (Global Access):</b><br>")
  base::cat(base::paste0("<small style='color: #6c757d;'><b>FROM:</b> <code>", dir_from, "</code></small><br>"))
  base::cat(base::paste0("<small style='color: #6c757d;'><b>TO:</b> <code>", dir_to, "</code></small><br>"))
  base::cat(base::paste0("<small style='color: #333;'>Items to process: <b>", base::length(vector_from), "</b></small><br><br>"))

  # 3. Ejecución de la copia con captura de estados
  results_list <- base::lapply(base::seq_along(vector_from), function(i) {
    path_src <- vector_from[i]
    path_dst <- vector_to[i]

    status_origin <- "MISSING"
    status_copy   <- "FAILED"
    file_size     <- "0 B"

    # Verificar si el origen existe
    if (fs::file_exists(path_src)) {
      status_origin <- "OK"

      # Intentar la copia (overwrite = TRUE para asegurar actualización)
      base::tryCatch({
        # Asegurar que la carpeta de destino exista (útil para rutas externas nuevas)
        dst_dir <- fs::path_dir(path_dst)
        if (!fs::dir_exists(dst_dir)) fs::dir_create(dst_dir, recurse = TRUE)

        fs::file_copy(path_src, path_dst, overwrite = TRUE)

        # Verificación post-copia
        if (fs::file_exists(path_dst)) {
          status_copy <- "SUCCESS"
          file_size   <- base::format(fs::file_size(path_dst), units = "auto")
        }
      }, error = function(e) {
        status_copy <- "LOCKED/ERROR"
      })
    }

    base::data.frame(
      File = fs::path_file(path_src),
      Origin = status_origin,
      Copy_Result = status_copy,
      Size = file_size,
      stringsAsFactors = FALSE
    )
  })

  # Unificar resultados y agregar numeración
  df_audit <- base::do.call(base::rbind, results_list) %>%
    dplyr::mutate(`#` = dplyr::row_number()) %>%
    dplyr::select(`#`, File, Origin, Copy_Result, Size)

  # 4. Generar Tabla Estilizada para Quarto
  tabla_html <- df_audit %>%
    dplyr::mutate(
      Origin = kableExtra::cell_spec(Origin, color = "white", bold = TRUE,
                                     background = ifelse(Origin == "OK", "#28a745", "#dc3545")),
      Copy_Result = kableExtra::cell_spec(Copy_Result, color = "white", bold = TRUE,
                                          background = dplyr::case_when(
                                            Copy_Result == "SUCCESS" ~ "#28a745",
                                            Copy_Result == "FAILED"  ~ "#dc3545",
                                            TRUE                     ~ "#fd7e14" # Warning para bloqueos de sistema
                                          ))
    ) %>%
    knitr::kable(format = "html", escape = FALSE, align = "cllll") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = F, position = "left") %>%
    kableExtra::column_spec(1, color = "#6c757d", width = "40px") %>%
    kableExtra::column_spec(2, bold = TRUE, width = "350px") %>%
    kableExtra::column_spec(5, color = "#6c757d", width = "100px")

  # Imprimir tabla en el reporte
  base::cat(tabla_html)
  base::cat("<br>")

  # 5. Veredicto Final
  total_files <- base::length(vector_from)
  total_success <- base::sum(df_audit$Copy_Result == "SUCCESS")

  if (total_success == total_files) {
    base::cat(base::paste0("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> All items synchronized successfully (", total_success, "/", total_files, ").</span><br><br>"))
    return(TRUE)
  } else {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Synchronization incomplete! (Only ", total_success, "/", total_files, " verified). Check permissions on external drive.</span><br><br>"))
    return(FALSE)
  }
} # Upgraded!


fn_check_multi_files_pattern <- function(vector_file_paths, str_regex) {
  library("fs")
  library("dplyr")
  library("knitr")
  library("kableExtra")
  library("stringr")

  if (base::length(vector_file_paths) == 0) return(TRUE)

  # 1. HEADER
  base::cat("<b>File Pattern Audit:</b><br>")
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Rule: Filename must match </small><code>", str_regex, "</code><br>"))

  # 2. LOGIC: Check Regex + Existence
  df_audit <- base::data.frame(
    Full_Path = vector_file_paths,
    File_Name = fs::path_file(vector_file_paths),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      `#` = dplyr::row_number(),
      Exists = fs::file_exists(Full_Path),
      Matches_Pattern = stringr::str_detect(File_Name, str_regex),
      Status = dplyr::case_when(
        !Exists ~ "MISSING",
        Exists & Matches_Pattern ~ "MATCH_OK",
        Exists & !Matches_Pattern ~ "PATTERN_FAIL",
        TRUE ~ "ERROR"
      )
    )

  # 3. STYLIZED TABLE
  html_table <- df_audit %>%
    dplyr::mutate(
      Status = kableExtra::cell_spec(Status, color = "white", bold = TRUE,
                                     background = dplyr::case_when(
                                       Status == "MATCH_OK"     ~ "#28a745",
                                       Status == "MISSING"      ~ "#dc3545",
                                       Status == "PATTERN_FAIL" ~ "#fd7e14" # Naranja para error de nombre
                                     ))
    ) %>%
    dplyr::select(`#`, File_Name, Status) %>%
    knitr::kable(format = "html", escape = FALSE, align = "cll") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = F, position = "left") %>%
    kableExtra::column_spec(1, color = "#6c757d", width = "40px") %>%
    kableExtra::column_spec(2, bold = TRUE, width = "450px")

  base::cat(html_table)
  base::cat("<br>")

  # 4. FINAL VERDICT
  total_errors <- base::sum(df_audit$Status != "MATCH_OK")

  if (total_errors == 0) {
    base::cat("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> All files exist and follow the '_STONE.qmd' naming convention.</span><br><br>")
    return(TRUE)
  } else {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Pattern mismatch or missing files detected (", total_errors, " issue(s) found).</span><br><br>"))
    return(FALSE)
  }
}


fn_export_files_zzz_output <- function(str_file_path_from, str_file_path_to,
                                       bln_overwrite = FALSE, bln_production = FALSE) {
  library("fs")
  library("dplyr")
  library("knitr")
  library("kableExtra")

  # 1. Path Normalization
  str_path_from <- fs::path_tidy(str_file_path_from)
  str_path_to   <- fs::path_tidy(str_file_path_to)

  # 2. Report Header
  base::cat(base::paste0("<b>Export Audit:</b> <code>zzz_output*</code> (Archivos y Carpetas Finales)<br>"))

  env_label <- if (bln_production)
    "<span style='background:#007bff; color:white; padding:2px 5px; border-radius:3px; font-size:10px;'>PRODUCTION</span>" else
      "<span style='background:#6f42c1; color:white; padding:2px 5px; border-radius:3px; font-size:10px;'>TESTING/DEV</span>"

  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Status: ", env_label, "</small><br>"))

  # 3. Destination Folder Display
  base::cat(base::paste0(
    "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 8px; margin: 10px 0; border-radius: 4px;'>",
    "<b>Destination Path:</b> <code>", str_path_to, "</code></div>"
  ))

  # 4. File/Folder Listing and Analysis (MODIFICADO)
  # Filtramos: Solo elementos cuyo nombre final (base name) contenga 'zzz_output'
  all_contents <- fs::dir_ls(str_path_from, recurse = TRUE)

  # Lógica: fs::path_file extrae el nombre final.
  # Luego filtramos los que contienen 'zzz_output' en ese nombre final.
  list_items <- all_contents[base::grepl("zzz_output", fs::path_file(all_contents))]

  if (base::length(list_items) == 0) {
    base::cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> No files or folders found ending with 'zzz_output'.</span><br><br>")
    return(TRUE)
  }

  # 5. Decision Logic
  final_overwrite <- if (!bln_production) TRUE else bln_overwrite
  ovw_status_label <- if (final_overwrite) {
    "<span style='color: #28a745; font-weight: bold;'>Enabled</span>"
  } else {
    "<span style='color: #4682B4; font-weight: bold;'>Safe Mode (Disabled)</span>"
  }

  # 6. Build Audit Dataframe
  audit_data <- base::lapply(base::seq_along(list_items), function(i) {
    current_full_path <- list_items[i]

    rel_path <- fs::path_rel(current_full_path, start = str_path_from)
    dest_path <- fs::path(str_path_to, rel_path)

    f_info <- fs::file_info(current_full_path)
    already_there <- fs::file_exists(dest_path)
    is_dir <- fs::is_dir(current_full_path)

    status_label <- if (already_there) "Found" else "New"
    item_type <- if (is_dir) "Folder" else "File"

    if (already_there && !final_overwrite) {
      action_label <- "Skipped"
      do_copy <- FALSE
    } else if (already_there && final_overwrite) {
      action_label <- "Overwritten"
      do_copy <- TRUE
    } else {
      action_label <- "Copied"
      do_copy <- TRUE
    }

    base::data.frame(
      Relative_Path = rel_path,
      Size = if(is_dir) "-" else base::format(f_info$size, units = "auto"),
      Type = item_type,
      Status = status_label,
      Action = action_label,
      execute = do_copy,
      source_full = current_full_path,
      dest_full = dest_path,
      stringsAsFactors = FALSE
    )
  })

  df_audit <- base::do.call(base::rbind, audit_data) %>%
    dplyr::mutate(`#` = dplyr::row_number())

  # 7. Summary Block
  base::cat(base::paste0(
    "<div style='margin-bottom: 15px; border-left: 4px solid #dee2e6; padding-left: 10px;'>",
    "<small style='color: #333;'><b>Overwrite:</b> ", ovw_status_label, "</small><br>",
    "<small style='color: #333;'><b>Scan:</b> Detected <b>", base::length(list_items),
    "</b> elements (Files/Folders).</small></div>"
  ))

  # 8. Generate Table
  html_table <- df_audit %>%
    dplyr::mutate(
      Action = kableExtra::cell_spec(Action, color = "white", bold = TRUE,
                                     background = dplyr::case_when(
                                       Action == "Copied"      ~ "#28a745",
                                       Action == "Overwritten" ~ "#ffc107",
                                       Action == "Skipped"     ~ "#6c757d"
                                     )),
      Relative_Path = base::paste0("<code>", Relative_Path, "</code>")
    ) %>%
    dplyr::select(`#`, Relative_Path, Type, Size, Status, Action) %>%
    knitr::kable(format = "html", escape = FALSE, align = "clllll",
                 col.names = c("#", "Relative Export Path", "Type", "Size", "Status", "Action")) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = F, position = "left") %>%
    kableExtra::column_spec(1, color = "#6c757d", width = "40px") %>%
    kableExtra::column_spec(2, width = "400px")

  base::cat(html_table)
  base::cat("<br>")

  # 9. Physical Execution (MODIFICADO para carpetas)
  if (base::any(df_audit$execute)) {
    exec_df <- df_audit[df_audit$execute, ]

    check_export <- base::tryCatch({
      for (j in 1:base::nrow(exec_df)) {
        d_dir <- fs::path_dir(exec_df$dest_full[j])
        if (!fs::dir_exists(d_dir)) fs::dir_create(d_dir, recurse = TRUE)

        # Si es carpeta, usamos copy_dir; si es archivo, file_copy
        if (exec_df$Type[j] == "Folder") {
          # Borramos destino si overwrite está activo para evitar mezclas
          if (final_overwrite && fs::dir_exists(exec_df$dest_full[j])) fs::dir_delete(exec_df$dest_full[j])
          fs::dir_copy(exec_df$source_full[j], exec_df$dest_full[j], overwrite = final_overwrite)
        } else {
          fs::file_copy(exec_df$source_full[j], exec_df$dest_full[j], overwrite = final_overwrite)
        }
      }
      TRUE
    }, error = function(e) return(e$message))
  } else {
    check_export <- TRUE
  }

  # 10. Final Verdict
  if (base::isTRUE(check_export)) {
    base::cat("<span style='color: #28a745;'>✔ <b>Audit Complete:</b> All files and folders processed successfully.</span><br><br>")
    return(TRUE)
  } else {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Error:</b> ", check_export, "</span><br><br>"))
    return(FALSE)
  }
}
################################################################################
fn_check_multi_folders <- function(vector_folder_paths) {
  library("fs")
  library("dplyr")
  library("knitr")
  library("kableExtra")

  if (base::length(vector_folder_paths) == 0) return(TRUE)

  vector_folder_paths <- fs::path_tidy(vector_folder_paths)

  # 1. HEADER
  base::cat("<b>Bulk Folder Audit:</b><br>")
  base::cat(base::paste0("<small style='color: #6c757d; font-style: italic;'>Action: Existence and accessibility audit for ", base::length(vector_folder_paths), " directories.</small><br>"))

  # 2. LOGIC & AUDIT DATA
  df_audit <- base::lapply(vector_folder_paths, function(path) {
    exists <- fs::dir_exists(path)

    if (exists) {
      # Metadata para carpetas existentes
      info         <- fs::file_info(path)
      folder_perms <- info$permissions
      item_count   <- base::length(fs::dir_ls(path, all = TRUE, fail = FALSE))
      status_label <- "EXISTS"
    } else {
      # Metadata para carpetas faltantes
      folder_perms <- "---"
      item_count   <- 0
      status_label <- "MISSING"
    }

    base::data.frame(
      Folder = fs::path_file(path),
      Status = status_label,
      Items  = item_count,
      Perms  = folder_perms,
      Full_Path = path,
      stringsAsFactors = FALSE
    )
  }) %>%
    base::do.call(base::rbind, .) %>%
    dplyr::mutate(`#` = dplyr::row_number())

  # 3. ALERT BLOCK (Solo si hay faltantes)
  total_missing <- base::sum(df_audit$Status == "MISSING")

  if (total_missing > 0) {
    base::cat(base::paste0("<span style='color: #dc3545;'>✘ <b>Alert:</b> ", total_missing, " folder(s) not found.</span><br>"))
  }

  # 4. STYLIZED TABLE
  html_table <- df_audit %>%
    dplyr::mutate(
      Status = kableExtra::cell_spec(Status, color = "white", bold = TRUE,
                                     background = base::ifelse(Status == "EXISTS", "#28a745", "#dc3545"))
    ) %>%
    dplyr::select(`#`, Folder, Status, Items, Perms, Full_Path) %>%
    knitr::kable(format = "html", escape = FALSE, align = "clllll") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = F, position = "left") %>%
    kableExtra::column_spec(1, color = "#6c757d", width = "40px") %>%
    kableExtra::column_spec(2, bold = TRUE, width = "250px") %>%
    kableExtra::column_spec(4, width = "80px") %>%
    kableExtra::column_spec(6, color = "#6c757d", extra_css = "font-size: 9px; opacity: 0.7;")

  base::cat(html_table)
  base::cat("<br>")

  # 5. FINAL VERDICT
  if (total_missing == 0) {
    base::cat("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> All folders verified and accessible.</span><br><br>")
    return(TRUE)
  } else {
    base::cat("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Audit failed. Some directories are missing.</span><br><br>")
    return(FALSE)
  }
}


################################################################################

fn_render_qmd_files <- function(vector_file_path) {
  library("quarto")
  library("dplyr")
  library("knitr")
  library("kableExtra")

  # 1. Header
  base::cat("<b>Batch Rendering Process:</b><br>")
  base::cat("<small style='color: #6c757d; font-style: italic;'>Action: Executing Quarto render for all STONE documents.</small><br><br>")

  render_results <- list()

  # 2. Iteration with Error Handling
  for (file in vector_file_path) {
    status_val <- "SUCCESS"
    error_msg  <- "None"

    # Attempt to render
    res <- base::tryCatch({
      quarto::quarto_render(
        input = file,
        execute_dir = base::dirname(file),
        quiet = TRUE
      )
      TRUE
    }, error = function(e) {
      status_val <<- "FAILED"
      error_msg  <<- e$message
      base::return(FALSE)
    })

    render_results[[file]] <- base::data.frame(
      File = fs::path_file(file),
      Status = status_val,
      Details = error_msg,
      stringsAsFactors = FALSE
    )

    # STOP execution if a single file fails
    if (status_val == "FAILED") break
  }

  # 3. Build Audit Table
  df_render <- base::do.call(base::rbind, render_results) %>%
    dplyr::mutate(`#` = dplyr::row_number())

  rownames(df_render) <- 1:nrow(df_render)
  html_table <- df_render %>%
    dplyr::mutate(
      Status = kableExtra::cell_spec(Status, color = "white", bold = TRUE,
                                     background = base::ifelse(Status == "SUCCESS", "#28a745", "#dc3545"))
    ) %>%
    dplyr::select(`#`, File, Status, Details) %>%
    knitr::kable(format = "html", escape = FALSE, align = "clll") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = F, position = "left") %>%
    # Updated styling logic
    kableExtra::column_spec(1, color = "#6c757d", width = "40px") %>%
    kableExtra::column_spec(2, bold = TRUE, width = "400px") %>%
    kableExtra::column_spec(4, color = "#dc3545", extra_css = "font-size: 10px;")

  # 4. Final Output and Flow Control
  base::cat(html_table)
  base::cat("<br>")

  if (base::any(df_render$Status == "FAILED")) {
    base::cat("<span style='color: #dc3545;'>✘ <b>Critical Failure:</b> Rendering interrupted. Runner execution halted to prevent data inconsistency.</span><br><br>")
    knitr::knit_exit()
  } else {
    base::cat("<span style='color: #28a745;'>✔ <b>Success:</b> All documents rendered successfully.</span><br><br>")
    base::return(TRUE)
  }
}


