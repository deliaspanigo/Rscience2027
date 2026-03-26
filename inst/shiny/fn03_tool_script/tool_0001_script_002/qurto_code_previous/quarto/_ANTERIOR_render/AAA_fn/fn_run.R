#' Execute an R Script in an Isolated Environment and Save Results
#'
#' @param script_path String. The full path to the .R script to be executed.
#' @param output_rdata String. The path and name of the resulting .RData file.
#'
#' @return Returns the environment invisibly.
run_script_to_rdata <- function(script_path, output_rdata) {

  # 1. Create a new, empty environment
  # This acts as a container to prevent polluting the Global Environment
  isolated_env <- new.env(parent = .GlobalEnv)

  # 2. Execute the script within the isolated environment
  # sys.source is preferred for parsing files into specific environments
  tryCatch({
    sys.source(script_path, envir = isolated_env)
    message("✔ Script executed successfully within the isolated environment.")
  }, error = function(e) {
    stop("✘ Execution failed: ", e$message)
  })

  # 3. Retrieve all object names created inside the environment
  object_list <- ls(envir = isolated_env, all.names = TRUE)

  if (length(object_list) == 0) {
    warning("! The script executed but no objects were created to save.")
    return(invisible(isolated_env))
  }

  # 4. Save the objects to an .RData file
  # We specify the environment so R knows where to find the objects in 'object_list'
  save(list = object_list,
       file = output_rdata,
       envir = isolated_env)

  message("💾 Snapshot saved to: ", output_rdata)
  message("📦 Total objects stored: ", length(object_list))

  # Return the environment invisibly for debugging if needed
  return(invisible(isolated_env))
}
