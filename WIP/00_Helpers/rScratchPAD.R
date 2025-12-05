


# Testing the ENVIRO-LOCKED R List Helper Function ...
rvsListNames_ <- c("VAR_A", "VAR_B", "VAR_C", "VAR_X", "VAR_Y", "VAR_Z", "VAR_G")
rlsListVals_  <- list(
  1982, "Value for VAR_B", "R-Object for VAR_C", FALSE, 
  "Value for VAR_Y", TRUE, "R-List for VAR_G"
)



rlsEnvLockdLIST <- code.return.env.locked.list(
  vsListNames = rvsListNames_, lsListVals = rlsListVals_, sbLockList = TRUE
)   ### <- Extracts Library Information ... 

rlsEnvLockdLIST$VAR_X        # -> Returns the value FALSE !!!
rlsEnvLockdLIST[["VAR_C"]]   # -> Returns the value 'R-Object for VAR_C' !!! 



create_env_locked_list <- function(names, values, lock_values = TRUE) {
  
  # Input validation
  if (!is.character(names)) {
    stop("'names' must be a character vector")
  }
  
  if (!is.list(values)) {
    stop("'values' must be a list")
  }
  
  if (length(names) != length(values)) {
    stop("Length of 'names' (", length(names), 
         ") must equal length of 'values' (", length(values), ")")
  }
  
  if (any(duplicated(names))) {
    dup_names <- names[duplicated(names)]
    stop("Duplicate names found: ", paste(unique(dup_names), collapse = ", "))
  }
  
  if (any(names == "")) {
    stop("Empty names are not allowed")
  }
  
  # Create new environment
  env <- new.env(parent = emptyenv())
  
  # Assign values to environment
  for (i in seq_along(names)) {
    assign(names[i], values[[i]], envir = env)
  }
  
  # Lock environment if requested
  if (lock_values) {
    lockEnvironment(env, bindings = TRUE)
  }
  
  # Create S3 object with custom class
  obj <- structure(
    list(
      .env = env,
      .locked = lock_values,
      .names = names
    ),
    class = "env_locked_list"
  )
  
  # Add active bindings for element access
  for (name in names) {
    makeActiveBinding(
      sym = name,
      fun = local({
        current_name <- name
        function() {
          if (exists(current_name, envir = env, inherits = FALSE)) {
            get(current_name, envir = env)
          } else {
            stop("Element '", current_name, "' not found")
          }
        }
      }),
      env = obj
    )
  }
  
  return(obj)
}



rlsEnvLockdLIST_v01 <- create_env_locked_list(
  names = rvsListNames_, values = rlsListVals_, lock_values = TRUE
)

