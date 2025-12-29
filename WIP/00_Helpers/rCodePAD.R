#######|       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Code-Pad > R SCRIPT   <-       |#######
####`   -> Use this R Script <file> for Random Coding Tasks during Code Development Cycle !!!   ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######

MFMRutils::devs.check.code.specs(sbCheckCRAN = T)



class(coRENVs)

grep("code", ls(getNamespace("MFMRutils"), all.names = TRUE), 
     value = TRUE, ignore.case = TRUE)  







"search_r_code" <- function(pattern, target = "ggplot2", use_regex = TRUE) {
  # 1. Resolve the search target (Library Namespace or Environment)
  if (is.character(target)) {
    target_env <- tryCatch(getNamespace(target), 
                           error = function(e) stop(paste("Library", target, "not found.")))
    lib_id <- target
  } else if (is.environment(target)) {
    target_env <- target
    lib_id <- "Environment"
  } else {
    stop("Target must be a package name (string) or an environment.")
  }
  
  # 2. Get all object names in the target
  obj_names <- ls(target_env, all.names = TRUE)
  results <- list()
  
  # 3. Iterate through all objects
  for (fn_name in obj_names) {
    obj <- get(fn_name, envir = target_env)
    
    # We only care about functions (including hidden/internal ones)
    if (is.function(obj)) {
      sr <- getSrcref(obj)
      
      # Determine if real source code is available or if we must deparse
      if (!is.null(sr)) {
        # 'srcref' is available: exact file and line numbers
        code_lines <- as.character(sr)
        start_line <- as.numeric(sr)[1]
        file_name  <- getSrcFilename(sr)
      } else {
        # 'srcref' missing: fallback to deparse (standard for CRAN packages)
        code_lines <- deparse(obj)
        start_line <- 1
        file_name  <- "Deparsed (Standard CRAN install)"
      }
      
      # 4. Search for the pattern in the code lines
      match_indices <- which(grepl(pattern, code_lines, fixed = !use_regex, ignore.case = TRUE))
      
      if (length(match_indices) > 0) {
        for (idx in match_indices) {
          # Construct the match row
          match_data <- data.frame(
            LIBRARY_ID    = lib_id,
            FUNCTION_NAME = fn_name,
            LINE_NUMBER   = start_line + idx - 1,
            CODE_SNIPPET  = trimws(code_lines[idx]),
            FILE_NAME     = if(length(file_name) == 0) "Unknown" else file_name,
            stringsAsFactors = FALSE
          )
          results[[length(results) + 1]] <- match_data
        }
      }
    }
  }
  
  # 5. Compile and return result
  if (length(results) == 0) {
    message("No matches found for pattern: ", pattern)
    return(data.frame())
  }
  
  do.call(rbind, results)
}









#' Search R Environments and Libraries for Text Patterns
#' 
#' @param pattern Character string or regex pattern to search for
#' @param packages Character vector of package names to search (NULL = all loaded)
#' @param envs List of environments to search (default: global environment)
#' @param search_internals Logical, whether to search internal/non-exported functions
#' @param regex Logical, whether pattern is a regular expression
#' @param ignore_case Logical, whether to ignore case
#' @param snippet_length Number of characters to include in code snippet
#' @param include_global Logical, whether to include global environment in search
#' @param verbose Logical, whether to show progress messages
#' @return Data frame with search results
#' @export
search_r_functions <- function(
  pattern, packages = NULL, envs = list(globalenv()), search_internals = FALSE, regex = TRUE, 
  ignore_case = TRUE, snippet_length = 82, include_global = TRUE, verbose = TRUE
) {
  
  # Validate inputs
  if (missing(pattern) || !is.character(pattern) || length(pattern) != 1) {
    stop("pattern must be a single character string")
  }
  
  # Initialize results
  results <- data.frame(
    LIBRARY_ID = character(0),
    FUNCTION_NAME = character(0),
    LINE_NUMBER = integer(0),
    CODE_SNIPPET = character(0),
    FILE_NAME = character(0),
    IS_EXPORTED = logical(0),
    SEARCH_PATTERN = character(0),
    stringsAsFactors = FALSE
  )
  
  # Helper function to get source file info
  get_source_file <- function(func) {
    # Try multiple methods to get source file
    src_file <- NA_character_
    
    # Method 1: Check srcref attribute
    srcref <- attr(func, "srcref", exact = TRUE)
    if (!is.null(srcref)) {
      src_file <- tryCatch({
        attr(srcref, "srcfile", exact = TRUE)$filename
      }, error = function(e) NA_character_)
    }
    
    # Method 2: Check environment attributes
    if (is.na(src_file)) {
      env <- environment(func)
      if (!is.null(env) && !is.null(attr(env, "name"))) {
        src_file <- attr(env, "name")
      }
    }
    
    # Method 3: Try to get from package source
    if (is.na(src_file)) {
      pkg <- utils::packageName(environment(func))
      if (!is.null(pkg)) {
        src_file <- paste0("package:", pkg)
      }
    }
    
    return(src_file)
  }
  
  # Helper function to search a single function
  search_single_function <- function(func, func_name, lib_id, is_exported = TRUE) {
    if (!is.function(func)) return(NULL)
    
    # Get function source code
    func_code <- tryCatch({
      # Use deparse with control to preserve source
      deparse(func, control = c("keepInteger", "keepNA"))
    }, error = function(e) character(0))
    
    if (length(func_code) == 0) return(NULL)
    
    # Search for pattern
    matches <- grep(pattern, func_code, 
                    ignore.case = ignore_case, 
                    perl = regex, 
                    value = FALSE)
    
    if (length(matches) == 0) return(NULL)
    
    # Get source file
    src_file <- get_source_file(func)
    
    # Prepare results
    func_results <- data.frame(
      "LIBRARY_ID" = rep(lib_id, length(matches)),
      "FUNC_NAME" = rep(func_name, length(matches)),
      "LINE_NUMBER" = matches,
      "CODE_SNIP" = character(length(matches)),
      "FILE_NAME" = rep(src_file, length(matches)),
      "IS_EXPORTED" = rep(is_exported, length(matches)),
      "SEARCH_TERM" = rep(pattern, length(matches)),
      stringsAsFactors = FALSE
    )
    
    # Extract code snippets
    for (i in seq_along(matches)) {
      line_num <- matches[i]
      line_text <- func_code[line_num]
      
      # Truncate if too long
      if (nchar(line_text) > snippet_length) {
        snippet <- paste0(substr(line_text, 1, snippet_length - 3), "...")
      } else {
        snippet <- line_text
      }
      
      func_results$CODE_SNIPPET[i] <- snippet
    }
    
    return(func_results)
  }
  
  # Search packages
  if (!is.null(packages)) {
    for (pkg in packages) {
      if (verbose) message(sprintf("Searching package: %s", pkg))
      
      # Check if package is loaded/available
      if (!requireNamespace(pkg, quietly = TRUE)) {
        warning(sprintf("Package '%s' not available. Skipping.", pkg))
        next
      }
      
      # Get package namespace
      ns <- tryCatch(asNamespace(pkg), error = function(e) NULL)
      if (is.null(ns)) next
      
      # Get functions to search
      if (search_internals) {
        # Get all objects in namespace
        func_names <- ls(ns, all.names = TRUE)
      } else {
        # Get only exported functions
        func_names <- getNamespaceExports(pkg)
      }
      
      # Filter to functions only
      func_names <- func_names[sapply(func_names, function(x) {
        obj <- tryCatch(get(x, envir = ns), error = function(e) NULL)
        !is.null(obj) && is.function(obj)
      })]
      
      # Search each function
      for (func_name in func_names) {
        func <- get(func_name, envir = ns)
        
        # Check if function is exported
        is_exported <- func_name %in% getNamespaceExports(pkg)
        
        func_results <- search_single_function(func, func_name, pkg, is_exported)
        
        if (!is.null(func_results) && nrow(func_results) > 0) {
          results <- rbind(results, func_results)
        }
      }
      
      if (verbose) {
        message(sprintf("  Found %d matches in %d functions", 
                        sum(results$LIBRARY_ID == pkg), 
                        length(unique(results$FUNCTION_NAME[results$LIBRARY_ID == pkg]))))
      }
    }
  }
  
  # Search environments
  for (env_idx in seq_along(envs)) {
    env <- envs[[env_idx]]
    env_name <- tryCatch({
      if (identical(env, globalenv())) {
        ".GlobalEnv"
      } else if (!is.null(attr(env, "name"))) {
        attr(env, "name")
      } else {
        paste0("Environment_", env_idx)
      }
    }, error = function(e) paste0("Environment_", env_idx))
    
    if (verbose) message(sprintf("Searching environment: %s", env_name))
    
    # Get all objects in environment
    obj_names <- tryCatch(ls(env, all.names = TRUE), error = function(e) character(0))
    
    # Search each object that is a function
    for (obj_name in obj_names) {
      obj <- tryCatch(get(obj_name, envir = env), error = function(e) NULL)
      
      if (!is.null(obj) && is.function(obj)) {
        func_results <- search_single_function(obj, obj_name, env_name, TRUE)
        
        if (!is.null(func_results) && nrow(func_results) > 0) {
          results <- rbind(results, func_results)
        }
      }
    }
    
    if (verbose) {
      matches_in_env <- sum(results$LIBRARY_ID == env_name)
      message(sprintf("  Found %d matches", matches_in_env))
    }
  }
  
  # Search global environment if requested
  if (include_global) {
    if (verbose) message("Searching global environment")
    
    global_objs <- ls(globalenv(), all.names = TRUE)
    
    for (obj_name in global_objs) {
      obj <- get(obj_name, envir = globalenv())
      
      if (is.function(obj)) {
        func_results <- search_single_function(obj, obj_name, ".GlobalEnv", TRUE)
        
        if (!is.null(func_results) && nrow(func_results) > 0) {
          results <- rbind(results, func_results)
        }
      }
    }
    
    if (verbose) {
      matches_in_global <- sum(results$LIBRARY_ID == ".GlobalEnv")
      message(sprintf("  Found %d matches", matches_in_global))
    }
  }
  
  # Sort and finalize results
  if (nrow(results) > 0) {
    results <- results[order(results$LIBRARY_ID, results$FUNCTION_NAME, results$LINE_NUMBER), ]
    rownames(results) <- NULL
    
    # Add unique match ID
    results$MATCH_ID <- seq_len(nrow(results))
  }
  
  # Add attributes
  attr(results, "search_info") <- list(
    pattern = pattern,
    packages = packages,
    search_internals = search_internals,
    regex = regex,
    ignore_case = ignore_case,
    timestamp = Sys.time(),
    total_matches = nrow(results),
    unique_functions = length(unique(results$FUNCTION_NAME)),
    unique_libraries = length(unique(results$LIBRARY_ID))
  )
  
  class(results) <- c("function_search_results", "data.frame")
  
  return(results)
}



dfCodeV01_GI_ <- search_r_code("plot", target = "maps")
nrow(dfCodeV01_GI_)
head(dfCodeV01_GI_, 7)

dfCodeV02_GI_ <- search_r_code("base::file", target = "MFMRutils")
nrow(dfCodeV02_GI_)
head(dfCodeV02_GI_, 7)



dfCodeV01_DS_ <- search_r_functions(
  pattern = "plot", packages = "maps", search_internals = TRUE
)
nrow(dfCodeV01_DS_)
head(dfCodeV01_DS_, 7)

dfCodeV02_DS_ <- search_r_functions(
  pattern = "base::file", packages = "MFMRutils", search_internals = TRUE
)
nrow(dfCodeV02_DS_)
head(dfCodeV02_DS_, 7)








