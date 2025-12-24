#? ### ### ### ### ### ### ###
#' @title Locate Text or Regex Strings in R Functions
#' @name devs.find.code.instances
#' 
#' 
#' @description
#' Easily search and find text or regular expression (sbUseRegex) snippets in any R Function.
#'
#'
#' @param ssFindText Character string or sbUseRegex ssFindText to search for
#' @param vsTargetLibs Character vector of package names to search (NULL = all loaded)
#' @param coRENVs List of environments to search (default: global environment)
#' @param sbSearchInternals Logical, whether to search internal/non-exported functions
#' @param sbUseRegex Logical, whether ssFindText is a regular expression
#' @param sbIgnoreCase Logical, whether to ignore case
#' @param snRetSnipSize Number of characters to include in code snippet
#' @param sbIncludeGlobal Logical, whether to include global environment in search
#' @param sbVerboseSearch Logical, whether to show progress messages
#' 
#' 
#' @return
#' This function returns a Data Frame that contains the following variables <columns>:
#' * FUNC_NAME   -> a [character] identifier of the R Function where the matching sbUseRegex was found.   
#' * LINE_NUMBER -> an [integer] variable denoting the R Function code line number where the 
#'                  matching text or sbUseRegex value was found.   
#' * CODE_SNIP   -> a [character] variable that captures a code snippet from the matching line where 
#'                  the text or sbUseRegex value was found.
#' * FILE_NAME   -> a [character] variable that captures the <parent> file identifier (i.e. name) of 
#'                  the R Function where the matching text or sbUseRegex value was found.
#'
#'
#' @examples
#' ### Use the "Path-Cleaning" Function as follows: ...
#' library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...
#' 
#' 
#' # Example 1: Basic search in a package
#' library(ggplot2)
#' results <- devs.find.code.instances(
#'   ssFindText = "aes\\(",
#'   vsTargetLibs = "ggplot2",
#'   sbSearchInternals = FALSE
#' )
#' 
#' # View results
#' print(results, n = 20)
#' summary(results)
#' 
#' 
#' # Example 2: Search with sbUseRegex
#' results <- devs.find.code.instances(
#'   ssFindText = "^#.*TODO|FIXME",
#'   vsTargetLibs = c("dplyr", "tidyr"),
#'   sbUseRegex = TRUE,
#'   sbIgnoreCase = FALSE
#' )
#' 
#' # Example 3: Search internal functions
#' results <- devs.find.code.instances(
#'   ssFindText = "lapply",
#'   vsTargetLibs = "base",
#'   sbSearchInternals = TRUE,
#'   snRetSnipSize = 80
#' )
#' 
#' # Example 4: Search specific environments
#' my_env <- new.env()
#' my_env$custom_func <- function(x) {
#'   # TODO: optimize this
#'   result <- x * 2
#'   print(result)
#'   return(result)
#' }
#' 
#' results <- devs.find.code.instances(
#'   ssFindText = "TODO",
#'   coRENVs = list(my_env),
#'   sbIncludeGlobal = TRUE
#' )
#' 
#' 
#' # Example 5: Complex sbUseRegex search
#' # Find all function definitions that take a 'data' parameter
#' results <- devs.find.code.instances(
#'   ssFindText = "function\\(.*data.*\\)",
#'   vsTargetLibs = "ggplot2",
#'   sbUseRegex = TRUE
#' )
#' 
#' # Example 6: Search for error handling ssFindTexts
#' results <- devs.find.code.instances(
#'   ssFindText = "stop\\|warning\\|message\\|tryCatch",
#'   vsTargetLibs = c("base", "rlang"),
#'   sbUseRegex = TRUE
#' )
#' 
#' # Example 7: Case-sensitive search for S3 methods
#' results <- devs.find.code.instances(
#'   ssFindText = "print\\.",
#'   vsTargetLibs = "base",
#'   sbIgnoreCase = FALSE,
#'   sbUseRegex = TRUE
#' )
#' 
#' # Example 8: Search for specific variable assignments
#' results <- devs.find.code.instances(
#'   ssFindText = "<-\\s*function\\(",
#'   vsTargetLibs = "ggplot2",
#'   sbUseRegex = TRUE
#' )
#' 
#' # Example 9: Create a custom environment and search it
#' env1 <- new.env()
#' env1$func1 <- function(x) paste("Result:", x)
#' env1$func2 <- function(y) cat("Output:", y, "\n")
#' 
#' results <- devs.find.code.instances(
#'   ssFindText = "cat|print",
#'   coRENVs = list(env1),
#'   sbIncludeGlobal = FALSE
#' )
#' 
#' # Example 10: Batch search across multiple vsTargetLibs
#' vsTargetLibs_to_search <- c("stats", "utils", "graphics", "grDevices")
#' results <- devs.find.code.instances(
#'   ssFindText = "par\\(",
#'   vsTargetLibs = vsTargetLibs_to_search,
#'   sbVerboseSearch = TRUE
#' )
#' 
#' # Get statistics
#' cat("\nSearch Statistics:\n")
#' cat(sprintf("Total matches: %d\n", attr(results, "search_info")$total_matches))
#' cat(sprintf("Unique functions: %d\n", attr(results, "search_info")$unique_functions))
#' 
#' # Example 11: Search for plotting functions
#' results <- devs.find.code.instances(
#'   ssFindText = "plot\\(",
#'   vsTargetLibs = c("graphics", "ggplot2"),
#'   sbSearchInternals = TRUE
#' )
#' 
#' # Visualize results
#' if (nrow(results) > 0) {
#'   library(ggplot2)
#'   plot_data <- as.data.frame(table(results$LIBRARY_ID))
#'   ggplot(plot_data, aes(x = Var1, y = Freq)) +
#'     geom_bar(stat = "identity") +
#'     labs(x = "Library", y = "Matches", title = "Search Results by Library") +
#'     theme_minimal()
#' }
#' 
#'
#' @export
#? ### ### ###
"devs.find.code.instances" <- function(
    ssFindText, vsTargetLibs=NULL, coRENVs=base::list(base::globalenv()), sbSearchInternals = FALSE, 
    sbUseRegex = TRUE, sbIgnoreCase = TRUE, sbIncludeGlobal = TRUE, sbVerboseSearch = TRUE, 
    snRetSnipSize = 82
) {
  
  ####   STEP 01 - Prime "Function Self-ID" CONSTANTS   ####
  RCT_SYS_TIME_NOW_     <- base::Sys.time();             # <- Extract active System Date-Time !!!
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";                  # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHRT_ <- "Find.Code";                  # <- FSID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Find.Code.Instances";   # <- FSID - LONG !!!
  
  # Validate inputs
  if (missing(ssFindText) || !is.character(ssFindText) || length(ssFindText) != 1) {
    stop("ssFindText must be a single character string")
  }
  
  # Initialize results
  results <- data.frame(
    "LIBRARY_ID"  = character(0),
    "FUNC_NAME"   = character(0),
    "IS_EXPORTED" = logical(0),
    "LINE_NUMBER" = integer(0),
    "CODE_SNIP"   = character(0),
    "FILE_NAME"   = character(0),
    "SEARCH_TERM" = character(0),
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
    
    # Search for ssFindText
    matches <- grep(ssFindText, func_code, 
                    ignore.case = sbIgnoreCase, 
                    perl = sbUseRegex, 
                    value = FALSE)
    
    if (length(matches) == 0) return(NULL)
    
    # Get source file
    src_file <- get_source_file(func)
    
    # Prepare results
    func_results <- data.frame(
      "LIBRARY_ID"  = rep(lib_id, length(matches)),
      "FUNC_NAME"   = rep(func_name, length(matches)),
      "IS_EXPORTED" = rep(is_exported, length(matches)),
      "LINE_NUMBER" = matches,
      "CODE_SNIP"   = character(length(matches)),
      "FILE_NAME"   = rep(src_file, length(matches)),
      "SEARCH_TERM" = rep(ssFindText, length(matches)),
      stringsAsFactors = FALSE
    )
    
    # Extract code snippets
    for (i in seq_along(matches)) {
      line_num <- matches[i]
      line_text <- func_code[line_num]
      
      # Truncate if too long
      if (nchar(line_text) > snRetSnipSize) {
        snippet <- paste0(substr(line_text, 1, snRetSnipSize - 3), "...")
      } else {
        snippet <- line_text
      }
      
      func_results$CODE_SNIP[i] <- snippet
    }
    
    return(func_results)
  }
  
  # Search vsTargetLibs
  if (!is.null(vsTargetLibs)) {
    for (pkg in vsTargetLibs) {
      if (sbVerboseSearch) message(sprintf("Searching package: %s", pkg))
      
      # Check if package is loaded/available
      if (!requireNamespace(pkg, quietly = TRUE)) {
        warning(sprintf("Package '%s' not available. Skipping.", pkg))
        next
      }
      
      # Get package namespace
      ns <- tryCatch(asNamespace(pkg), error = function(e) NULL)
      if (is.null(ns)) next
      
      # Get functions to search
      if (sbSearchInternals) {
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
      
      if (sbVerboseSearch) {
        message(sprintf("  Found %d matches in %d functions", 
                        sum(results$LIBRARY_ID == pkg), 
                        length(unique(results$FUNC_NAME[results$LIBRARY_ID == pkg]))))
      }
    }
  }
  
  # Search environments
  for (env_idx in seq_along(coRENVs)) {
    env <- coRENVs[[env_idx]]
    env_name <- tryCatch({
      if (identical(env, globalenv())) {
        ".GlobalEnv"
      } else if (!is.null(attr(env, "name"))) {
        attr(env, "name")
      } else {
        paste0("Environment_", env_idx)
      }
    }, error = function(e) paste0("Environment_", env_idx))
    
    if (sbVerboseSearch) message(sprintf("Searching environment: %s", env_name))
    
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
    
    if (sbVerboseSearch) {
      matches_in_env <- sum(results$LIBRARY_ID == env_name)
      message(sprintf("  Found %d matches", matches_in_env))
    }
  }
  
  # Search global environment if requested
  if (sbIncludeGlobal) {
    if (sbVerboseSearch) message("Searching global environment")
    
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
    
    if (sbVerboseSearch) {
      matches_in_global <- sum(results$LIBRARY_ID == ".GlobalEnv")
      message(sprintf("  Found %d matches", matches_in_global))
    }
  }
  
  # Sort and finalize results
  if (nrow(results) > 0) {
    results <- results[order(results$LIBRARY_ID, results$FUNC_NAME, results$LINE_NUMBER), ]
    rownames(results) <- NULL
    
    # Add unique match ID
    results$MATCH_ID <- seq_len(nrow(results))
  }
  
  # Add attributes
  attr(results, "search_info") <- list(
    "SEARCH_TERM"    = ssFindText,
    "R_LIBRARIES"    = vsTargetLibs,
    "INTERN_SEARCH"  = sbSearchInternals,
    "IS_USE_REGEX"   = sbUseRegex,
    "IS_IGNORE_CASE" = sbIgnoreCase,
    "TIME_STAMP"     = Sys.time(),
    "TOTAL_MATCHES"  = nrow(results),
    "UNIQUE_FUNCS"   = length(unique(results$FUNC_NAME)),
    "UNIQUE_LIBS"    = length(unique(results$LIBRARY_ID))
  )
  
  class(results) <- c("Search_Results", "data.frame")
  
  return(results)
  
}


