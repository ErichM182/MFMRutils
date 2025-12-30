#? ### ### ### ### ### ### ###
#' @title Locate Text or Regex Strings in R Code ("SuiteMFMR" DevTools)
#' @name devs.find.code.instances
#' @family SuiteMFMR DevTools
#' 
#' 
#' @description
#' Easily search for text or Regular Expressions (Regex) code snippets in any R Function installed 
#' in your own or other (3rd Party) R Libraries installed on your local machine. This function was 
#' intended to served as a Development Utility (i.e. as part of the "SuiteMFMR" DevTools) and 
#' enables R Library Developers to search the entire R Code Base as for specific strings/texts and
#' Regular Expressions as contained or instantiated on their Local R Development Environments.
#'
#'
#' @param ssFindText ([character]) A String or Regular Expression stub to search for in the target R 
#'                   code bases.
#' @param vsTargetLibs ([vector] of [character]s) A Vector of Strings (or character vector) denoting 
#'                     the R Libraries to be searched (i.e. Target R Libraries). NOTE: R Libraries 
#'                     must be installed locally (i.e. on local machine) prior to search.
#' @param coRENVs ([list] of [environment]s) A List of R Environments to search for the `ssFindText` 
#'                and `vsTargetLibs` combination (default: Global R Environment [.GlobalEnv])
#' @param sbSearchInternals ([logical]) A Boolean (logical) function argument that specifies whether 
#'                          to search internal (i.e. non-exported) R functions as well.
#' @param sbUseRegex ([logical]) A Boolean (logical) function argument that specifies whether the 
#'                   `ssFindText` value should be treated as a Regular Expression (Regex) or not.
#' @param sbIgnoreCase ([logical]) A Boolean (logical) function argument that specifies whether to 
#'                     ignore case-sensitivity during search or not.
#' @param snRetSnipSize ([numeric]) A Numeric function argument that indicates how many characters 
#'                      to include in code snippets (i.e. returned search results code-snippets).
#' @param sbIncludeGlobal ([logical]) A Boolean (logical) function argument that specifies whether 
#'                        to include the Global R Environment in the search or not.
#' @param sbVerboseSearch ([logical]) A Boolean (logical) function argument that specifies whether 
#'                        to show search results message or not.
#' 
#' 
#' @return
#' This function returns a Data Frame that contains the following variables <columns>:
#' * LIBRARY_ID  -> a [factor] variable of the R Library Identifier (name) where the Code Search 
#'                  encountered positive hits for the `ssFindText` value.
#' * FUNC_NAME   -> a [factor] variable of the R Function Identifier (name) where the Code Search 
#'                  encountered positive hits for the `ssFindText` value. 
#' * IS_EXPORTED -> a [logical] variable of the R Function Export Status where the Code Search 
#'                  encountered positive hits for the `ssFindText` value.   
#' * LINE_NUMBER -> an [integer] variable denoting the R Function code line number where the Code 
#'                  Search encountered positive hits for the `ssFindText` value.   
#' * CODE_SNIP   -> a [character] variable that captures a code snippet from the matching line where 
#'                  where the Code Search encountered positive hits for the `ssFindText` value.
#' * FILE_NAME   -> a [character] variable that captures the <parent> file identifier (i.e. name) of 
#'                  the R Function where the Code Search encountered positive hits for the 
#'                  `ssFindText` value.
#' * SEARCH_TERM -> a [character] variable that captures the actual `ssFindText` value.
#' * MATCH_ID    -> a [integer] variable that captures the match identifier (i.e. a unique sequence
#'                  identifier) for all matches returned from the Code Search result.
#'
#'
#' @examples
#' ### Use the "R-Code-Searching" Function as follows: ...
#' library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...
#' 
#' 
#' ### Example 1: Basic search inside specified R packages ...
#' results <- devs.find.code.instances(
#'   ssFindText = "plot",
#'   vsTargetLibs = c("base", "utils", "graphics"),
#'   sbSearchInternals = FALSE
#' )
#' 
#' ### View code search results ...
#' str(results)
#' head(results, 7)
#' summary(results)
#' 
#' 
#' \dontrun{   ### <- Examples beyond this point is over-kill - run only if really interested !!!
#' 
#'   ### Example 2: Search with the use of Regular Expressions (RegEx) ...
#'   results <- devs.find.code.instances(
#'     ssFindText = "^#.*TODO|FIXME|DEPRECATED",
#'     vsTargetLibs = c("dplyr", "tidyr"),
#'     sbUseRegex = TRUE, sbIgnoreCase = TRUE
#'   )
#'   
#'   
#'   
#'   ### Example 3: Search internal R functions ...
#'   results <- devs.find.code.instances(
#'     ssFindText = "lapply",
#'     vsTargetLibs = "base",
#'     sbSearchInternals = TRUE,
#'     snRetSnipSize = 80
#'   )
#' 
#' 
#' 
#'   ### Example 4: Search specific environments ... (# <- TODO: This DOES NOT WORK - Fix later !!!)
#'   my_env <- new.env()
#'   my_env$custom_func <- function(x) {
#'     # TODO: optimize this !!!
#'     result <- x * 2
#'     print(result)
#'     return(result)
#'   }
#'   
#'   results <- devs.find.code.instances(
#'     ssFindText = "TODO:",
#'     coRENVs = list(my_env),
#'     sbIncludeGlobal = TRUE
#'   )
#'   
#'   
#'   
#'   ### Example 5: Complex Regular Expression (Regex) searches ...
#'   # Find all function definitions that take ax 'x' parameter ...
#'   results <- devs.find.code.instances(
#'     ssFindText = "function\\(.*x.*\\)",
#'     vsTargetLibs = "ggplot2",
#'     sbUseRegex = TRUE
#'   )
#'   
#'   
#'   
#'   ### Example 6: Search for error handling patterns ...
#'   results <- devs.find.code.instances(
#'     ssFindText = "^#.*stop|warning|message|tryCatch",
#'     vsTargetLibs = c("base", "rlang"),
#'     sbUseRegex = TRUE
#'   )
#'   
#'   
#'   
#'   ### Example 7: Case-sensitive search for S3 methods ...
#'   results <- devs.find.code.instances(
#'     ssFindText = "print\\.",
#'     vsTargetLibs = "base",
#'     sbIgnoreCase = FALSE,
#'     sbUseRegex = TRUE
#'   )
#'   
#'   
#'   
#'   ### Example 8: Search for specific variable assignments ...
#'   results <- devs.find.code.instances(
#'     ssFindText = "<-\\s*function\\(",
#'     vsTargetLibs = "ggplot2",
#'     sbUseRegex = TRUE
#'   )
#'   
#'   
#'   
#'   ### Example 9: Create a custom environment and search it ...
#'   env1 <- new.env()
#'   env1$func1 <- function(x) paste("Result:", x)
#'   env1$func2 <- function(y) cat("Output:", y, "\n")
#'   
#'   results <- devs.find.code.instances(
#'     ssFindText = "cat|print",
#'     coRENVs = list(env1),
#'     sbIncludeGlobal = FALSE
#'   )
#'   
#'   
#'   
#'   ### Example 10: Batch search across multiple R Libraries ...
#'   vsTargetLibs_to_search <- c("stats", "utils", "graphics", "grDevices")
#'   results <- devs.find.code.instances(
#'     ssFindText = "par\\(",
#'     vsTargetLibs = vsTargetLibs_to_search,
#'     sbVerboseSearch = TRUE
#'   )
#'   
#'   ### Get statistics ...
#'   cat("\nSearch Statistics:\n")
#'   cat(sprintf("Total matches: %d\n", attr(results, "search_info")$TOTAL_MATCHES))
#'   cat(sprintf("Unique functions: %d\n", attr(results, "search_info")$UNIQUE_FUNCS))
#'   
#'   
#'   
#'   ### Example 11: Search for plotting functions ...
#'   results <- devs.find.code.instances(
#'     ssFindText = "plot\\(",
#'     vsTargetLibs = c("stats", "utils", "graphics", "grDevices", "base"),
#'     sbSearchInternals = TRUE
#'   )
#' }
#' 
#' 
#' 
#' \dontrun{   ### <- Code example below should not be executed during normal "R_CMD_CHECK" code
#'             ###    check procedures - since it causes problems with R Temporary Folders !!!
#'             
#'   ## Visualize results ...
#'   library(ggplot2)   # <- Ensures "ggplot2" is installed on the local machine !!!
#'   if (nrow(results) > 0) {
#'     library(ggplot2)
#'     plot_data <- as.data.frame(table(results$LIBRARY_ID))
#'     ggplot(plot_data, aes(x = Var1, y = Freq)) +
#'       geom_bar(stat = "identity") +
#'       labs(x = "R-Library", y = "Search Term Matches", title = "Search Results by Library ...") +
#'       theme_minimal()
#'   }
#'   
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
  RCT_TAG_FUNC_ID_FULL_ <- "devs.find.code.instances";   # <- FSID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for ALL Functions   ####
  ###    NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasBaseLS                  <- base::ls;
  rasBaseGET                 <- base::get;
  rasBaseREP                 <- base::rep;
  rasBaseSUM                 <- base::sum;
  rasBaseATTR                <- base::attr;
  rasBaseGREP                <- base::grep;
  rasBaseNROW                <- base::nrow;
  rasBaseLIST                <- base::list;
  rasBaseSTOP                <- base::stop;
  rasBaseORDER               <- base::order;
  rasBaseRBIND               <- base::rbind;
  rasBaseIsNA                <- base::is.na;
  rasBaseNCHAR               <- base::nchar;
  rasBaseLENGTH              <- base::length;
  rasBaseRETURN              <- base::return;
  rasBasePASTE0              <- base::paste0;
  rasBaseSAPPLY              <- base::sapply;
  rasBaseSubSTR              <- base::substr;
  rasBaseSPRINTF             <- base::sprintf;
  rasBaseDEPARSE             <- base::deparse;
  rasBaseIsNULL              <- base::is.null;
  rasBaseMISSING             <- base::missing;
  rasBaseINTEGER             <- base::integer;
  rasBaseLOGICAL             <- base::logical;
  rasBaseMESSAGE             <- base::message;
  rasBaseSEQLEN              <- base::seq_len;
  rasBaseWARNING             <- base::warning;
  rasBaseAsFACTOR            <- base::as.factor;
  rasBaseSysTIME             <- base::Sys.time;
  rasBaseTryCATCH            <- base::tryCatch;
  rasBaseCHARACTER           <- base::character;
  rasBaseGlobalENV           <- base::globalenv;
  rasBaseSeqALONG            <- base::seq_along;
  rasBaseDataFRAME           <- base::data.frame;
  rasBaseIsFUNCTION          <- base::is.function;
  rasBaseENVIRONMENT         <- base::environment;
  rasBaseIsCHARACTER         <- base::is.character;
  rasBaseAsNAMESPACE         <- base::asNamespace;
  rasBaseGetNameSpaceEXPORTS <- base::getNamespaceExports;
  
  rasUtilsPackageNAME <- utils::packageName;
  
  
  
  # Validate inputs
  if (rasBaseMISSING(ssFindText) || 
      !rasBaseIsCHARACTER(ssFindText) || rasBaseLENGTH(ssFindText) != 1) {
    rasBaseSTOP("ssFindText must be a single character string")
  }
  
  # Initialize results data frame ...
  results <- rasBaseDataFRAME(
    "LIBRARY_ID"  = rasBaseCHARACTER(0),
    "FUNC_NAME"   = rasBaseCHARACTER(0),
    "IS_EXPORTED" = rasBaseLOGICAL(0),
    "LINE_NUMBER" = rasBaseINTEGER(0),
    "CODE_SNIP"   = rasBaseCHARACTER(0),
    "FILE_NAME"   = rasBaseCHARACTER(0),
    "SEARCH_TERM" = rasBaseCHARACTER(0),
    stringsAsFactors = FALSE
  )
  
  # Helper function to get source file info ...
  rcf_get.source.file <- function(func) {
    
    # Try multiple methods to get source file ...
    src_file <- NA_character_;
    
    # Method 1: Check the `srcref` attribute ...
    srcref <- rasBaseATTR(func, "srcref", exact = TRUE)
    if (!rasBaseIsNULL(srcref)) {
      src_file <- rasBaseTryCATCH({
        rasBaseATTR(srcref, "srcfile", exact = TRUE)$filename
      }, error = function(e) NA_character_)
    }
    
    # Method 2: Check environment attributes
    if (rasBaseIsNA(src_file)) {
      env <- rasBaseENVIRONMENT(func)
      if (!rasBaseIsNULL(env) && !rasBaseIsNULL(rasBaseATTR(env, "name"))) {
        src_file <- rasBaseATTR(env, "name")
      }
    }
    
    # Method 3: Try to get from package source
    if (rasBaseIsNA(src_file)) {
      pkg <- rasUtilsPackageNAME(rasBaseENVIRONMENT(func))
      if (!rasBaseIsNULL(pkg)) {
        src_file <- rasBasePASTE0("package:", pkg)
      }
    }
    
    rasBaseRETURN(src_file)
  }
  
  # Helper function to search a single function ...
  rcf_search.single.func <- function(func, func_name, lib_id, is_exported = TRUE) {
    if (!rasBaseIsFUNCTION(func)) rasBaseRETURN(NULL)
    
    # Get function source code ...
    srcref <- rasBaseATTR(func, "srcref", exact = TRUE)
    start_line_offset <- 0;
    
    if (!rasBaseIsNULL(srcref)) {
      # Use the literal source code preserved in the srcref ...
      func_code <- as.character(srcref);
      # Extract the absolute starting line number from the file ..
      start_line_offset <- as.vector(srcref)[1] - 2;
    } else {
      # Fallback to deparse ONLY if source is not available
      # 'useSource = TRUE' attempts to find the original formatting
      func_code <- rasBaseTryCATCH({
        rasBaseDEPARSE(func, control = c("keepInteger", "keepNA", "useSource"))
      }, error = function(e) rasBaseCHARACTER(0))
    }
    
    if (rasBaseLENGTH(func_code) == 0) rasBaseRETURN(NULL);
    
    # Search for "ssFindText" values ...
    matches <- rasBaseGREP(
      ssFindText, func_code, 
      ignore.case = sbIgnoreCase, 
      perl = sbUseRegex, 
      value = FALSE
    );
    
    if (rasBaseLENGTH(matches) == 0) rasBaseRETURN(NULL);
    
    # Get source file ..
    src_file <- rcf_get.source.file(func)
    
    # Prepare results ...
    func_results <- rasBaseDataFRAME(
      "LIBRARY_ID"  = rasBaseREP(lib_id, rasBaseLENGTH(matches)),
      "FUNC_NAME"   = rasBaseREP(func_name, rasBaseLENGTH(matches)),
      "IS_EXPORTED" = rasBaseREP(is_exported, rasBaseLENGTH(matches)),
      "LINE_NUMBER" = matches + start_line_offset,   # Return absolute line number in source file!!!
      "CODE_SNIP"   = rasBaseCHARACTER(rasBaseLENGTH(matches)),
      "FILE_NAME"   = rasBaseREP(src_file, rasBaseLENGTH(matches)),
      "SEARCH_TERM" = rasBaseREP(ssFindText, rasBaseLENGTH(matches)),
      stringsAsFactors = FALSE
    );
    
    # Extract code snippets ...
    for (i in rasBaseSeqALONG(matches)) {
      line_num <- matches[i]
      line_text <- func_code[line_num]
      
      # Truncate if too long ...
      if (rasBaseNCHAR(line_text) > snRetSnipSize) {
        snippet <- rasBasePASTE0(
          rasBaseSubSTR(line_text, 1, snRetSnipSize - 3), "..."
        );
      } else {
        snippet <- line_text
      }
      
      func_results$CODE_SNIP[i] <- snippet
    }
    
    rasBaseRETURN(func_results)
  }
  
  # Search vsTargetLibs ...
  if (!rasBaseIsNULL(vsTargetLibs)) {
    for (pkg in vsTargetLibs) {
      if (sbVerboseSearch) rasBaseMESSAGE(rasBaseSPRINTF(" -> Searching package: %s", pkg))
      
      # Check if package is loaded/available ...
      if (!requireNamespace(pkg, quietly = TRUE)) {
        rasBaseWARNING(
          rasBaseSPRINTF(" -> Package '%s' not available (not installed locally). Skipping.", pkg)
        )
        next
      }
      
      # Get package namespace ...
      ns <- rasBaseTryCATCH(rasBaseAsNAMESPACE(pkg), error = function(e) NULL)
      if (rasBaseIsNULL(ns)) next
      
      # Get functions to search ...
      if (sbSearchInternals) {
        # Get all objects in namespace ...
        func_names <- rasBaseLS(ns, all.names = TRUE)
      } else {
        # Get only exported functions ...
        func_names <- rasBaseGetNameSpaceEXPORTS(pkg)
      }
      
      # Filter to functions only
      func_names <- func_names[rasBaseSAPPLY(func_names, function(x) {
        obj <- rasBaseTryCATCH(rasBaseGET(x, envir = ns), error = function(e) NULL)
        !rasBaseIsNULL(obj) && rasBaseIsFUNCTION(obj)
      })]
      
      # Search each function
      for (func_name in func_names) {
        func <- rasBaseGET(func_name, envir = ns)
        
        # Check if function is exported
        is_exported <- func_name %in% rasBaseGetNameSpaceEXPORTS(pkg)
        
        func_results <- rcf_search.single.func(func, func_name, pkg, is_exported)
        
        if (!rasBaseIsNULL(func_results) && rasBaseNROW(func_results) > 0) {
          results <- rasBaseRBIND(results, func_results)
        }
      }
      
      if (sbVerboseSearch) {
        rasBaseMESSAGE(
          rasBaseSPRINTF("  -> Found %d matches in %d functions", 
                         rasBaseSUM(results$LIBRARY_ID == pkg), 
                         rasBaseLENGTH(
                           unique(results$FUNC_NAME[results$LIBRARY_ID == pkg])
                         )
          )
        )
      }
    }
  }
  
  # Search environments ...
  for (env_idx in rasBaseSeqALONG(coRENVs)) {
    env <- coRENVs[[env_idx]];
    env_name <- rasBaseTryCATCH({
      if (identical(env, rasBaseGlobalENV())) {
        ".GlobalEnv"
      } else if (!rasBaseIsNULL(rasBaseATTR(env, "name"))) {
        rasBaseATTR(env, "name")
      } else {
        rasBasePASTE0("Environment_", env_idx)
      }
    }, error = function(e) rasBasePASTE0("Environment_", env_idx))
    
    if (sbVerboseSearch) rasBaseMESSAGE(rasBaseSPRINTF(" -> Searching environment: %s", env_name))
    
    # Get all objects in environment ...
    obj_names <- rasBaseTryCATCH(
      rasBaseLS(env, all.names = TRUE), error = function(e) rasBaseCHARACTER(0)
    );
    
    # Search each object that is a function ...
    for (obj_name in obj_names) {
      obj <- rasBaseTryCATCH(rasBaseGET(obj_name, envir = env), error = function(e) NULL)
      
      if (!rasBaseIsNULL(obj) && rasBaseIsFUNCTION(obj)) {
        func_results <- rcf_search.single.func(obj, obj_name, env_name, TRUE)
        
        if (!rasBaseIsNULL(func_results) && rasBaseNROW(func_results) > 0) {
          results <- rasBaseRBIND(results, func_results)
        }
      }
    }
    
    if (sbVerboseSearch) {
      matches_in_env <- rasBaseSUM(results$LIBRARY_ID == env_name)
      rasBaseMESSAGE(rasBaseSPRINTF("  |-> Found %d matches", matches_in_env))
    }
  }
  
  # Search global environment if requested ...
  if (sbIncludeGlobal) {
    if (sbVerboseSearch) rasBaseMESSAGE(" -> Searching global environment")
    
    global_objs <- rasBaseLS(rasBaseGlobalENV(), all.names = TRUE)
    
    for (obj_name in global_objs) {
      obj <- rasBaseGET(obj_name, envir = rasBaseGlobalENV())
      
      if (rasBaseIsFUNCTION(obj)) {
        func_results <- rcf_search.single.func(obj, obj_name, ".GlobalEnv", TRUE)
        
        if (!rasBaseIsNULL(func_results) && rasBaseNROW(func_results) > 0) {
          results <- rasBaseRBIND(results, func_results)
        }
      }
    }
    
    if (sbVerboseSearch) {
      matches_in_global <- rasBaseSUM(results$LIBRARY_ID == ".GlobalEnv")
      rasBaseMESSAGE(rasBaseSPRINTF("  |-> Found %d matches", matches_in_global))
    }
  }
  
  # Sort and finalize results ...
  if (rasBaseNROW(results) > 0) {
    results <- results[rasBaseORDER(results$LIBRARY_ID, results$FUNC_NAME, results$LINE_NUMBER), ]
    base::rownames(results) <- NULL
    
    # Add unique match ID ...
    results$MATCH_ID <- rasBaseSEQLEN(rasBaseNROW(results));
    
    # Reclassify key Variables as Factors ...
    results$LIBRARY_ID  <- rasBaseAsFACTOR(results$LIBRARY_ID);
    results$FUNC_NAME   <- rasBaseAsFACTOR(results$FUNC_NAME);
    results$FILE_NAME   <- rasBaseAsFACTOR(results$FILE_NAME);
    results$SEARCH_TERM <- rasBaseAsFACTOR(results$SEARCH_TERM);
  }
  
  # Add attributes ...
  base::attr(results, "search_info") <- rasBaseLIST(
    "SEARCH_TERM"    = ssFindText,
    "R_LIBRARIES"    = vsTargetLibs,
    "INTERN_SEARCH"  = sbSearchInternals,
    "IS_USE_REGEX"   = sbUseRegex,
    "IS_IGNORE_CASE" = sbIgnoreCase,
    "TIME_STAMP"     = rasBaseSysTIME(),
    "TOTAL_MATCHES"  = rasBaseNROW(results),
    "UNIQUE_FUNCS"   = rasBaseLENGTH(unique(results$FUNC_NAME)),
    "UNIQUE_LIBS"    = rasBaseLENGTH(unique(results$LIBRARY_ID))
  );
  
  class(results) <- c("Search_Results", "data.frame");
  
  rasBaseRETURN(results);
  
}


