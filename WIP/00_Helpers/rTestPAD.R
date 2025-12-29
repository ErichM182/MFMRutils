#######|       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Test-Pad > R SCRIPT   <-       |#######
###  -> Use this R Script <file> purely for Testing Purposes during Code Development Cycle !!!  ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######



### Use the "Path-Cleaning" Function as follows: ...
library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...



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
#' 
#' ### Example 2: Search with the use of Regular Expressions (RegEx) ...
#' results <- devs.find.code.instances(
#'   ssFindText = "^#.*TODO|FIXME|DEPRECATED",
#'   vsTargetLibs = c("dplyr", "tidyr"),
#'   sbUseRegex = TRUE, sbIgnoreCase = TRUE
#' )
#' 
#' 
#' 
#' ### Example 3: Search internal R functions ...
#' results <- devs.find.code.instances(
#'   ssFindText = "lapply",
#'   vsTargetLibs = "base",
#'   sbSearchInternals = TRUE,
#'   snRetSnipSize = 80
#' )
#' 
#' 
#' 
#' ### Example 4: Search specific environments ... (# <- TODO: This DOES NOT WORK - Fix Problem !!!)
#' my_env <- new.env()
#' my_env$custom_func <- function(x) {
#'   # TODO: optimize this !!!
#'   result <- x * 2
#'   print(result)
#'   return(result)
#' }
#' #' 
#' results <- devs.find.code.instances(
#'   ssFindText = "TODO:",
#'   coRENVs = list(my_env),
#'   sbIncludeGlobal = TRUE
#' )
#' 
#' 
#' 
#' ### Example 5: Complex Regular Expression (Regex) searches ...
#' # Find all function definitions that take ax 'x' parameter ...
#' results <- devs.find.code.instances(
#'   ssFindText = "function\\(.*x.*\\)",
#'   vsTargetLibs = "ggplot2",
#'   sbUseRegex = TRUE
#' )
#' 
#' 
#' 
#' ### Example 6: Search for error handling patterns ...
#' results <- devs.find.code.instances(
#'   ssFindText = "^#.*stop|warning|message|tryCatch",
#'   vsTargetLibs = c("base", "rlang"),
#'   sbUseRegex = TRUE
#' )
#' 
#' 
#' 
#' ### Example 7: Case-sensitive search for S3 methods ...
#' results <- devs.find.code.instances(
#'   ssFindText = "print\\.",
#'   vsTargetLibs = "base",
#'   sbIgnoreCase = FALSE,
#'   sbUseRegex = TRUE
#' )
#' 
#' 
#' 
#' ### Example 8: Search for specific variable assignments ...
#' results <- devs.find.code.instances(
#'   ssFindText = "<-\\s*function\\(",
#'   vsTargetLibs = "ggplot2",
#'   sbUseRegex = TRUE
#' )
#' 
#' 
#' 
#' ### Example 9: Create a custom environment and search it ...
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
#' 
#' 
#' ### Example 10: Batch search across multiple R Libraries ...
#' vsTargetLibs_to_search <- c("stats", "utils", "graphics", "grDevices")
#' results <- devs.find.code.instances(
#'   ssFindText = "par\\(",
#'   vsTargetLibs = vsTargetLibs_to_search,
#'   sbVerboseSearch = TRUE
#' )
#' 
#' ### Get statistics ...
#' cat("\nSearch Statistics:\n")
#' cat(sprintf("Total matches: %d\n", attr(results, "search_info")$TOTAL_MATCHES))
#' cat(sprintf("Unique functions: %d\n", attr(results, "search_info")$UNIQUE_FUNCS))
#' 
#' 
#' 
#' ### Example 11: Search for plotting functions ...
#' results <- devs.find.code.instances(
#'   ssFindText = "plot\\(",
#'   vsTargetLibs = c("stats", "utils", "graphics", "grDevices", "base"),
#'   sbSearchInternals = TRUE
#' )
#' 
#' \dontrun{   ### <- Code example below should not be executed during normal "R_CMD_CHECK" code
#'             ###    check procedures - since it causes problems with R Temporary Folders !!!
#'   # Visualize results ...
#'   library(ggplot2)   # <- Ensures "ggplot2" is installed on the local machine !!!
#'   if (nrow(results) > 0) {
#'     library(ggplot2)
#'     plot_data <- as.data.frame(table(results$LIBRARY_ID))
#'     ggplot(plot_data, aes(x = Var1, y = Freq)) +
#'       geom_bar(stat = "identity") +
#'       labs(x = "R-Library", y = "Search Term Matches", title = "Search Results by Library ...") +
#'       theme_minimal()
#'   }
#' }
