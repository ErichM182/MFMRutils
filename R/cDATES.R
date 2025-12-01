#? ### ### ### ### ### ### ###
#' @title Date-Time Formats for use in R (the "SuiteMFMR" selection)
#' @name cDATES
#' 
#' @description
#' A collection of frequently used Date and Date-Time (text) Formats to support
#' the "MFMR Suite of R Functions" (aka "SuiteMFMR").
#'
#' @examples
#' ### Load the required R Library ...
#' library(MFMRutils)   # <- Loads library (if already installed locally) !!!
#' 
#' ### ... then easily assign DATE FORMATS as follows ...
#' cDATES$SHORT_V01   ### -> formats Date to "Saturday, January 16 1982" !!!
#' cDATES$LONG_V02    ### -> formats Date to "Sat, Jan 16 1982 @ 21:57:38" !!!
#' cDATES$LONG_V03    ### -> formats Date to "Sat, 16 Jan 1982 @ 21:57:38" !!!
#' cDATES$SHORT_V03   ### -> formats Date to "Sat, 16 Jan 1982" !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::cDATES$SHORT_V02   ### -> formats Date to "Sat, Jan 16 1982" !!!
#'
#' @export
#? ### ### ###
"cDATES" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of Date-Time Text Formats !!!
  envList <- base::list2env(
    base::list(
      SHORT_V01 = "%A, %d %B %Y",        # -> Output: "Saturday, January 16 1982" !!!
      SHORT_V02 = "%a, %b %d %Y",        # -> Output: "Sat, Jan 16 1982" !!!
      SHORT_V03 = "%a, %d %b %Y",        # -> Output: "Sat, 16 Jan 1982" !!!
      LONG_V01  = "%A, %d %B %Y @ %X",   # -> Output: "Saturday, January 16 1982 @ 21:57:38" !!!
      LONG_V02  = "%a, %b %d %Y @ %X",   # -> Output: "Sat, Jan 16 1982 @ 21:57:38" !!!
      LONG_V03  = "%a, %d %b %Y @ %X"    # -> Output: "Sat, 16 Jan 1982 @ 21:57:38" !!!
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
