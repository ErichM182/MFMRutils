#? ### ### ### ### ### ### ###
#' @title Date-Time Formats for use in R (the "SuiteMFMR" selection)
#' @name EnvDATES
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
#' EnvDATES$SHORTv01   ### -> formats Date to "Saturday, January 16 1982" !!!
#' EnvDATES$LONGv02    ### -> formats Date to "Sat, Jan 16 1982 @ 21:57:38" !!!
#' EnvDATES$LONGv03    ### -> formats Date to "Sat, 16 Jan 1982 @ 21:57:38" !!!
#' EnvDATES$SHORTv03   ### -> formats Date to "Sat, 16 Jan 1982" !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::EnvDATES$SHORTv02   ### -> formats Date to "Sat, Jan 16 1982" !!!
#'
#' @export
#? ### ### ###
"EnvDATES" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of Date-Time Text Formats !!!
  envList <- base::list2env(
    base::list(
      SHORTv01 = "%A, %d %B %Y",        # -> Output: "Saturday, January 16 1982" !!!
      SHORTv02 = "%a, %b %d %Y",        # -> Output: "Sat, Jan 16 1982" !!!
      SHORTv03 = "%a, %d %b %Y",        # -> Output: "Sat, 16 Jan 1982" !!!
      LONGv01  = "%A, %d %B %Y @ %X",   # -> Output: "Saturday, January 16 1982 @ 21:57:38" !!!
      LONGv02  = "%a, %b %d %Y @ %X",   # -> Output: "Sat, Jan 16 1982 @ 21:57:38" !!!
      LONGv03  = "%a, %d %b %Y @ %X"    # -> Output: "Sat, 16 Jan 1982 @ 21:57:38" !!!
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
