#? ### ### ### ### ### ### ###
#' @title ANSI Text Formats for use in R (the "SuiteMFMR" selection)
#' @name EnvFORMATS
#' 
#' @description
#' A collection of frequently used ANSI (American National Standards Institute)
#' Text Font Formats to support the "MFMR Suite of R Functions" (aka "SuiteMFMR").
#'
#' @examples
#' ### Load the required R Library ...
#' library(MFMRutils)   # <- Loads library (if already installed locally) !!!
#' 
#' ### ... then easily apply TEXT FORMATS as follows ...
#' EnvFORMATS$BOLD      ### -> sets the text font format to BOLD !!!
#' EnvFORMATS$ITALICS   ### -> sets the text font format to ITALICS !!!
#' EnvFORMATS$RESET     ### -> removes actively applied ANSI Text Formatting !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::EnvFORMATS$BOLD   ### -> sets the text font format to BOLD !!!
#'
#' @export
#? ### ### ###
"EnvFORMATS" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of "ANSI" Text Formats !!!
  envList <- base::list2env(
    base::list(
      RESET     = "\033[0m",   ### Output: Normal text !!!
      BOLD      = "\033[1m",   ### Output: BOLD text !!!
      ITALICS   = "\033[3m",   ### Output: Italicized text !!!
      UNDERLINE = "\033[4m",   ### Output: Underlined text !!!
      REVERSE   = "\033[7m"    ### Output: Inverted text (character order) !!!
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
