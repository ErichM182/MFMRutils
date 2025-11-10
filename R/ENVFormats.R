#? ### ### ### ### ### ### ###
#' @title ANSI Text Formats for use in R (the `MFMRutils` selection)
#' @name ENVFormats
#' @description
#' A collection of frequently used ANSI (American National Standards Institute)
#' Text Font Formats to support the MFMR Suite of R Functions.
#'
#' @usage ENVFormats   ### -> if "MFMRutils" is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' ENVFormats$BOLD      ### -> sets the text font format to BOLD !!!
#' ENVFormats$ITALICS   ### -> sets the text font format to ITALICS !!!
#' ENVFormats$RESET     ### -> removes actively applied ANSI Text Formatting !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::ENVFormats$BOLD   ### -> sets the text font format to BOLD !!!
#'
#' @export
#? ### ### ###
"ENVFormats" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
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
