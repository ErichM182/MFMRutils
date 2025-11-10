#? ### ### ### ### ### ### ###
#' @title ANSI Text Formats for use in R (the `MFMRutils` selection)
#' @name MFMRFormat
#' @description
#' A collection of frequently used ANSI (American National Standards Institute)
#' Text Font Formats to support the MFMR Suite of R Functions.
#'
#' @usage MFMRFormat   ### -> if "MFMRutils" is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' MFMRFormat$BOLD      ### -> sets the text font format to BOLD !!!
#' MFMRFormat$ITALICS   ### -> sets the text font format to ITALICS !!!
#' MFMRFormat$RESET     ### -> removes actively applied ANSI Text Formatting !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::MFMRFormat$BOLD   ### -> sets the text font format to BOLD !!!
#'
#' @export
#? ### ### ###
"MFMRFormat" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
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
