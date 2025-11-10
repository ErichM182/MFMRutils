#? ### ### ### ### ### ### ###
#' @title The Date-Time Formats for use in R (the `MFMRutils` selection)
#' @name MFMRDates
#' @description
#' A collection of frequently used Date-Time (text) Formats to support the MFMR
#' Suite of R Functions.
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' MFMRDates$BOLD      ### -> sets the text font format to BOLD !!!
#' MFMRDates$ITALICS   ### -> sets the text font format to ITALICS !!!
#' MFMRDates$RESET     ### -> removes actively applied ANSI Text Formatting !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::MFMRFormat$BOLD   ### -> sets the text font format to BOLD !!!
#'
#' @export
#? ### ### ###
"MFMRDates" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of Date-Time Text Formats !!!
  envList <- base::list2env(
    base::list(
      LONGv01 = "%A, %d %B %Y @ %X",   ### -> Output: "Saturday, January 16 1982 @ 21:57:38" !!!
      LONGv02 = "%a, %b %d %Y @ %X",   ### -> Output: "Sat, Jan 16 1982 @ 21:57:38" !!!
      LONGv03 = "%a, %d %b %Y @ %X"    ### -> Output: "Sat, 16 Jan 1982 @ 21:57:38" !!!
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
