#? ### ### ### ### ### ### ###
#' @title Text & Date Formats for use in R (the "SuiteMFMR" selection)
#' @name RENV_FORMATS
#' @family SuiteMFMR Constants
#' 
#' 
#' @description
#' A collection of frequently used ANSI (American National Standards Institute) Text Font and POSIX 
#' Date Formats to support the "MFMR Suite of R Functions" (a.k.a. "SuiteMFMR").
#' 
#' 
#' @section CONSTANTS
#'
#'
#' @returns 
#' This R Object returns an R Environment-Locked List of
#' 
#'
#' @examples
#' ### Load the required R Library ...
#' library(MFMRutils)   # <- Loads library (if already installed locally) !!!
#' 
#' ### ... then easily apply TEXT FORMATS as follows ...
#' RENV_FORMATS$ANSI_BOLD      # -> sets the text font format to ANSI_BOLD !!!
#' RENV_FORMATS$ANSI_ITALICS   # -> sets the text font format to ANSI_ITALICS !!!
#' RENV_FORMATS$ANSI_RESET     # -> removes actively applied ANSI Text Formatting !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::RENV_FORMATS$ANSI_BOLD   # -> sets the text font format to ANSI_BOLD !!!
#'
#'
#' @export
#? ### ### ###
"RENV_FORMATS" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of "ANSI" Text Formats !!!
  envList <- base::list2env(
    base::list(
      ### ANSI Text Formats ...
      ANSI_RESET     = "\033[0m",   # <- ANSI Form: Normal text !!!
      ANSI_BOLD      = "\033[1m",   # <- ANSI Form: Bold text !!!
      ANSI_ITALICS   = "\033[3m",   # <- ANSI Form: Italicized text !!!
      ANSI_UNDERLINE = "\033[4m",   # <- ANSI Form: Underlined text !!!
      ANSI_REVERSE   = "\033[7m",   # <- ANSI Form: Inverted text (character order) !!!
      
      ### POSIX Date-Time (DT) Formats ...
      DATE_SHORT_V01 = "%A, %d %B %Y",        # <- DT Form: `Saturday, January 16 1982` !!!
      DATE_SHORT_V02 = "%a, %b %d %Y",        # <- DT Form: `Sat, Jan 16 1982` !!!
      DATE_SHORT_V03 = "%a, %d %b %Y",        # <- DT Form: `Sat, 16 Jan 1982` !!!
      DATE_LONG_V01  = "%A, %d %B %Y @ %X",   # <- DT Form: `Saturday, January 16 1982 @ 21:57:38`.
      DATE_LONG_V02  = "%a, %b %d %Y @ %X",   # <- DT Form: `Sat, Jan 16 1982 @ 21:57:38` !!!
      DATE_LONG_V03  = "%a, %d %b %Y @ %X"    # <- DT Form: `Sat, 16 Jan 1982 @ 21:57:38` !!!
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
