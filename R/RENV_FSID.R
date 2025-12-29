#? ### ### ### ### ### ### ###
#' @title List of Function Self-ID Argument Tags ("SuiteMFMR" DevTools)
#' @name RENV_FSID
#' @family SuiteMFMR Constants
#' 
#' 
#' @description
#' A collection of frequently used ANSI (American National Standards Institute)
#' Text Font Formats to support the "MFMR Suite of R Functions" (aka "SuiteMFMR").
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
#' ### Install the required R Library ...
#' require(MFMRutils)   # <- Ensures the "MFMRutils" library is installed locally !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' RENV_FSID$PROJ_ID     ### -> sets the text font format to BOLD !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::RENV_FSID$PROJ_ID     ### -> sets the text font format to BOLD !!!
#' MFMRutils::RENV_FSID$PROJ_ID     ### -> sets the text font format to BOLD !!!
#' MFMRutils::RENV_FSID$FUNC_MODE   ### -> sets the text font format to ITALICS !!!
#' MFMRutils::RENV_FSID$TIME_STOP   ### -> removes actively applied ANSI Text Formatting !!!
#'
#'
#' @export
#? ### ### ###
"RENV_FSID" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of "ANSI" Text Formats !!!
  envList <- base::list2env(
    base::list(
      PROJ_ID           = "ssProjID", 
      FUNC_CELN_STOP    = "siStopCELN",
      FUNC_CELN_START   = "siStartCELN", 
      FUNC_SID          = "ssFuncSelfID", 
      FUNC_MODE         = "siFuncMode01", 
      FUNC_CALLER       = "ssFuncCallerID", 
      TIME_START        = "csTimeStart", 
      TIME_STOP         = "csTimeStop", 
      TIME_FORMAT       = "csFormatDT", 
      ICON_CARAT        = "csIconCarat", 
      ICON_SPLIT        = "csIconSplit", 
      COLOR_CARAT       = "csColorCarat", 
      COLOR_SPLIT       = "csColorSplit",
      COLOR_PROJ_ID     = "csColorProjID", 
      COLOR_MAIN_TEXT   = "csColorMainText",
      COLOR_FUNC_TYPE   = "csColorFuncType", 
      COLOR_FUNC_CALLER = "csColorCallerID", 
      COLOR_TIME_STAMP  = "csColorTimeStamp",
      IS_PRINT_PRETTY   = "sbPrintPretty",
      IS_RUN_SELF_ID    = "sbRunSelfID",
      IS_DEBUG_MODE     = "rsbRunModeDEBUG_"
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
