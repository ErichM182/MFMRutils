#? ### ### ### ### ### ### ###
#' @title The Func-Self-ID Args List (for use with "SuiteMFMR")
#' @name EnvArgsFSID
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
"EnvArgsFSID" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
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
      IS_DEBUG_MODE     = "sbDebugMode"
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
