#? ### ### ### ### ### ### ###
#' @title List of Function Self-ID Argument Tags ("SuiteMFMR" DevTools)
#' @name RENV_FSID
#' @family SuiteMFMR CONSTANTS
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
      ### Function Arguments ('FARGS') -> for the Function Self-Id (FSID) Process ...
      F_ARGS_PROJ_ID                = "ssProjID", 
      F_ARGS_FUNC_ID                = "ssFuncID", 
      F_ARGS_FUNC_CELN_STOP         = "siStopCELN",
      F_ARGS_FUNC_CELN_START        = "siStartCELN", 
      F_ARGS_FUNC_SID               = "ssFuncSelfID", 
      F_ARGS_FUNC_MODE              = "siFuncMode01", 
      F_ARGS_FUNC_CALLER            = "ssFuncCallerID", 
      F_ARGS_FUNC_CALLER_CELN       = "siCallCELN", 
      F_ARGS_TIME_START             = "csTimeStart", 
      F_ARGS_TIME_STOP              = "csTimeStop", 
      F_ARGS_TIME_FORMAT            = "csFormatDT", 
      F_ARGS_ICON_CARAT             = "csIconCarat", 
      F_ARGS_ICON_SPLIT             = "csIconSplit", 
      F_ARGS_ICON_TAIL              = "csIconTail", 
      F_ARGS_COLOR_CARAT            = "csColorCarat",
      F_ARGS_COLOR_SPLIT            = "csColorSplit",
      F_ARGS_COLOR_TAIL             = "csColorTail",
      F_ARGS_COLOR_TEXT_HEAD        = "csColorHead",
      F_ARGS_COLOR_TEXT_BODY        = "csColorBody",
      F_ARGS_COLOR_PROJ_ID          = "csColorProjID",
      F_ARGS_COLOR_MAIN_TEXT        = "csColorMainText",
      F_ARGS_COLOR_FUNC_TYPE        = "csColorFuncType", 
      F_ARGS_COLOR_FUNC_CALLER      = "csColorCallerID", 
      F_ARGS_COLOR_TIME_STAMP       = "csColorTimeStamp",
      F_ARGS_BOOL_PRINT_PRETTY      = "sbPrintPretty",
      F_ARGS_BOOL_SHOW_TAIL         = "sbShowTail",
      F_ARGS_BOOL_RUN_SELF_ID       = "sbRunSelfID",
      F_ARGS_BOOL_POST_ALWAYS       = "sbPostAlways",
      F_ARGS_BOOL_NEW_LINE_PRE_ONE  = "sbPrePend1NL",
      F_ARGS_BOOL_NEW_LINE_POST_ONE = "sbPostPend1NL",
      F_ARGS_BOOL_NEW_LINE_POST_TWO = "sbPostPend2NLs",
      
      ### R Function & Project Constants ('CONSTS') -> for the Function Self-Id (FSID) Process ...
      CONSTS_LIBR_ID             = "RCT_TAG_R_LIBR_ID_",
      CONSTS_CELN_STOP           = "RCT_INT_CELN_STOP_",
      CONSTS_CELN_START          = "RCT_INT_CELN_START_",
      CONSTS_FUNC_ID_LONG        = "RCT_TAG_R_FUNC_ID_LONG_",
      CONSTS_FUNC_ID_SHORT       = "RCT_TAG_R_FUNC_ID_SHORT_",
      CONSTS_PROJ_ID_SHORT       = "RCT_TAG_R_PROJ_ID_SHORT_",
      CONSTS_BOOL_IS_DEBUG       = "RCT_IS_DEBUG_RT_MODE_",     # <- 'RT' == 'Run Time'.
      CONSTS_BOOL_IS_VERBOSE     = "RCT_IS_VERBOSE_RT_MODE_",   # <- 'RT' == 'Run Time'.
      CONSTS_FUNC_RUN_TIME_STOP  = "RCT_DBL_R_FUNC_RT_STOP_",   # <- 'RT' == 'Run Time'.
      CONSTS_FUNC_RUN_TIME_START = "RCT_DBL_R_FUNC_RT_START_"   # <- 'RT' == 'Run Time'.
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
