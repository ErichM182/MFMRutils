#? ### ### ### ### ### ### ###
#' @title The "SuiteMFMR" List of Useful Constants
#' @name cMISC
#' 
#' @description
#' A collection of surprisingly useful CONSTANTS that are required throughout the
#' "SuiteMFMR" R Project. This Environment-LOCKED R List standardizes & improves
#' the extraction & use of key values across multiple functions of the "MFMR Suite
#' of R Libraries (aka "SuiteMFMR").
#'
#' @examples
#' ### Load the required R Library ...
#' library(MFMRutils)   # <- Loads library (if already installed locally) !!!
#' 
#' ### ... then easily apply TEXT FORMATS as follows ...
#' cMISC$PATH_FOLDER_WIP_PROD   # -> Returns the Work-In-Progress (WIP) 
#'                              #    path for the R Library Development
#'                              #    Staging Folder ("01_NextForPROD") !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::cMISC$PATH_FOLDER_WIP_PROD
#'
#' @export
#? ### ### ###
"cMISC" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of "ANSI" Text Formats !!!
  envList <- base::list2env(
    base::list(
      FORMAT_TIME_DEV_LOG_V01  = "%Y.%m.%d",
      FORMAT_TIME_DEV_LOG_V02  = "%H:%M:%OS3 %Z",
      PATH_FILE_R_PKG_DESC     = "./DESCRIPTION",
      PATH_FOLDER_WIP_HELPERS  = "./WIP/00_Helpers",
      PATH_FOLDER_WIP_PROD     = "./WIP/01_NextForPROD",
      PATH_FILE_WIP_TIME_STAMP = "./WIP/DevsVersTimeStamp.txt"
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
