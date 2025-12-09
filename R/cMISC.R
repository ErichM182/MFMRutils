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
#' ### Install the required R Library ...
#' require(MFMRutils)   # <- Ensures the "MFMRutils" library is installed locally !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils:::cMISC$PATH_FOLDER_WIP_PROD
#' 
#' ### ... then easily apply TEXT FORMATS as follows ...
#' MFMRutils:::cMISC$PATH_FOLDER_WIP_PROD   ### -> Returns the Work-In-Progress PATH (i.e. "./WIP) 
#'                                          ###    for the R Library Code Development Staging 
#'                                          ###    Folder (i.e. "./WIP/01_NextForPROD") !!!
#'
#' @keywords internal
#' @noRd
#? ### ### ###
"cMISC" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of "ANSI" Text Formats !!!
  envList <- base::list2env(
    base::list(
      PATH_TO_FOLDER_WIP              = "./WIP",
      FORMAT_TIME_DEV_LOG_V01         = "%Y.%m.%d",
      PATH_TO_FILE_GIT_IGNORE         = "./.gitignore",
      FORMAT_TIME_DEV_LOG_V02         = "%H:%M:%OS3 %Z",
      PATH_TO_FILE_R_PACKAGE_DESC     = "./DESCRIPTION",
      PATH_TO_FILE_R_BUILD_IGNORE     = "./.Rbuildignore",
      PATH_TO_FOLDER_WIP_HELPERS      = "./WIP/00_Helpers",
      PATH_TO_FOLDER_WIP_PROD         = "./WIP/01_NextForPROD",
      FORMAT_TIME_DEV_LOG_V03         = "%d %b %Y @ %H:%M:%OS3 %Z",
      PATH_TO_FILE_ACT_DEV_INFO_TRCKR = "./WIP/00_ACT_DEV_TRCKR.txt"
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
