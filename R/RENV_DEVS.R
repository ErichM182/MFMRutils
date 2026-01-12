#? ### ### ### ### ### ### ###
#' @title The "SuiteMFMR" List of Useful Constants
#' @name RENV_DEVS
#' @family SuiteMFMR CONSTANTS
#' 
#' 
#' @description
#' A collection of useful CONSTANTS that are utilized throughout the "MFMR Suite of R Packages" 
#' (i.e. "SuiteMFMR"). This Environment-LOCKED R List standardizes and improves the use of key 
#' values required by multiple "SuiteMFMR" functions.
#' 
#' 
#' @section CONSTANTS
#'
#'
#' @returns 
#' This R Object returns an R Environment-Locked List of CONSTANTS (encapsulating character, numeric
#' and other R Objects <values>) used extensively throughout the "SuiteMFMR" R Project.
#' 
#'
#' @examples
#' ### Install the required R Library ...
#' require(MFMRutils)   # <- Ensures the "MFMRutils" library is installed locally !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' RENV_DEVS$PATH_FOLDER_WIP_PROD
#' 
#' ### ... then easily apply TEXT FORMATS as follows ...
#' RENV_DEVS$PATH_FOLDER_WIP_PROD   # -> Returns the Work-In-Progress Project PATH (i.e. "./WIP) 
#'                                  #    for the R Library Code Development Staging Folder (i.e. 
#'                                  #    "./WIP/01_NextForPROD") !!!
#'
#'
#' @export
#? ### ### ###
"RENV_DEVS" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of "ANSI" Text Formats !!!
  envList <- base::list2env(
    base::list(
      ### Paths to DEVELOPMENT FOLDER (in regent R-Library Project) ...
      PATH_TO_FOLDER_WIP         = "./WIP",
      PATH_TO_FOLDER_WIP_HELPERS = "./WIP/00_Helpers",
      PATH_TO_FOLDER_WIP_PROD    = "./WIP/01_NextForPROD",
      
      ### Paths to DEVELOPMENT FILES (in regent R-Library Project) ...
      PATH_TO_FILE_GIT_IGNORE         = "./.gitignore",
      PATH_TO_FILE_R_PACKAGE_DESC     = "./DESCRIPTION",
      PATH_TO_FILE_R_BUILD_IGNORE     = "./.Rbuildignore",
      PATH_TO_FILE_ACT_DEV_INFO_TRCKR = "./WIP/00_ACT_DEV_TRCKR.txt",
      
      FORMAT_TIME_DEV_LOG_V01 = "%Y.%m.%d",
      FORMAT_TIME_DEV_LOG_V02 = "%H:%M:%OS3 %Z",
      FORMAT_TIME_DEV_LOG_V03 = "%d %b %Y @ %H:%M:%OS3 %Z"
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS -> DO NOT use `base::return(envList)` !!!
  
}
