#? ### ### ### ### ### ### ###
#' @title Full List of "SuiteMFMR" R Project Packages
#' @name RENV_SuiteMFMR
#' @family SuiteMFMR CONSTANTS
#' 
#' 
#' @description
#' The complete list of R Libraries that constitute the "SuiteMFMR" R Project. This Environment-
#' Locked List contains the "standard" IDs (i.e. "tags" or names) and URLs (i.e. the online source 
#' repository links) for the "MFMR Suite of R Packages" (a.k.a. "SuiteMFMR").
#' 
#' 
#' @section CONSTANTS
#'
#'
#' @returns 
#' This R Object returns an R Environment-Locked List of lists containing the names and URLs of all
#' the R Libraries that form part of the "SuiteMFMR" R Project.
#' 
#'
#' @examples
#' ### Load the required R Library ...
#' library(MFMRutils)   # <- Loads library (if already installed locally) !!!
#' 
#' ### ... then easily extract the "SuiteMFMR" R Library IDs as follows ...
#' RENV_SuiteMFMR$UTILS   # -> extracts the "Utils" MFMR R Library Name and URL information !!!
#' RENV_SuiteMFMR$PLOTS   # -> extracts the "Plots" MFMR R Library Name and URL information !!!
#' RENV_SuiteMFMR$STATS   # -> extracts the "Stats" MFMR R Library Name and URL information !!!
#'
#'
#' @export
#? ### ### ###
"RENV_SuiteMFMR" <- {   # <- MUST BE LIKE THIS -> DO NOT use `function(){}` !!!
  
  # -> Define a static list of MFMR R Suite Library Identifiers (IDs) or TAGs !!!
  renvList <- base::list2env(
    base::list(
      SUITE = base::list(
        NAME = "SuiteMFMR", URL = "https://github.com/swat-grn-mfmr/SuiteMFMR"
      ),
      ADMIN = base::list(
        NAME = "MFMRadmin", URL = "https://github.com/swat-grn-mfmr/MFMRadmin"
      ),
      DATA  = base::list(
        NAME = "MFMRdata",  URL = "https://github.com/swat-grn-mfmr/MFMRdata"
      ),
      MAPS  = base::list(
        NAME = "MFMRmaps",  URL = "https://github.com/swat-grn-mfmr/MFMRmaps"
      ),
      PLOTS = base::list(
        NAME = "MFMRplots", URL = "https://github.com/swat-grn-mfmr/MFMRplots"
      ),
      STATS = base::list(
        NAME = "MFMRstats", URL = "https://github.com/swat-grn-mfmr/MFMRstats"
      ),
      UTILS = base::list(
        NAME = "MFMRutils", URL = "https://github.com/swat-grn-mfmr/MFMRutils"
      )
    )
  );
  
  # Setting the R Environment 'Bindings = TRUE' makes the bindings immutable ...
  base::lockEnvironment(renvList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  renvList;   # <- MUST BE LIKE THIS -> DO NOT use `base::return(envList)` !!!
}
