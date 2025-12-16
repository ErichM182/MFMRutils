#? ### ### ### ### ### ### ###
#' @title Full List of "SuiteMFMR" R Packages
#' @name cMFMR
#' 
#' @description
#' The complete list of R Libraries that make up the "MFMR R Library Suite". This
#' Environment-Locked List contains the "standard" IDs <tags> for the "MFMR Suite 
#' of R Libraries" (aka "SuiteMFMR").
#'
#' @examples
#' ### Load the required R Library ...
#' library(MFMRutils)   # <- Loads library (if already installed locally) !!!
#' 
#' ### ... then easily extract the "SuiteMFMR" R Library IDs as follows ...
#' cMFMR$UTILS   # -> extracts the "Utils" MFMR R Library Identifier !!!
#' cMFMR$PLOTS   # -> extracts the "Plots" MFMR R Library Identifier !!!
#' cMFMR$STATS   # -> extracts the "Stats" MFMR R Library Identifier !!!
#'
#' @export
#? ### ### ###
"cMFMR" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of MFMR R Suite Library Identifiers (IDs) or TAGs !!!
  renvList <- base::list2env(
    base::list(
      SUITE = "SuiteMFMR",   # -> ... !!!
      ADMIN = "MFMRadmin",   # -> ... !!!
      DATA  = "MFMRdata",    # -> ... !!!
      MAPS  = "MFMRmaps",    # -> ... !!!
      PLOTS = "MFMRplots",   # -> ... !!!
      STATS = "MFMRstats",   # -> ... !!!
      UTILS = "MFMRutils"    # -> ... !!!
    )
  );
  
  # Setting the R Environment 'Bindings = TRUE' makes the bindings immutable ...
  base::lockEnvironment(renvList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  renvList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
