#? ### ### ### ### ### ### ###
#' @title The MFMR Suite <list> of R Libraries ...
#' @name EnvSuiteMFMR
#' 
#' @description
#' The complete list of R Libraries that make up the "MFMR R Library Suite". This
#' Environment-Locked List contains the "standard" IDs for the MFMR R Libraries.
#'
#' @examples
#' ### Load the required R Library ...
#' library(MFMRutils)   # <- Loads library (if already installed locally) !!!
#' 
#' ### Easily extract the 'SuiteMFMR' R Library Identifiers <tags> as follows ...
#' EnvSuiteMFMR$UTILS   # -> extracts the 'Utils' MFMR R Library Identifier !!!
#' EnvSuiteMFMR$PLOTS   # -> extracts the 'Plots' MFMR R Library Identifier !!!
#' EnvSuiteMFMR$STATS   # -> extracts the 'Stats' MFMR R Library Identifier !!!
#'
#' @export
#? ### ### ###
"EnvSuiteMFMR" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of MFMR R Suite Library Identifiers (IDs) !!!
  envList <- base::list2env(
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
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
