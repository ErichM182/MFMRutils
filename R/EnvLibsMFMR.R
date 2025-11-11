#? ### ### ### ### ### ### ###
#' @title The MFMR Suite of R Libraries ...
#' @name EnvLibsMFMR
#' @description
#' The complete list of R Libraries that make up the "MFMR R Library Suite". This
#' Environment-Locked List contains the "standard" IDs for the MFMR R Libraries.
#'
#' @examples
#' ### Easily print & assign Date Formats as follows ...
#' EnvLibsMFMR$UTILS   ### -> formats Date to "Saturday, January 16 1982" !!!
#' EnvLibsMFMR$UTILS   ### -> formats Date to "Sat, Jan 16 1982" !!!
#' EnvLibsMFMR$UTILS   ### -> formats Date to "Sat, 16 Jan 1982" !!!
#'
#' @export
#? ### ### ###
"EnvLibsMFMR" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of MFMR R Suite Library Identifiers (IDs) !!!
  envList <- base::list2env(
    base::list(
      ADMIN = "MFMRadmin",   ### -> ... !!!
      DATA  = "MFMRdata",    ### -> ... !!!
      MAPS  = "MFMRmaps",    ### -> ... !!!
      PLOTS = "MFMRplots",   ### -> ... !!!
      STATS = "MFMRstats",   ### -> ... !!!
      UTILS = "MFMRutils"    ### -> ... !!!
    )
  );
  
  # Setting the R Environment 'Bindings = TRUE' makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
