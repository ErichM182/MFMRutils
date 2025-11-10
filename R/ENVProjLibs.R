#? ### ### ### ### ### ### ###
#' @title The MFMR Suite of R Libraries ...
#' @name ENVLibsMFMR
#' @description
#' The complete list of R Libraries that make up the "MFMR R Library Suite". This
#' Environment-Locked List contains the "standard" IDs for the MFMR R Libraries.
#'
#' @examples
#' ### Easily print & assign Date Formats as follows ...
#' ENVLibsMFMR$UTILS   ### -> formats Date to "Saturday, January 16 1982" !!!
#' ENVLibsMFMR$UTILS   ### -> formats Date to "Sat, Jan 16 1982" !!!
#' ENVLibsMFMR$UTILS   ### -> formats Date to "Sat, 16 Jan 1982" !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::ENVFormats$BOLD   ### -> sets the text font format to BOLD !!!
#'
#' @export
#? ### ### ###
"ENVLibsMFMR" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of Date-Time Text Formats !!!
  envList <- base::list2env(
    base::list(
      UTILS = "MFMRutils",   ### -> ... !!!
      ADMIN = "MFMRadmin",   ### -> ... !!!
      PLOTS = "MFMRplots",   ### -> ... !!!
      MAPS  = "MFMRmaps",    ### -> ... !!!
      STATS = "MFMRstats",   ### -> ... !!!
      DATA  = "MFMRdata"     ### -> ... !!!
    )
  );
  
  # Setting the R Environment 'Bindings = TRUE' makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
