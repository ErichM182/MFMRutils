#? ### ### ### ### ### ### ###
#' @title Extract R Package <Library> Information
#' @name "pkgs.get.lib.info"
#' @description
#' A tiny R function that extracts relevant package information from <internal> 
#' R Library Attributes.
#'
#'
#' @param ssPathFileDESC a character (string) value that denotes the <local> path
#'                       <directory> to the R Library "DESCRIPTION" file.
#'
#'
#' @examples
#' ### Activate the "MFMRutils" R Library (if previously installed) ...
#' library(MFMRutils)   # -> Loads the "MFMRutils" library !!!
#' 
#' ### Extract the R Library Information (for active R Library <project>) ...
#' rlsLibINFO <- pkgs.get.lib.info()
#' 
#' rlsLibINFO[["PkgNAME"]]      # <- Returns 'Package Name' value <result> ...
#' rlsLibINFO[["PkgVERSION"]]   # <- Returns 'Package Version' value <result> ...
#' 
#'
#' @export
#? ### ### ###
"pkgs.get.lib.info" <- function(ssPathFileDESC="./DESCRIPTION") {
  
  ### STEP 01 - Define the "Function Self-ID" tag ... ####
  ssFuncTAG_ <- "MFMRutils_Get.Lib.Info";
  
  ### STEP 02 - Create <internal> "Aliases" ... ####
  # NB: Assign "Local Aliases" for frequently used functions !!!
  rasGetFIELD      <- desc::desc_get_field;
  rasGetVERS       <- desc::desc_get_version;
  rscPathFileDESC_ <- base::file.path(ssPathFileDESC);
  
  ### STEP 02 - Create <internal> "Aliases" ... ####
  base::return(
    base::list(
      PkgNAME = rasGetFIELD(file = rscPathFileDESC_, key = "Package"),
      PkgVERSION = rasGetVERS(file = rscPathFileDESC_)
    )
  );
  
}
