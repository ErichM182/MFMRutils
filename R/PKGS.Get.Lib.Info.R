#? ### ### ### ### ### ### ###
#' @title Extract R Package <Library> Information
#' 
#' @description
#' A tiny R function that extracts relevant package information from <internal> 
#' R Library Attributes.
#'
#' @param ssPathFileDESC a character (string) value that denotes the <local> path
#'                       <directory> to the R Library "DESCRIPTION" file.
#'
#' @examples
#' ### Activate the "MFMRutils" R Library (if previously installed) ...
#' library(MFMRutils)   # -> Loads the "MFMRutils" library !!!
#' 
#' ### Extract the R Library Information (for active R Library <project>) ...
#' rlsLibINFO <- pkgs.get.lib.info()
#' 
#' rlsLibINFO[["NAME"]]      # <- Returns 'Package Name' value <result> ...
#' rlsLibINFO[["VERSION"]]   # <- Returns 'Package Version' value <result> ... 
#'
#' @export
#? ### ### ###
"pkgs.get.lib.info" <- function(
  ssPathFileDESC="./DESCRIPTION", sbIsAbsolutePath=TRUE
) {
  
  ### STEP 01 - Define the "Function Self-ID" tag ... ####
  ssFuncTAG_ <- "Get.Lib.Info";
  
  
  
  ### STEP 02 - Internalize Function Arguments ... ####
  rscPathFileDESC_  <- ssPathFileDESC;
  sbIsAbsolutePath_ <- sbIsAbsolutePath;
  
  
  
  ### STEP 03 - Create <internal> "Aliases" ... ####
  # NB: Assign "Local Aliases" for frequently used functions !!!
  rasLIST     <- base::list;
  rasRETURN   <- base::return;
  rasFilePATH <- base::file.path;
  rasGetFIELD <- desc::desc_get_field;
  # rasGetVERS  <- desc::desc_get_version;
  if (!sbIsAbsolutePath_) {
    rscPathFileDESC_ <- rasFilePATH(ssPathFileDESC);
  }
  
  
  
  ### STEP 04 - Create <internal> "Aliases" ... ####
  rasRETURN(
    rasLIST(
      NAME    = rasGetFIELD(file = rscPathFileDESC_, key = "Package"),
      ## VERSION = rasGetVERS(file = rscPathFileDESC_)
      VERSION = rasGetFIELD(file = rscPathFileDESC_, key = "Version")
    )
  );
  
}
