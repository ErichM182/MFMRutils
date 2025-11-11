#? ### ### ### ### ### ### ###
#' @title Extract R Package <Library> Information
#' @name "pkgs.get.lib.info"
#' @description
#' A tiny R function that extracts relevant package information from <internal> 
#' R Library Attributes.
#'
#' @examples
#' ### Easily print & assign Date Formats as follows ...
#' ENVDates$LONGv01   ### -> formats Date to "Saturday, January 16 1982" !!!
#' ENVDates$LONGv02   ### -> formats Date to "Sat, Jan 16 1982" !!!
#' ENVDates$LONGv03   ### -> formats Date to "Sat, 16 Jan 1982" !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::ENVFormats$BOLD   ### -> sets the text font format to BOLD !!!
#'
#' @export
#? ### ### ###
"pkgs.get.lib.info" <- function(ssPathToFileDESC="./DESCRIPTION") {
  
  ### STEP 01 - Define the "Function Self-ID" tag ... ####
  ssFuncTAG_ <- "MFMRutils_Get.Lib.Info";
  
  ### STEP 02 - Create <internal> "Aliases" ... ####
  # NB: Assign "Local Aliases" for frequently used functions !!!
  rasGetFIELD      <- desc::desc_get_field;
  rasGetVERS       <- desc::desc_get_version;
  rscPathFileDESC_ <- base::file.path(ssPathToFileDESC);
  
  ### STEP 02 - Create <internal> "Aliases" ... ####
  base::return(
    base::list(
      PkgNAME = rasGetFIELD(file = rscPathFileDESC_, key = "Package"),
      PkgVERSION = rasGetVERS(file = rscPathFileDESC_)
    )
  );
  
}
