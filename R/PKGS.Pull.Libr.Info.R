#? ### ### ### ### ### ### ###
#' @title Extract R Package <Library DESCRIPTION File> Information
#' @name pkgs.pull.libr.info
#' 
#' @description
#' A tiny R function that extracts relevant package information from <internal> 
#' R Library Attributes.
#'
#' @param ssPathFileDESC a character (string) value that denotes the <local> path
#'                       <directory> to the R Library "DESCRIPTION" file.
#' @param sbIsAbsolutePath a logical (boolean) value that specifies whether the 
#'                         DESCRIPTION File Path argument "ssPathFileDESC" is set
#'                         (or provided) as an absolute path or not.
#'
#' @examples
#' ### Activate the "MFMRutils" R Library (if previously installed) ...
#' library(MFMRutils)   ### -> Loads the "MFMRutils" library !!!
#' 
#' ### Extract the R Library Information (for active R Library <project>) ...
#' rlsLibINFO <- pkgs.pull.libr.info()  ### <- Extracts Library Information ... 
#' 
#' rlsLibINFO[["NAME"]]      # -> Returns 'Package Name' value <result> ...
#' rlsLibINFO[["VERSION"]]   # -> Returns 'Package Version' value <result> ... 
#'
#' @export
#? ### ### ###
"pkgs.pull.libr.info" <- function(
  ssPathFileDESC="./DESCRIPTION", sbIsAbsolutePath=TRUE
) {
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  rssTagFuncIDv01_ <- "Pull.Libr.Info";        # <- Function ID - SHORT !!!
  rssTagFuncIDv02_ <- "PKGS.Pull.Libr.Info";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Create <internal> "Aliases"   ####
  # NB: Assign "Local Aliases" for frequently used functions !!!
  rasLIST     <- base::list;
  rasRETURN   <- base::return;
  rasFilePATH <- base::file.path;
  rasGetFIELD <- desc::desc_get_field;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rscPathFileDESC_  <- ssPathFileDESC;
  sbIsAbsolutePath_ <- sbIsAbsolutePath;
  
  
  
  ####   STEP 04 - Check 'DESCRIPTION' File Path   ####
  if (!sbIsAbsolutePath_) {
    rscPathFileDESC_ <- rasFilePATH(ssPathFileDESC);
  }
  
  
  
  ####   STEP 05 - Compile and return Library Information   ####
  rasRETURN(
    rasLIST(
      NAME          = rasGetFIELD(file = rscPathFileDESC_, key = "Package"),
      TITLE         = rasGetFIELD(file = rscPathFileDESC_, key = "Title"),
      DESC          = rasGetFIELD(file = rscPathFileDESC_, key = "Description"),
      VERSION       = rasGetFIELD(file = rscPathFileDESC_, key = "Version"),
      BUGS_URL      = rasGetFIELD(file = rscPathFileDESC_, key = "BugReports"),
      AUTHORS       = rasGetFIELD(file = rscPathFileDESC_, key = "Authors@R"),
      LICENSE       = rasGetFIELD(file = rscPathFileDESC_, key = "License"),
      ENCODING      = rasGetFIELD(file = rscPathFileDESC_, key = "Encoding"),
      LAZY_DATA     = rasGetFIELD(file = rscPathFileDESC_, key = "LazyData"),
      R_OXYGEN      = rasGetFIELD(file = rscPathFileDESC_, key = "Roxygen"),
      R_OXYGEN_NOTE = rasGetFIELD(file = rscPathFileDESC_, key = "RoxygenNote"),
      DEPENDENCIES  = rasGetFIELD(file = rscPathFileDESC_, key = "Imports")
    )
  );
  
}
