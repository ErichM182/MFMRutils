#? ### ### ### ### ### ### ###
#' @title Extract R Package Meta-Information ("SuiteMFMR" DevTools)
#' @name devs.pull.libr.info
#' @family SuiteMFMR DevTools
#' 
#' @description
#' A tiny R function that extracts relevant package information from the R Library Manifest or 
#' R-Project Attributes File (i.e. the R Library DESCRIPTION File).
#'
#' @param ssPathFileDESC ([character]) A String (i.e. character vector) value that denotes the local 
#'                       path <directory> to the R Library "DESCRIPTION" file.
#' @param sbIsAbsolutePath ([logical]) A boolean value that specifies whether the DESCRIPTION File 
#'                         Path argument `ssPathFileDESC` is set (or provided) as an absolute path 
#'                         or not.
#'
#' @examples
#' \dontrun{   ### <- This function constitutes a development utility !!! This function requires a 
#'             ###    special development directory ("./WIP") that is created during the init-run
#'             ###    (initial R project setup) phase and is intended to facilitate a user-friendly
#'             ###    R Library development process. For these reasons the code examples below
#'             ###    should not be executed during "R_CMD_CHECK" code check procedures.
#'             
#' ### Activate the "MFMRutils" R Library (if previously installed) ...
#' library(MFMRutils)   ### -> Loads the "MFMRutils" library !!!
#' 
#' ### Extract the R Library Information (for active R Library <project>) ...
#' rlsLibINFO <- devs.pull.libr.info()  ### <- Extracts Library Information ... 
#' 
#' rlsLibINFO[["NAME"]]      # -> Returns 'Package Name' value <result> ...
#' rlsLibINFO[["VERSION"]]   # -> Returns 'Package Version' value <result> ... 
#' 
#' }
#'
#' @export
#? ### ### ###
"devs.pull.libr.info" <- function(
  ssPathFileDESC="DESCRIPTION", sbIsAbsolutePath=FALSE
) {
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";             # <- R Library Identifier !!!
  rssTagFuncIDv01_      <- "Pull.Libr.Info";        # <- Function ID - SHORT !!!
  rssTagFuncIDv02_      <- "DEVS.Pull.Libr.Info";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Create <internal> "Aliases"   ####
  # NB: Assign "Local Aliases" for frequently used functions !!!
  rasBaseLIST       <- base::list;
  rasBaseGetWorkDIR <- base::getwd;
  rasBaseRETURN     <- base::return;
  rasBaseFilePATH   <- base::file.path;
  rasDescGetFIELD   <- desc::desc_get_field;
  
  rasMfmrRetEnvLckdLIST <- MFMRutils::code.return.renv.locked.list;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rssPathFileDESC_   <- ssPathFileDESC;
  rsbIsAbsolutePath_ <- sbIsAbsolutePath;
  
  
  
  ####   STEP 04 - Check 'DESCRIPTION' File Path   ####
  if (rsbIsAbsolutePath_) {
    rssPathFileDESC_ <- rasBaseFilePATH(ssPathFileDESC);
  } else {
    rssPathFileDESC_ <- rasBaseFilePATH(rasBaseGetWorkDIR(), ssPathFileDESC);
  }
  
  
  
  ####   STEP 05 - Compile and return Library Information   ####
  rasBaseRETURN(
    rasBaseLIST(
      "NAME"          = rasDescGetFIELD(file = rssPathFileDESC_, key = "Package"),
      "TITLE"         = rasDescGetFIELD(file = rssPathFileDESC_, key = "Title"),
      "DESC"          = rasDescGetFIELD(file = rssPathFileDESC_, key = "Description"),
      "VERSION"       = rasDescGetFIELD(file = rssPathFileDESC_, key = "Version"),
      "BUGS_URL"      = rasDescGetFIELD(file = rssPathFileDESC_, key = "BugReports"),
      "AUTHORS"       = rasDescGetFIELD(file = rssPathFileDESC_, key = "Authors@R"),
      "LICENSE"       = rasDescGetFIELD(file = rssPathFileDESC_, key = "License"),
      "ENCODING"      = rasDescGetFIELD(file = rssPathFileDESC_, key = "Encoding"),
      "LAZY_DATA"     = rasDescGetFIELD(file = rssPathFileDESC_, key = "LazyData"),
      "R_OXYGEN"      = rasDescGetFIELD(file = rssPathFileDESC_, key = "Roxygen"),
      "R_OXYGEN_NOTE" = rasDescGetFIELD(file = rssPathFileDESC_, key = "RoxygenNote"),
      "DEPENDENCIES"  = rasDescGetFIELD(file = rssPathFileDESC_, key = "Imports")
    )
  );
  
}
