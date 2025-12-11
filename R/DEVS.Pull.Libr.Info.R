#? ### ### ### ### ### ### ###
#' @title Extract R Package <DESCRIPTION File> Information (the easy way)
#' @name devs.pull.libr.info
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
#' rlsLibINFO <- devs.pull.libr.info()  ### <- Extracts Library Information ... 
#' 
#' rlsLibINFO[["NAME"]]      # -> Returns 'Package Name' value <result> ...
#' rlsLibINFO[["VERSION"]]   # -> Returns 'Package Version' value <result> ... 
#'
#' @export
#? ### ### ###
"devs.pull.libr.info" <- function(
  ssPathFileDESC="DESCRIPTION", sbIsAbsolutePath=FALSE
) {
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  rssTagFuncIDv01_ <- "Pull.Libr.Info";        # <- Function ID - SHORT !!!
  rssTagFuncIDv02_ <- "DEVS.Pull.Libr.Info";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Create <internal> "Aliases"   ####
  # NB: Assign "Local Aliases" for frequently used functions !!!
  rasBaseLIST       <- base::list;
  rasBaseGetWorkDIR <- base::getwd;
  rasBaseRETURN     <- base::return;
  rasBaseFilePATH   <- base::file.path;
  rasDescGetFIELD   <- desc::desc_get_field;
  
  rasMfmrRetEnvLckdLIST <- MFMRutils::code.return.env.locked.list;
  
  
  
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
  ### rssPackageID_          <- rasDescGetFIELD(file = rssPathFileDESC_, key = "Package");
  ### rssPackageTITLE_       <- rasDescGetFIELD(file = rssPathFileDESC_, key = "Title");
  ### rssPackageDESCRIPTION_ <- rasDescGetFIELD(file = rssPathFileDESC_, key = "Description");
  ### rssPackageVERSION_     <- rasDescGetFIELD(file = rssPathFileDESC_, key = "Version");
  ### rssPackageBugREPORTS_  <- rasDescGetFIELD(file = rssPathFileDESC_, key = "BugReports");
  ### rssPackageAUTHORS_     <- rasDescGetFIELD(file = rssPathFileDESC_, key = "Authors@R");
  ### rssPackageLICENSE_     <- rasDescGetFIELD(file = rssPathFileDESC_, key = "License");
  ### rssPackageENCODING_    <- rasDescGetFIELD(file = rssPathFileDESC_, key = "Encoding");
  ### rssPackageLazyDATA_    <- rasDescGetFIELD(file = rssPathFileDESC_, key = "LazyData");
  ### rssPackageROXYGEN_     <- rasDescGetFIELD(file = rssPathFileDESC_, key = "Roxygen");
  ### rssPackageRoxygenNOTE_ <- rasDescGetFIELD(file = rssPathFileDESC_, key = "RoxygenNote");
  ### rssPackageIMPORTS_     <- rasDescGetFIELD(file = rssPathFileDESC_, key = "Imports");
  ### rasMfmrRetEnvLckdLIST(
  ###   sbLockList = TRUE,
  ###   vsListNames = c(
  ###     "NAME",    
  ###     "TITLE",   
  ###     "DESC",    
  ###     "VERSION",
  ###     "BUGS_URL",   
  ###     "AUTHORS",  
  ###     "LICENSE",    
  ###     "ENCODING",   
  ###     "LAZY_DATA", 
  ###     "R_OXYGEN", 
  ###     "R_OXYGEN_NOTE",
  ###     "DEPENDENCIES"  
  ###   ),
  ###   lsListVals = rasBaseLIST(
  ###     rssPackageID_,
  ###     rssPackageTITLE_,
  ###     rssPackageDESCRIPTION_,
  ###     rssPackageVERSION_,
  ###     rssPackageBugREPORTS_,
  ###     rssPackageAUTHORS_,
  ###     rssPackageLICENSE_,
  ###     rssPackageENCODING_,
  ###     rssPackageLazyDATA_,
  ###     rssPackageROXYGEN_,
  ###     rssPackageRoxygenNOTE_,
  ###     rssPackageIMPORTS_
  ###   )
  ### );
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
