#? ### ### ### ### ### ### ###
#' @title The Active Development Tracker File Updater ("SuiteMFMR" DevTools)
#' @name devs.update.act.dev.trckr
#' 
#' @description
#' A Helper Function that updates important information inside the "Active Development Information
#' Tracker File" (i.e. "00_ACT_DEV_TRCKR.txt") located in the active project's development or
#' launchpad folder (i.e. the "./WIP/" path or directory). This "Active Development Information
#' Tracker File" will be programmatically initialized (created) by this helper function if not 
#' found in the default project directory (i.e. "./WIP/" project folder).
#'
#' @param sbIsProdRel a logical (boolean) value that captures if the code-check and/or code commit
#'                    process (action) is a "Production Release" action or not.
#'
#' @returns
#' * This function returns the programmatically amended or updated (real-time or
#'   active) version number for the active R Library Project as a list of character
#'   objects.
#'
#' @examples
#' ### Run R Package DevCode easily as follows ...
#' library(MFMRutils)   # <- Loads "MFMRutils" library (if already installed) !!!
#'
#' ### Run 2 different types of code check/validation processes ...
#' devs.update.act.dev.trackr.file()   # -> Executes only the DevTools Documentation 
#'                                     #    Process.
#'
#' @keywords internal
#' @noRd
#? ### ### ###
"devs.update.act.dev.trckr" <- function(sbIsProdRel=FALSE) {
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_ID_SHRT_ <- "Update.Dev.Trckr";            # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Update.Act.Dev.Trckr";   # <- Function ID - LONG !!!
  RCT_TAG_FUNC_LIBR_ID_ <- MFMRutils::devs.pull.libr.info()[["NAME"]];
  
  
  ####   STEP 02 - Prime NB "Aliases" used locally (inside function)   ####
  rasBaseCLASS         <- base::class;
  rasBaseRETURN        <- base::return;
  rasBasePASTE0        <- base::paste0;
  rasBaseFORMAT        <- base::format;
  rasBaseIfELSE        <- base::ifelse;
  rasBaseUNLIST        <- base::unlist;
  rasBaseOPTIONS       <- base::options;
  rasBaseSPRINTF       <- base::sprintf;
  rasBaseTryCATCH      <- base::tryCatch;
  rasBaseSysTimeNOW    <- base::Sys.time;
  rasBaseStrSPLIT      <- base::strsplit;
  rasBaseReadCHAR      <- base::readChar;
  rasBaseReadLINE      <- base::readline;
  rasBaseFileINFO      <- base::file.info;
  rasBaseFilePATH      <- base::file.path;
  rasBaseReadLINES     <- base::readLines;
  rasBaseAsNUMERIC     <- base::as.numeric;
  rasBaseDirCREATE     <- base::dir.create;
  rasBaseWriteLINES    <- base::writeLines;
  rasMfmrCONSTS        <- MFMRutils::cMISC;
  `%?!%`               <- MFMRutils::`%?!%`;   # <- VERY COOL Alias <NCO> !!!
  rasBaseFileEXISTS    <- base::file.exists;
  rasBaseFileCREATE    <- base::file.create;
  rasBaseAsCHAR        <- base::as.character;
  rasJsonLiteFromJSON  <- jsonlite::fromJSON;
  rasStringrStrEXTRACT <- stringr::str_extract;
  rasUtilsLibsInfoCRAN <- utils::available.packages;
  rasMfmrPullLibrINFO  <- MFMRutils::devs.pull.libr.info;
  rasMfmrPatchLibrVERS <- MFMRutils:::devs.patch.libr.vers.number;
  
  
  ####   STEP 03 - Internalize Function Arguments   ####
  sbIsProdRel_ <- sbIsProdRel;
  
  
  ####   STEP 04 - Create Folder & File ( IF NOT EXISTS )   ####
  rsbIsNewActDevTRCKR_ <- FALSE;
  RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_ <- rasMfmrCONSTS$PATH_TO_FILE_ACT_DEV_INFO_TRCKR;
  if (!rasBaseFileEXISTS(RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_)) {   # <- Checks that FILE DOES NOT EXIST !!!
    rasBaseDirCREATE(   # -> Creates the "./WIP" directory (if not already exists) ...
      path = rasMfmrCONSTS$PATH_TO_FOLDER_WIP, recursive = T, showWarnings = F
    );
    rsbIsNewActDevTRCKR_ <- TRUE;   # <- Save confirmation that "ActDev TRCKR" was newly created !!!
    rasBaseFileCREATE(RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_);   # -> Creates the required file ...
  }
  
  
  ####   STEP 05 - COMPILE Important CODE VERSIONING INFO   ####
  rsnVersStubDEVS_  <- 0; rsnVersStubBETA_   <- 0;
  rsnVersStubALPHA_ <- 0; rsnVersStubSTABLE_ <- 0;
  rssVersNewDEVS_ <- "0.0.0.001"; rssVersNewPROD_ <- "0.0.1"; 
  
  RCT_SYS_DATE_TIME_NOW_         <- rasBaseSysTimeNOW();
  RCT_FORMAT_TIME_DEV_03_        <- rasMfmrCONSTS$FORMAT_TIME_DEV_LOG_V03;
  RCT_FILE_R_PKG_DESC_           <- rasMfmrCONSTS$PATH_TO_FILE_R_PACKAGE_DESC;
  RCT_REGENT_R_LIB_DESC_INFO_    <- rasMfmrPullLibrINFO(RCT_FILE_R_PKG_DESC_);
  RCT_REGENT_R_LIB_ID_           <- RCT_REGENT_R_LIB_DESC_INFO_[["NAME"]];
  RCT_REGENT_R_LIB_VERS_         <- RCT_REGENT_R_LIB_DESC_INFO_[["VERSION"]];
  RCT_REGENT_LIBS_VERS_ROXYGEN2_ <- RCT_REGENT_R_LIB_DESC_INFO_[["R_OXYGEN_NOTE"]];
  
  RCT_CODE_PUSH_TYPE_ <- rasBaseIfELSE(
    sbIsProdRel_, "PRODUCTION Release", "ACT-DEV Release"
  );
  
  #### Update the Code Version stubs accordingly ... 
  ## NOTE: Full <debug> Code Version Form -> "STABLE.BETA.ALPHA.DEV" == "0.0.1.001" !!!
  ##       Production Version Form -> "STABLE.BETA.ALPHA" == "0.0.1" !!!
  rvsLibrVersPartsCRAN_ <- c(-999, -999, -999, -999);
  if (sbIsProdRel_ || rsbIsNewActDevTRCKR_) {   # <- Action == PRODUCTION CODE ACTION (Code Check or Code Commit) !!!
    
    # STEP 1 - Source CRAN: get current <extant> R Library Version from CRAN ...
    RCT_NULL_ERROR_TEXT_ <- "R-Library NOT in CRAN !!!";
    RCT_LIB_INFO_CRAN_ <- rasBaseTryCATCH(
      {
        rasJsonLiteFromJSON(
          # txt = rasBasePASTE0("https://crandb.r-pkg.org/", "desc")
          txt = rasBasePASTE0("https://crandb.r-pkg.org/", "ggplot2")
          # txt = rasBasePASTE0("https://crandb.r-pkg.org/", "MFMRutils")
        )
      }, 
      error = function(e) {    # <- An ERROR occurred (during the online <CRAN> Library Check) ...
        RCT_NULL_ERROR_TEXT_   # -> ... so return the "NULL" text <CRAN> Library Check FAIL Result !!!
      }
    );
    
    
    # STEP 2 - Extract Version Information, Update or Source TRCKR <local> ...
    if (rasBaseCLASS(RCT_LIB_INFO_CRAN_) == "list") {   # <- Online <CRAN> Check was SUCCESSFUL !!!
      
      ### STEP 2.1 - Extract the R Library Name & Version number values (results) ... 
      rssLibrNameCRAN_ <- RCT_LIB_INFO_CRAN_$Package;
      rssLibrVersCRAN_ <- RCT_LIB_INFO_CRAN_$Version;
      if (rssLibrNameCRAN_ == RCT_REGENT_R_LIB_ID_) {   # <- NB -> Ensure Library Name[s] match !!!
        
        ### STEP 2.1.1a: Split returned Version Number into the constituent parts ...
        rvsLibrVersPartsCRAN_ <- rasBaseUNLIST(
          rasBaseStrSPLIT(rasBaseAsCHAR(rssLibrVersCRAN_), split = "\\.")
        );
        
        ### STEP 2.1.1b: Patch the various Version Number stubs (accordingly) ...
        rsnListFULL_ <- rasMfmrPatchLibrVERS(rvsLibrVersPartsCRAN_);
        
        rsnVersStubALPHA_  <- rasMfmrPatchLibrVERS(rvsLibrVersPartsCRAN_)[["VERS_ALPHA"]];
        rsnVersStubBETA_   <- rasMfmrPatchLibrVERS(rvsLibrVersPartsCRAN_)[["VERS_BETA"]];
        rsnVersStubSTABLE_ <- rasMfmrPatchLibrVERS(rvsLibrVersPartsCRAN_)[["VERS_STABLE"]];
        
        
      } else {   # <- Local vs. Remote Library Name[s] DID NOT MATCH (Name-Check FAILED) !!!
        cat("ERROR -> Local vs. Remote (CRAN) Library Names DID NOT MATCH !!!");
      }
      
    } else {   # <- Online <CRAN> Check was NOT SUCCESSFUL (CRAN call FAILED) !!!
      
      ### STEP 2.2 - Source TRCKR: get R Library Version from the "00_ACT_DEV_TRCKR.txt" file ...
      if (!rsbIsNewActDevTRCKR_) {   # <- Ensures TRCKR file is not newly created & thus EMPTY !!!
        
        ### STEP 2.2.1a: Read the complete contents of the TRCKR file ...
        rcsFullTextTRCKR_ <- rasBaseReadCHAR(
          con = RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_, 
          nchars = rasBaseFileINFO(RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_)$size
        );
        
        ### STEP 2.2.1b: Extract the ACTIVE-DEVELOPMENT CODE VERSION NUMBER (value) ...
        rssActDevVersNumFromTRCKR_ <- rasStringrStrEXTRACT(
          string = rcsFullTextTRCKR_, 
          # # Extract text between two markers <given by "RegEx" pattern (.*?) below> ...
          pattern = "==>(.*?)devs-release"   # <- Two markers: "==>" and "
        );
        rvsActDevVersNumFromTRCKR_n3_ <- rasBaseUNLIST(
          rasBaseStrSPLIT(rssActDevVersNumFromTRCKR_, " ")
        )[3];
        
        ### STEP 2.2.1c: Split returned Version Number into the constituent parts ...
        rvsLibrVersPartsTRCKR_ <- rasBaseUNLIST(
          rasBaseStrSPLIT(rasBaseAsCHAR(rvsActDevVersNumFromTRCKR_n3_), split = "\\.")
        )[1:3];   # <- Extract only the first 3 numbers (i.e. PRODUCTION Version Stubs) !!!
        
        ### STEP 2.2.1b: Patch the various Version Number stubs (accordingly) ...
        rsnVersStubALPHA_  <- rasMfmrPatchLibrVERS(rvsLibrVersPartsTRCKR_)[["VERS_ALPHA"]];
        rsnVersStubBETA_   <- rasMfmrPatchLibrVERS(rvsLibrVersPartsTRCKR_)[["VERS_BETA"]];
        rsnVersStubSTABLE_ <- rasMfmrPatchLibrVERS(rvsLibrVersPartsTRCKR_)[["VERS_STABLE"]];
        
      } else {   # <- This means the TRCKR file IS newly created & thus EMPTY <void> !!!
        
        ### STEP 2.2.2a - Source DESC: get R Library Version from the "./DESCRIPTION" file ...
        rvsLibrVersPartsDESC_ <- rasBaseUNLIST(
          rasBaseStrSPLIT(rasBaseAsCHAR(RCT_REGENT_R_LIB_VERS_), split = "\\.")
        )[1:3];   # <- Extract only the first 3 numbers (i.e. PRODUCTION Version Stubs) !!!
        
        ### STEP 2.2.2b: Patch the various Version Number stubs (accordingly) ...
        rsnVersStubALPHA_  <- rasMfmrPatchLibrVERS(rvsLibrVersPartsDESC_)[["VERS_ALPHA"]];
        rsnVersStubBETA_   <- rasMfmrPatchLibrVERS(rvsLibrVersPartsDESC_)[["VERS_BETA"]];
        rsnVersStubSTABLE_ <- rasMfmrPatchLibrVERS(rvsLibrVersPartsDESC_)[["VERS_STABLE"]];
        
      }
      
    }
    
  } else {   # <- Action == ACTIVE-DEVELOPMENT CODE ACTION (Code Check or Code Commit) !!!
    
    ### STEP 2.2 - Source TRCKR: get R Library Version from the "00_ACT_DEV_TRCKR.txt" file ...
    if (!rsbIsNewActDevTRCKR_) {   # <- Ensures TRCKR file is not newly created & thus EMPTY !!!
      
      ### STEP 2.2.1a: Read the complete contents of the TRCKR file ...
      rcsFullTextTRCKR_ <- rasBaseReadCHAR(
        con = RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_, 
        nchars = rasBaseFileINFO(RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_)$size
      );
      
      ### STEP 2.2.1b: Extract the ACTIVE-DEVELOPMENT CODE VERSION NUMBER (value) ...
      rssActDevVersNumFromTRCKR_ <- rasStringrStrEXTRACT(
        string = rcsFullTextTRCKR_, 
        # # Extract text between two markers <given by "RegEx" pattern (.*?) below> ...
        pattern = "==>(.*?)devs-release"   # <- Two markers: "==>" and "
      );
      rvsActDevVersNumFromTRCKR_n3_ <- rasBaseUNLIST(
        rasBaseStrSPLIT(rssActDevVersNumFromTRCKR_, " ")
      )[3];
      
      ### STEP 2.2.1c: Split returned Version Number into the constituent parts ...
      rvsLibrVersPartsTRCKR_ <- rasBaseUNLIST(
        rasBaseStrSPLIT(rasBaseAsCHAR(rvsActDevVersNumFromTRCKR_n3_), split = "\\.")
      );
      
      ### STEP 2.2.1b: Patch the various Version Number stubs (accordingly) ...
      rsnVersStubDEBUG_  <- rasMfmrPatchLibrVERS(rvsLibrVersPartsTRCKR_)[["VERS_DEBUG"]];
      rsnVersStubALPHA_  <- rasMfmrPatchLibrVERS(rvsLibrVersPartsTRCKR_)[["VERS_ALPHA"]];
      rsnVersStubBETA_   <- rasMfmrPatchLibrVERS(rvsLibrVersPartsTRCKR_)[["VERS_BETA"]];
      rsnVersStubSTABLE_ <- rasMfmrPatchLibrVERS(rvsLibrVersPartsTRCKR_)[["VERS_STABLE"]];
      
    } else {   # <- This means the TRCKR file IS newly created & thus EMPTY <void> !!!
      
      ### STEP 2.2.2a - Source DESC: get R Library Version from the "./DESCRIPTION" file ...
      rvsLibrVersPartsDESC_ <- rasBaseUNLIST(
        rasBaseStrSPLIT(rasBaseAsCHAR(RCT_REGENT_R_LIB_VERS_), split = "\\.")
      );
      
      ### STEP 2.2.2b: Patch the various Version Number stubs (accordingly) ...
      rsnVersStubDEBUG_  <- rasMfmrPatchLibrVERS(rvsLibrVersPartsDESC_)[["VERS_DEBUG"]];
      rsnVersStubALPHA_  <- rasMfmrPatchLibrVERS(rvsLibrVersPartsDESC_)[["VERS_ALPHA"]];
      rsnVersStubBETA_   <- rasMfmrPatchLibrVERS(rvsLibrVersPartsDESC_)[["VERS_BETA"]];
      rsnVersStubSTABLE_ <- rasMfmrPatchLibrVERS(rvsLibrVersPartsDESC_)[["VERS_STABLE"]];
      
    }
    
  }
  
  
  ####   STEP 06 - Compile FINAL CODE VERSION Numbers   ####
  rssVersNewPROD_ <- rasBasePASTE0(
    rsnVersStubSTABLE_, ".", rsnVersStubBETA_, ".", rsnVersStubALPHA_
  );
  rssVersNewDEVS_ <- rasBasePASTE0(
    rsnVersStubSTABLE_, ".", rsnVersStubBETA_, ".", rsnVersStubALPHA_, ".",
    rasBaseSPRINTF(fmt = "%03d", rsnVersStubDEBUG_)
  );
  
  
  
  RCT_ACT_DEV_INFO_HEADER_ <- rasBasePASTE0(
    "=== === === === === === === === === === === === === === === === === === === === ===",
    "|        Active Development Tracking Information (regent R Library Project)       |",
    "=== === === === === === === === === === === === === === === === === === === === ==="
  );
  RCT_ACT_DEV_INFO_BODY_LVL_01_ <- rasBasePASTE0(   # -> Creates a Devs TimeStamp ...
    "-> LAST CODE PUSH (Code-Check and/or Code-Commit) INFORMATION (KEY Stats) ...", "\n",
    '> R-Library Project ID: "', RCT_REGENT_R_LIB_ID_, '" \n',
    "> Code Push TYPE  ==>  ", RCT_CODE_PUSH_TYPE_, "\n",
    "> Code Push TIME  ==>  ", rasBaseFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_03_),
    "> Code Push PRODUCTION VERSION #  ==>  ", rssVersNewPROD_, "      (prod-release)", "\n",
    "> Code Push ACTIVE-DEV VERSION #  ==>  ", rssVersNewDEVS_, "  (devs-release)", "\n"
  );
  RCT_ACT_DEV_INFO_BODY_LVL_02_ <- rasBasePASTE0(   # -> Creates a Devs TimeStamp ...
    "-> R-Library (3rd Party) Development Support Packages: ", "\n",
    "> desc      ==>  v", rasBaseFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_03_),
    "> devtools  ==>  v", rasBaseFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_03_),
    "> roxygen2  ==>  v", RCT_REGENT_LIBS_VERS_ROXYGEN2_, "\n"
  );
  
}
