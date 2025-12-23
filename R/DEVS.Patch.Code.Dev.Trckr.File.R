#? ### ### ### ### ### ### ###
#' @title Update the R Code Development Tracker File ("SuiteMFMR" DevTools)
#' @name devs.patch.code.dev.trckr.file
#' 
#' 
#' @description
#' A Helper Function that updates important information inside the "Active Development Information
#' Tracker File" (i.e. "00_ACT_DEV_TRCKR.txt") located in the active project's development or
#' launchpad folder (i.e. the "./WIP/" path or directory). This "Active Development Information
#' Tracker File" will be programmatically initialized (created) by this helper function if not 
#' found in the default project directory (i.e. "./WIP/" project folder).
#'
#'
#' @param sbIsProdRel ([logical]) A boolean value that defines whether the code-check and/or code
#'                    commit process (action) is a "Production Release" action or not.
#' @param sbAudioNote ([logical]) A boolean value that specifies whether an audio notification 
#'                    should be played at the completion of this function's code execution (upon 
#'                    successful patching <updating> of the "Active Development Information Tracker
#'                    File").
#'
#'
#' @returns
#' * This function programmatically amends <patches or updates> the active (i.e. regent) R Library
#'   Project's Code Development Tracking Information (i.e. TRCKR) file for code versioning and
#'   project (i.e. library code) development tracking purposes.
#'
#'
#' @examples
#' \dontrun{   ### <- This function constitutes a development utility !!! This function requires a 
#'             ###    special code development directory ("./WIP") created during the init-run (i.e.
#'             ###    initial R project setup) phase and is intended to facilitate a user-friendly
#'             ###    R Library development process. For these reasons the code examples below
#'             ###    should not be executed during normal "R_CMD_CHECK" code check procedures.
#'             
#' ### Run R Package DevCode easily as follows ...
#' library(MFMRutils)   # <- Loads "MFMRutils" library (if already installed) !!!
#'
#' ### Run 2 different types of code check/validation processes ...
#' devs.patch.code.dev.trckr.file()   # -> Executes only the DevTools Documentation 
#'                                    #    Process.
#'                                    
#'}
#'
#'
#' @export
#? ### ### ###
"devs.patch.code.dev.trckr.file" <- function(sbIsProdRel=FALSE, sbAudioNote=FALSE) {
  
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";                        # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHRT_ <- "Patch.TRCKR";                      # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Patch.Code.Dev.Trckr.File";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Prime NB "Aliases" used locally (inside function)   ####
  rasBaseLIST       <- base::list;
  rasBaseCLASS      <- base::class;
  rasBaseLENGTH     <- base::length;
  rasBaseRETURN     <- base::return;
  rasBasePASTE0     <- base::paste0;
  rasBaseFORMAT     <- base::format;
  rasBaseIfELSE     <- base::ifelse;
  rasBaseUNLIST     <- base::unlist;
  rasBaseIsNULL     <- base::is.null;
  rasBaseOPTIONS    <- base::options;
  rasBaseSPRINTF    <- base::sprintf;
  rasBaseTryCATCH   <- base::tryCatch;
  rasBaseStrSPLIT   <- base::strsplit;
  rasBaseReadCHAR   <- base::readChar;
  rasBaseReadLINE   <- base::readline;
  rasBaseSysTimeNOW <- base::Sys.time;
  rasBase_R_VERSION <- base::R.version;
  rasBaseFileINFO   <- base::file.info;
  rasBaseFilePATH   <- base::file.path;
  rasBaseReadLINES  <- base::readLines;
  rasBaseAsNUMERIC  <- base::as.numeric;
  rasBaseDirCREATE  <- base::dir.create;
  rasBaseWriteLINES <- base::writeLines;
  rasBaseFileEXISTS <- base::file.exists;
  rasBaseFileCREATE <- base::file.create;
  rasBaseAsCHAR     <- base::as.character;
  
  rasBeeprBEEP <- beepr::beep;
  
  rasDescSetVERSION <- desc::desc_set_version;
  
  rasJsonLiteFromJSON <- jsonlite::fromJSON;
  
  rasMfmrCONSTS           <- cMISC;   # <- An `MFMRutils` library that is NOT EXPORTED !!!
  `%??%`                  <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasMfmrPatchLibrVersNUM <- devs.patch.libr.vers.number;
  rasMfmrPullLibrINFO     <- MFMRutils::devs.pull.libr.info;
  rasMfmrAppendToFILE     <- MFMRutils::code.append.text.to.file;
  rasMfmrReturnLockedLIST <- MFMRutils::code.return.renv.locked.list;
  
  rasStringrStrEXTRACT <- stringr::str_extract;
  
  rasUtilsPackageVERSION <- utils::packageVersion;
  
  
  
  ####   STEP 03 - Internalize Function Arguments   ####
  rsbIsProdRel_ <- sbIsProdRel;
  rsbAudioNote_ <- sbAudioNote;
  
  
  
  ####   STEP 04 - Create Folder & File ( IF NOT EXISTS )   ####
  rsbIsNewActDevTRCKR_ <- FALSE;
  RCT_PATH_FILE_R_BUILD_IGNORE_     <- rasMfmrCONSTS$PATH_TO_FILE_R_BUILD_IGNORE;
  RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_ <- rasMfmrCONSTS$PATH_TO_FILE_ACT_DEV_INFO_TRCKR;
  if (!rasBaseFileEXISTS(RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_)) {   # <- Checks if FILE DOES NOT EXIST.
    
    ### 4.1 - Create the "./WIP" directory (if not already exists) ...
    rasBaseDirCREATE(
      recursive = T, showWarnings = F,
      path = rasMfmrCONSTS$PATH_TO_FOLDER_WIP
    );
    
    ### 4.2 - Create the "00_ACT_DEV_TRCKR.txt" Code Development TRACKER File ...
    rsbIsNewActDevTRCKR_ <- TRUE;   # <- Save confirmation that TRACKER FILE was newly created !!!
    rasBaseFileCREATE(RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_);   # -> Creates the required file ...
    
    ### 4.3 - Patch the ".Rbuildignore" File to exclude the "./WIP" Directory + Contents ...
    rssPathProjROOT_ <- "."; rssFileID_BUILD_IGNORE_ <- ".Rbuildignore";
    if (!rasBaseFileEXISTS(RCT_PATH_FILE_R_BUILD_IGNORE_)) {
      
      ### 4.3.1 - Create the "./.Rbuildignore" File (since it does NOT ALREADY EXIST) ...
      rasBaseFileCREATE(RCT_PATH_FILE_R_BUILD_IGNORE_);   # -> Creates the required file ...
      
    }
    rasMfmrAppendToFILE(
      ssAppendText = "^WIP$",                    # <- Add the "^WIP$" Text Stub !!!
      ssFilePath = rssPathProjROOT_,
      ssFileID = rssFileID_BUILD_IGNORE_, 
      sbMultiAppend = FALSE, sbPostPend = TRUE
    );
    
  }
  
  
  
  ####   STEP 05 - COMPILE Important CODE VERSIONING INFO   ####
  rsnVersStubDEBUG_ <- 0; rsnVersStubBETA_   <- 0;
  rsnVersStubALPHA_ <- 0; rsnVersStubSTABLE_ <- 0;
  rssVersNewDEVS_ <- "0.0.0.01"; rssVersNewPROD_ <- "0.0.1"; 
  
  RCT_SYS_DATE_TIME_NOW_         <- rasBaseSysTimeNOW();
  RCT_FORMAT_TIME_DEV_03_        <- rasMfmrCONSTS$FORMAT_TIME_DEV_LOG_V03;
  RCT_FILE_R_PKG_DESC_           <- rasMfmrCONSTS$PATH_TO_FILE_R_PACKAGE_DESC;
  
  RCT_REGENT_R_LIB_DESC_INFO_    <- rasMfmrPullLibrINFO(RCT_FILE_R_PKG_DESC_);
  RCT_REGENT_R_LIB_ID_           <- RCT_REGENT_R_LIB_DESC_INFO_[["NAME"]];
  RCT_REGENT_R_LIB_VERS_         <- RCT_REGENT_R_LIB_DESC_INFO_[["VERSION"]];
  RCT_REGENT_LIBS_VERS_ROXYGEN2_ <- RCT_REGENT_R_LIB_DESC_INFO_[["R_OXYGEN_NOTE"]];
  
  RCT_CODE_PUSH_TYPE_ <- rasBaseIfELSE(
    rsbIsProdRel_, "PRODUCTION (public) Release", "DEVELOPMENT (act-dev) Release"
  );
  
  #### Update the Code Version stubs accordingly ... 
  ## NOTE: Full <debug> Code Version Form -> "STABLE.BETA.ALPHA.DEV" == "0.0.1.01" !!!
  ##       Production Version Form -> "STABLE.BETA.ALPHA" == "0.0.1" !!!
  rvsLibrVersPartsCRAN_ <- c(-999, -999, -999, -999);
  if (rsbIsProdRel_ || rsbIsNewActDevTRCKR_) {   # <- Action == PRODUCTION CODE ACTION (Code Check 
                                                 #    or Code Commit) !!!
    
    # STEP 1 - Source CRAN: get current <extant> R Library Version from CRAN ...
    RCT_NULL_ERROR_TEXT_ <- "R-Library NOT in CRAN !!!";
    RCT_LIB_INFO_CRAN_ <- rasBaseTryCATCH(
      {
        rasJsonLiteFromJSON(
          txt = rasBasePASTE0("https://crandb.r-pkg.org/", RCT_REGENT_R_LIB_ID_)
        )
      }, 
      error = function(e) {    # <- An ERROR occurred (during the online <CRAN> Library Check) ...
        RCT_NULL_ERROR_TEXT_   # -> ... so return the "NULL" text <CRAN> Library Check FAIL Result.
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
        rsnListFullCRAN_ <- rasMfmrPatchLibrVersNUM(rvsLibrVersPartsCRAN_);
        
        rsnVersStubALPHA_  <- rsnListFullCRAN_[["VERS_ALPHA"]];
        rsnVersStubBETA_   <- rsnListFullCRAN_[["VERS_BETA"]];
        rsnVersStubSTABLE_ <- rsnListFullCRAN_[["VERS_STABLE"]];
        
        
      } else {   # <- Local vs. Remote Library Name[s] DID NOT MATCH (Name-Check FAILED) !!!
        
        base::cat("ERROR -> Local vs. Remote (CRAN) Library Names DID NOT MATCH !!!\n");
        
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
        rsnListFullTRCKR_ <- rasMfmrPatchLibrVersNUM(rvsLibrVersPartsTRCKR_);
        rsnVersStubALPHA_  <- rsnListFullTRCKR_[["VERS_ALPHA"]];
        rsnVersStubBETA_   <- rsnListFullTRCKR_[["VERS_BETA"]];
        rsnVersStubSTABLE_ <- rsnListFullTRCKR_[["VERS_STABLE"]];
        
      } else {   # <- This means the TRCKR file IS newly created & thus EMPTY <void> !!!
        
        ### STEP 2.2.2a - Source DESC: get R Library Version from the "./DESCRIPTION" file ...
        rvsLibrVersPartsDESC_ <- rasBaseUNLIST(
          rasBaseStrSPLIT(rasBaseAsCHAR(RCT_REGENT_R_LIB_VERS_), split = "\\.")
        )[1:3];   # <- Extract only the first 3 numbers (i.e. PRODUCTION Version Stubs) !!!
        
        ### STEP 2.2.2b: Patch the various Version Number stubs (accordingly) ...
        rsnListFullDESC_ <- rasMfmrPatchLibrVersNUM(rvsLibrVersPartsDESC_);
        rsnVersStubALPHA_  <- rsnListFullDESC_[["VERS_ALPHA"]];
        rsnVersStubBETA_   <- rsnListFullDESC_[["VERS_BETA"]];
        rsnVersStubSTABLE_ <- rsnListFullDESC_[["VERS_STABLE"]];
        
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
      rsnListFullTRCKR_  <- rasMfmrPatchLibrVersNUM(rvsLibrVersPartsTRCKR_);
      rsnVersStubDEBUG_  <- rsnListFullTRCKR_[["VERS_DEBUG"]];
      rsnVersStubALPHA_  <- rsnListFullTRCKR_[["VERS_ALPHA"]];
      rsnVersStubBETA_   <- rsnListFullTRCKR_[["VERS_BETA"]];
      rsnVersStubSTABLE_ <- rsnListFullTRCKR_[["VERS_STABLE"]];
      
    } else {   # <- This means the TRCKR file IS newly created & thus EMPTY <void> !!!
      
      ### STEP 2.2.2a - Source DESC: get R Library Version from the "./DESCRIPTION" file ...
      rvsLibrVersPartsDESC_ <- rasBaseUNLIST(
        rasBaseStrSPLIT(rasBaseAsCHAR(RCT_REGENT_R_LIB_VERS_), split = "\\.")
      );
      
      ### STEP 2.2.2b: Patch the various Version Number stubs (accordingly) ...
      rsnListFullDESC_ <- rasMfmrPatchLibrVersNUM(rvsLibrVersPartsDESC_);
      rsnVersStubDEBUG_  <- rsnListFullDESC_[["VERS_DEBUG"]];
      rsnVersStubALPHA_  <- rsnListFullDESC_[["VERS_ALPHA"]];
      rsnVersStubBETA_   <- rsnListFullDESC_[["VERS_BETA"]];
      rsnVersStubSTABLE_ <- rsnListFullDESC_[["VERS_STABLE"]];
      
    }
    
  }
  
  
  
  ####   STEP 06 - Extract the (3rd Party) Support Libraries Version Numbers   ####
  rssVersDEVTOOLS_ <- rasUtilsPackageVERSION(pkg = "devtools");
  rssVersROXYGEN2_ <- rasUtilsPackageVERSION(pkg = "roxygen2");
  if (!rasBaseIsNULL(RCT_REGENT_LIBS_VERS_ROXYGEN2_) && 
      rssVersDEVTOOLS_ != RCT_REGENT_LIBS_VERS_ROXYGEN2_) {
    rssVersROXYGEN2_ <- RCT_REGENT_LIBS_VERS_ROXYGEN2_
  }
  
  ### Compile information on the list of dependencies ...
  rvsLibrSplitDEPs_ <- rasBaseStrSPLIT(rasMfmrPullLibrINFO()[["DEPENDENCIES"]], ", ");
  rssLibrDepsLIST_ <- NULL;   # <- Compile a list of 3rd Party Dependencies for this R-Library !!!
  for (libr in rvsLibrSplitDEPs_[[1]]) {
    rssLibrDepsLIST_ <- rasBasePASTE0(
      rssLibrDepsLIST_,   # <- Take the whatever already exists -> and add (con-cat) to it ...
      "> ", libr, " v", rasUtilsPackageVERSION(pkg = libr), "\n"
    )
  }
  rsnDepsN_ <- rasBaseLENGTH(rvsLibrSplitDEPs_[[1]]);
  
  
  
  ####   STEP 07 - Compile FINAL OUTPUT Particulars   ####
  rssVersNewPROD_ <- rasBasePASTE0(
    rsnVersStubSTABLE_, ".", rsnVersStubBETA_, ".", rsnVersStubALPHA_
  );
  rssVersNewDEVS_ <- rasBasePASTE0(
    rsnVersStubSTABLE_, ".", rsnVersStubBETA_, ".", rsnVersStubALPHA_, ".",
    rasBaseSPRINTF(fmt = "%02d", rsnVersStubDEBUG_)   # <- Ensure text is formatted as 2 digits !!!
  );
  rcoCodePushDateTIME_ <- rasBaseFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_03_);
  
  ### Update the Library <code> Development Tracking Data prior to writing to file ... 
  RCT_ACT_DEV_INFO_HEADER_ <- rasBasePASTE0(
    "=== === === === === === === === === === === === === === === === === === === === === === ===", "\n",
    "|           Active Development Tracking Information (regent R Library Project)            |", "\n",
    "=== === === === === === === === === === === === === === === === === === === === === === ===", "\n"
  );
  RCT_ACT_DEV_INFO_BODY_LVL_01_ <- rasBasePASTE0(   # -> Creates a Devs TimeStamp ...
    "-> LAST CODE PUSH (Code-Check and/or Code-Commit) INFORMATION (NB Stats) ...", "\n",
    '> R-Library (Project ID): `', RCT_REGENT_R_LIB_ID_, '` \n',
    "> Code Push TYPE  ==>  ", RCT_CODE_PUSH_TYPE_, "\n",
    "> Code Push TIME  ==>  ", rcoCodePushDateTIME_, "\n",
    "> Code Push PRODUCTION VERSION #  ==>  ", rssVersNewPROD_, "     (prod-release)", "\n",
    "> Code Push ACTIVE-DEV VERSION #  ==>  ", rssVersNewDEVS_, "  (devs-release)", "\n"
  );
  RCT_ACT_DEV_INFO_BODY_LVL_02_ <- rasBasePASTE0(
    "-> R SOFTWARE: Specifics on `R-core` [as PROG-LANG for R Library Development] ... ", "\n",
    "> ", rasBase_R_VERSION[["version.string"]], "\n",
    '> Nickname  ==>  "', rasBase_R_VERSION[["nickname"]], '" !!!', "\n"
  );
  RCT_ACT_DEV_INFO_BODY_LVL_03_ <- rasBasePASTE0(   # -> Creates a Devs TimeStamp ...
    "-> CODE SUPPORT: 3rd Party R Packages used (as DEV-TOOLS) during development [ n: ",
    "2 ] ...", "\n",
    "> devtools  ==>  v", rssVersDEVTOOLS_, "\n",
    "> roxygen2  ==>  v", rssVersROXYGEN2_, "\n"
  );
  RCT_ACT_DEV_INFO_BODY_LVL_04_ <- rasBasePASTE0(   # -> Creates a Devs TimeStamp ...
    "-> DEPENDENCIES: 3rd Party R Packages used (as DEPs) in this R-Library Project [ n: ",
        rsnDepsN_, " ] ...", "\n",
    rssLibrDepsLIST_   # <- Adds the R-Library DEPENDECIES <compiled> List !!!
  );
  RCT_ACT_DEV_INFO_BODY_END_STUB_ <- rasBasePASTE0(   # -> Creates a Devs TimeStamp ...
    "| === === === === === === === END of 'ACT-DEV' TRCKR File !!! === === === === === === === |",
    "\n"
  );
  
  
  
  ####   STEP 08 - Write Updated DATA to ( Act_Dev_TRCKR.txt ) File   ####
  rasBaseWriteLINES(   # -> Writes the compiled data to the "Act_Dev_TRCKR.txt" file ...
    con = RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_, 
    text = rasBasePASTE0(
      RCT_ACT_DEV_INFO_HEADER_,      "\n\n\n", 
      RCT_ACT_DEV_INFO_BODY_LVL_01_, "\n\n\n",
      RCT_ACT_DEV_INFO_BODY_LVL_02_, "\n\n\n",
      RCT_ACT_DEV_INFO_BODY_LVL_03_, "\n\n\n",
      RCT_ACT_DEV_INFO_BODY_LVL_04_, "\n\n\n",
      RCT_ACT_DEV_INFO_BODY_END_STUB_
    )
  );
  
  
  
  ####   STEP 09 - Patch (update) Version in Project DESCRIPTION File   ####
  rasDescSetVERSION(
    file = RCT_FILE_R_PKG_DESC_,
    version = rasBaseIfELSE(
      sbIsProdRel, rssVersNewPROD_, rssVersNewDEVS_
    )
  );
  
  
  
  ####   STEP 10 - COMPLETION AUDIO Feedback (only prod-release)   ####
  if (rsbAudioNote_) {
    rasBeeprBEEP(2);   # <- Plays the "Coin" audio clip from the `beepr` R Library !!!
  }
  
  
  
  ####   STEP 11 - Output <key> Function Results   ####
  rasMfmrReturnLockedLIST(
    vsListNames = c(
      "CODE_NAME_TAG",    # <- R Library Project Identifier -> TAG !!!
      "CODE_VERS_PROD",   # <- R Library <Code> PRODUCTION VERSION NUMBER -> TAG !!!
      "CODE_VERS_DEVS",   # <- R Library <Code> DEVELOPMENT <Debug> VERSION NUMBER -> TAG !!!
      "CODE_PUSH_TYPE",   # <- R Library <Code> PUSH (i.e. Code Check or Commit) TYPE -> TAG !!!
      "CODE_PUSH_DATE"    # <- R Library <Code> PUSH (i.e. Code Check or Commit) DateTime -> TAG !!!
    ),
    lsListVals = rasBaseLIST(
      RCT_REGENT_R_LIB_ID_,   # <- R Library Project Identifier -> VALUE !!! 
      rssVersNewPROD_,        # <- R Library <Code> PRODUCTION VERSION NUMBER -> VALUE !!!
      rssVersNewDEVS_,        # <- R Library <Code> DEVELOPMENT <Debug> VERSION NUMBER -> VALUE !!!
      RCT_CODE_PUSH_TYPE_,    # <- R Library <Code> PUSH (i.e. Code Check or Commit) TYPE -> VALUE.
      rcoCodePushDateTIME_    # <- R Library <Code> PUSH (i.e. Code Check/Commit) DateTime -> VALUE.
    ),
    sbLockList = TRUE
  );
  
}
