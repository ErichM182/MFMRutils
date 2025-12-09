#? ### ### ### ### ### ### ###
#' @title CRAN Code Validation with Code Version Tracking ("SuiteMFMR" DevTools)
#' @name devs.check.code.specs
#' 
#' @description
#' A Helper Function that executes the CRAN pre-requisite Code Checking Procedure during active R
#' Package Development. This function programmatically updates the package version number in the R 
#' Project DESCRIPTION file before running the required documentation and/or CRAN Package 
#' Pre-Submission Code Requirement Checks (i.e. CRAN Code Validation) during iterative development 
#' cycles. This Helper Function was originally crafted as technical support tool for the "MFMR
#' Suite of R Libraries" (aka "SuiteMFMR"), but may also be helpful as a support tool during the R 
#' Package Development Cycle of other (3rd Party) R Projects.
#'
#' @param sbCheckDocs a logical (boolean) value that specifies whether to run the standard package 
#'                    documentation process only (not the FULL CRAN Code Validation Process/Check).
#' @param sbCheckCRAN a logical (boolean) value that specifies whether to run the FULL CRAN Code 
#'                    Validation Process/Check (inclusive of the pre-check documentation steps).
#' @param ssTimeZone a simple character vector (string) that defines the Time Zone to be used for 
#'                   the package documentation.
#' @param sbIsProdRel a logical (boolean) value that captures if the code-check and/or code commit
#'                    process (action) is a "Production Release" code development action or not.
#' @param ... the fall-through function arguments (i.e. DotsArgs) used for nested functions within 
#'            this main function. The "DotsArgs" implementation here pertain to the following nested 
#'            R functions (i.e. R functions used within this main R function): <br>
#'               1. [MFMRutils::info.post.func.self.id()], and <br>
#'               2. [MFMRutils::devs.patch.code.dev.trckr.file()].
#'
#' @returns
#' * This function returns the programmatically amended or updated (real-time or active) version 
#'   number for the active R Library Project as a list of character objects.
#' * This function creates a Work-In-Progress (WIP) directory at the root of the active R-Libs 
#'   Project (i.e. "./WIP" <- if not already exists). The "./WIP" folder and all of its contents 
#'   are included in GIT push-&-pull processes. You must MANUALLY ADD the following code stub 
#'   (^WIP$) <sans parentheses> to the ".Rbuildignore" R Project file to ensure the "./WIP" 
#'   folder + contents are excluded from the R Project Build Process. (<- this is IMPORTANT !!!)
#' * This function also creates a "DevsVersTimeStamp.txt" file in the "./WIP" folder for secondary 
#'   development version tracking purposes (as needed).
#'
#' @import beepr desc devtools
#'
#' @examples
#' ### Run R Package DevCode easily as follows ...
#' library(MFMRutils)   # <- Loads "MFMRutils" library (if already installed) !!!
#'
#' ### Run 2 different types of code check/validation processes ...
#' devs.check.code.specs(sbCheckDocs = TRUE)   # -> Executes only the DevTools Documentation 
#'                                             #    Process.
#' devs.check.code.specs(sbCheckCRAN = TRUE)   # -> Executes the more complete (stringent) CRAN Code 
#'                                             #    Validation Process.
#'
#' ### Check (i.e. "rasDevToolsCHECK()") overrides the documentation process ...
#' # The complete (CRAN) Documentation Process will only be executed once if both are TRUE !!!
#' devs.check.code.specs(sbCheckDocs = TRUE, sbCheckCRAN = TRUE)
#'
#' @export
#? ### ### ###
"devs.check.code.specs" <- function(
  sbCheckDocs=TRUE, sbCheckCRAN=FALSE, 
  ssTimeZone="Africa/Windhoek", sbIsProdRel = FALSE, ...
) {
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_ID_SHRT_ <- "Check.Specs";              # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Check.Code.Specs";   # <- Function ID - LONG !!!
  
  base::Sys.setenv(TZ = ssTimeZone);   # <- Set correct Time Zone BEFORE querying System CLOCK !!!
  RCT_FUNC_RUN_TIME_START_ <- base::Sys.time();
  RCT_FUNC_CELN_START_ <- 57L; RCT_FUNC_CELN_STOP_ <- 228L;
  RCT_TAG_FUNC_LIBR_ID_ <- MFMRutils::devs.pull.libr.info()[["NAME"]];
  
  
  
  ####   STEP 02 - Define "Local Aliases" for ALL Functions   ####
  ###    NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasBaseCAT        <- base::cat;
  rasBaseLIST       <- base::list;
  rasBaseTRUNC      <- base::trunc;
  rasBaseROUND      <- base::round;
  rasBaseRETURN     <- base::return;
  rasBasePASTE0     <- base::paste0;
  rasBaseIfELSE     <- base::ifelse;
  rasBaseFORMAT     <- base::format;
  rasBaseUNLIST     <- base::unlist;
  rasBaseLENGTH     <- base::length;
  rasBaseSPRINTF    <- base::sprintf;
  rasBaseDiffTIME   <- base::difftime;
  rasBaseStrSPLIT   <- base::strsplit;
  rasBaseSysTimeNOW <- base::Sys.time;
  rasBaseINVISIBLE  <- base::invisible;
  rasBaseFilePATH   <- base::file.path;
  rasBaseDirCREATE  <- base::dir.create;
  rasBaseAsNUM      <- base::as.numeric;
  rasBaseSysSetENV  <- base::Sys.setenv;
  rasBaseWriteLINES <- base::writeLines;
  rasBaseFileCREATE <- base::file.create;
  rasBaseFileEXISTS <- base::file.exists;
  rasBaseAsCHAR     <- base::as.character;
  
  rasDescSetVERSION <- desc::desc_set_version;
  
  rasDevToolsCHECK    <- devtools::check;
  rasDevToolsLoadALL  <- devtools::load_all;
  rasDevToolsDOCUMENT <- devtools::document;
  rasDevToolsCleanDLL <- devtools::clean_dll;
  
  `%??%`                       <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!! 
  rasMfmrICONS                 <- MFMRutils::cICONS;
  rasMfmrCONSTS                <- cMISC;
  rasMfmrCOLORS                <- MFMRutils::cCOLORS;
  rasMfmrFORMATS               <- MFMRutils::cFORMATS;
  rasMfmrPullLibrINFO          <- MFMRutils::devs.pull.libr.info;
  rasMfmrInfoPostFuncSelfID    <- MFMRutils::info.post.func.self.id;
  rasMfmrDevsPrintCodeCheckRES <- devs.print.code.check.res;
  rasMfmrDevsPatchCodeDevTRCKR <- devs.patch.code.dev.trckr.file;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rsbCheckDocs_ <- sbCheckDocs;
  rsbCheckCRAN_ <- sbCheckCRAN;
  rssTimeZone_  <- ssTimeZone;
  rsbIsProdRel_ <- sbIsProdRel;
  
  rvsDotsArgs_     <- rasBaseLIST(...);
  rsbAudioNote_    <- rvsDotsArgs_[["sbAudioNote"]]    %??% FALSE;
  rsbPrintPretty_  <- rvsDotsArgs_[["sbPrintPretty"]]  %??% TRUE;
  rsbRunSelfID_    <- rvsDotsArgs_[["sbRunSelfID"]]    %??% FALSE;
  rssFuncCallerID_ <- rvsDotsArgs_[["ssFuncCallerID"]] %??% "TOP-LVL (rsProjMAIN)";
  
  
  
  ####   STEP 04 - Run Function SELF-ID <ENTRY> (if requested)   ####
  rasMfmrInfoPostFuncSelfID(
    ssFuncSelfID = RCT_TAG_FUNC_ID_FULL_,
    siFuncMode01 = 1L, ssFuncCallerID = rssFuncCallerID_,
    sbRunSelfID = rsbRunSelfID_, sbPrintPretty = rsbPrintPretty_, 
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUN_TIME_START_, csTimeStop = RCT_FUNC_RUN_TIME_START_
  );
  
  
  
  ####   STEP 05 - Define Critical Constants   ####
  RCT_SYS_DATE_TIME_NOW_ <- rasBaseSysTimeNOW();
  
  RCT_ANSI_BOLD_    <- rasMfmrFORMATS$BOLD;
  RCT_ANSI_RESET_   <- rasMfmrFORMATS$RESET;
  RCT_ANSI_ITALICS_ <- rasMfmrFORMATS$ITALICS;
  
  RCT_ICON_POINT_UP_      <- rasMfmrICONS$PointUP;
  RCT_ICON_WHITE_X_       <- rasMfmrICONS$X_White;
  RCT_ICON_SMILEY_SAD_    <- rasMfmrICONS$SmileySad;
  RCT_ICON_OOGLY_EYES_    <- rasMfmrICONS$OoglyEyes;
  RCT_ICON_CHECK_MARK_    <- rasMfmrICONS$CheckMark;
  RCT_ICON_ARROW_RIGHT_   <- rasMfmrICONS$ArrowRIGHT;
  RCT_ICON_SMILEY_PONDER_ <- rasMfmrICONS$SmileyPonder;
  
  RCT_COLOR_RED_          <- rasMfmrCOLORS$RedFORE;
  RCT_COLOR_BLUE_         <- rasMfmrCOLORS$BlueFORE;
  RCT_COLOR_GREEN_        <- rasMfmrCOLORS$GreenFORE;
  RCT_COLOR_TELLOW_       <- rasMfmrCOLORS$YellowFORE;
  RCT_FOLDER_WIP_PROD_    <- rasMfmrCONSTS$PATH_FOLDER_WIP_PROD;
  RCT_FORMAT_TIME_DEV_01_ <- rasMfmrCONSTS$FORMAT_TIME_DEV_LOG_V01;
  RCT_FORMAT_TIME_DEV_02_ <- rasMfmrCONSTS$FORMAT_TIME_DEV_LOG_V02;
  RCT_FOLDER_WIP_HELPERS_ <- rasMfmrCONSTS$PATH_FOLDER_WIP_HELPERS;
  RCT_FILE_DEV_TIME_LOG_  <- rasMfmrCONSTS$PATH_FILE_WIP_TIME_STAMP;
  RCT_FILE_R_PKG_DESC_    <- rasMfmrCONSTS$PATH_TO_FILE_R_PACKAGE_DESC;
    
  
  
  ####   STEP 06 - Patch CODE DEV TRACKER   ####
  # 2. Extract the current version number from the DESCRIPTION file ...
  rlsCodeVERS_ <- rasMfmrDevsPatchCodeDevTRCKR(
    sbIsProdRel = rsbIsProdRel_, sbAudioNote = rsbAudioNote_
  );
  rssActProjID_ <- rlsCodeVERS_[["CODE_NAME_TAG"]];
  ssVersNewFULL <- rlsCodeVERS_[["CODE_VERS_DEVS"]];
  
  
  ####   STEP 07 - Run DOCs and/or CRAN Code Checks   ####
  # 6. Finally - Run the required R-Libs Project Documentation & CRAN Checks !!!
  if (rsbCheckCRAN_ || rsbIsProdRel_) {   # -> Runs the COMPLETE Documentation & CRAN Requirements 
                                          #    Checking Processes !!!
    rasDevToolsCleanDLL();   # <- ALWAYS run BEFORE 'rasDevToolsLoadALL()' !!!
    rasDevToolsLoadALL();    # <- ALWAYS run AFTER 'rasDevToolsCleanDLL()' !!!
    
    coCheckResult_ <- rasDevToolsCHECK();
    snLenNOTEs_    <- rasBaseLENGTH(coCheckResult_$notes);
    snLenERRORs_   <- rasBaseLENGTH(coCheckResult_$errors);
    snLenWARNINGs_ <- rasBaseLENGTH(coCheckResult_$warnings);
    rasMfmrDevsPrintCodeCheckRES(
      snLenERRORs = snLenERRORs_, snLenWARNs = snLenWARNINGs_, snLenNOTEs = snLenNOTEs_, 
      sbIsProdRel = rsbIsProdRel_, ssActProjID = rssActProjID_, ssProjVers = ssVersNewFULL
    );
    
  }
  
  # -> Runs the Documentation process ONLY IF the "sbCheckCRAN" value is FALSE !!!
  if (!rsbCheckCRAN_ && rsbCheckDocs_) {   
    rasDevToolsDOCUMENT(roclets = c('rd', 'collate', 'namespace'));
  }
  
  
  
  ####   STEP 06 - Run Function SELF-ID <EXIT> (if requested)   ####
  RCT_FUNC_RUN_TIME_STOP_ <- rasBaseSysTimeNOW();
  rasMfmrInfoPostFuncSelfID(
    ssFuncSelfID = RCT_TAG_FUNC_ID_FULL_,
    siFuncMode01 = 0L, ssFuncCallerID = rssFuncCallerID_,
    sbRunSelfID = rsbRunSelfID_, sbPrintPretty = rsbPrintPretty_, 
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUN_TIME_START_, csTimeStop = RCT_FUNC_RUN_TIME_STOP_
  );
  
  
  
  # 7. Return the new created Project Version Number as a character object ...
  rasBaseRETURN(
    rasBaseINVISIBLE(
      rasBaseLIST(
        "ProjID" = rssActProjID_,
        "CodeVers" = rasBasePASTE0("v", ssVersNewFULL),
        "CodeTime" = rasBaseFORMAT(RCT_FUNC_RUN_TIME_STOP_, RCT_FORMAT_TIME_DEV_02_)
      )
    )
  );
  
}


