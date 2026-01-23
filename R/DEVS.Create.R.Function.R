#? ### ### ### ### ### ### ###
#' @title CRAN Code Validation with Code Version Tracking ("SuiteMFMR" DevTools)
#' @name devs.create.r.function
#' @family SuiteMFMR DevTools
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
#' \dontrun{   ### <- This function constitutes a development utility !!! This function creates (and
#'             ###    requires) a special development directory ("./WIP") that is created during the
#'             ###    init-run (initial R project setup) phase and is intended to facilitate a user-
#'             ###    friendly R Library development process. For these reasons the code examples 
#'             ###    below should not be executed during "R_CMD_CHECK" code check procedures.
#'             
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
#' }
#'
#' @export
#? ### ### ###
"devs.create.r.function" <- function(
  ssFuncID="CODE.New.R.Func", ssFuncDir="./R", sbIsLibrFunc=TRUE, ...
) {
  
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_DBL_R_FUNC_RT_START_ <- base::Sys.time();           # <- Extract <active> System Date-Time !!!
  RCT_TAG_R_LIBR_ID_       <- "MFMRutils";                # <- R Library Identifier !!!
  RCT_TAG_R_FUNC_ID_SHORT_ <- "Create.R.Func";            # <- Function ID - TRUNCATED !!!
  RCT_TAG_FUNC_ID_LONG_    <- "DEVS.Create.R.Function";   # <- Function ID - DETAILED !!! 
  
  RCT_INT_CELN_START_ <- 66L;    # <- The Code Editor Line Number (CELN) at which the function 
                                 #    OPENING <normal> brace/bracket "(" is located !!!
  RCT_INT_CELN_STOP_  <- 239L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    CLOSING <curly> brace/bracket "}" is located !!!
  coDotsArgs_ <- base::list(...);   # <- Capture all the "DotsArgs" values here !!!
  
  
  
  ####   STEP 02 - Prime NB "Aliases" used locally (inside function)   ####
  ##   NOTES: This approach is implemented for improving R Session <run-time> Memory Efficiency ...
  rasBaseC      <- base::c;
  rasBaseCAT    <- base::cat;
  rasBaseLIST   <- base::list;
  rasBaseTRUNC  <- base::trunc;
  rasBaseROUND  <- base::round;
  rasBaseRETURN <- base::return;
  
  `%??%`                    <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!! 
  rasMfmrDEVS               <- MFMRutils::RENV_DEVS;
  rasMfmrICONS              <- MFMRutils::RENV_ICONS;
  rasMfmrCOLORS             <- MFMRutils::RENV_COLOURS;
  rasMfmrFORMATS            <- MFMRutils::RENV_FORMATS;
  rasMfmrPullLibrINFO       <- MFMRutils::devs.pull.libr.info;
  rasMfmrInfoPostFuncSelfID <- MFMRutils::info.post.func.self.id;
  
  
  
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
  RCT_FOLDER_WIP_PROD_    <- rasMfmrDEVS$PATH_FOLDER_WIP_PROD;
  RCT_FORMAT_TIME_DEV_01_ <- rasMfmrDEVS$FORMAT_TIME_DEV_LOG_V01;
  RCT_FORMAT_TIME_DEV_02_ <- rasMfmrDEVS$FORMAT_TIME_DEV_LOG_V02;
  RCT_FOLDER_WIP_HELPERS_ <- rasMfmrDEVS$PATH_FOLDER_WIP_HELPERS;
  RCT_FILE_DEV_TIME_LOG_  <- rasMfmrDEVS$PATH_FILE_WIP_TIME_STAMP;
  RCT_FILE_R_PKG_DESC_    <- rasMfmrDEVS$PATH_TO_FILE_R_PACKAGE_DESC;
    
  
  
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
    rasDevToolsDOCUMENT(roclets = rasBaseC('rd', 'collate', 'namespace'));
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


