#? ### ### ### ### ### ### ###
#' @title CRAN Code Validation with Code Version Tracking ("SuiteMFMR" DevTools)
#' @name devs.create.r.function
#' @family SuiteMFMR DevTools
#' 
#' @description
#' A Helper Function that executes the CRAN pre-requisite Code Checking Procedure during active R
#' Package Development. This function programmatically updates the package version number in the R 
#' Project DESCRIPTION file before running the required documentation and CRAN Package 
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
#' @param sbIsProdRel a logical (boolean) value that captures if the code-check and code commit
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
  ssFuncName="CODE.New.R.FUNC", ssFuncPath="./R", sbIsLibrFunc=TRUE, sbForcePath=FALSE, 
  sbForceCreate=FALSE, ssFuncEXT=".R", ssNameDELIM=".", ...
) {
  
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_DBL_R_FUNC_RT_START_ <- base::Sys.time();           # <- Extract <active> System Date-Time !!!
  RCT_TAG_R_LIBR_ID_       <- "MFMRutils";                # <- R Library Identifier !!!
  RCT_TAG_R_FUNC_ID_SHORT_ <- "Create.R.FUNC";            # <- Function ID - TRUNCATED !!!
  RCT_TAG_R_FUNC_ID_LONG_  <- "DEVS.Create.R.Function";   # <- Function ID - DETAILED !!! 
  
  RCT_INT_CELN_START_ <- 66L;    # <- The Code Editor Line Number (CELN) at which the function 
                                 #    OPENING <normal> brace/bracket "(" is located !!!
  RCT_INT_CELN_STOP_  <- 239L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    CLOSING <curly> brace/bracket "}" is located !!!
  coDotsArgs_ <- base::list(...);   # <- Capture all the "DotsArgs" values here !!!
  
  
  
  ####   STEP 02 - Prime NB "Aliases" used locally (inside function)   ####
  ##   NB: This approach is implemented for improving R Session <run-time> Memory Efficiency ...
  rasBaseC          <- base::c;
  rasBaseLIST       <- base::list;
  rasBaseSTOP       <- base::stop;
  rasBaseNCHAR      <- base::nchar;
  rasBaseLENGTH     <- base::length;
  rasBaseRETURN     <- base::return;
  rasBasePASTE0     <- base::paste0;
  rasBaseSubSTR     <- base::substr;
  rasBaseWARN       <- base::warning;
  rasBaseIsNULL     <- base::is.null;
  rasBaseToLOWER    <- base::tolower;
  rasBaseFilePATH   <- base::file.path;
  rasBaseIsBOOL     <- base::is.logical;
  rasBaseDirCREATE  <- base::dir.create;
  rasBaseWriteLINES <- base::writeLines;
  rasBaseDirEXISTS  <- base::dir.exists;
  rasBaseFileEXISTS <- base::file.exists;
  rasBaseFileCREATE <- base::file.create;
  rasBaseIsCHAR     <- base::is.character;
  
  `%??%`               <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasMfmrFSID          <- MFMRutils::RENV_FSID;
  rasMfmrGetCELN       <- MFMRutils::code.get.celn;
  rasMfmrPostNOTE      <- MFMRutils::info.post.note;
  rasMfmrPullLibrINFO  <- MFMRutils::devs.pull.libr.info;
  rasMfmrCleanPATH     <- MFMRutils::code.clean.file.path;
  rasMfmrPostFSID      <- MFMRutils::info.post.func.self.id;
  rasMfmrSplitOnVECTOR <- MFMRutils::code.split.string.on.vector;
  
  
  
  ###   Run Function Self-ID (ENTRY) Notification   ###
  rasMfmrPostFSID(
    ssFuncSelfID = RCT_TAG_R_FUNC_ID_LONG_, siFuncMode01L = 1L, 
    siStartCELN = RCT_INT_CELN_START_, siStopCELN = RCT_INT_CELN_STOP_, ...
  );
  
  
  
  ####   STEP 03 - Prime NB Function Variables   ####
  ##   3.1 - Internalize Function Arguments ...
  ssFuncEXT_     <- ssFuncEXT;
  ssFuncName_    <- ssFuncName;
  sbForcePath_   <- sbForcePath;
  ssNameDELIM_   <- ssNameDELIM;
  sbIsLibrFunc_  <- sbIsLibrFunc;
  sbForceCreate_ <- sbForceCreate;
  ssFuncPath_    <- rasMfmrCleanPATH(ssFuncPath);
  
  ##   3.2 - Prime Function Variables ...
  sbShowTAIL_      <- FALSE;
  ssFindCodeALIAS_ <- "rasMfmrPostNOTE\\(";
  
  rasMfmrPostNOTE(
    ssHead = RCT_TAG_R_FUNC_ID_SHORT_, sbShowTail = sbShowTAIL_,
    ssBody = "Internalized Function Arguments ...",
    siCallCELN = rasMfmrGetCELN(
      ssFuncName = RCT_TAG_R_FUNC_ID_LONG_,
      siCallIndex = 1L, sbUseAlias = TRUE, ssAliasValue = ssFindCodeALIAS_,
      sbRunByForce = coDotsArgs_[[rasMfmrFSID$F_ARGS_BOOL_RUN_BY_FORCE]] %??% FALSE
    ), 
    ...
  );
  
  
  
  ####   STEP 04 - Run Func-Args NULL Checks   ####
  if (rasBaseIsNULL(ssFuncName_) || rasBaseNCHAR(ssFuncName_) == 0 || !rasBaseIsCHAR(ssFuncName_)) {
    rasBaseSTOP(
      "  -> The Function ARGUMENT [`ssFuncName`] FAILED the NULL and DATA TYPE CHECKS !!!\n"
    );
  }
  if (rasBaseIsNULL(ssFuncPath_) || rasBaseNCHAR(ssFuncPath_) == 0 || !rasBaseIsCHAR(ssFuncPath_)) {
    rasBaseSTOP(
      "  -> The Function ARGUMENT [`ssFuncPath`] FAILED the NULL and DATA TYPE CHECKS !!!\n"
    );
  }
  if (rasBaseIsNULL(sbIsLibrFunc_) || 
      rasBaseNCHAR(sbIsLibrFunc_) == 0 || !rasBaseIsBOOL(sbIsLibrFunc_)) {
    rasBaseSTOP(
      "  -> The Function ARGUMENT [`sbIsLibrFunc`] FAILED the NULL and DATA TYPE CHECKS !!!\n"
    );
  }
  
  rasMfmrPostNOTE(
    ssHead = RCT_TAG_R_FUNC_ID_SHORT_, sbShowTail = sbShowTAIL_,
    ssBody = "Function Arguments NULL-CHECKS PASSED !!!",
    siCallCELN = rasMfmrGetCELN(
      ssFuncName = RCT_TAG_R_FUNC_ID_LONG_,
      siCallIndex = 2L, sbUseAlias = TRUE, ssAliasValue = ssFindCodeALIAS_,
      sbRunByForce = coDotsArgs_[[rasMfmrFSID$F_ARGS_BOOL_RUN_BY_FORCE]] %??% FALSE
    ),
    ...
  );
  
  
  
  ####   STEP 05 - Run File-Path Check   ####
  if (!rasBaseDirEXISTS(ssFuncPath_)) {
    if (sbForcePath_) {   # <- Create File Directory (by brute force), if not exists !!!
      rasBaseWARN(
        rasBasePASTE0(
          "  -> DIRECTORY [`ssFuncPath`] CREATED VIA BRUTE-FORCE !!!\n",
          "       ~> DIRECTORY NOT FOUND, but [`sbForcePath`] ARGUMENT was set to TRUE !!!\n"
        )
      );
      rasBaseDirCREATE(
        recursive = T, showWarnings = F, path = ssFuncPath_
      );
    } else {
      rasBaseSTOP(
        rasBasePASTE0(
          "  -> Specified DIRECTORY NOT FOUND (path/directory does not exist) !!!\n",
          "       ~> Please CHECK SPELLING OF the PATH ARGUMENT [`ssFuncPath`] VALUE !!!\n",
          "       ~> SPECIFIED [`ssFuncPath`] VALUE: \"", ssFuncPath_,"\" !!!\n"
        )
      );
    }
  }
  
  rasMfmrPostNOTE(
    ssHead = RCT_TAG_R_FUNC_ID_SHORT_, sbShowTail = sbShowTAIL_,
    ssBody = "Function Creation File Path VERIFIED !!!",
    siCallCELN = rasMfmrGetCELN(
      ssFuncName = RCT_TAG_R_FUNC_ID_LONG_,
      siCallIndex = 3L, sbUseAlias = TRUE, ssAliasValue = ssFindCodeALIAS_,
      sbRunByForce = coDotsArgs_[[rasMfmrFSID$F_ARGS_BOOL_RUN_BY_FORCE]] %??% FALSE
    ),
    ...
  );
  
  
  
  ####   STEP 06 - Run File-Clash Check   ####
  ##   6.01 - Compile the <FULL> File Name (i.e. file path + name + ext)   ####
  svFileNamePathFULL_ <- NULL;
  svFileEXT_ <- rasMfmrSplitOnVECTOR(
    ssSplitString = ssFuncName_, vsSplitVector = c(ssNameDELIM)
  )[["SPLITS"]];
  ssActivFileEXT_ <- rasBasePASTE0(".", svFileEXT_[rasBaseLENGTH(svFileEXT_)]);
  if (ssActivFileEXT_ == ssFuncEXT_ || ssActivFileEXT_ == rasBaseToLOWER(ssFuncEXT_)) {
    ssFuncName_ <- rasBaseSubSTR(x = ssFuncName_, start = 1, stop = rasBaseNCHAR(ssFuncName_) - 2);
    svFileNamePathFULL_ <- rasBaseFilePATH(ssFuncPath_, rasBasePASTE0(ssFuncName_, ssFuncEXT_));
  } else {
    svFileNamePathFULL_ <- rasBaseFilePATH(ssFuncPath_, rasBasePASTE0(ssFuncName_, ssFuncEXT_));
  }
  
  rasMfmrPostNOTE(
    ssHead = RCT_TAG_R_FUNC_ID_SHORT_, sbShowTail = sbShowTAIL_,
    ssBody = "Function File Name <idenfitier> Sanitation COMPLETED !!!",
    siCallCELN = rasMfmrGetCELN(
      ssFuncName = RCT_TAG_R_FUNC_ID_LONG_,
      siCallIndex = 4L, sbUseAlias = TRUE, ssAliasValue = ssFindCodeALIAS_,
      sbRunByForce = coDotsArgs_[[rasMfmrFSID$F_ARGS_BOOL_RUN_BY_FORCE]] %??% FALSE
    ),
    ...
  );
  
  
  ##   6.02 - WARN User if File Already Exists (file name-space/path clash)   ####
  if (rasBaseFileEXISTS(svFileNamePathFULL_)) {
    if (sbForceCreate_) {
      rasBaseWARN(
        rasBasePASTE0(
          "  -> Requested FUNCTION [`", ssFuncName_, ssFuncEXT_, "`] ALREADY EXISTS !!!\n",
          "       ~> FUNCTION ALREADY EXISTS, but [`sbForceCreate`] ARGUMENT was set to TRUE !!!\n",
          "       ~> FUNCTION RENAMED with \"_v02\" POST-PENDED STUB -> New FUNC-ID: [`", 
          ssFuncName_, ssFuncEXT_, "`] !!!\n"
        )
      );
    } else {
      rasBaseSTOP(
        rasBasePASTE0(
          "  -> Requested FUNCTION [`", ssFuncName_, ssFuncEXT_, "`] ALREADY EXISTS !!!"
        )
      );
    }
  }
  
  
  ###   Run Function Self-ID (EXIT) Notification   ###
  rasMfmrPostFSID(
    ssFuncSelfID = RCT_TAG_R_FUNC_ID_LONG_, siFuncMode01L = 0L, 
    siStartCELN = RCT_INT_CELN_START_, siStopCELN = RCT_INT_CELN_STOP_, 
    csTimeStart = RCT_DBL_R_FUNC_RT_START_, csTimeStop = base::Sys.time(), ...
  );
  
  
  
  # 7. Return the new created Project Version Number as a character object ...
  rasBaseRETURN(
    rasBaseLIST(
      "ProjID" = rssActProjID_,
      "CodeVers" = rasBasePASTE0("v", ssVersNewFULL),
      "CodeTime" = rasBaseFORMAT(RCT_FUNC_RUN_TIME_STOP_, RCT_FORMAT_TIME_DEV_02_)
    )
  );
  
}


