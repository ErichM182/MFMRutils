#? ### ### ### ### ### ### ###
#' @title The Active Development Tracker File Updater ("SuiteMFMR" DevTools)
#' @name devs.update.act.dev.trackr.file
#' 
#' @description
#' A Helper Function that updates important information inside the "Active Development Information
#' Tracker File" (i.e. "00_ACT_DEVS_INFO_TRACKER.txt") located in the active project's development 
#' or launchpad folder (i.e. the "./WIP/" path or directory). This "Active Development Information
#' Tracker File" will be programmatically initialized (created) by this helper function if not 
#' found in the default project directory (i.e. "./WIP/" project folder).
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
#'                                             #    Process.
#'
#' @keywords internal
#' @noRd
#? ### ### ###
"devs.update.act.dev.trackr.file" <- function() {
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_ID_SHRT_ <- "Update.Dev.Trackr";                 # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Update.Act.Dev.Trackr.File";   # <- Function ID - LONG !!!
  RCT_TAG_FUNC_LIBR_ID_ <- MFMRutils::devs.pull.libr.info()[["NAME"]];
  
  
  ####   STEP 02 - Prime NB "Aliases" used locally (inside function)   ####
  `%?!%`              <- MFMRutils::`%?!%`;   # <- VERY COOL Alias <NCO> !!!
  rasRETURN           <- base::return;
  rasPASTE0           <- base::paste0;
  rasFORMAT           <- base::format;
  rasSysTimeNOW       <- base::Sys.time;
  rasFilePATH         <- base::file.path;
  rasMfmrCONSTS       <- MFMRutils::cMISC;
  rasDirCREATE        <- base::dir.create;
  rasWriteLINES       <- base::writeLines;
  rasFileEXISTS       <- base::file.exists;
  rasFileCREATE       <- base::file.create;
  rasMfmrPullLibrINFO <- MFMRutils::devs.pull.libr.info;
  
  
  ####   STEP 03 - Create Folder & File ( IF NOT EXISTS )   ####
  RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_ <- rasMfmrCONSTS$PATH_TO_FILE_ACT_DEV_INFO_TRACKR;
  if (!rasFileEXISTS(RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_)) {   # <- Checks that FILE DOES NOT EXIST !!!
    rasDirCREATE(   # -> Creates the "./WIP" directory (if not already exists) ...
      path = rasMfmrCONSTS$PATH_TO_FOLDER_WIP, recursive = T, showWarnings = F
    );
    rasFileCREATE(RCT_PATH_FILE_ACT_DEV_INFO_TRCKR_);   # -> Creates the required file ...
  }
  
  
  ####   STEP 04 - COMPILE Important "ActDev" INFO   ####
  RCT_FORMAT_TIME_DEV_01_     <- rasMfmrCONSTS$FORMAT_TIME_DEV_LOG_V01;
  RCT_FORMAT_TIME_DEV_02_     <- rasMfmrCONSTS$FORMAT_TIME_DEV_LOG_V02;
  RCT_SYS_DATE_TIME_NOW_      <- rasSysTimeNOW();
  rssVersNewFULL_             <- rasPASTE0(   # -> Format the NEW Version Date accordingly ...
    rasFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_01_), ".", ssVersNEW
  );
  RCT_REGENT_R_LIB_DESC_INFO_ <- rasMfmrPullLibrINFO(rasMfmrCONSTS$PATH_TO_FILE_R_PACKAGE_DESC);
  RCT_REGENT_R_LIB_ID_        <- RCT_REGENT_R_LIB_DESC_INFO_[["NAME"]];
  RCT_REGENT_R_LIB_VERS_      <- RCT_REGENT_R_LIB_DESC_INFO_[["VERSION"]];
  RCT_ACT_DEV_INFO_HEADER_    <- rasPASTE0(
    "| === === === === === === === === === === === === === === === === === === === === |",
    "|       Active Development Tracking Information (regent R Library Project)        |",
    "| === === === === === === === === === === === === === === === === === === === === |"
  );
  RCT_ACT_DEV_INFO_BODY_LVL_01_ <- rasPASTE0(   # -> Creates a Devs TimeStamp ...
    "> R-Lib Project ID: ", RCT_REGENT_R_LIB_ID_, "\n",
    "> Last Code Push (vers)  ==>  ", RCT_REGENT_R_LIB_VERS_, "\n",
    "> Last Code Push (time)  ==>  ", rasFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_02_)
  );
  
}
