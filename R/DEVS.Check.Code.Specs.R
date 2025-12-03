#? ### ### ### ### ### ### ###
#' @title CRAN Code Validation with File Version Tracking ("SuiteMFMR" DevTools)
#' @name devs.check.code.specs
#' 
#' @description
#' A Helper Function that executes the CRAN pre-requisite Code Checking Procedure
#' during active R Package Development. This function programmatically updates the
#' package version number in the R Project DESCRIPTION file before running the
#' required documentation and/or CRAN Package Pre-Submission Code Requirement
#' Checks (i.e. CRAN Code Validation) during iterative development cycles. This 
#' Helper Function was originally crafted as technical support tool for the "MFMR
#' Suite of R Libraries" (aka "SuiteMFMR"), but may also be helpful as a support
#' tool during the R Package Development Cycle of other (3rd Party) R Projects.
#'
#' @param sbCheckDocs a logical (boolean) value that specifies whether to run the
#'                    standard package documentation process (only - not the FULL
#'                    CRAN Code Validation Process/Check).
#' @param sbCheckCRAN a logical (boolean) value that specifies whether to run the
#'                    FULL CRAN Code Validation Process/Check (inclusive of the 
#'                    pre-check documentation steps).
#' @param ssTimeZone a simple character vector (string) that defines the Time Zone
#'                   to be used for the package documentation.
#' @param ... the fall-through (DotsArgs) function arguments used to prime nested
#'            functions within this main function. These "DotsArgs" pertain mostly
#'            to the "Func-SELF-ID" SuiteMFMR helper function.
#'
#' @returns
#' * This function returns the programmatically amended or updated (real-time or
#'   active) version number for the active R Library Project as a list of character
#'   objects.
#' * This function creates a Work-In-Progress (WIP) directory at the root of the
#'   active R-Libs Project (i.e. "./WIP" <- if not already exists). The "./WIP"
#'   folder and all of its contents are included in GIT push-&-pull processes. You
#'   must MANUALLY ADD the following code stub (^WIP$) <sans parentheses> to the 
#'   ".Rbuildignore" R Project file to ensure the "./WIP" folder + contents are 
#'   excluded from the R Project Build Process. (<- this is IMPORTANT !!!)
#' * This function also creates a "DevsVersTimeStamp.txt" file in the "./WIP"
#'   folder for secondary development version tracking purposes (as needed).
#'
#' @import desc devtools
#'
#' @examples
#' ### Run R Package DevCode easily as follows ...
#' library(MFMRutils)   # <- Loads "MFMRutils" library (if already installed) !!!
#'
#' ### Run 2 different types of code check/validation processes ...
#' devs.check.code.specs(sbCheckDocs = TRUE)   # -> Executes only the DevTools Documentation 
#'                                             #    Process.
#' devs.check.code.specs(sbCheckCRAN = TRUE)   # -> Executes the more complete CRAN Code Validation 
#'                                             #    Process.
#'
#' ### Check (i.e. "rasDevToolsCHECK()") overrides the documentation process ...
#' # The Documentation Process will only be executed once if both are TRUE !!!
#' devs.check.code.specs(sbCheckDocs = TRUE, sbCheckCRAN = TRUE)
#'
#' @export
#? ### ### ###
"devs.check.code.specs" <- function(
  sbCheckDocs=TRUE, sbCheckCRAN=FALSE, ssTimeZone="Africa/Windhoek", ...
) {
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_ID_SHRT_ <- "Code.Specs";              # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Check.Code.Specs";   # <- Function ID - LONG !!!
  
  base::Sys.setenv(TZ = ssTimeZone);   # <- Set correct Time Zone BEFORE querying System CLOCK !!!
  RCT_FUNC_RUN_TIME_START_ <- base::Sys.time();
  RCT_FUNC_CELN_START_ <- 58L; RCT_FUNC_CELN_STOP_ <- 390L;
  RCT_TAG_FUNC_LIBR_ID_ <- MFMRutils::devs.pull.libr.info()[["NAME"]];
  
  ### SPECIAL: This a CRITICAL "Alias" that needs to be done here ALWAYS !!!
  `%?!%` <- MFMRutils::`%?!%`;   # <- VERY COOL Alias <NCO> !!! 
  
  
  
  ####   STEP 02 - Internalize ALL Function Arguments   ####
  rvsDotsArgs_    <- base::list(...);
  rsbCheckDocs_   <- sbCheckDocs;
  rsbCheckCRAN_   <- sbCheckCRAN;
  rssTimeZone_    <- ssTimeZone;
  sbPrintPretty_  <- rvsDotsArgs_[["sbPrintPretty"]]  %?!% TRUE;
  sbRunSelfID_    <- rvsDotsArgs_[["sbRunSelfID"]]    %?!% FALSE;
  ssFuncCallerID_ <- rvsDotsArgs_[["ssFuncCallerID"]] %?!% "TOP-LVL (rProjMAIN)";
  
  
  
  ####   STEP 03 - Run <ENTRY> Function SELF-ID (if requested)   ####
  MFMRutils::info.post.func.self.id(
    ssFuncSelfID = RCT_TAG_FUNC_ID_FULL_,
    siFuncMode01 = 1L, ssFuncCallerID = ssFuncCallerID_,
    sbRunSelfID = sbRunSelfID_, sbPrintPretty = sbPrintPretty_, 
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUN_TIME_START_, csTimeStop = RCT_FUNC_RUN_TIME_START_
  );
  
  
  ####   STEP 04 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasCAT              <- base::cat;
  rasLIST             <- base::list;
  rasTRUNC            <- base::trunc;
  rasROUND            <- base::round;
  rasRETURN           <- base::return;
  rasPASTE0           <- base::paste0;
  rasIfELSE           <- base::ifelse;
  rasFORMAT           <- base::format;
  rasUNLIST           <- base::unlist;
  rasLENGTH           <- base::length;
  rasSPRINTF          <- base::sprintf;
  rasDiffTIME         <- base::difftime;
  rasStrSPLIT         <- base::strsplit;
  rasSysTimeNOW       <- base::Sys.time;
  rasINVISIBLE        <- base::invisible;
  rasFilePATH         <- base::file.path;
  rasDirCREATE        <- base::dir.create;
  rasAsNUM            <- base::as.numeric;
  rasSysSetENV        <- base::Sys.setenv;
  rasWriteLINES       <- base::writeLines;
  rasFileCREATE       <- base::file.create;
  rasFileEXISTS       <- base::file.exists;
  rasAsCHAR           <- base::as.character;
  rasDevToolsCHECK    <- devtools::check;
  rasDevToolsLoadALL  <- devtools::load_all;
  rasDevToolsDOCUMENT <- devtools::document;
  rasDevToolsCleanDLL <- devtools::clean_dll;
  rasMfmrCONSTS       <- MFMRutils::cMISC;
  rasMfmrICONS        <- MFMRutils::cICONS;
  rasMfmrCOLORS       <- MFMRutils::cCOLORS;
  rasMfmrFORMATS      <- MFMRutils::cFORMATS;
  rasDescSetVERSION   <- desc::desc_set_version;
  rasMfmrPullLibrINFO <- MFMRutils::devs.pull.libr.info;
  
  
  
  ####   STEP 05 - Define Critical Constants   ####
  RCT_SYS_DATE_TIME_NOW_ <- rasSysTimeNOW();
  
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
  RCT_FILE_R_PKG_DESC_    <- rasMfmrCONSTS$PATH_FILE_R_PKG_DESC;
  RCT_FOLDER_WIP_PROD_    <- rasMfmrCONSTS$PATH_FOLDER_WIP_PROD;
  RCT_FORMAT_TIME_DEV_01_ <- rasMfmrCONSTS$FORMAT_TIME_DEV_LOG_V01;
  RCT_FORMAT_TIME_DEV_02_ <- rasMfmrCONSTS$FORMAT_TIME_DEV_LOG_V02;
  RCT_FOLDER_WIP_HELPERS_ <- rasMfmrCONSTS$PATH_FOLDER_WIP_HELPERS;
  RCT_FILE_DEV_TIME_LOG_  <- rasMfmrCONSTS$PATH_FILE_WIP_TIME_STAMP;
    

  # 2. Extract the current version number from the DESCRIPTION file ...
  rlsLibrINFO_     <- rasMfmrPullLibrINFO(RCT_FILE_R_PKG_DESC_);   # -> Identifies the "DESCRIPTION" file (with path).
  rssActProjID_    <- rlsLibrINFO_[["NAME"]];      # -> Extracts the R-Libs Project ID ...
  rssProjVersCURR_ <- rlsLibrINFO_[["VERSION"]];   # -> Extracts the current version number from "DESCRIPTION" file.

  # 3. Increment the active version number ...
  rvsProjVersOLD_ <- rasUNLIST(   # -> Extracts the last section of the split ...
    rasStrSPLIT(rasAsCHAR(rssProjVersCURR_), split = "\\.")
  );   # -> VERY NB: Extracts only the 4th value of the split string !!!
  snVersNEW  <- rasAsNUM(rvsProjVersOLD_[4]) + 1;   # -> Increment the version number !!!
  sbIsSameYr <- rasAsNUM(rvsProjVersOLD_[1]) == rasAsNUM(rasFORMAT(RCT_SYS_DATE_TIME_NOW_, "%Y"));
  sbIsSameMn <- rasAsNUM(rvsProjVersOLD_[2]) == rasAsNUM(rasFORMAT(RCT_SYS_DATE_TIME_NOW_, "%m"));
  sbIsSameDy <- rasAsNUM(rvsProjVersOLD_[3]) == rasAsNUM(rasFORMAT(RCT_SYS_DATE_TIME_NOW_, "%d"));

  if (snVersNEW >= 1000 && sbIsSameYr && sbIsSameMn && sbIsSameDy) {
    
    # ANSI escape codes for colors ...
    csANSIbold    <- RCT_ANSI_BOLD_;      # "\033[1m";
    csANSIreset   <- RCT_ANSI_RESET_;     # "\033[0m";
    csANSIitalics <- RCT_ANSI_ITALICS_;   # "\033[3m";
    
    csANSIred    <- RCT_COLOR_RED_;      # "\033[91m";
    csANSIblue   <- RCT_COLOR_BLUE_;     # "\033[94m";
    csANSIgreen  <- RCT_COLOR_GREEN_;    # "\033[92m";
    csANSIyellow <- RCT_COLOR_TELLOW_;   # "\033[93m";
    
    
    # Unicode characters for hand with the index finger pointing upwards ...
    csUniCodePointUP           <- RCT_ICON_POINT_UP_;
    csUniCodeEyes              <- RCT_ICON_OOGLY_EYES_;
    csUniCodeCryingEmoticon    <- RCT_ICON_SMILEY_SAD_;
    csUniCodeArrowRight        <- RCT_ICON_ARROW_RIGHT_;
    csUniCodePonderingEmoticon <- RCT_ICON_SMILEY_PONDER_;
    
    # Calculate delta in hours between current time and midnight ...
    ssHrsSinceMidNight <- rasAsCHAR(
      rasROUND(
        digits = 3,
        x = rasDiffTIME(
          time1 = RCT_SYS_DATE_TIME_NOW_, units = "hours",
          time2 = rasTRUNC(RCT_SYS_DATE_TIME_NOW_, "days") + 1
        )
      )
    );
    
    
    # Tell the user to take a break for the day ...
    ssNote1000thChange <- rasPASTE0(
      rasPASTE0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      rasPASTE0(
        csANSIbold, csANSIgreen, "WOW !!! ", csUniCodePointUP, " That was the ", csANSIreset
      ),
      rasPASTE0(
        csANSIbold, csANSIitalics, csANSIred, "1000th ", csANSIreset
      ),
      rasPASTE0(
        csANSIbold, csANSIgreen, "code check for today alone ... ", csUniCodeEyes, " \n", csANSIreset
      )
    );
    ssNoteTakeBreak <- rasPASTE0(
      rasPASTE0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      rasPASTE0(
        csANSIbold, csANSIblue, "You really should take a break now - maybe go for a walk in the park? ", csUniCodePonderingEmoticon, "\n", csANSIreset
      )
    );
    ssNoteCodeReactivatesTomorrow <- rasPASTE0(
      rasPASTE0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      rasPASTE0(
        csANSIbold, csANSIyellow, "The full functionality of this helper function will only reset tomorrow ... sorry. ", csUniCodeCryingEmoticon, "\n", csANSIreset
      )
    );
    ssNoteAboveCodeNoJoke <- rasPASTE0(
      rasPASTE0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      rasPASTE0(
        csANSIbold, csANSIblue, "NB: The above note ", csUniCodePointUP, " is not a joke - this ", csANSIreset
      ),
      rasPASTE0(
        csANSIbold, csANSIred, "function will only reset in ", 24 - rasAsNUM(ssHrsSinceMidNight), " hours ", csANSIreset
      ),
      rasPASTE0(
        csANSIbold, csANSIblue, "!!! \n\n", csANSIreset
      )
    );
    
    rasCAT(
      rasPASTE0(
        ssNote1000thChange, ssNoteTakeBreak,
        ssNoteCodeReactivatesTomorrow, ssNoteAboveCodeNoJoke
      )
    )
  } else {
    ssVersNEW <- NULL;
    if (sbIsSameYr && sbIsSameMn && sbIsSameDy) {   # -> If TRUE then it's the SAME DAY ... so simply increment from the last number value (or count) !!!
      ssVersNEW <- rasSPRINTF(   # -> Increments & pads the value with leading zeros (to create a 3-digit character value) ...
        fmt = "%03d", rasAsNUM(rvsProjVersOLD_[4]) + 1
      );
    } else {    # -> If FALSE then it's a NEW DAY ... which means start count at 1 !!!
      ssVersNEW <- rasSPRINTF(   # -> Starts count at 1 ... and pads the value with leading zeros (to create a 3-digit character value) ...
        fmt = "%03d", 0 + 1
      );
    }
    ssVersNewFULL <- rasPASTE0(   # -> Format the NEW Version Date accordingly ...
      rasFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_01_), ".", ssVersNEW
    );
    
    # 4. Update the version number (accordingly) in the "DESCRIPTION" file ...
    rasDescSetVERSION(ssVersNewFULL, file = RCT_FILE_R_PKG_DESC_);
    
    # 5. Create a Secondary Version Tracking File (in the "WIP" directory) ...
    ssVersFileWIP_  <- rasFilePATH(RCT_FILE_DEV_TIME_LOG_);
    vsDirsToCreate_ <- c(RCT_FOLDER_WIP_HELPERS_, RCT_FOLDER_WIP_PROD_);
    ssVersNewTimeSTAMP_ <- rasPASTE0(   # -> Creates a Devs TimeStamp ...
      "> R-Libs Project ID: ", rssActProjID_, "\n",
      "> Last Code Push (vers)  ==>  ", ssVersNewFULL, "\n",
      "> Last Code Push (time)  ==>  ", rasFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_02_)
    );
    if (rasFileEXISTS(ssVersFileWIP_)) {   # -> File already exists ...
      rasWriteLINES(   # -> Writes the new version number into file ...
        con = ssVersFileWIP_, text = ssVersNewTimeSTAMP_
      );
    } else {   # -> File DOES NOT already exist ...
      rasDirCREATE(path = vsDirsToCreate_[1], recursive = T, showWarnings = F);   # -> Creates the "WIP" directory ...
      rasDirCREATE(path = vsDirsToCreate_[2], recursive = T, showWarnings = F);   # -> Creates the "WIP" directory ...
      rasFileCREATE(ssVersFileWIP_);   # -> Creates the required file ...
      rasWriteLINES(   # -> Writes the new version number into file ...
        con = ssVersFileWIP_, text = ssVersNewTimeSTAMP_
      );
    }
    
    # 6. Finally - Run the required R-Libs Project Documentation & CRAN Checks !!!
    if (rsbCheckCRAN_) {   # -> Runs the COMPLETE Documentation & CRAN Requirements Checking Processes !!!
      rasDevToolsCleanDLL();
      rasDevToolsLoadALL();
      coCheckResult_ <- rasDevToolsCHECK();
      snLenNOTEs_    <- rasLENGTH(coCheckResult_$notes);
      snLenERRORs_   <- rasLENGTH(coCheckResult_$errors);
      snLenWARNINGs_ <- rasLENGTH(coCheckResult_$warnings);
      MFMRutils:::devs.print.code.check.res(
        snLenERRORs = snLenERRORs_, snLenWARNs = snLenWARNINGs_,
        snLenNOTEs = snLenNOTEs_, ssActProjID = rssActProjID_, ssProjVers = ssVersNewFULL
      );
    }
    
    # -> Runs the Documentation process ONLY IF the "sbCheckCRAN" value is FALSE !!!
    if (!rsbCheckCRAN_ && rsbCheckDocs_) {   
      rasDevToolsDOCUMENT(roclets = c('rd', 'collate', 'namespace'));
    }
    
    
    
    ####   STEP 06 - Run <EXIT> Function SELF-ID (if requested)   ####
    RCT_FUNC_RUN_TIME_STOP_ <- rasSysTimeNOW();
    MFMRutils::info.post.func.self.id(
      ssFuncSelfID = RCT_TAG_FUNC_ID_FULL_,
      siFuncMode01 = 0L, ssFuncCallerID = ssFuncCallerID_,
      sbRunSelfID = sbRunSelfID_, sbPrintPretty = sbPrintPretty_, 
      siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
      csTimeStart = RCT_FUNC_RUN_TIME_START_, csTimeStop = RCT_FUNC_RUN_TIME_STOP_
    );
    
    
    
    # 7. Return the new created Project Version Number as a character object ...
    rasRETURN(
      rasINVISIBLE(
        rasLIST(
          "ProjID" = rssActProjID_,
          "CodeVers" = rasPASTE0("v", ssVersNewFULL),
          "CodeTime" = rasFORMAT(RCT_SYS_DATE_TIME_NOW_, RCT_FORMAT_TIME_DEV_02_)
        )
      )
    );
  }
  
}


