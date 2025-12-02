#? ### ### ### ### ### ### ###
#' @title ANSI Formatted Code Validation Printouts ("SuiteMFMR" DevTools)
#' @name devs.print.code.check.res
#' 
#' @description
#' A Helper Function that formats and prints (to console) the CRAN Code Validation
#' results as returned from the DevTools Code Check function. This function merely 
#' receives the output from the DevTools Code Check function and applies ANSI text
#' formatting to the results.
#'
#' @param snLenERRORs a simple number (numeric) value that captures the ERROR outputs from the CRAN
#'                    Code Check (DevTools) function.
#' @param snLenWARNs a simple number (numeric) value that captures the WARNING outputs from the CRAN
#'                   Code Check (DevTools) function.
#' @param snLenNOTEs a simple number (numeric) value that captures the NOTE outputs from the CRAN
#'                    Code Check (DevTools) function.
#' @param ssActProjID a simple string (character vector) that receives the R Project Identifier (ID)
#'                    for the active R Library Project.
#' @param ssProjVers a simple string (character vector) that receives the R Project Verion Number
#'                   for the active R Library Project..
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
#' devs.check.code.specs(sbCheckDocs = TRUE)   # -> Executes only the DevTools Documentation 
#'                                             #    Process.
#'
#' @keywords internal
#' @noRd
#? ### ### ###
"devs.print.code.check.res" <- function(
  snLenERRORs=NULL, snLenWARNs=NULL, snLenNOTEs=NULL, ssActProjID=NULL, ssProjVers=NULL
) {
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_ID_SHRT_ <- "Print.Checks";                # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Print.Code.Check.Res";   # <- Function ID - LONG !!!
  RCT_TAG_FUNC_LIBR_ID_ <- MFMRutils::devs.pull.libr.info()[["NAME"]];
  
  
  ### SPECIAL: This a CRITICAL "Alias" that needs to be done here ALWAYS !!!
  `%?!%` <- MFMRutils::`%?!%`;   # <- VERY COOL Alias <NCO> !!! 
  
  
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
  
  
  
  # ANSI escape codes for TEXT FORMATS ...
  csANSIbold    <- RCT_ANSI_BOLD_;      # "\033[1m";
  csANSIreset   <- RCT_ANSI_RESET_;     # "\033[0m";
  csANSIitalics <- RCT_ANSI_ITALICS_;   # "\033[3m";
  
  # ANSI escape codes for COLORS ...
  csANSIred    <- RCT_COLOR_RED_;      # "\033[91m";
  csANSIblue   <- RCT_COLOR_BLUE_;     # "\033[94m";
  csANSIgreen  <- RCT_COLOR_GREEN_;    # "\033[92m";
  csANSIyellow <- RCT_COLOR_TELLOW_;   # "\033[93m";
  
  # Unicode characters for ICONS ...
  csUniCodeCross      <- RCT_ICON_WHITE_X_;
  csUniCodeCheckmark  <- RCT_ICON_CHECK_MARK_;
  csUniCodeArrowRight <- RCT_ICON_ARROW_RIGHT_;
  
  # Create the output string
  output <- rasPASTE0(
    # R Project ID & Version information print out ...
    rasPASTE0(
      csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
    ),
    rasPASTE0(csANSIbold, "R Project: ", csANSIreset),
    rasPASTE0(
      csANSIbold, csANSIblue, "", ssActProjID, csANSIreset
    ),
    rasPASTE0(
      csANSIbold, csANSIblue, " v", ssProjVers, csANSIreset
    ),
    rasPASTE0(
      csANSIbold, csANSIyellow, " ... \n", csANSIreset
    ),
    
    # CRAN Code Check results print out ...
    rasPASTE0(
      csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
    ),
    rasPASTE0(csANSIbold, "CRAN Code Check:  ", csANSIreset),
    rasIfELSE(
      snLenERRORs > 0,
      rasPASTE0(
        csANSIitalics, csANSIbold, csANSIred, snLenERRORs,
        rasIfELSE(snLenERRORs == 1, " ERROR ", " ERRORs "),
        csUniCodeCross, csANSIreset
      ),
      rasPASTE0(
        csANSIitalics, csANSIbold, csANSIgreen, snLenERRORs,
        rasIfELSE(snLenERRORs == 1, " ERROR ", " ERRORs "),
        csUniCodeCheckmark, csANSIreset
      )
    ),
    rasPASTE0(csANSIbold, "  |  ", csANSIreset),
    rasIfELSE(
      snLenWARNs > 0,
      rasPASTE0(
        csANSIitalics, csANSIbold, csANSIyellow, snLenWARNs,
        rasIfELSE(snLenWARNs == 1, " WARNING ", " WARNINGs "),
        csUniCodeCross, csANSIreset
      ),
      rasPASTE0(
        csANSIitalics, csANSIbold, csANSIgreen, snLenWARNs,
        rasIfELSE(snLenWARNs == 1, " WARNING ", " WARNINGs "),
        csUniCodeCheckmark, csANSIreset
      )
    ),
    rasPASTE0(csANSIbold, "  |  ", csANSIreset),
    rasIfELSE(
      snLenNOTEs > 0,
      rasPASTE0(
        csANSIitalics, csANSIbold, csANSIblue, snLenNOTEs,
        rasIfELSE(snLenNOTEs == 1, " NOTE ", " NOTEs "),
        csUniCodeCross, csANSIreset
      ),
      rasPASTE0(
        csANSIitalics, csANSIbold, csANSIgreen, snLenNOTEs,
        rasIfELSE(snLenNOTEs == 1, " NOTE ", " NOTEs "),
        csUniCodeCheckmark, csANSIreset
      )
    )
  )
  
  # Output the final result ...
  rasRETURN(
    rasCAT(output, "\n\n")
  );
}
