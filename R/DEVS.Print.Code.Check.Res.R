#? ### ### ### ### ### ### ###
#' @title ANSI Formatted Code Validation Printouts ("SuiteMFMR" DevTools)
#' @name devs.print.code.check.res
#' @family SuiteMFMR DevTools
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
#' @param ssProjVers a simple string (character vector) that receives the R Project Version Number
#'                   for the active R Library Project.
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
#' devs.check.code.specs(sbCheckDocs = TRUE)   # -> Executes only the DevTools Documentation 
#'                                             #    Process.
#'
#' @keywords internal
#' @noRd
#? ### ### ###
"devs.print.code.check.res" <- function(
  snLenERRORs=NULL, snLenWARNs=NULL, snLenNOTEs=NULL, 
  ssActProjID=NULL, ssProjVers=NULL, sbIsProdRel=FALSE
) {
  
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";             # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHRT_ <- "Print.Checks";                # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Print.Code.Check.Res";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasBaseCAT    <- base::cat;
  rasBaseRETURN <- base::return;
  rasBasePASTE0 <- base::paste0;
  rasBaseIfELSE <- base::ifelse;
  
  `%??%`         <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!! 
  rasMfmrMISC    <- MFMRutils::RENV_MISC;
  rasMfmrICONS   <- MFMRutils::RENV_ICONS;
  rasMfmrCOLORS  <- MFMRutils::RENV_COLOURS;
  rasMfmrFORMATS <- MFMRutils::RENV_FORMATS;
  
  
  
  ####   STEP 03 - Define Critical Constants   ####
  RCT_ANSI_BOLD_    <- rasMfmrFORMATS$ANSI_BOLD;
  RCT_ANSI_RESET_   <- rasMfmrFORMATS$ANSI_RESET;
  RCT_ANSI_ITALICS_ <- rasMfmrFORMATS$ANSI_ITALICS;
  
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
  RCT_FILE_R_PKG_DESC_    <- rasMfmrMISC$PATH_FILE_R_PKG_DESC;
  RCT_FOLDER_WIP_PROD_    <- rasMfmrMISC$PATH_FOLDER_WIP_PROD;
  RCT_FORMAT_TIME_DEV_01_ <- rasMfmrMISC$FORMAT_TIME_DEV_LOG_V01;
  RCT_FORMAT_TIME_DEV_02_ <- rasMfmrMISC$FORMAT_TIME_DEV_LOG_V02;
  RCT_FOLDER_WIP_HELPERS_ <- rasMfmrMISC$PATH_FOLDER_WIP_HELPERS;
  RCT_FILE_DEV_TIME_LOG_  <- rasMfmrMISC$PATH_FILE_WIP_TIME_STAMP;
  
  
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
  output <- rasBasePASTE0(
    
    # R Project ID & Version information print out ...
    rasBasePASTE0(
      csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
    ),
    rasBasePASTE0(csANSIbold, "R Project: ", csANSIreset),
    rasBasePASTE0(
      csANSIbold, csANSIblue, "", ssActProjID, csANSIreset
    ),
    rasBasePASTE0(
      csANSIbold, csANSIblue, " v", ssProjVers, csANSIreset
    ),
    rasBasePASTE0(
      csANSIbold, csANSIyellow, " ... \n", csANSIreset
    ),
    
    # Code Release Type information print out ...
    rasBasePASTE0(
      csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
    ),
    rasBasePASTE0(csANSIbold, "Code State: ", csANSIreset),
    rasBasePASTE0(
      csANSIbold, 
      rasBaseIfELSE(
        sbIsProdRel, csANSIgreen, csANSIblue
      ), 
      rasBaseIfELSE(
        sbIsProdRel, 
        "PRODUCTION (public) Release", 
        "DEVELOPMENT (act-dev) Release"
      ), 
      csANSIreset
    ),
    rasBasePASTE0(
      csANSIbold, 
      rasBaseIfELSE(
        sbIsProdRel, csANSIred, csANSIyellow
      ), 
      rasBaseIfELSE(sbIsProdRel, " !!! \n", " ... \n"), 
      csANSIreset
    ),
    
    # CRAN Code Check results print out ...
    rasBasePASTE0(
      csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
    ),
    rasBasePASTE0(csANSIbold, "CRAN Code Check:  ", csANSIreset),
    rasBaseIfELSE(
      snLenERRORs > 0,
      rasBasePASTE0(
        csANSIitalics, csANSIbold, csANSIred, snLenERRORs,
        rasBaseIfELSE(snLenERRORs == 1, " ERROR ", " ERRORs "),
        csUniCodeCross, csANSIreset
      ),
      rasBasePASTE0(
        csANSIitalics, csANSIbold, csANSIgreen, 
        "0 ERRORs ", csUniCodeCheckmark, csANSIreset
      )
    ),
    rasBasePASTE0(csANSIbold, "  |  ", csANSIreset),
    rasBaseIfELSE(
      snLenWARNs > 0,
      rasBasePASTE0(
        csANSIitalics, csANSIbold, csANSIyellow, snLenWARNs,
        rasBaseIfELSE(snLenWARNs == 1, " WARNING ", " WARNINGs "),
        csUniCodeCross, csANSIreset
      ),
      rasBasePASTE0(
        csANSIitalics, csANSIbold, csANSIgreen, 
        "0 WARNINGs ", csUniCodeCheckmark, csANSIreset
      )
    ),
    rasBasePASTE0(csANSIbold, "  |  ", csANSIreset),
    rasBaseIfELSE(
      snLenNOTEs > 0,
      rasBasePASTE0(
        csANSIitalics, csANSIbold, csANSIblue, snLenNOTEs,
        rasBaseIfELSE(snLenNOTEs == 1, " NOTE ", " NOTEs "),
        csUniCodeCross, csANSIreset
      ),
      rasBasePASTE0(
        csANSIitalics, csANSIbold, csANSIgreen,  
        "0 NOTEs ", csUniCodeCheckmark, csANSIreset
      )
    )
  );
  
  # Output the final result ...
  rasBaseRETURN(
    rasBaseCAT(output, "\n\n")
  );
}
