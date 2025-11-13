#? ### ### ### ### ### ### ###
#' @title CRAN Code Validation with File Version Tracking ("SuiteMFMR" DevTools)
#' @name pkgs.check.code.specs
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
#' pkgs.check.code.specs(sbCheckDocs = TRUE)   # -> Executes only the DevTools Documentation Process.
#' pkgs.check.code.specs(sbCheckCRAN = TRUE)   # -> Executes the more complete CRAN Code Validation Process.
#'
#' ### Check (i.e. "devtools::check()") overrides the documentation process ...
#' # The Documentation Process will only be executed once if both are TRUE !!!
#' pkgs.check.code.specs(sbCheckDocs = TRUE, sbCheckCRAN = TRUE)
#'
#' @export
#? ### ### ###
"pkgs.check.code.specs" <- function(
  sbCheckDocs=TRUE, sbCheckCRAN=FALSE, ssTimeZone="Africa/Windhoek"
) {
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  rssTagFuncIDv01_ <- "Check.Code.Specs";        # <- Function ID - SHORT !!!
  rssTagFuncIDv02_ <- "PKGS.Check.Code.Specs";   # <- Function ID - LONG !!!
  rssTagFuncLibID_ <- MFMRutils::pkgs.pull.libr.info()[["NAME"]];
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasCAT      <- base::cat;
  rasLIST     <- base::list;
  rasRETURN   <- base::return;
  rasPASTE0   <- base::paste0;
  rasIfELSE   <- base::ifelse;
  rasFORMAT   <- base::format;
  rasUNLIST   <- base::unlist;
  rasStrSPLIT <- base::strsplit;
  rasAsNUM    <- base::as.numeric;
  rasAsCHAR   <- base::as.character;
  

  # Define a special (colour-formatting) <internal> function ...
  "rcf_format.outputs.pkgs.check.code" <- function(
    errors, warnings, notes, ssProjID, ssProjVers
  ) {
    
    # ANSI escape codes for colors ...
    csANSIbold <- EnvFORMATS$BOLD; # "\033[1m";
    csANSIitalics <- EnvFORMATS$ITALICS; # "\033[3m";
    
    csANSIred <- EnvCOLORS$RedFORE; # "\033[91m";
    csANSIblue <- EnvCOLORS$BlueFORE; # "\033[94m";
    csANSIgreen <- EnvCOLORS$GreenFORE; # "\033[92m";
    csANSIyellow <- EnvCOLORS$YellowFORE; # "\033[93m";
    
    csANSIreset <- EnvFORMATS$RESET; # "\033[0m";
    
    # Unicode characters for CheckMark and Cross ...
    csUniCodeCross <- EnvICONS$XSlanted;
    csUniCodeCheckmark <- EnvICONS$CheckMark;
    csUniCodeArrowRight <- EnvICONS$ArrowRIGHT;
    
    # Create the output string
    output <- rasPASTE0(
      # R Project ID & Version information print out ...
      rasPASTE0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      rasPASTE0(csANSIbold, "R Project: ", csANSIreset),
      rasPASTE0(
        csANSIbold, csANSIblue, "", ssProjID, csANSIreset
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
        errors > 0,
        rasPASTE0(
          csANSIitalics, csANSIbold, csANSIred, errors,
          rasIfELSE(errors == 1, " ERROR ", " ERRORs "),
          csUniCodeCross, csANSIreset
        ),
        rasPASTE0(
          csANSIitalics, csANSIbold, csANSIgreen, errors,
          rasIfELSE(errors == 1, " ERROR ", " ERRORs "),
          csUniCodeCheckmark, csANSIreset
        )
      ),
      rasPASTE0(csANSIbold, "  |  ", csANSIreset),
      rasIfELSE(
        warnings > 0,
        rasPASTE0(
          csANSIitalics, csANSIbold, csANSIyellow, warnings,
          rasIfELSE(warnings == 1, " WARNING ", " WARNINGs "),
          csUniCodeCross, csANSIreset
        ),
        rasPASTE0(
          csANSIitalics, csANSIbold, csANSIgreen, warnings,
          rasIfELSE(warnings == 1, " WARNING ", " WARNINGs "),
          csUniCodeCheckmark, csANSIreset
        )
      ),
      rasPASTE0(csANSIbold, "  |  ", csANSIreset),
      rasIfELSE(
        notes > 0,
        rasPASTE0(
          csANSIitalics, csANSIbold, csANSIblue, notes,
          rasIfELSE(warnings == 1, " NOTE ", " NOTEs "),
          csUniCodeCross, csANSIreset
        ),
        rasPASTE0(
          csANSIitalics, csANSIbold, csANSIgreen, notes,
          rasIfELSE(warnings == 1, " NOTE ", " NOTEs "),
          csUniCodeCheckmark, csANSIreset
        )
      )
    )
    
    # Output the final result ...
    rasRETURN(
      rasCAT(output, "\n\n")
    );
  }

  # 1. Extract the current DateTime & create the new version number ...
  base::Sys.setenv(TZ = ssTimeZone);
  ssDateTimeCURR <- base::Sys.time();

  # 2. Extract the current version number from the DESCRIPTION file ...
  ### rvsLibINFO_ <- MFMRutils::pkgs.get.lib.info();
  ssFileDESC <- base::file.path("./DESCRIPTION");     # -> Identifies the "DESCRIPTION" file (with path).
  ssVersCURR <- desc::desc_get_version(ssFileDESC);   # -> Extracts the current version number from "DESCRIPTION" file.
  ssProjID <- desc::desc_get_field(   # -> Extracts the R-Libs Project ID ...
    key = "Package", file = ssFileDESC
  );

  # 3. Increment the active version number ...
  vsVersOLD <- rasUNLIST(   # -> Extracts the last section of the split ...
    rasStrSPLIT(rasAsCHAR(ssVersCURR), split = "\\.")
  );   # -> VERY NB: Extracts only the 4th value of the split string !!!
  snVersNEW <- rasAsNUM(vsVersOLD[4]) + 1;   # -> Increment the version number !!!
  sbIsSameYr = rasAsNUM(vsVersOLD[1]) == rasAsNUM(rasFORMAT(ssDateTimeCURR, "%Y"));
  sbIsSameMn = rasAsNUM(vsVersOLD[2]) == rasAsNUM(rasFORMAT(ssDateTimeCURR, "%m"));
  sbIsSameDy = rasAsNUM(vsVersOLD[3]) == rasAsNUM(rasFORMAT(ssDateTimeCURR, "%d"));

  if (snVersNEW >= 1000 && sbIsSameYr && sbIsSameMn && sbIsSameDy) {
    
    # ANSI escape codes for colors ...
    csANSIbold    <- EnvFORMATS$BOLD;      # "\033[1m";
    csANSIitalics <- EnvFORMATS$ITALICS;   # "\033[3m";
    
    csANSIred    <- EnvCOLORS$RedFORE;      # "\033[91m";
    csANSIblue   <- EnvCOLORS$BlueFORE;     # "\033[94m";
    csANSIgreen  <- EnvCOLORS$GreenFORE;    # "\033[92m";
    csANSIyellow <- EnvCOLORS$YellowFORE;   # "\033[93m";
    
    csANSIreset <- EnvFORMATS$RESET;   # "\033[0m";
    
    # Unicode characters for hand with the index finger pointing upwards ...
    csUniCodeEyes = EnvICONS$OoglyEyes;
    csUniCodeArrowRight <- EnvICONS$ArrowRIGHT;
    csUniCodePointUP <- EnvICONS$PointUP;
    csUniCodeCryingEmoticon <- EnvICONS$SmileySad;
    csUniCodePonderingEmoticon <- EnvICONS$SmileyPonder;
    
    # Calculate delta in hours between current time and midnight ...
    ssHrsSinceMidNight <- rasAsCHAR(
      base::round(
        digits = 3,
        x = base::difftime(
          time1 = ssDateTimeCURR, units = "hours",
          time2 = base::trunc(ssDateTimeCURR, "days") + 1
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
      ssVersNEW <- base::sprintf(   # -> Increments & pads the value with leading zeros (to create a 3-digit character value) ...
        fmt = "%03d", rasAsNUM(vsVersOLD[4]) + 1
      );
    } else {    # -> If FALSE then it's a NEW DAY ... which means start count at 1 !!!
      ssVersNEW <- base::sprintf(   # -> Starts count at 1 ... and pads the value with leading zeros (to create a 3-digit character value) ...
        fmt = "%03d", 0 + 1
      );
    }
    ssVersNewFULL <- rasPASTE0(   # -> Format the NEW Version Date accordingly ...
      rasFORMAT(ssDateTimeCURR, "%Y.%m.%d"), ".", ssVersNEW
    );
    
    # 4. Update the version number (accordingly) in the "DESCRIPTION" file ...
    desc::desc_set_version(ssVersNewFULL, file = ssFileDESC);
    
    # 5. Create a Secondary Version Tracking File (in the "WIP" directory) ...
    ssVersFileWIP <- base::file.path("./WIP/DevsVersTimeStamp.txt");
    ssVersNewTimeStamp <- rasPASTE0(   # -> Creates a Devs TimeStamp ...
      "> R-Libs Project ID: ", ssProjID, "\n",
      "> Last Code Push (vers)  ==>  ", ssVersNewFULL, "\n",
      "> Last Code Push (time)  ==>  ", rasFORMAT(ssDateTimeCURR, "%H:%M:%OS3 %Z")
    );
    if (base::file.exists(ssVersFileWIP)) {   # -> File already exists ...
      base::writeLines(   # -> Writes the new version number into file ...
        con = ssVersFileWIP, text = ssVersNewTimeStamp
      );
    } else {   # -> File DOES NOT already exist ...
      base::dir.create(path = "./WIP", recursive = T);   # -> Creates the "WIP" directory ...
      base::file.create(ssVersFileWIP);   # -> Creates the required file ...
      base::writeLines(   # -> Writes the new version number into file ...
        con = ssVersFileWIP, text = ssVersNewTimeStamp
      );
    }
    
    # 6. Finally - Run the required R-Libs Project Documentation & CRAN Checks !!!
    if (sbCheckCRAN) {   # -> Runs the COMPLETE Documentation & CRAN Requirements Checking Processes !!!
      devtools::clean_dll();
      devtools::load_all();
      coCheckResult <- devtools::check();
      snLenNOTEs <- base::length(coCheckResult$notes);
      snLenERRORs <- base::length(coCheckResult$errors);
      snLenWARNINGs <- base::length(coCheckResult$warnings);
      rcf_format.outputs.pkgs.check.code.specs(
        errors = snLenERRORs, warnings = snLenWARNINGs,
        notes = snLenNOTEs, ssProjID = ssProjID, ssProjVers = ssVersNewFULL
      );
    }
    if (!sbCheckCRAN && sbCheckDocs) {   # -> Runs the Documentation process ONLY IF the "sbCheckCRAN" value is FALSE !!!
      devtools::document(roclets = c('rd', 'collate', 'namespace'));
    }
    
    # 7. Return the new created Project Version Number as a character object ...
    rasRETURN(
      base::invisible(
        rasLIST(
          "ProjID" = ssProjID,
          "CodeVers" = rasPASTE0("v", ssVersNewFULL),
          "CodeTime" = rasFORMAT(ssDateTimeCURR, "%H:%M:%OS3 %Z")
        )
      )
    );
  }
}


