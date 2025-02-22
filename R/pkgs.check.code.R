#? ### ### ### ### ### ### ###
#' @title CRAN Code Check with real-time File Version Tracking
#' @description
#' A Helper Function that executes the CRAN pre-requisite Code Checking Procedure during active R Package Development. This function programmatically updates the package version number in the R Project DESCRIPTION file before running the required documentation and/or CRAN Package Pre-Submission Requirements Checks during iterative development cycles.
#'
#' @param sbRunDocu a logical (boolean) value that specifies whether to run the standard package documentation process.
#' @param sbRunCheck a logical (boolean) value that specifies whether to run the standard package documentation process.
#'
#' @returns
#' * This function returns the programmatically amended or updated (active or real-time) version number for the active R-Libs Project as a list of character objects.
#' * This function creates a Work-In-Progress (WIP) directory at the root of the active R-Libs Project (if not already exists).
#' * This function also creates a "DevsVersTimeStamp.txt" file in the "./WIP" project path for secondary development version tracking.
#'
#' @import desc devtools
#'
#' @examples
#' ### Print a dummy notification ...
#' require(MFMRutils)             # <= Installs and loads the "MFMRutils" package ...
#' pkgs.check.code()              # <= when "MFMRutils" library is loaded !!!
#' MFMRutils::pkgs.check.code()   # <= when "MFMRutils" library is NOT loaded !!!
#'
#' ### Run 2 different types of code checks ...
#' pkgs.check.code(sbRunDocu = TRUE)    # <= Executes only the DevTools Documentation Process ...
#' pkgs.check.code(sbRunCheck = TRUE)   # <= Excecute the more complete CRAN Code Checks ...
#'
#' ### Check (i.e. "devtools::check()") overrides the documentation process ...
#' # The Documentation Process will only be executed once if both are == TRUE !!!
#' pkgs.check.code(sbRunDocu = TRUE, sbRunCheck = TRUE)
#'
#' @export
#? ### ### ###
"pkgs.check.code" <- function(
  sbRunDocu=TRUE, sbRunCheck=FALSE
) {

  # Function ID <tag> (very useful under certain scenarios) ...
  ssTagFuncID_ <- "pkgs.check.code";

  # Define a special (colour-formatting) <internal> function ...
  "rcf_format.check.results" <- function(errors, warnings, notes) {

    # ANSI escape codes for colors ...
    csANSIbold <- crayon::bold; # "\033[1m";
    csANSIitalics <- crayon::italic; # "\033[3m";

    csANSIred <- crayon::red; # "\033[91m";
    csANSIblue <- crayon::blue; # "\033[94m";
    csANSIgreen <- crayon::green; # "\033[92m";
    csANSIyellow <- crayon::yellow; # "\033[93m";

    csANSIreset <- crayon::reset; # "\033[0m";

    # Unicode characters for CheckMark and Cross ...
    csUniCodeCross <- "\u2716";
    csUniCodeCheckmark <- "\u2714";
    csUniCodeArrowRight <- "\u279C";

    # Create the output string
    output <- base::paste0(
      base::paste0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      base::paste0(csANSIbold, "CRAN Code Check:  ", csANSIreset),
      base::ifelse(
        errors > 0,
        base::paste0(
          csANSIitalics, csANSIbold, csANSIred, errors,
          base::ifelse(errors == 1, " ERROR ", " ERRORs "),
          csUniCodeCross, csANSIreset
        ),
        base::paste0(
          csANSIitalics, csANSIbold, csANSIgreen, errors,
          base::ifelse(errors == 1, " ERROR ", " ERRORs "),
          csUniCodeCheckmark, csANSIreset
        )
      ),
      base::paste0(csANSIbold, "  |  ", csANSIreset),
      base::ifelse(
        warnings > 0,
        base::paste0(
          csANSIitalics, csANSIbold, csANSIyellow, warnings,
          base::ifelse(warnings == 1, " WARNING ", " WARNINGs "),
          csUniCodeCross, csANSIreset
        ),
        base::paste0(
          csANSIitalics, csANSIbold, csANSIgreen, warnings,
          base::ifelse(warnings == 1, " WARNING ", " WARNINGs "),
          csUniCodeCheckmark, csANSIreset
        )
      ),
      base::paste0(csANSIbold, "  |  ", csANSIreset),
      base::ifelse(
        notes > 0,
        base::paste0(
          csANSIitalics, csANSIbold, csANSIblue, notes,
          base::ifelse(warnings == 1, " NOTE ", " NOTEs "),
          csUniCodeCross, csANSIreset
        ),
        base::paste0(
          csANSIitalics, csANSIbold, csANSIgreen, notes,
          base::ifelse(warnings == 1, " NOTE ", " NOTEs "),
          csUniCodeCheckmark, csANSIreset
        )
      )
    )

    # Output the final result ...
    base::return(
      base::cat(output, "\n\n")
    );
  }

  # 1. Extract the current DateTime & create the new version number ...
  ssDateTimeCURR <- base::Sys.time();

  # 2. Extract the current version number from the DESCRIPTION file ...
  ssFileDESC <- base::file.path("./DESCRIPTION");     # <- Identifies the "DESCRIPTION" file (with path).
  ssVersCURR <- desc::desc_get_version(ssFileDESC);   # <- Extracts the current version number from "DESCRIPTION" file.
  ssProjID <- desc::desc_get_field(   # <- Extracts the R-Libs Project ID ...
    key = "Package", file = ssFileDESC
  );

  # 3. Increment the active version number ...
  vsVersOLD <- base::unlist(   # <- Extracts the last section of the split ...
    base::strsplit(base::as.character(ssVersCURR), split = "\\.")
  );   # <- VERY NB: Extracts only the 4th value of the split string !!!
  snVersNEW <- base::as.numeric(vsVersOLD[4]) + 1;   # <- Increment the version number !!!
  if (snVersNEW >= 1000) {

    # ANSI escape codes for colors ...
    csANSIbold <- crayon::bold; # "\033[1m";
    csANSIitalics <- crayon::italic; # "\033[3m";

    csANSIred <- crayon::red; # "\033[91m";
    csANSIblue <- crayon::blue; # "\033[94m";
    csANSIgreen <- crayon::green; # "\033[92m";
    csANSIyellow <- crayon::yellow; # "\033[93m";

    csANSIreset <- crayon::reset; # "\033[0m";

    # Unicode characters for hand with the index finger pointing upwards ...
    csUniCodeArrowRight <- "\u279C";
    csUniCodePointUP <- "\U0001F446";
    csUniCodeCryingEmoticon <- "\U0001F622";
    csUniCodePonderingEmoticon <- "\U0001F914";

    # Calculate delta in hours between current time and midnight ...
    ssHrsSinceMidNight <- base::as.character(
      base::round(
        digits = 3,
        x = base::difftime(
          time1 = ssDateTimeCURR, units = "hours",
          time2 = base::trunc(ssDateTimeCURR, "days") + 1
        )
      )
    );


    # Tell the user to take a break for the day ...
    ssNote1000thChange <- base::paste0(
      base::paste0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      base::paste0(
        csANSIbold, csANSIgreen, "WOW !!! ", csUniCodePointUP, " That was the ", csANSIreset
      ),
      base::paste0(
        csANSIbold, csANSIitalics, csANSIred, "1000th ", csANSIreset
      ),
      base::paste0(
        csANSIbold, csANSIgreen, "code check for today alone ... \n", csANSIreset
      )
    );
    ssNoteTakeBreak <- base::paste0(
      base::paste0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      base::paste0(
        csANSIbold, csANSIblue, "You really should take a break now - maybe go for a walk in the park? ", csUniCodePonderingEmoticon, "\n", csANSIreset
      )
    );
    ssNoteCodeReactivatesTomorrow <- base::paste0(
      base::paste0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      base::paste0(
        csANSIbold, csANSIyellow, "The full functionality of this helper function will only reset tomorrow ... sorry. ", csUniCodeCryingEmoticon, "\n", csANSIreset
      )
    );
    ssNoteAboveCodeNoJoke <- base::paste0(
      base::paste0(
        csANSIyellow, " ", csUniCodeArrowRight, " ", csANSIreset
      ),
      base::paste0(
        csANSIbold, csANSIblue, "NB: The above note ", csUniCodePointUP, " is not a joke - this ", csANSIreset
      ),
      base::paste0(
        csANSIbold, csANSIred, "function will only reset in ", 24 - base::as.numeric(ssHrsSinceMidNight), " hours ", csANSIreset
      ),
      base::paste0(
        csANSIbold, csANSIblue, "!!! \n\n", csANSIreset
      )
    );

    base::cat(
      base::paste0(
        ssNote1000thChange, ssNoteTakeBreak,
        ssNoteCodeReactivatesTomorrow, ssNoteAboveCodeNoJoke
      )
    )
  } else {
    ssVersNEW <- NULL;
    if (
      base::as.numeric(vsVersOLD[1]) == base::as.numeric(base::format(ssDateTimeCURR, "%Y")) &&
        base::as.numeric(vsVersOLD[2]) == base::as.numeric(base::format(ssDateTimeCURR, "%m")) &&
        base::as.numeric(vsVersOLD[3]) == base::as.numeric(base::format(ssDateTimeCURR, "%d"))
    ) {   # <- If TRUE then it's the SAME DAY ... so simply increment from the last number value (or count) !!!
      ssVersNEW <- base::sprintf(  # <- Increments & pads the value with leading zeros (to create a 3-digit character value) ...
        fmt = "%03d", base::as.numeric(vsVersOLD[4]) + 1
      );
    } else {    # <- If FALSE then it's a NEW DAY ... which means start count at 1 !!!
      ssVersNEW <- base::sprintf(  # <- Starts count at 1 ... and pads the value with leading zeros (to create a 3-digit character value) ...
        fmt = "%03d", 0 + 1
      );
    }
    ssVersNewFULL <- base::paste0(   # <- Format the NEW Version Date accordingly ...
      base::format(ssDateTimeCURR, "%Y.%m.%d"), ".", ssVersNEW
    );

    # 4. Update the version number (accordingly) in the "DESCRIPTION" file ...
    desc::desc_set_version(ssVersNewFULL, file = ssFileDESC);

    # 5. Create a Secondary Version Tracking File (in the "WIP" directory) ...
    ssVersFileWIP <- base::file.path("./WIP/DevsVersTimeStamp.txt");
    ssVersNewTimeStamp <- base::paste0(   # <- Creates a Devs TimeStamp ...
      "> R-Libs Project ID: ", ssProjID, "\n",
      "> Last Code Push (vers)  ==>  ", ssVersNewFULL, "\n",
      "> Last Code Push (time)  ==>  ", base::format(ssDateTimeCURR, "%H:%M:%OS3 %Z")
    );
    if (base::file.exists(ssVersFileWIP)) {   # <- File already exists ...
      base::writeLines(   # <- Writes the new version number into file ...
        con = ssVersFileWIP, text = ssVersNewTimeStamp
      );
    } else {   # <- File DOES NOT already exist ...
      base::dir.create(path = "./WIP", recursive = T);   # <- Creates the "WIP" directory ...
      base::file.create(ssVersFileWIP);   # <- Creates the required file ...
      base::writeLines(   # <- Writes the new version number into file ...
        con = ssVersFileWIP, text = ssVersNewTimeStamp
      );
    }

    # 6. Finally - Run the required R-Libs Project Documentation & CRAN Checks !!!
    if (sbRunCheck) {   # <- Runs the COMPLETE Documentation & CRAN Requirements Checking Processes !!!
      coCheckResult <- devtools::check();
      snLenNOTEs <- base::length(coCheckResult$notes);
      snLenERRORs <- base::length(coCheckResult$errors);
      snLenWARNINGs <- base::length(coCheckResult$warnings);
      rcf_format.check.results(snLenERRORs, snLenWARNINGs, snLenNOTEs);
    }
    if (!sbRunCheck && sbRunDocu) {   # <- Runs the Documentation process ONLY IF the "sbRunCheck" value is FALSE !!!
      devtools::document(roclets = c('rd', 'collate', 'namespace'));
    }

    # 7. Return the new created Project Version Number as a character object ...
    base::return(
      base::invisible(
        base::list(
          "ProjID" = ssProjID,
          "CodeVers" = base::paste0("v", ssVersNewFULL),
          "CodeTime" = base::format(ssDateTimeCURR, "%H:%M:%OS3 %Z")
        )
      )
    );
  }
}


### pkgs.check.code(sbRunDocu=T)
### pkgs.check.code(sbRunCheck=T)
### pkgs.check.code(sbRunDocu=T, sbRunCheck=T)
### ssProjINFO <- pkgs.check.code(sbRunDocu=F)
### ssProjINFO$ProjID
