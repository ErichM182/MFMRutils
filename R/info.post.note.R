#? ### ### ### ### ### ### ###
#' @title Post interactive (real-time) Project Notifications
#' @description
#' A Helper Function that standardizes the User / Project Information Posting
#' (i.e. notification) Processes. This custom function was intended to mainly
#' support the MFMR Suite of R Functions, but can be utilized as a standalone
#' function in other R packages.
#'
#' @param ssPostNote the character vector (i.e. string or text or note) to be
#' printed to the console. This text note is also returned as a function output
#' under the "value" option of this function's results list.
#' @param ssFuncSelfID a character vector (i.e. string or text) serving as a
#' self-identifier (tag) for this "info.post.note()" function.
#' @param sbRunSelfID a logical (i.e. boolean) value that specifies whether the
#' "info.post.note()" function should output its own identifier information
#' alongside the user specified info (notification).
#' @param ssFuncCallerID a character vector (string or text) serving as the
#' identifier (tag) for the R Function or R Project that called (invoked) this
#' "info.post.note()" function.
#' @param ssFuncType a character vector (string or text) that specifies the
#' function type. OPTIONS: "Helper" (small to medium custom functions; less than
#' 300 lines of code) and "LARGE" (massive custom function with >300 lines of
#' code). If set to NULL the function type will be assigned the default value
#' of "Helper".
#' @param siPostMode123 an integer value that defines the form of the output
#' (notification) message to be printed to the console. OPTIONS: 1L (applies the
#' "START" or "HEADER" form to the printed message), 2L (applies the "Normal"
#' or "DEFAULT" form to the printed message) and 3L (applies the "STOP" or
#' "TERMINAL" form to the printed message). This function argument is extremely
#' useful for defining the beginning and terminal sections of custom R Functions
#' and/or R Projects.
#' @param sbRetFuncInfo a logical (boolean) value that specifies whether to
#' output or return the function (i.e. internally processed) information in the
#' results of the current function. This function argument can be useful when
#' nesting multiple custom functions.
#' @param ssPreStub a character vector (string or text) that defines the
#' starting text of the output message (notification). This function argument
#' is useful in standardizing the notification format for an entire R Project.
#' @param ssMidStub a character vector (string or text) that defines the middle
#' (ID "tag" and Messages) text of the output message (notification). This
#' function argument is useful in standardizing the notification format for an
#' entire R Project.
#' @param sbPrePendNL a logical (boolean) value that specifies whether a new
#' line (blank space or row) should be added to the START of a posted note.
#' @param sbPostPendNL a logical (boolean) value that specifies whether a new
#' line (blank space or row) should be added to the END of a posted note.
#' @param sbPostPend2ndNL a logical (boolean) value that specifies whether a
#' second line (blank space or row) should be added to the END of a posted note.
#' This function argument can be useful for clearly delineating sections within
#' a custom R Function and/or R Project.
#' @param sbShowTrailIcon a logical (boolean) value that specifies whether a
#' show (print) the trailing (terminal) icon object or not.
#' @param csTrailIcon a complex (string) object value that defines the image to
#' be used for the trailing (terminal) icon object.
#' @param csColorPreStub a character (string) value that defines the text color
#' for the caret (pre-stub) object of the posted note.
#' @param csColorMidStub a character (string) value that defines the text color
#' for the middle separator (mid-stub) object of the posted note.
#' @param csColorCFID a character (string) value that defines the text color for
#' the CFID (Calling Function ID) object of the posted note.
#' @param csColorPostNote a character (string) value that defines the text color
#' for the actual (main) notification text object of the posted note.
#' @param csColorTrailIcon a character (string) value that defines the text color
#' for the trailing (terminal) icon object of the posted note.
#' @param ssFormatDT a character vector (string or text) that specifies the
#' DateTime format to be used for displaying date-times in the console.
#' @param sbFormatANSI a logical (boolean) argument whether to format the output
#' post with "ANSI" formatting or to simply apply no formatting to the output
#' post / notification.
#' @param csANSIformCFID a character (string) value that defines the ANSI text
#' font formatting for the CFID (Calling Function ID) string or character object
#' of the posted note.
#' @param csANSIformMidStub a character (string) value that defines the ANSI
#' text font formatting for the middle separator (mid-stub) object of the posted
#' note.
#' @param csANSIformPostNote a character (string) value that defines the ANSI
#' text font formatting for the actual (main) notification text object of the
#' posted note.
#' @param sbPrintPretty a logical (boolean) argument that specifies whether the
#' ANSI text font formatting should be applied to the printed notification or not.
#'
#' @returns
#' * This function prints the specified text (notification) directly to the
#' console even if the function outputs are assigned to a variable.
#' * This function also outputs additional function information (i.e. internally
#' computed function information) as a list object.
#'
#' @examples
#' ### Print a dummy notification ...
#' info.post.note()              # -> when "MFMRutils" library is loaded ...
#' MFMRutils::info.post.note()   # -> when "MFMRutils" library is NOT loaded !!!
#'
#' ### Print 3 different types of notifications ...
#' info.post.note(siPostMode123 = 1L)   # -> Prints a START (header) or beginning notification ...
#' info.post.note(siPostMode123 = 2L)   # -> Prints a Normal (default) or body notification ...
#' info.post.note(siPostMode123 = 3L)   # -> Prints an STOP (footer) or terminal notification ...
#'
#' ### Print additional (function internal) information ...
#' info.post.note(sbRetFuncInfo = TRUE)   # -> Outputs additional function information in list form ...
#'
#' @export
#? ### ### ###
"info.post.note" <- function(
  ssPostNote="NOTE to POST !!!", ssFuncSelfID="Post Note",
  csTrailIcon=MFMRIcons$FireFlame, sbShowTrailIcon=FALSE,
  sbRunSelfID=FALSE, ssFuncCallerID=NULL, ssFuncType=NULL,
  sbPrePendNL=FALSE, sbPostPendNL=TRUE, sbPostPend2ndNL=FALSE,
  ssFormatDT="%a, %b %d %Y @ %X", csANSIformCFID=MFMRFormat$BOLD,
  csColorMidStub=NULL, csColorPostNote=NULL, csColorTrailIcon=NULL,
  csANSIformMidStub=MFMRFormat$BOLD, csANSIformPostNote=MFMRFormat$BOLD,
  csColorPreStub=MFMRColors$YellowFORE, csColorCFID=MFMRColors$CyanFORE,
  siPostMode123=2L, sbRetFuncInfo=FALSE, ssPreStub=" => ", ssMidStub=" | ",
  sbFormatANSI=FALSE, sbPrintPretty=FALSE
) {

  # Prime NB function parameters ...
  rdtFuncSTART <- base::Sys.time();    # -> Extract Function START Time ...
  rssFormatDTI <- ssFormatDT;          # -> DateTime Format for "FuncSelfID" Process ...
  ssPreSTUB_ <- ssPreStub;             # -> A standardized start to all posted notes ...
  ssFuncCallerID_ <- ssFuncCallerID;   # -> Simply use the normal text ...
  ssMidSTUB_ <- ssMidStub;             # -> The middle separator stub ...


  if (base::is.null(ssFuncCallerID)) {
    ssFuncCallerID <- base::get0(
      "rssTagProjID_",
      envir = .GlobalEnv,
      ifnotfound = "UNK. Proj. ID"
    );
  }

  if (base::is.null(ssFuncType)) {
    ssFuncType <- "Helper";   # ->  Options: "LARGE" ...or... "Helper" Function !!!
  }

  if (sbRunSelfID) {
    base::cat(
      base::paste0(
        ssPreSTUB_, ssFuncSelfID, ssMidSTUB_, "START  { F-Type: '",
        ssFuncType, "', Caller: '", ssFuncCallerID_, "', Time: ",
        base::format(rdtFuncSTART, rssFormatDTI), " }\n"
      )
    );
  }

  # Initialize the ANSI Text Format resetting code ...
  ### scTextFormatBOLD <- MFMRFormat$BOLD; # "\033[1m";
  scTextFormatRESET <- MFMRFormat$RESET; # "\033[0m";   # -> Reset ANSI formatting !!!

  # Update & format the caret (pre-stub) text object ...
  if (sbPrintPretty) {
    if (ssPreStub == " => " || ssPreStub == "=>" ||
        ssPreStub == " -> "  || ssPreStub == "->") {
      ssPreSTUB_ <- base::paste0(
        csColorPreStub,         # -> Apply the ANSI text color scheme ...
        " ",                    # -> Add a leading white space character ...
        MFMRIcons$ArrowRIGHT,   # -> Assign the MFMR Arrow Icon !!!
        " ",                    # -> Add a trailing white space character ...
        scTextFormatRESET       # -> Deactivate the ANSI text formatting !!!
      )
    } else {
      ssPreSTUB_ <- base::paste0(
        csColorPreStub,     # -> Apply the ANSI text color scheme ...
        " ",                # -> Add a leading white space character ...
        ssPreStub,          # -> Apply the provided (requested) pre-stub !!!
        " ",                # -> Add a trailing white space character ...
        scTextFormatRESET   # -> Deactivate the ANSI text formatting !!!
      )
    }
  } else {
    if (ssPreStub == " => " || ssPreStub == "=>" ||
        ssPreStub == " -> "  || ssPreStub == "->") {
      ssPreSTUB_ <- " => ";  # -> Simply use a standard right arrow !!!
    } else {
      ssPreSTUB_ <- ssPreStub;
    }
  }

  # Update & format the Caller Function ID text object ...
  if (sbPrintPretty) {
    ssFuncCallerID_ <- base::paste0(
      csANSIformCFID,     # -> Apply the ANSI text format ...
      csColorCFID,        # -> Apply the ANSI text color scheme ...
      ssFuncCallerID,     # -> Apply the Calling Function's Identifier tag !!!
      scTextFormatRESET   # -> Deactivate the ANSI text formatting !!!
    );
  } else {
    ssFuncCallerID_ <- ssFuncCallerID;   # -> Simply use the normal text ...
  }

  # Update & format the middle separator (mid-stub) text object ...
  if (sbPrintPretty) {
    ssMidSTUB_ <- base::paste0(
      csANSIformMidStub,   # -> Apply the ANSI text format ...
      base::ifelse(        # -> Apply the ANSI text color scheme ...
        base::is.null(csColorMidStub), csColorCFID, csColorMidStub
      ),
      ssMidStub,           # -> Insert the Mid-Stub character object !!!
      scTextFormatRESET    # -> Deactivate the ANSI text formatting !!!
    );
  } else {
    ssMidSTUB_ <- ssMidStub;
  }

  # Update & format the actual (main) Post Note text object ...
  if (sbPrintPretty) {
    ssPostNOTE_ <- base::paste0(
      csANSIformPostNote,   # -> Apply the ANSI text format ...
      base::ifelse(         # -> Apply the ANSI text color scheme ...
        base::is.null(csColorPostNote), csColorCFID, csColorPostNote
      ),
      ssPostNote,           # -> Insert the Post Note text object !!!
      scTextFormatRESET     # -> Deactivate the ANSI text formatting !!!
    );
  } else {
    ssPostNOTE_ <- ssPostNote;
  }

  if (ssPostNote == "NOTE to POST !!!") {
    sbShowTrailIcon <- TRUE;   # -> Activate the icon is on default text !!!
  }

  # Update & format the trailing icon object ...
  csTrailingICON_ <- "";    # -> Start with a blank text object ...
  if (sbShowTrailIcon) {
    csTrailingICON_ <- base::paste0(
      base::ifelse(         # -> Apply the ANSI text color scheme ...
        base::is.null(csColorTrailIcon), "", csColorTrailIcon
      ),
      " ",                  # -> Add a leading whites pace character ...
      csTrailIcon,          # -> Insert the Trailing Icon object !!!
      " ",                  # -> Add a trailing white space character ...
      scTextFormatRESET     # -> Deactivate the ANSI text formatting !!!
    );
  }

  # Compile the function exiting parameters ...
  rdtFuncSTOP <- base::Sys.time();   # ->  Extract Function STOP Time ...
  rcoFuncINFO <- base::list(         # ->  Collate Key Function SelfID Information ...
    "FuncID" = ssFuncSelfID, "CallerID" = ssFuncCallerID_,
    "FuncSTART" = rdtFuncSTART, "FuncSTOP" = rdtFuncSTOP, "FuncType" = ssFuncType
  )
  if (sbRunSelfID) {
    base::cat(
      base::paste0(
        ssPreSTUB_, ssFuncSelfID, ssMidSTUB_, "STOP  { F-Type: '", ssFuncType,
        "', Caller: '", ssFuncCallerID_, "', Time: ",
        base::format(rdtFuncSTOP, rssFormatDTI), " }\n"
      )
    );
  }

  if (!sbRetFuncInfo) {
    if (siPostMode123 == 1L) {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(sbPrePendNL, "\n", ""),
            ssPreSTUB_, ssFuncSelfID, ssMidSTUB_, "START  { F-Type: '", ssFuncType,
            "', Caller: '", ssFuncCallerID_, "', Time: ",
            base::format(rdtFuncSTART, rssFormatDTI), " }",
            base::ifelse(sbPostPendNL, "\n", ""),
            base::ifelse(sbPostPend2ndNL, "\n", "")
          )
        )
      );
    } else if (siPostMode123 == 2L) {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(sbPrePendNL, "\n", ""),
            ssPreSTUB_, ssFuncCallerID_, ssMidSTUB_, ssPostNOTE_, csTrailingICON_,
            base::ifelse(sbPostPendNL, "\n", ""),
            base::ifelse(sbPostPend2ndNL, "\n", "")
          )
        )
      );
    } else {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(sbPrePendNL, "\n", ""),
            ssPreSTUB_, ssFuncSelfID, ssMidSTUB_, "STOP  { F-Type: '", ssFuncType,
            "', Caller: '", ssFuncCallerID_, "', Time: ",
            base::format(rdtFuncSTOP, rssFormatDTI), " }",
            base::ifelse(sbPostPendNL, "\n", ""),
            base::ifelse(sbPostPend2ndNL, "\n", "")
          )
        )
      );
    }
  } else {
    ssPostNoteFINAL <- NULL;
    if (siPostMode123 == 1L) {
      ssPostNoteFINAL <- base::paste0(
        ssPreSTUB_, ssFuncSelfID, ssMidSTUB_, "START  { F-Type: '", ssFuncType,
        "', Caller: '", ssFuncCallerID_, "', Time: ",
        base::format(rdtFuncSTART, rssFormatDTI), " }"
      );
    } else if (siPostMode123 == 2L) {
      ssPostNoteFINAL <- base::paste0(
        "> ", ssFuncCallerID_, " | ", ssPostNOTE_, csTrailingICON_
      );
    } else {
      ssPostNoteFINAL <- base::paste0(
        ssPreSTUB_, ssFuncSelfID, ssMidSTUB_, "STOP  { F-Type: '", ssFuncType,
        "', Caller: '", ssFuncCallerID_, "', Time: ",
        base::format(rdtFuncSTOP, rssFormatDTI), " }"
      );
    }
    base::return(base::list("Value" = ssPostNoteFINAL, "FI" = rcoFuncINFO));
  }
}


