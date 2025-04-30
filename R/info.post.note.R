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
#' info.post.note(siPostMode123 = 1L)   # -> Prints a START (header or beginning) notification ...
#' info.post.note(siPostMode123 = 2L)   # -> Prints a Normal (default or body) notification ...
#' info.post.note(siPostMode123 = 3L)   # -> Prints an STOP (footer or terminal) notification ...
#'
#' ### Print additional (function internal) information ...
#' info.post.note(sbRetFuncInfo = TRUE)   # -> Outputs additional function information in list form ...
#'
#' @export
#? ### ### ###
"info.post.note" <- function(
  ssNote="NOTE to POST !!!", 
  ssHeader=NULL, csIconCarat="=>",
  csColorNote=MFMRutils::MFMRColors$CyanFORE,
  csColorHeader=MFMRutils::MFMRColors$GreenFORE, 
  csColorCarat=MFMRutils::MFMRColors$YellowFORE, 
  csColorSplit=MFMRutils::MFMRColors$YellowFORE,
  csIconSplit="|", csIconTail=MFMRIcons$FireFlame, sbShowTail=TRUE,
  sbPostPendNL=TRUE, sbPostPend2ndNL=FALSE, sbPrintPretty=FALSE, ...
) {
  
  ### STEP 01 - Define the "Function Self-ID" tag ... ####
  #            ( this👆 is THE ONLY FUNCTION [in the MFMR Suite of R Functions]
  #            THAT DOES NOT SELF-IDENTIFY !!! )
  ssFuncSelfID_ <- "MFMR-Post.Note";
  csTimeSTART_ <- base::Sys.time();
  siStartCELN_ <- 112; siStopCELN_ <- 165;
  
  
  
  ### STEP 02 - Capture NB Function "DotsArgs" here ... ####
  #            ( the "dots-args" will be handed over in subsequent steps ) ...
  vsDotsArgs_ <- base::list(...);
  sbDotArgRunSelfID_ <- vsDotsArgs_[["sbRunSelfID"]];
  ssDotArgFuncCallrID_ <- vsDotsArgs_[["ssFuncCallerID"]];
  
  
  
  ### STEP 03 - Internalize ALL Function Arguments here ... ####
  #            ( i.e. hand-over all to func-args to func-local variables )
  coListFuncRes_ <- NULL;   # -> The <final> function outputs <results> object.
  csIconSplit_ <- csIconSplit; csIconTail_ <- csIconTail;
  sbShowTail_ <- sbShowTail; sbPrintPretty_ <- sbPrintPretty;
  csColorCarat_ <- csColorCarat; csColorSplit_ <- csColorSplit;
  csColorHeader_ <- csColorHeader; csColorNote_ <- csColorNote;
  sbPostPendNL_ <- sbPostPendNL; sbPostPend2ndNL_ <- sbPostPend2ndNL;
  csIconCarat_ <- csIconCarat; ssHeader_ <- ssHeader; ssNote_ <- ssNote;
  
  
  
  ### STEP 04 - Prime the "Header" text ... ####
  if (base::is.null(ssHeader_)) {
    ssHeader_ <- base::get0(
      "rssTagProjID_",
      envir = .GlobalEnv,
      ifnotfound = "UNK. Proj. ID"
    );
  }
  
  
  
  ### STEP 05 - Prime the "Header" text ... ####
  if (!base::is.null(sbDotArgRunSelfID_) && sbDotArgRunSelfID_) {
    MFMRutils::info.post.func.self.id(
      ssProjID = ssHeader_, siFuncMode01 = 1L, 
      sbPrintPretty = sbPrintPretty_, csTimeStart = csTimeSTART_,
      ssFuncSelfID = ssFuncSelfID_, ssFuncCallerID = ssDotArgFuncCallrID_,
      ssFuncType = MFMRutils::code.classify.func(siStartCELN_, siStopCELN_)
    );
  }

  
}

## MFMRutils::info.post.note(
##   sbRunSelfID = T, sbPrintPretty = T, ssFuncCallerID = "TESTr"
## )

