#? ### ### ### ### ### ### ###
#' @title Post interactive (real-time) Project Notifications
#' @description
#' A Helper Function that standardizes the User / Project Information Posting
#' (i.e. notification) Processes. This custom function was intended to mainly
#' support the MFMR Suite of R Functions, but can be utilized as a standalone
#' function in other (3rd Party) R packages.
#'
#' @param ssNote the character vector (i.e. text string or note) to be printed
#'               to the console. This text note is also returned as a function
#'               output under the "value" option of this function's results list.
#' @param ssHeader a character vector (string or text) that defines the starting
#'                 text of the output message (notification). This function 
#'                 argument is useful in standardizing the notification format 
#'                 for an entire R Project.
#' @param csIconCarat a character vector (text string or object) that defines 
#'                    the leading symbol (icon) of the printed notification.
#' @param csIconSplit a character vector (text string or object) that defines 
#'                    the middle of the output message (i.e. separator between 
#'                    the "ssHeader" and the "ssNote" parts of the note).
#' @param csIconTail a compound character vector (text string or object) value 
#'                   that defines the image (icon) to be used for the trailing 
#'                   (terminal or "tail") icon object of the posted note.
#' @param sbShowTail a logical (boolean) value that specifies whether to show or
#'                   print the trailing (terminal or "tail") icon object or not.
#' @param sbPrePendNL a logical (boolean) value that specifies whether a new
#'                    line (blank space or row) should be added to the START of 
#'                    a posted note.
#' @param sbPostPendNL a logical (boolean) value that specifies whether a new
#'                     line (blank space or row) should be added to the END of 
#'                     a posted note.
#' @param sbPostPend2ndNL a logical (boolean) value that specifies whether a
#'                        second line (blank space or row) should be added to 
#'                        the END of a posted note. This function argument can 
#'                        be useful for clearly delineating sections within a 
#'                        custom R Function or R Project output (printed) code.
#' @param csColorCarat a character (text string) value that defines the object 
#'                     color for the leading (starting) symbol (icon) object of 
#'                     the posted (output) notification (note).
#' @param csColorHeader a character (text string) value that defines the text 
#'                      color for the header object of the posted note.
#' @param csColorSplit a character (text string) value that defines the text color
#'                     for the middle separator (icon) object of the posted note.
#' @param csColorNote a character (text string) value that defines the text color
#'                    for the note (main text) object of the posted note.
#' @param sbPrintPretty a logical (boolean) argument that specifies whether the
#'                      ANSI text font formatting (i.e. colour & weight) should
#'                      be applied to the printed notification or not.
#' @param ... all "fall-through" function arguments to be used as inputs to any
#'            nested functions. In this specific case these "fall-through" args 
#'            are related to the "MFMRutils::info.post.func.self.id()" function
#'            (e.g. "ssProjID", "csFormatDT", "csIconSplit", "csIconCarat", 
#'            "sbRunSelfID", "csColorMain", "csColorSplit", "csColorCarat", 
#'            "csColorProjID", "ssFuncCallerID", "csColorCallerID", 
#'            "csColorFuncType", "csColorTimeStamp") <- list is NOT exhaustive !!! 
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
  csIconSplit="|", sbShowTail=TRUE,
  sbPrePendNL=FALSE, sbPrintPretty=TRUE,
  sbPostPendNL=TRUE, sbPostPend2ndNL=FALSE, 
  csIconTail=MFMRutils::MFMRIcons$FireFlame,
  csColorNote=MFMRutils::MFMRColors$CyanFORE,
  csColorHeader=MFMRutils::MFMRColors$GreenFORE, 
  csColorCarat=MFMRutils::MFMRColors$YellowFORE, 
  csColorSplit=MFMRutils::MFMRColors$YellowFORE, ...
) {
  
  ### STEP 01 - Define the "Function Self-ID" tag ... ####
  #            ( this👆 is THE ONLY FUNCTION [in the MFMR Suite of R Functions]
  #            THAT DOES NOT SELF-IDENTIFY !!! )
  ssFuncSelfID_ <- "MFMR-Post.Note";
  csTimeSTART_ <- base::Sys.time();
  siStartCELN_ <- 81; siStopCELN_ <- 165;
  
  
  
  ### STEP 02 - Capture NB Function "DotsArgs" here ... ####
  #            ( the "dots-args" will be handed over in subsequent steps ) ...
  vsDotsArgs_ <- base::list(...);
  ssDotArgProjID_ <- vsDotsArgs_[["ssProjID"]];
  csDotArgFormatDT_ <- vsDotsArgs_[["csFormatDT"]];
  csDotArgIconSplit_ <- vsDotsArgs_[["csIconSplit"]];
  csDotArgIconCarat_ <- vsDotsArgs_[["csIconCarat"]];
  sbDotArgRunSelfID_ <- vsDotsArgs_[["sbRunSelfID"]];
  csDotArgColorMain_ <- vsDotsArgs_[["csColorMain"]];
  csDotArgColorSplit_ <- vsDotsArgs_[["csColorSplit"]];
  csDotArgColorCarat_ <- vsDotsArgs_[["csColorCarat"]];
  csDotArgColorProjID_ <- vsDotsArgs_[["csColorProjID"]];
  ssDotArgFuncCallrID_ <- vsDotsArgs_[["ssFuncCallerID"]];
  csDotArgColorCallerID_ <- vsDotsArgs_[["csColorCallerID"]];
  csDotArgColorFuncType_ <- vsDotsArgs_[["csColorFuncType"]];
  csDotArgColorTimeStamp_ <- vsDotsArgs_[["csColorTimeStamp"]];
  
  
  
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
  
  
  
  ### STEP 05 - Deploy the "Header" or "START" Self-ID note ... ####
  if (!base::is.null(sbDotArgRunSelfID_) && sbDotArgRunSelfID_) {
    MFMRutils::info.post.func.self.id(
      ssProjID = ssDotArgProjID_, siFuncMode01 = 1L,
      sbPrintPretty = sbPrintPretty_, csTimeStart = csTimeSTART_,
      ssFuncSelfID = ssFuncSelfID_, ssFuncCallerID = ssDotArgFuncCallrID_,
      csIconCarat = csDotArgIconCarat_, csColorCarat = csDotArgColorCarat_,
      csIconSplit = csDotArgIconSplit_, csColorSplit = csDotArgColorSplit_,
      ssFuncType = MFMRutils::code.classify.func(siStartCELN_, siStopCELN_),
      csFormatDT = csDotArgFormatDT_, csColorTimeStamp = csDotArgColorTimeStamp_,
      csColorCallerID = csDotArgColorCallerID_, csColorMain = csDotArgColorMain_,
      csColorProjID = csDotArgColorProjID_, csColorFuncType = csDotArgColorFuncType_
    );
  }
  
  
  
  ### STEP 07 - Deploy the "Terminal", "STOP" or "EXIT" Self-ID note ... ####
  if (!base::is.null(sbDotArgRunSelfID_) && sbDotArgRunSelfID_) {
    csTimeSTOP_ <- base::Sys.time();
    MFMRutils::info.post.func.self.id(
      sbPrintPretty = sbPrintPretty_, csTimeStart = csTimeSTART_,
      ssFuncSelfID = ssFuncSelfID_, ssFuncCallerID = ssDotArgFuncCallrID_,
      csIconCarat = csDotArgIconCarat_, csColorCarat = csDotArgColorCarat_,
      csIconSplit = csDotArgIconSplit_, csColorSplit = csDotArgColorSplit_,
      ssFuncType = MFMRutils::code.classify.func(siStartCELN_, siStopCELN_),
      csTimeStop = csTimeSTOP_, ssProjID = ssDotArgProjID_, siFuncMode01 = 0L,
      csFormatDT = csDotArgFormatDT_, csColorTimeStamp = csDotArgColorTimeStamp_,
      csColorCallerID = csDotArgColorCallerID_, csColorMain = csDotArgColorMain_,
      csColorProjID = csDotArgColorProjID_, csColorFuncType = csDotArgColorFuncType_
    );
  }

  
}



### rssTagProjID_ <- "MFMR-R-Suite v101"

### MFMRutils::info.post.note(
###   sbRunSelfID = T, sbPrintPretty = T, ssFuncCallerID = "TESTr"
### )


