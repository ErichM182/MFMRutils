#? ### ### ### ### ### ### ###
#' @title Post Standardized R-Project Notifications
#' 
#' @description
#' A Helper Function that standardizes the User / Project Information Posting
#' (i.e. notification) Processes. This custom function was intended to mainly
#' support the MFMR Suite of R Functions, but can be utilized as a standalone
#' function in other (3rd Party) R packages.
#'
#' @param ssNote the character vector (i.e. text string or note) to be printed
#'               to the R console. This text note is also returned as a function
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
#' * This function prints the supplied text (notification) directly to the active
#'   R Session Console <even if function outputs are assigned to a variable> !!!
#' * This function also outputs the full (complete) notification message as an
#'   invisible function return value (i.e. result).
#'
#' @examples
#' 
#' ### Print a dummy notification ...
#' library(MFMRutils)            # -> Loads "MFMRutils" library <if previously installed> ...
#' ## info.post.note()              # -> use this when "MFMRutils" is <already> loaded !!!
#' MFMRutils::info.post.note()   # -> use this when "MFMRutils" is installed, 
#'                               #    but NOT <already> loaded !!!
#'
#'
#' ### Use "fall-through" function arguments to activate additional outputs ...
#' info.post.note(sbRunSelfID = TRUE)   # -> Prints the custom R function's START (ENTRY) and 
#'                                      #    STOP (EXIT) "Self-Identifier" information ...
#' info.post.note(                                  
#'   sbRunSelfID = TRUE, 
#'   ssFuncCallerID = "rTestFunc"       # -> Sets the Calling Function Identifier (tag) in the
#' )                                    #    "Self-ID" info to a value of `rTestFunc` ...
#' 
#' info.post.note(                                  
#'   sbRunSelfID = TRUE, 
#'   ssProjID = "MFMR-R-Suite"          # -> Sets the R Project Identifier (tag) in the "Self-ID"
#' )                                    #    info to a value of `MFMR-R-Suite` ...
#'
#' @export
#? ### ### ###
"info.post.note" <- function(
  ssHeader=NULL, ssNote="NOTE to POST !!!",
  csIconCarat="=>", csIconSplit="|", sbShowTail=TRUE, sbPrePendNL=FALSE, 
  sbPrintPretty=TRUE, sbPostPendNL=TRUE, sbPostPend2ndNL=FALSE,
  csIconTail=MFMRutils::EnvICONS$FireFlame,
  csColorNote=MFMRutils::EnvCOLORS$CyanFORE,
  csColorHeader=MFMRutils::EnvCOLORS$GreenFORE, 
  csColorCarat=MFMRutils::EnvCOLORS$YellowFORE, 
  csColorSplit=MFMRutils::EnvCOLORS$YellowFORE, ...
) {
  
  ### STEP 01 - Define the "Function Self-ID" tag ... ####
  csTimeSTART_ <- base::Sys.time();
  rssTagFuncID_ <- "MFMR-Post.Note";
  siStartCELN_ <- 87; siStopCELN_ <- 286;
  rssTagLibrID_ <- MFMRutils::pkgs.get.lib.info()[["NAME"]];
  
  
  
  ### STEP 02 - Capture NB "DotsArgs" Inputs here ... ####
  # NOTES: the "dots-args" are handed over in subsequent steps (as required) ...
  vsDotsArgs_             <- base::list(...);
  ssDotArgProjID_         <- vsDotsArgs_[["ssProjID"]];
  csDotArgFormatDT_       <- vsDotsArgs_[["csFormatDT"]];
  csDotArgIconSplit_      <- vsDotsArgs_[["csIconSplit"]];
  csDotArgIconCarat_      <- vsDotsArgs_[["csIconCarat"]];
  sbDotArgRunSelfID_      <- vsDotsArgs_[["sbRunSelfID"]];
  csDotArgColorMain_      <- vsDotsArgs_[["csColorMain"]];
  csDotArgColorSplit_     <- vsDotsArgs_[["csColorSplit"]];
  csDotArgColorCarat_     <- vsDotsArgs_[["csColorCarat"]];
  csDotArgColorProjID_    <- vsDotsArgs_[["csColorProjID"]];
  ssDotArgFuncCallrID_    <- vsDotsArgs_[["ssFuncCallerID"]];
  csDotArgColorCallerID_  <- vsDotsArgs_[["csColorCallerID"]];
  csDotArgColorFuncType_  <- vsDotsArgs_[["csColorFuncType"]];
  csDotArgColorTimeStamp_ <- vsDotsArgs_[["csColorTimeStamp"]];
  
  
  ### Assign "Local Aliases" for frequently used functions !!!
  # NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  elseIF <- base::ifelse;
  isNULL <- base::is.null;
  conCatSTR <- base::paste0;
  
  
  
  ### STEP 03 - Internalize ALL Function Arguments here ... ####
  # NOTES: hand-over all func-args to func-local <internal> variables ...
  coListFuncRes_ <- NULL;   # -> The <final> function outputs <results> object.
  csIconSplit_ <- csIconSplit; csIconTail_ <- csIconTail;
  sbShowTail_ <- sbShowTail; sbPrintPretty_ <- sbPrintPretty;
  csColorCarat_ <- csColorCarat; csColorSplit_ <- csColorSplit;
  csColorHeader_ <- csColorHeader; csColorNote_ <- csColorNote;
  sbPostPendNL_ <- sbPostPendNL; sbPostPend2ndNL_ <- sbPostPend2ndNL;
  csIconCarat_ <- csIconCarat; ssHeader_ <- ssHeader; ssNote_ <- ssNote;
  
  
  
  ### STEP 04 - Prime the "Header" text ... ####
  if (isNULL(ssHeader_)) {
    if (!isNULL(ssDotArgProjID_)) {
      ssHeader_ <- ssDotArgProjID_;
    } else {
      ssHeader_ <- base::get0(
        "rssTagProjID_",
        envir = .GlobalEnv,
        ifnotfound = "UNK. Proj. ID"
      );
    }
  }
  
  
  
  ### STEP 05 - Deploy the "Header" or "START" Self-ID note ... ####
  if (!isNULL(sbDotArgRunSelfID_) && sbDotArgRunSelfID_) {
    MFMRutils::info.post.func.self.id(
      ssProjID = ssDotArgProjID_, siFuncMode01 = 1L,
      sbPrintPretty = sbPrintPretty_, csTimeStart = csTimeSTART_,
      ssFuncSelfID = rssTagFuncID_, ssFuncCallerID = ssDotArgFuncCallrID_,
      csIconCarat = csDotArgIconCarat_, csColorCarat = csDotArgColorCarat_,
      csIconSplit = csDotArgIconSplit_, csColorSplit = csDotArgColorSplit_,
      ### ssFuncType = MFMRutils::code.classify.func(siStartCELN_, siStopCELN_),
      csFormatDT = csDotArgFormatDT_, csColorTimeStamp = csDotArgColorTimeStamp_,
      csColorCallerID = csDotArgColorCallerID_, csColorMain = csDotArgColorMain_,
      csColorProjID = csDotArgColorProjID_, csColorFuncType = csDotArgColorFuncType_
    );
  }
  
  
  
  ### . --- --- --- > Custom Function CODE LOGIC - START < --- --- --- . ####
  ### STEP 06 - Execute this Custom Function's Code logic here ... ####
  ## 6.1 - Prime Standard Text Formatters here ... ####
  csFormatBOLD_ <- MFMRutils::EnvFORMATS$BOLD;
  csFormatRESET_ <- MFMRutils::EnvFORMATS$RESET;
  
  
  ## 6.2 - Prime the CARAT icon accordingly ... ####
  if (sbPrintPretty) {
    if (csIconCarat_ == "=>" || csIconCarat_ == " => " ||
        csIconCarat_ == "->" || csIconCarat_ == " -> ") {
      csIconCarat_ <- conCatSTR(
        csFormatBOLD_, csColorCarat_, " ",   # -> Adds the BOLD & Colour text formats + a <pre-pended> spacer ...
        MFMRutils::EnvICONS$ArrowRIGHT,     # -> Adds a default <standardized> "Right-Arrow" icon ...
        csFormatRESET_, " "                  # -> Closes text formatting and adds a <post-pended> spacer ...
      );
    } else {
      csIconCarat_ <- conCatSTR(
        csFormatBOLD_, csColorCarat_, " ",   # -> Adds the BOLD & Colour text formats + a <pre-pended> spacer ...
        csIconCarat_,                        # -> Adds the user-defined carat icon (symbol or text object) ...
        csFormatRESET_, " "                  # -> Closes text formatting and adds a <post-pended> spacer ...
      );
    }
  }
  
  
  ## 6.3 - Prime the HEADER text accordingly ... ####
  if (sbPrintPretty) {
    ssHeader_ <- conCatSTR(
      csFormatBOLD_, csColorHeader_,   # -> Adds the BOLD & Colour text formats ...
      ssHeader_,                       # -> Adds the HEADER text value ...
      csFormatRESET_                   # -> Closes text formatting ...
    );
  }
  
  
  ## 6.4 - Prime the SPLIT icon accordingly ... ####
  if (sbPrintPretty) {
    if (csIconSplit_ == "|" || csIconSplit_ == " | "  || csIconSplit_ == "  |  ") {
      csIconSplit_ <- conCatSTR(
        csFormatBOLD_, csColorSplit_,   # -> Adds the BOLD & Colour text formats ...
        " | ",                          # -> Adds a default <standardized> SPLIT icon ...
        csFormatRESET_                  # -> Closes text formatting ...
      );
    } else {
      csIconSplit_ <- conCatSTR(
        csFormatBOLD_, csColorSplit_, " ",   # -> Adds the BOLD & Colour text formats ...
        csIconSplit_,                        # -> Adds the user-defined SPLIT icon ...
        csFormatRESET_, " "                  # -> Closes text formatting ...
      );
    }
  }
  
  
  ## 6.5 - Prime the NOTE text accordingly ... ####
  if (sbPrintPretty) {
    ssNote_ <- conCatSTR(
      csFormatBOLD_, csColorNote_,   # -> Adds the BOLD & Colour text formats ...
      ssNote_,                       # -> Adds the NOTE text value ...
      csFormatRESET_                 # -> Closes text formatting ...
    );
  }
  
  
  ## 6.6 - Prime the TAIL icon accordingly ... ####
  if (sbShowTail_) {
    if (sbPrintPretty) {
      csIconTail_ <- conCatSTR(
        " ",          # -> Adds a pre-pended spacer ...
        csIconTail_   # -> Adds the defined TAIL icon (or symbol) ...
      );
    }
  } else {
    csIconTail_ <- "";   # -> Assigns a "zero-byte" value <blank> as the TAIL icon !!!
  }
  
  
  ## 6.7 - FINALLY -> Compile & Post FULL MESSAGE text !!! ####
  csFullNote_ <- conCatSTR(
    elseIF(sbPrePendNL, "\n", ""),       # -> Adds pre-pended NEW LINE (if so requested) !!!
    csIconCarat_, ssHeader_, csIconSplit_,     # -> Adds CARAT icon, HEADER text & SPLIT icon in sequence ...
    ssNote_, csIconTail_,                      # -> Adds NOTE (main body) text and TAIL icon (if requested) in sequence ...
    elseIF(sbPostPendNL_, "\n", ""),     # -> Adds 1st post-pended NEW LINE (if so requested) !!!
    elseIF(sbPostPend2ndNL_, "\n", "")   # -> Adds 2nd post-pended NEW LINE (if so requested) !!!
  );
    
  base::cat(csFullNote_);   # -> Prints (outputs) full notification <message> to active R-Session Console window !!!
  ### . --- --- --- > Custom Function CODE LOGIC - STOP < --- --- --- . ####
  
  
  
  ### STEP 07 - Deploy the "Terminal", "STOP" or "EXIT" Self-ID note ... ####
  if (!isNULL(sbDotArgRunSelfID_) && sbDotArgRunSelfID_) {
    csTimeSTOP_ <- base::Sys.time();
    MFMRutils::info.post.func.self.id(
      sbPrintPretty = sbPrintPretty_, csTimeStart = csTimeSTART_,
      ssFuncSelfID = rssTagFuncID_, ssFuncCallerID = ssDotArgFuncCallrID_,
      csIconCarat = csDotArgIconCarat_, csColorCarat = csDotArgColorCarat_,
      csIconSplit = csDotArgIconSplit_, csColorSplit = csDotArgColorSplit_,
      ### ssFuncType = MFMRutils::code.classify.func(siStartCELN_, siStopCELN_),
      csTimeStop = csTimeSTOP_, ssProjID = ssDotArgProjID_, siFuncMode01 = 0L,
      csFormatDT = csDotArgFormatDT_, csColorTimeStamp = csDotArgColorTimeStamp_,
      csColorCallerID = csDotArgColorCallerID_, csColorMain = csDotArgColorMain_,
      csColorProjID = csDotArgColorProjID_, csColorFuncType = csDotArgColorFuncType_
    );
  }

  ### Output the full notification text <message> as the function's return value ###
  base::invisible(csFullNote_);
}


