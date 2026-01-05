#? ### ### ### ### ### ### ###
#' @title Post Standardized R-Project Notifications
#' @name info.post.note
#' @family SuiteMFMR INFO Functions
#' 
#' 
#' @description
#' A Helper Function that standardizes the R Project Information Posting (i.e. User Notification) 
#' Processes. This function was intended to mainly support the MFMR Suite of R Functions, but may 
#' be utilized as a standalone function in other (i.e. 3rd Party) R packages.
#'
#'
#' @param ssNoteHead a character vector (string or text) that defines the starting
#'                 text of the output message (notification). This function 
#'                 argument is useful in standardizing the notification format 
#'                 for an entire R Project.
#' @param ssNoteBody the character vector (i.e. text string or note) to be printed
#'               to the R console. This text note is also returned as a function
#'               output under the "value" option of this function's results list.
#' @param csIconCarat a character vector (text string or object) that defines 
#'                    the leading symbol (icon) of the printed notification.
#' @param csIconSplit a character vector (text string or object) that defines 
#'                    the middle of the output message (i.e. separator between 
#'                    the `ssNoteHead` and the `ssNoteBody` parts of the note).
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
#'
#'
#' @returns
#' * This function prints the supplied text (notification) directly to the active
#'   R Session Console <even if function outputs are assigned to a variable> !!!
#' * This function also outputs the full (complete) notification message as an
#'   invisible function return value (i.e. result).
#'
#'
#' @examples
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
#'
#' @export
#? ### ### ###
"info.post.note" <- function(
  ssNoteHead=NULL, ssNoteBody="NOTE to POST !!!", csIconCarat="=>", csIconSplit="|", 
  csIconTail=MFMRutils::RENV_ICONS$FireFlame, csColorNote=MFMRutils::RENV_COLOURS$CyanFORE,
  csColorHeader=MFMRutils::RENV_COLOURS$GreenFORE, csColorCarat=MFMRutils::RENV_COLOURS$YellowFORE, 
  csColorSplit=MFMRutils::RENV_COLOURS$YellowFORE, sbPrePendNL=FALSE, sbShowTail=TRUE, 
  sbPostPendNL=TRUE, sbPostPend2ndNL=FALSE, sbPrintPretty=TRUE, sbPostAlways=FALSE
) {
  
  
  ####   STEP 01 - Prime "Function Self-ID" CONSTANTS   ####
  RCT_DBL_SYS_TIME_NOW_ <- base::Sys.time();   # <- Extract the <active> System Date-Time !!!
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";        # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHRT_ <- "Post.Note";        # <- FSID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "INFO.Post.Note";   # <- FSID - LONG !!!
  RCT_INT_CELN_START_   <- 86L;                # <- The Code Editor Line Number (CELN) at which the
                                               #    function opening brace "(" is located !!!
  
  
  
  ####   STEP 02 - Alias ALL <required> Functions   ####
  # NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasBaseCAT         <- base::cat;
  rasBaseGET0        <- base::get0;
  rasBaseElseIF      <- base::ifelse;
  rasBasePASTE0      <- base::paste0;
  rasBaseIsNULL      <- base::is.null;
  rasBaseIsINVISIBLE <- base::invisible;
  
  `%??%`         <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasMfmrICONS   <- MFMRutils::RENV_ICONS;
  rasMfmrFORMATS <- MFMRutils::RENV_FORMATS;
  
  
  
  ### STEP 03 - Internalize ALL Function Arguments ... ####
  # NOTES: hand-over all func-args to func-local <internal> variables ...
  coListFuncRes_ <- NULL;   # -> The <final> function outputs <results> object.
  csIconSplit_ <- csIconSplit; csIconTail_ <- csIconTail;
  sbShowTail_ <- sbShowTail; sbPrintPretty_ <- sbPrintPretty;
  csColorCarat_ <- csColorCarat; csColorSplit_ <- csColorSplit;
  csColorHeader_ <- csColorHeader; csColorNote_ <- csColorNote;
  sbPostPendNL_ <- sbPostPendNL; sbPostPend2ndNL_ <- sbPostPend2ndNL;
  csIconCarat_ <- csIconCarat; ssNoteHead_ <- ssNoteHead; ssNoteBody_ <- ssNoteBody;
  
  
  
  ### STEP 04 - Prime the "Header" text ... ####
  if (rasBaseIsNULL(ssNoteHead_)) {
    if (!rasBaseIsNULL(ssDotArgProjID_)) {
      ssNoteHead_ <- ssDotArgProjID_;
    } else {
      ssNoteHead_ <- rasBaseGET0(
        "rssTagProjID_",
        ### envir = .GlobalEnv,
        ifnotfound = "UNK. Proj. ID"
      );
    }
  }
  
  
  
  ### . --- --- --- > Custom Function CODE LOGIC - START < --- --- --- . ####
  ### STEP 06 - Execute this Custom Function's Code logic here ... ####
  ## 6.1 - Prime Standard Text Formatters here ... ####
  csFormatBOLD_ <- rasMfmrFORMATS$BOLD;
  csFormatRESET_ <- rasMfmrFORMATS$RESET;
  
  
  ## 6.2 - Prime the CARAT icon accordingly ... ####
  if (sbPrintPretty) {
    if (csIconCarat_ == "=>" || csIconCarat_ == " => " ||
        csIconCarat_ == "->" || csIconCarat_ == " -> ") {
      csIconCarat_ <- rasBasePASTE0(
        csFormatBOLD_, csColorCarat_, " ",   # -> Adds the BOLD & Colour text formats + a <pre-pended> spacer ...
        rasMfmrICONS$ArrowRIGHT,     # -> Adds a default <standardized> "Right-Arrow" icon ...
        csFormatRESET_, " "                  # -> Closes text formatting and adds a <post-pended> spacer ...
      );
    } else {
      csIconCarat_ <- rasBasePASTE0(
        csFormatBOLD_, csColorCarat_, " ",   # -> Adds the BOLD & Colour text formats + a <pre-pended> spacer ...
        csIconCarat_,                        # -> Adds the user-defined carat icon (symbol or text object) ...
        csFormatRESET_, " "                  # -> Closes text formatting and adds a <post-pended> spacer ...
      );
    }
  }
  
  
  ## 6.3 - Prime the HEADER text accordingly ... ####
  if (sbPrintPretty) {
    ssNoteHead_ <- rasBasePASTE0(
      csFormatBOLD_, csColorHeader_,   # -> Adds the BOLD & Colour text formats ...
      ssNoteHead_,                       # -> Adds the HEADER text value ...
      csFormatRESET_                   # -> Closes text formatting ...
    );
  }
  
  
  ## 6.4 - Prime the SPLIT icon accordingly ... ####
  if (sbPrintPretty) {
    if (csIconSplit_ == "|" || csIconSplit_ == " | "  || csIconSplit_ == "  |  ") {
      csIconSplit_ <- rasBasePASTE0(
        csFormatBOLD_, csColorSplit_,   # -> Adds the BOLD & Colour text formats ...
        " | ",                          # -> Adds a default <standardized> SPLIT icon ...
        csFormatRESET_                  # -> Closes text formatting ...
      );
    } else {
      csIconSplit_ <- rasBasePASTE0(
        csFormatBOLD_, csColorSplit_, " ",   # -> Adds the BOLD & Colour text formats ...
        csIconSplit_,                        # -> Adds the user-defined SPLIT icon ...
        csFormatRESET_, " "                  # -> Closes text formatting ...
      );
    }
  }
  
  
  ## 6.5 - Prime the NOTE text accordingly ... ####
  if (sbPrintPretty) {
    ssNoteBody_ <- rasBasePASTE0(
      csFormatBOLD_, csColorNote_,   # -> Adds the BOLD & Colour text formats ...
      ssNoteBody_,                       # -> Adds the NOTE text value ...
      csFormatRESET_                 # -> Closes text formatting ...
    );
  }
  
  
  ## 6.6 - Prime the TAIL icon accordingly ... ####
  if (sbShowTail_) {
    if (sbPrintPretty) {
      csIconTail_ <- rasBasePASTE0(
        " ",          # -> Adds a pre-pended spacer ...
        csIconTail_   # -> Adds the defined TAIL icon (or symbol) ...
      );
    }
  } else {
    csIconTail_ <- "";   # -> Assigns a "zero-byte" value <blank> as the TAIL icon !!!
  }
  
  
  ## 6.7 - FINALLY -> Compile & Post FULL MESSAGE text !!! ####
  csFullNote_ <- rasBasePASTE0(
    rasBaseElseIF(sbPrePendNL, "\n", ""),       # -> Adds pre-pended NEW LINE (if so requested) !!!
    csIconCarat_, ssNoteHead_, csIconSplit_,     # -> Adds CARAT icon, HEADER text & SPLIT icon in sequence ...
    ssNoteBody_, csIconTail_,                      # -> Adds NOTE (main body) text and TAIL icon (if requested) in sequence ...
    rasBaseElseIF(sbPostPendNL_, "\n", ""),     # -> Adds 1st post-pended NEW LINE (if so requested) !!!
    rasBaseElseIF(sbPostPend2ndNL_, "\n", "")   # -> Adds 2nd post-pended NEW LINE (if so requested) !!!
  );
    
  rasBaseCAT(csFullNote_);   # -> Prints (outputs) full notification <message> to active R-Session Console window !!!
  ### . --- --- --- > Custom Function CODE LOGIC - STOP < --- --- --- . ####
  
  

  ### Output the full notification text <message> as the function's return value ###
  rasBaseIsINVISIBLE(csFullNote_);
}


