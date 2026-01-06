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
#' @param csCarat ([character]) A complex String or Text (i.e. [character] [vector]) object that 
#'                defines what the first or starting symbol (i.e. carat) of the note, to be posted, 
#'                should be or look like. This can be a simple text or a complex (i.e. ANSI, RegEx 
#'                or escaped character) object - but must be an acceptable R [character] object.
#' @param ssHead ([character]) A simple String or Text (i.e. [character] [vector]) argument that 
#'               defines the starting (i.e. header) text of the note (to be posted). This function 
#'               argument is useful in standardizing the notification format at an R Project level.
#' @param csSplit ([character]) A complex String or Text (i.e. [character] [vector]) object that 
#'                defines what the middle symbol (i.e. separator between the `ssHead` and `ssBody` 
#'                components) of the note, to be posted, should be or look like. This can be a 
#'                simple text or a complex (i.e. ANSI, RegEx or escaped character) object - but must 
#'                be an acceptable R [character] object.
#' @param ssBody ([character]) A simple String or Text (i.e. [character] [vector]) argument that 
#'               defines the main (i.e. body) text of the note (to be posted).
#' @param csTail ([character]) A complex String or Text (i.e. [character] [vector]) object that 
#'               defines what the ending symbol (i.e. tail or terminal icon) of the note, to be 
#'               posted, should be or look like. This can be a simple text or a complex (i.e. ANSI, 
#'               RegEx or escaped character) object, but must be an acceptable R [character] object.
#' @param sbShowTail ([logical]) A Boolean value that specifies whether to include the tail (i.e.
#'                   trailing or terminal symbol or icon) object in the note to be posted or not 
#'                   (default == `TRUE`).
#' @param sbPrintPretty ([logical]) A Boolean value that specifies whether the built-in (i.e. the
#'                      function's internal) text formatting (i.e. ANSI <font weight and colour>
#'                      formats) should be applied to the printed notification or not (default == 
#'                      `TRUE`).
#' @param csColorHead ([character]) A String or Text (i.e. [character] [vector]) value that defines 
#'                    the text colour for the header (`ssHead`) object of the note to be posted.
#' @param csColorBody ([character]) A String or Text (i.e. [character] [vector]) value that defines 
#'                    the text colour for the main text (`ssBody`) object of the note to be posted.
#' @param csColorCarat ([character]) A String or Text (i.e. [character] [vector]) value that defines 
#'                     the colour for the leading (starting) symbol or icon (`csCarat`) object of 
#'                     the note to be posted.
#' @param csColorSplit ([character]) A String or Text (i.e. [character] [vector]) value that defines 
#'                     the colour for the middle spacer (`csSplit`) object of the note to be posted.
#' @param sbPrePendNL ([logical]) A Boolean value that specifies whether a new line (blank space or 
#'                    row) should be added to the START of the note to be posted or not (default == 
#'                    `FALSE`).
#' @param sbPostPend1NL ([logical]) A Boolean value that specifies whether a SINGLE new line (blank 
#'                      space or row) should be added to the END of the note to be posted or not 
#'                      (default == `TRUE`).
#' @param sbPostPend2NLs ([logical]) A Boolean value that specifies whether a DOUBLE new line (blank 
#'                      spaces or rows) should be added to the END of the note to be posted or not. 
#'                      This function argument can be useful for clearly delineating sections within 
#'                      a custom R Function or R Project output (printed) code (default == `FALSE`).
#' @param sbPostAlways ([logical]) A Boolean value that specifies whether the note should ALWAYS be
#'                     posted (printed to the active R Console) or not (default == `TRUE`).
#' @param siCallCELN ([integer]) A Numeric value that specifies the Code Editor Line Number (CELN) 
#'                   at which this function was called from by its parent function or R Script.
#'
#'
#' @returns
#' * This function prints the supplied text (message) directly to the active R Session Console when 
#'   the function argument `sbPostAlways` (or either of the R Project DEBUG and VERBOSE trackers:
#'   i.e. `RCT_IS_DEBUG_MODE_` and `RCT_IS_VERBOSE_MODE_`, respectively) are set to `TRUE`.
#' * This function also outputs the full (complete) notification message as an invisible function 
#'   return value (function result).
#'
#'
#' @examples
#' ### Easily post notifications to the active R Console as follows ...
#' library(MFMRutils)   # <- Ensures the `MFMRutils` library is <already> installed locally.
#' 
#' 
#' ### Example 1: Post a dummy note (default notification) ...
#' info.post.note()   # <- Run function without <user-specified> inputs to generate a dummy post !!!
#' 
#' 
#' ### Example 2: Post a user-specified notification ...
#' info.post.note(
#'   csCarat = "~>",                               # <- Sets the leading <start> icon to a "~>".
#'   ssHead = "My-CUST-FUNC",                      # <- Sets the HEADER text of the notification ...
#'   ssBody = "This is my COOL NOTE -> YaY !!!",   # <- Sets the MAIN <body> text of notification.
#'   sbShowTail = FALSE                            # <- Hides the trailing <tail> icon object !!!
#' )
#' 
#' 
#' ### Example 3: Deactivate the built-in formatting (spaces, font weights and font colours ) ...
#' info.post.note(
#'   csCarat = "~>",                               
#'   ssHead = "My-CUST-FUNC",                      
#'   ssBody = "This is my COOL NOTE -> YaY !!!",   
#'   sbShowTail = FALSE,                           
#'   sbPrintPretty = FALSE   # <- Prints the input text as provided (i.e. without spaces or any 
#' )                         #    text formatting). You must format your text (as required) prior to
#'                           #    handing it to the function (when `sbPrintPretty` is set to FALSE).
#' 
#' 
#' ### Example 4: Post notifications on basis of the DEBUG and VERBOSE R Project trackers ...
#' RCT_IS_VERBOSE_MODE_ <- TRUE   # <- Set `RCT_IS_VERBOSE_MODE_` to TRUE anywhere in R Project ...
#' info.post.note(
#'   csCarat = "~>",                               
#'   ssHead = "My-CUST-FUNC",                      
#'   ssBody = "This is my COOL NOTE -> YaY !!!",   
#'   sbPostAlways = FALSE         # <- The R Project VERBOSE tracker (i.e. `RCT_IS_VERBOSE_MODE_`)
#' )                              #    overrides the `sbPostAlways` function argument -> this means
#'                                #    the function WILL PRINT its message to the R Console, even if
#'                                #    the `sbPostAlways` function argument is set to FALSE, when 
#'                                #    the VERBOSE tracker is set to a value of TRUE !!!
#'                                
#' ## Remove VERBOSE Tracker from Global R Environment ...
#' rm(list = c("RCT_IS_VERBOSE_MODE_"))   # <- Remove VERBOSE Tracker from R Environment !!!
#'
#'
#' RCT_IS_DEBUG_MODE_ <- TRUE   # <- Set `RCT_IS_DEBUG_MODE_` to TRUE anywhere in an R Project ...
#' info.post.note(
#'   csCarat = "~>",
#'   ssHead = "My-CUST-FUNC",
#'   ssBody = "This is my COOL NOTE -> YaY !!!",
#'   sbPostAlways = FALSE       # <- The R Project DEBUG tracker (i.e. `RCT_IS_DEBUG_MODE_`)
#' )                            #    overrides the `sbPostAlways` function argument -> this means
#'                              #    the function WILL PRINT its message to the R Console, even if
#'                              #    the `sbPostAlways` function argument is set to FALSE, when
#'                              #    the DEBUG tracker is set to a value of TRUE !!!
#' 
#' ### NB: DEBUG Tracker also activates the Code Editor Line Number (CELN) section of the 
#' ###    `info.post.note()` function output (CELN is positioned after the HEADER text) !!!
#'                                
#' ## Remove DEBUG Tracker from Global R Environment ...
#' rm(list = c("RCT_IS_DEBUG_MODE_"))   # <- Remove DEBUG Tracker from R Environment !!!
#'
#'
#' @export
#? ### ### ###
"info.post.note" <- function(
  csCarat="=>", ssHead=NULL, csSplit="|", ssBody="NOTE to POST !!!",
  csTail=MFMRutils::RENV_ICONS$FireFlame, sbShowTail=TRUE, sbPrintPretty=TRUE,
  csColorHead=MFMRutils::RENV_COLOURS$GreenFORE, csColorBody=MFMRutils::RENV_COLOURS$CyanFORE,
  csColorCarat=MFMRutils::RENV_COLOURS$YellowFORE, csColorSplit=MFMRutils::RENV_COLOURS$YellowFORE,
  sbPrePendNL=FALSE, sbPostPend1NL=TRUE, sbPostPend2NLs=FALSE, sbPostAlways=TRUE, siCallCELN=1982L
) {
  
  
  ####   STEP 01 - Prime "Function Self-ID" CONSTANTS   ####
  ## NB: This 👆 is THE ONLY FUNCTION [in the MFMR Suite of R Functions] THAT DOES
  ##     NOT SELF-IDENTIFY (since Self-ID here causes infinite recursion) !!!
  RCT_DBL_SYS_TIME_NOW_ <- base::Sys.time();   # <- Extract the <active> System Date-Time !!!
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";        # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_NSID_ <- "Post-Note";        # <- This is the only FUNC that DOES NOT SELF-ID !!! 
  RCT_TAG_FUNC_ID_FULL_ <- "INFO-Post-Note";   # <- FSID - LONG !!!
  RCT_INT_CELN_START_   <- 136L;               # <- The Code Editor Line Number (CELN) at which
                                               #    the function opening brace "(" is located !!!
  
  
  
  ####   STEP 02 - Alias ALL <required> Functions   ####
  ## NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasBaseCAT         <- base::cat;
  rasBaseGET0        <- base::get0;
  rasBaseElseIF      <- base::ifelse;
  rasBasePASTE0      <- base::paste0;
  rasBaseIsNULL      <- base::is.null;
  rasBaseIsINVISIBLE <- base::invisible;
  
  `%??%`         <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasMfmrICONS   <- MFMRutils::RENV_ICONS;
  rasMfmrFORMATS <- MFMRutils::RENV_FORMATS;
  rasMfmrGetCELN <- MFMRutils::code.get.celn;
  
  
  
  ###   STEP 03 - Internalize ALL Function Arguments   ####
  # NOTES: hand-over all func-args to func-local <internal> variables ...
  csCarat_         <- csCarat;
  ssHead_          <- ssHead;
  csSplit_         <- csSplit;
  ssBody_          <- ssBody;
  csTail_          <- csTail;
  sbShowTail_      <- sbShowTail;
  sbPrintPretty_   <- sbPrintPretty;
  csColorBody_     <- csColorBody;
  csColorHead_     <- csColorHead;
  csColorCarat_    <- csColorCarat;
  csColorSplit_    <- csColorSplit;
  sbPrePendNL_     <- sbPrePendNL;
  sbPostPend1NL_   <- sbPostPend1NL;
  sbPostPend2NLs_  <- sbPostPend2NLs;
  sbPostAlways_    <- sbPostAlways;
  siCallCELN_      <- siCallCELN;
  
  
  
  ###   STEP 04 - Run NULL Checks accordingly   ####
  if (rasBaseIsNULL(ssHead_)) {
    ssHead_ <- rasBaseGET0(
      "RCT_TAG_FUNC_ID_SHRT_",         # <- Find the parent <caller> Function ID (if defined) !!!
      envir = base::pos.to.env(-1L),   # <- The R environment the function was called from !!!
      ifnotfound = "UNK-Func-ID"       # <- Set a DEFAULT <caller> Function Identifier <UNKNOWN> !!! 
    );
  }
  rsbIsDEBUG_ <- rasBaseGET0(
    "RCT_IS_DEBUG_MODE_",   # <- Find the "DEBUG" Mode R-Project Value (if defined) !!!
    envir = .GlobalEnv,     # <- The GLOBAL R environment (i.e. active R Project Scope) ...
    ifnotfound = FALSE      # <- Set a DEFAULT boolean value (if variable does not exist) !!! 
  );
  rsbIsVERBOSE_ <- rasBaseGET0(
    "RCT_IS_VERBOSE_MODE_",   # <- Find the "VERBOSE" Mode R-Project Value (if defined) !!!
    envir = .GlobalEnv,       # <- The GLOBAL R environment (i.e. active R Project Scope) ...
    ifnotfound = FALSE        # <- Set a DEFAULT boolean value (if variable does not exist) !!!
  );
  
  
  if (sbPostAlways_ || rsbIsVERBOSE_ || rsbIsDEBUG_) {   # <- Run code if any of these are TRUE !!!
    
    ###   STEP 05 - Execute Custom Function's Code logic   ####
    ## 5.1 - Prime Standard Text Formatters ... ####
    csFormatBOLD_  <- rasMfmrFORMATS$ANSI_BOLD;
    csFormatRESET_ <- rasMfmrFORMATS$ANSI_RESET;
    
    
    ## 5.2 - Prime the CARAT icon accordingly ... ####
    if (sbPrintPretty) {
      if (csCarat_ == "=>" || csCarat_ == " => " || csCarat_ == "  =>  " || csCarat_ == "   =>   " ||
          csCarat_ == "->" || csCarat_ == " -> " || csCarat_ == "  ->  " || csCarat_ == "   ->   ") {
        csCarat_ <- rasBasePASTE0(
          csFormatBOLD_,             # <- Applies the BOLD ANSI Text formatting ...
          csColorCarat_, " ",        # <- Applies the Text Colour Formats & pre-pends a spacer ...
          rasMfmrICONS$ArrowRIGHT,   # <- Adds the default <standardized> "Right-Arrow" icon ...
          csFormatRESET_, " "        # <- Closes text formatting and adds a <post-pended> spacer ...
        );
      } else {
        csCarat_ <- rasBasePASTE0(
          csFormatBOLD_,        # <- Applies the BOLD ANSI Text formatting ...
          csColorCarat_, " ",   # <- Applies the Text Colour Formats & pre-pends a spacer ...
          csCarat_,             # <- Adds the user-defined carat icon (symbol or text object) ...
          csFormatRESET_, " "   # <- Closes text formatting and adds a <post-pended> spacer ...
        );
      }
    }
    
    
    ## 5.3 - Prime the HEADER text accordingly ... ####
    if (sbPrintPretty) {
      ssHead_ <- rasBasePASTE0(
        csFormatBOLD_, csColorHead_,                   # <- Adds the BOLD & Colour text formats ...
        ssHead_,                                       # <- Adds the Note HEADER text value ...
        rasBaseElseIF(rsbIsDEBUG_, " ", ""),           # <- Adds a spacer if in DEBUG Mode !!!
        rasBaseElseIF(rsbIsDEBUG_, siCallCELN_, ""),   # <- Adds a Caller CELN if in DEBUG Mode !!!
        csFormatRESET_                                 # <- Closes text formatting ...
      );
    } else {
      ssHead_ <- rasBasePASTE0(
        ssHead_,                                      # <- Adds the Note HEADER text value ...
        rasBaseElseIF(rsbIsDEBUG_, " ", ""),          # <- Adds a spacer if in DEBUG Mode !!!
        rasBaseElseIF(rsbIsDEBUG_, siCallCELN_, "")   # <- Adds a Caller CELN if in DEBUG Mode !!!
      );
    }
    
    
    ## 5.4 - Prime the SPLIT icon accordingly ... ####
    if (sbPrintPretty) {
      if (csSplit_ == "|" || csSplit_ == " | "  || csSplit_ == "  |  "  || csSplit_ == "   |   ") {
        csSplit_ <- rasBasePASTE0(
          csFormatBOLD_, csColorSplit_,   # <- Adds the BOLD & Colour text formats ...
          " | ",                          # <- Adds the default <standardized> SPLIT icon ...
          csFormatRESET_                  # <- Closes text formatting ...
        );
      } else {
        csSplit_ <- rasBasePASTE0(
          csFormatBOLD_,        # <- Applies the BOLD ANSI Text formatting ... 
          csColorSplit_, " ",   # <- Applies the Text Colour Formats & pre-pends a spacer ...
          csSplit_,             # <- Adds the user-defined SPLIT icon ...
          csFormatRESET_, " "   # <- Closes text formatting & post-pends a spacer...
        );
      }
    }
    
    
    ## 5.5 - Prime the NOTE text accordingly ... ####
    if (sbPrintPretty) {
      ssBody_ <- rasBasePASTE0(
        csFormatBOLD_, csColorBody_,   # -> Adds the BOLD & Colour text formats ...
        ssBody_,                       # -> Adds the NOTE text value ...
        csFormatRESET_                 # -> Closes text formatting ...
      );
    }
    
    
    ## 5.6 - Prime the TAIL icon accordingly ... ####
    if (sbShowTail_) {
      if (sbPrintPretty) {
        csTail_ <- rasBasePASTE0(
          " ",      # <= Adds a pre-pended spacer ...
          csTail_   # <= Adds the defined TAIL icon (or symbol) ...
        );
      }
    } else {
      csTail_ <- "";   # -> Assigns a "zero-byte" value <blank> as the TAIL icon !!!
    }
    
    
    ## 5.7 - Finalize the Terminal New Lines ####
    ssTerminalNLs_ <- "";
    if (sbPostPend2NLs_) {
      ssTerminalNLs_ <- "\n\n";
    } else if (sbPostPend1NL_) {
      ssTerminalNLs_ <- "\n";
    }
    
    
    ## 5.7 - Compile FULL MESSAGE text !!! ####
    csFullNote_ <- rasBasePASTE0(
      rasBaseElseIF(sbPrePendNL, "\n", ""),   # <- Adds pre-pended NEW LINE (if so requested) !!!
      csCarat_, ssHead_,                      # <- Adds the CARAT icon & HEADER text sequences ...
      csSplit_, ssBody_,                      # <- Adds the SPLIT icon & NOTE (main body) text ...
      csTail_,                                # <- Adds the TAIL icon (as patched in Step 5.6) ...
      ssTerminalNLs_                          # <- Adds the terminal NEW LINES (as requested) !!!
    );
    
    
    ## 5.8 - FINALLY -> Post FULL MESSAGE text !!! ####
    rasBaseCAT(csFullNote_);   # -> VERY NB: Prints (outputs) full notification <message> to active 
                               #    R-Session Console window (THIS IS THE MAIN OBJECTIVE OF THIS R
                               #    FUNCTION) !!!
    
    
    
    ###   STEP 06 - Return Results to Function Call   ####
    # Outputs the full notification text <message> as the function's return value ...
    rasBaseIsINVISIBLE(csFullNote_);
    
  }
  
}


