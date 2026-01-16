#? ### ### ### ### ### ### ###
#' @title Post Standardized R-Project Notifications
#' @name info.post.note
#' @family INFO Functions (SuiteMFMR)
#' 
#' 
#' @description
#' A Helper Function that standardizes the R Project Information Posting (i.e. User Notification) 
#' Processes. This function was intended to mainly support the MFMR Suite of R Functions, but may 
#' be utilized as a standalone function in other (i.e. 3rd Party) R packages.
#'
#'
#' @param csIconCarat ([character]) a String or Text (i.e. [character] [vector]) object that defines
#'                    what the first or starting symbol (i.e. carat) of the note, to be posted, 
#'                    should be or look like. This can be a simple text or a complex (i.e. ANSI, 
#'                    RegEx or escaped character) object - but must be an acceptable R [character] 
#'                    object (default: `=>`).
#' @param ssHead ([character]) a simple String or Text (i.e. [character] [vector]) argument that 
#'               defines the starting (i.e. header) text of the note (to be posted). This function 
#'               argument is useful in standardizing the notification format at an R Project level
#'               (default: `NULL`).
#' @param csIconSplit ([character]) a complex String or Text (i.e. [character] [vector]) object that 
#'                    defines what the middle symbol (i.e. separator between the `ssHead` and 
#'                    `ssBody` components) of the note, to be posted, should be or look like. This 
#'                    can be a simple text or a complex (i.e. ANSI, RegEx or escaped character) 
#'                    object - but must be an acceptable R [character] object (default: `|`).
#' @param ssBody ([character]) a simple String or Text (i.e. [character] [vector]) argument that 
#'               defines the main (i.e. body) text of the note to be posted (default: "POST-NOTE 
#'               'Body' Text NULL !!!").
#' @param csIconTail ([character]) a String or Text (i.e. [character] [vector]) object that defines
#'                   what the ending symbol (i.e. tail or terminal icon) of the note, to be posted,
#'                   should be or look like. This can be a simple text or a complex (i.e. ANSI, 
#'                   RegEx or escaped character) object, but must be an acceptable R [character] 
#'                   object (default: `MFMRutils::RENV_ICONS$FireFlame`).
#' @param sbShowTail ([logical]) a Boolean value that specifies whether to include the tail (i.e.
#'                   trailing or terminal symbol or icon) object in the note to be posted or not 
#'                   (default: `TRUE`).
#' @param sbPrintPretty ([logical]) a Boolean value that specifies whether the built-in (i.e. the
#'                      function's internal) text formatting (i.e. ANSI <font weight and colour>
#'                      formats) should be applied to the printed notification or not (default: 
#'                      `TRUE`).
#' @param csColorHead ([character]) a String or Text (i.e. [character] [vector]) value that defines 
#'                    the text colour for the header (`ssHead`) object of the note to be posted
#'                    (default: `MFMRutils::RENV_COLOURSGreenFORE`).
#' @param csColorBody ([character]) a String or Text (i.e. [character] [vector]) value that defines 
#'                    the text colour for the main text (`ssBody`) object of the note to be posted
#'                    (default: `MFMRutils::RENV_COLOURSCyanFORE`).
#' @param csColorCarat ([character]) a String or Text (i.e. [character] [vector]) value that defines 
#'                     the colour for the leading (starting) symbol or icon (`csIconCarat`) object 
#'                     of the note to be posted (default: `MFMRutils::RENV_COLOURSYellowFORE`).
#' @param csColorSplit ([character]) a String or Text (i.e. [character] [vector]) value that defines 
#'                     the colour for the middle spacer (`csIconSplit`) object of the note to be 
#'                     posted (default: `MFMRutils::RENV_COLOURSYellowFORE`).
#' @param sbPrePend1NL ([logical]) a Boolean value that specifies whether a new line (blank space or 
#'                     row) should be added to the START of the note to be posted or not (default: 
#'                     `FALSE`).
#' @param sbPostPend1NL ([logical]) a Boolean value that specifies whether a SINGLE new line (blank 
#'                      space or row) should be added to the END of the note to be posted or not 
#'                      (default: `TRUE`).
#' @param sbPostPend2NLs ([logical]) a Boolean value that specifies whether a DOUBLE new line (blank 
#'                      spaces or rows) should be added to the END of the note to be posted or not. 
#'                      This function argument can be useful for clearly delineating sections within 
#'                      a custom R Function or R Project output (printed) code (default: `FALSE`).
#' @param sbPostAlways ([logical]) a Boolean value that specifies whether the note should ALWAYS be
#'                     posted (printed to the active R Console) or not (default: `TRUE`).
#' @param siCallCELN ([integer]) a Numeric value that specifies the Code Editor Line Number (CELN) 
#'                   at which this function was called from by its parent function or R Script.
#' @param ... ([list]) a List of R Objects function argument to catch all "Fall-Through" or 
#'            "DotsArgs" values. The [...] argument for this function is used specifically as a 
#'            generic function argument feature (i.e. "Fall-Through-Args" back-up) that enables 
#'            passing "DotsArgs" from parent functions in which this function is nested. Function 
#'            arguments that can be passed here are collated in the [MFMRutils::RENV_FSID] immutable
#'            list (NB: all [MFMRutils::RENV_FSID] list items prefixed with `F_ARGS_` may be used).
#'
#'
#' @returns
#' * This function prints the supplied text (message) directly to the active R Session Console when 
#'   the function argument `sbPostAlways` (or either of the R Project DEBUG and VERBOSE <active> 
#'   Runtime Trackers: i.e. `RCT_IS_DEBUG_RT_MODE_` and `RCT_IS_VERBOSE_RT_MODE_`, respectively) are 
#'   set to the boolean value of `TRUE`.
#' * This function also outputs the full (complete) notification message as an invisible function 
#'   return value (function result or output).
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
#'   csIconCarat = "~>",                               # <- Sets the leading <start> icon to a "~>".
#'   ssHead = "My-CUST-FUNC",                      # <- Sets the HEADER text of the notification ...
#'   ssBody = "This is my COOL NOTE -> YaY !!!",   # <- Sets the MAIN <body> text of notification.
#'   sbShowTail = FALSE                            # <- Hides the trailing <tail> icon object !!!
#' )
#' 
#' 
#' ### Example 3: Deactivate the built-in formatting (spaces, font weights and font colours ) ...
#' info.post.note(
#'   csIconCarat = "~>",                               
#'   ssHead = "My-CUST-FUNC",                      
#'   ssBody = "This is my COOL NOTE -> YaY !!!",   
#'   sbShowTail = FALSE,                           
#'   sbPrintPretty = FALSE   # <- Prints the input text as provided (i.e. without spaces or any 
#' )                         #    text formatting). You must format your text (as required) prior to
#'                           #    handing it to the function (when `sbPrintPretty` is set to FALSE).
#' 
#' 
#' ### Example 4: Post notifications on basis of the DEBUG and VERBOSE R Project trackers ...
#' RCT_IS_VERBOSE_RT_MODE_ <- TRUE   # <- Set the R VERBOSE Tracker to TRUE anywhere in R Project...
#' info.post.note(
#'   csIconCarat = "~>",                               
#'   ssHead = "My-CUST-FUNC",                      
#'   ssBody = "This is my COOL NOTE -> YaY !!!",   
#'   sbPostAlways = FALSE   # <- The R Project VERBOSE tracker (`RCT_IS_VERBOSE_RT_MODE_`)
#' )                        #    overrides the `sbPostAlways` function argument -> this means
#'                          #    the function WILL PRINT its message to the R Console, even if
#'                          #    the `sbPostAlways` function argument is set to FALSE, when 
#'                          #    the VERBOSE tracker is set to a value of TRUE !!!
#'                                
#' ## Remove VERBOSE Tracker from Global R Environment ...
#' rm(list = c("RCT_IS_VERBOSE_RT_MODE_"))   # <- Remove VERBOSE Tracker from R Environment !!!
#'
#'
#' RCT_IS_DEBUG_RT_MODE_ <- TRUE   # <- Set the R DEBUG Tracker to TRUE anywhere in an R Project ...
#' info.post.note(
#'   csIconCarat = "~>",
#'   ssHead = "My-CUST-FUNC",
#'   ssBody = "This is my COOL NOTE -> YaY !!!",
#'   sbPostAlways = FALSE   # <- The R Project DEBUG tracker (`RCT_IS_DEBUG_RT_MODE_`)
#' )                        #    overrides the `sbPostAlways` function argument -> this means
#'                          #    the function WILL PRINT its message to the R Console, even if
#'                          #    the `sbPostAlways` function argument is set to FALSE, when
#'                          #    the DEBUG tracker is set to a value of TRUE !!!
#' 
#' ### NB: DEBUG Tracker also activates the Code Editor Line Number (CELN) section of the 
#' ###    `info.post.note()` function output (CELN is positioned after the HEADER text) !!!
#'                                
#' ## Remove DEBUG Tracker from Global R Environment ...
#' rm(list = c("RCT_IS_DEBUG_RT_MODE_"))   # <- Remove DEBUG Tracker from R Environment !!!
#'
#'
#' @export
#? ### ### ###
"info.post.note" <- function(
  csIconCarat=NULL, ssHead=NULL, csIconSplit=NULL, ssBody=NULL, csIconTail=NULL, sbShowTail=NULL, 
  sbPrintPretty=NULL, csColorHead=NULL, csColorBody=NULL, csColorCarat=NULL, csColorSplit=NULL, 
  sbPrePend1NL=NULL, sbPostPend1NL=NULL, sbPostPend2NLs=NULL, sbPostAlways=NULL, siCallCELN=NULL, 
  ...
) {
  
  
  ####   STEP 01 - Prime "Function Self-ID" CONSTANTS   ####
  ## NB: This 👆  is THE 1st OF ONLY 2 FUNCTIONS [in the entire MFMR Suite of R Functions] THAT DO
  ##     NOT SELF-IDENTIFY (since a Self-ID implementation here will cause infinite recursion) !!!
  RCT_DBL_SYS_TIME_NOW_ <- base::Sys.time();   # <- Extract the <active> System Date-Time !!!
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";        # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_NSID_ <- "Post.Note";        # <- This Function DOES NOT SELF-ID (NSID) !!! 
  RCT_TAG_FUNC_ID_LONG_ <- "INFO.Post.Note";   # <- FSID - LONG !!!
  
  RCT_INT_CELN_START_ <- 150L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    OPENING <normal> brace/bracket "(" is located !!!
  RCT_INT_CELN_STOP_  <- 372L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    CLOSING <curly> brace/bracket "}" is located !!!
  coDotsArgs_ <- base::list(...);   # <- Capture all the "DotsArgs" values here !!!
  
  
  
  ####   STEP 02 - Alias ALL <required> Functions   ####
  ##   NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasBaseCAT         <- base::cat;
  rasBaseGET0        <- base::get0;
  rasBaseNCHAR       <- base::nchar;
  rasBaseIfELSE      <- base::ifelse;
  rasBasePASTE0      <- base::paste0;
  rasBaseIsNULL      <- base::is.null;
  rasBaseIsINVISIBLE <- base::invisible;
  
  `%??%`         <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasMfmrFSID    <- MFMRutils::RENV_FSID;
  rasMfmrICONS   <- MFMRutils::RENV_ICONS;
  rasMfmrFORMATS <- MFMRutils::RENV_FORMATS;
  rasMfmrCOLOURS <- MFMRutils::RENV_COLOURS;
  rasMfmrGetCELN <- MFMRutils::code.get.celn;
  
  ## SPECIAL - Constant - TAG - Aliases (NB for the `INFO.Post.*` functions) ...
  RAS_TAG_FUNC_ID_SHORT_ <- rasMfmrFSID$CONSTS_FID_SHORT;
  RAS_IS_MODE_DEBUG_     <- rasMfmrFSID$CONSTS_BOOL_IS_DEBUG;
  RAS_IS_MODE_VERBOSE_   <- rasMfmrFSID$CONSTS_BOOL_IS_VERBOSE;
  RAS_IS_POST_ALWAYS_    <- rasMfmrFSID$F_ARGS_BOOL_POST_ALWAYS;
  
  
  
  ####   STEP 04 - Run NULL Checks accordingly   ####
  sbIsDEBUG_ <- base::get0(   # <- Searches the Global Environment of the Active R Session for
    RAS_IS_MODE_DEBUG_,       #    the <somewhat> uniquely named variable `RCT_IS_DEBUG_RT_MODE_`
    envir = .GlobalEnv,       #    and extracts its value.
    ifnotfound = FALSE        # -> Assigns a value of `FALSE` if the variable was NOT FOUND in
  );                          #    the Active R Session !!!
  sbIsVERBOSE_ <- base::get0(   # <- Searches the Global Environment of the Active R Session for
    RAS_IS_MODE_VERBOSE_,       #    the <somewhat> uniquely named variable `RCT_IS_VERBOSE_RT_MODE_`
    envir = .GlobalEnv,         #    and extracts its value.
    ifnotfound = FALSE          # -> Assigns a value of `FALSE` if the variable was NOT FOUND in
  );                            #    the Active R Session !!!
  
  
  sbPostAlways_ <- coDotsArgs_[[RAS_IS_POST_ALWAYS_]] %??% FALSE;   # <- NB to extract here !!!
  if (sbPostAlways_ || sbIsVERBOSE_ || sbIsDEBUG_) {   # <- Run code if any of these are TRUE !!!
    
    ####   STEP 03 - Execute Custom Function's Code logic   ####
    ##   3.1 - Prime Standard Text Formats ... ####
    csFormatBOLD_  <- rasMfmrFORMATS$ANSI_BOLD;
    csFormatRESET_ <- rasMfmrFORMATS$ANSI_RESET;
    
    ##   3.2 - Prime selected variables (akin to constants) ...
    csIconFLAME_    <- rasMfmrICONS$FireFlame;
    csColorsCYAN_   <- rasMfmrCOLOURS$CyanFORE;
    csColorsGREEN_  <- rasMfmrCOLOURS$GreenFORE;
    csColorsYELLOW_ <- rasMfmrCOLOURS$YellowFORE;
    
    ##   3.3 - Prime selected variables (akin to constants) ...
    RAS_ICON_CARAT_      <- rasMfmrFSID$F_ARGS_ICON_CARAT;
    RAS_ICON_SPLIT_      <- rasMfmrFSID$F_ARGS_ICON_SPLIT;
    RAS_ICON_TAIL_       <- rasMfmrFSID$F_ARGS_ICON_TAIL;
    RAS_IS_SHOW_TAIL_    <- rasMfmrFSID$F_ARGS_BOOL_SHOW_TAIL;
    RAS_IS_PRINT_PRETTY_ <- rasMfmrFSID$F_ARGS_BOOL_PRINT_PRETTY;
    RAS_COLOUR_HEAD_     <- rasMfmrFSID$F_ARGS_COLOR_TEXT_HEAD;
    RAS_COLOUR_BODY_     <- rasMfmrFSID$F_ARGS_COLOR_TEXT_BODY;
    RAS_COLOUR_CARAT_    <- rasMfmrFSID$F_ARGS_COLOR_CARAT;
    RAS_COLOUR_SPLIT_    <- rasMfmrFSID$F_ARGS_COLOR_SPLIT;
    RAS_IS_NL_PRE_ONE_   <- rasMfmrFSID$F_ARGS_BOOL_NEW_LINE_PRE_ONE;
    RAS_IS_NL_POST_1ST_  <- rasMfmrFSID$F_ARGS_BOOL_NEW_LINE_POST_ONE;
    RAS_IS_NL_POST_2ND_  <- rasMfmrFSID$F_ARGS_BOOL_NEW_LINE_POST_TWO;
    RAS_FUNC_CALLR_CELN_ <- rasMfmrFSID$F_ARGS_FUNC_CALLER_CELN;
    
    
    
    ###   STEP 04 - Internalize ALL Function Arguments   ####
    # NOTES: hand-over all func-args to func-local <internal> variables ...
    csIconCarat_    <- csIconCarat    %??% coDotsArgs_[[RAS_ICON_CARAT_]]      %??% "=>";
    ssHead_         <- ssHead         %??% "FSID-NULL";
    csIconSplit_    <- csIconSplit    %??% coDotsArgs_[[RAS_ICON_SPLIT_]]      %??% "|";
    ssBody_         <- ssBody         %??% "POST-NOTE 'Body' Text NULL !!!";
    csIconTail_     <- csIconTail     %??% coDotsArgs_[[RAS_ICON_TAIL_]]       %??% csIconFLAME_;
    sbShowTail_     <- sbShowTail     %??% coDotsArgs_[[RAS_IS_SHOW_TAIL_]]    %??% TRUE;
    sbPrintPretty_  <- sbPrintPretty  %??% coDotsArgs_[[RAS_IS_PRINT_PRETTY_]] %??% TRUE;
    csColorHead_    <- csColorHead    %??% coDotsArgs_[[RAS_COLOUR_HEAD_]]     %??% csColorsGREEN_;
    csColorBody_    <- csColorBody    %??% coDotsArgs_[[RAS_COLOUR_BODY_]]     %??% csColorsCYAN_;
    csColorCarat_   <- csColorCarat   %??% coDotsArgs_[[RAS_COLOUR_CARAT_]]    %??% csColorsYELLOW_;
    csColorSplit_   <- csColorSplit   %??% coDotsArgs_[[RAS_COLOUR_SPLIT_]]    %??% csColorsYELLOW_;
    sbPrePend1NL_   <- sbPrePend1NL   %??% coDotsArgs_[[RAS_IS_NL_PRE_ONE_]]   %??% FALSE;
    sbPostPend1NL_  <- sbPostPend1NL  %??% coDotsArgs_[[RAS_IS_NL_POST_1ST_]]  %??% TRUE;
    sbPostPend2NLs_ <- sbPostPend2NLs %??% coDotsArgs_[[RAS_IS_NL_POST_2ND_]]  %??% FALSE;
    siCallCELN_     <- siCallCELN     %??% coDotsArgs_[[RAS_FUNC_CALLR_CELN_]] %??% "";
    
    
    if (rasBaseIsNULL(ssHead_)) {
      ssHead_ <- rasBaseGET0(
        RAS_TAG_FUNC_ID_SHORT_,          # <- Find the parent <caller> Function ID (if defined) !!!
        envir = base::pos.to.env(-1L),   # <- The R environment the function was called from !!!
        ifnotfound = "UNK-Func-ID"       # <- Set a DEFAULT <caller> Function Identifier <UNKNOWN> !!! 
      );
    }
    
    ## 5.2 - Prime the CARAT icon accordingly ... ####
    if (sbPrintPretty_) {
      if (csIconCarat_ == "=>" || csIconCarat_ == " => " || csIconCarat_ == "  =>  " || 
          csIconCarat_ == "   =>   " || csIconCarat_ == "->" || csIconCarat_ == " -> " || 
          csIconCarat_ == "  ->  " || csIconCarat_ == "   ->   ") {
        csIconCarat_ <- rasBasePASTE0(
          csFormatBOLD_,             # <- Applies the BOLD ANSI Text formatting ...
          csColorCarat_, " ",        # <- Applies the Text Colour Formats & pre-pends a spacer ...
          rasMfmrICONS$ArrowRIGHT,   # <- Adds the default <standardized> "Right-Arrow" icon ...
          csFormatRESET_, " "        # <- Closes text formatting and adds a <post-pended> spacer ...
        );
      } else {
        csIconCarat_ <- rasBasePASTE0(
          csFormatBOLD_,        # <- Applies the BOLD ANSI Text formatting ...
          csColorCarat_, " ",   # <- Applies the Text Colour Formats & pre-pends a spacer ...
          csIconCarat_,             # <- Adds the user-defined carat icon (symbol or text object) ...
          csFormatRESET_, " "   # <- Closes text formatting and adds a <post-pended> spacer ...
        );
      }
    }
    
    
    ## 5.3 - Prime the HEADER text accordingly ... ####
    if (sbPrintPretty_) {
      sbHasVal_ <- rasBaseNCHAR(siCallCELN_) >= 1;   # <- Run Boolean check on CELN Val-length !!! 
      ssHead_ <- rasBasePASTE0(
        csFormatBOLD_, csColorHead_,   # <- Adds the BOLD & Colour text formats ...
        ssHead_,                       # <- Adds the Note HEADER text value ...
        rasBaseIfELSE(
          sbHasVal_ && sbIsDEBUG_ || 
          sbHasVal_ && sbIsVERBOSE_, 
          " ", ""                      # <= Adds a pre-pended spacer (if valid CELN Conditions) !!!
        ),
        siCallCELN_,                   # <- Adds a Caller CELN if in DEBUG Mode !!!
        csFormatRESET_                 # <- Closes text formatting ...
      );
    } else {
      ssHead_ <- rasBasePASTE0(
        ssHead_,                  # <- Adds the Note HEADER text value ...
        siCallCELN_               # <- Adds a Caller CELN if in DEBUG Mode !!!
      );
    }
    
    
    ## 5.4 - Prime the SPLIT icon accordingly ... ####
    if (sbPrintPretty_) {
      if (csIconSplit_ == "|" || csIconSplit_ == " | "  || csIconSplit_ == "  |  "  || 
          csIconSplit_ == "   |   ") {
        csIconSplit_ <- rasBasePASTE0(
          csFormatBOLD_, csColorSplit_,   # <- Adds the BOLD & Colour text formats ...
          " | ",                          # <- Adds the default <standardized> SPLIT icon ...
          csFormatRESET_                  # <- Closes text formatting ...
        );
      } else {
        csIconSplit_ <- rasBasePASTE0(
          csFormatBOLD_,        # <- Applies the BOLD ANSI Text formatting ... 
          csColorSplit_, " ",   # <- Applies the Text Colour Formats & pre-pends a spacer ...
          csIconSplit_,             # <- Adds the user-defined SPLIT icon ...
          csFormatRESET_, " "   # <- Closes text formatting & post-pends a spacer...
        );
      }
    }
    
    
    ## 5.5 - Prime the NOTE text accordingly ... ####
    if (sbPrintPretty_) {
      ssBody_ <- rasBasePASTE0(
        csFormatBOLD_, csColorBody_,   # -> Adds the BOLD & Colour text formats ...
        ssBody_,                       # -> Adds the NOTE text value ...
        csFormatRESET_                 # -> Closes text formatting ...
      );
    }
    
    
    ## 5.6 - Prime the TAIL icon accordingly ... ####
    if (sbShowTail_) {
      if (sbPrintPretty_) {
        csIconTail_ <- rasBasePASTE0(
          " ",      # <= Adds a pre-pended spacer ...
          csIconTail_   # <= Adds the defined TAIL icon (or symbol) ...
        );
      }
    } else {
      csIconTail_ <- "";   # -> Assigns a "zero-byte" value <blank> as the TAIL icon !!!
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
      rasBaseIfELSE(sbPrePend1NL, "\n", ""),   # <- Adds pre-pended NEW LINE (if so requested) !!!
      csIconCarat_, ssHead_,                  # <- Adds the CARAT icon & HEADER text sequences ...
      csIconSplit_, ssBody_,                  # <- Adds the SPLIT icon & NOTE (main body) text ...
      csIconTail_,                            # <- Adds the TAIL icon (as patched in Step 5.6) ...
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


