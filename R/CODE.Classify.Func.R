#? ### ### ### ### ### ### ###
#' @title Classify R functions on the basis of Code-Base-Weights (CBWs)
#' @name code.classify.func
#' 
#' @description
#' A <tiny> Helper Function that classifies a custom R function on the basis
#' of the size of code that defines that functions (i.e. on the basis of the
#' number of lines of code that constitutes the function itself). This custom
#' function was intended to mainly support the MFMR Suite of R Functions, but
#' can be utilized as a standalone function in other R packages.
#'
#' @param siStartCELN an integer that denotes the START of the function's block
#'                    of code. The Code Editor Line Number ("CELN") of the first
#'                    line of code that defines a custom function.
#' @param siStopCELN an integer that denotes the STOP (end) of the function's
#'                   block of code. The Code Editor Line Number ("CELN") of the
#'                   last line of code that defines a custom function - usually
#'                   identified by a closing curly brace.
#'
#' @returns This function returns a single <abbreviated> character value that
#'          classifies five categories (or classes) of custom R functions:
#'          * "TNY" -> a "Tiny" R Function (less than 28 lines of code);
#'          * "SML" -> a "Small" R Function (between 28 & 280 lines of code);
#'          * "MED" -> a "Medium" R Function (between 280 & 2800 lines of code);
#'          * "LRG" -> a "Large" R Function (between 2800 & 28000 lines of code);
#'          * "MSV" -> a "Massive" R Function (greater than 28000 lines of code);
#'
#' @examples
#' ### Classify your Custom R Function as follows:
#' require(MFMRutils)   ### -> Ensures the "MFMRutils" library is installed & loaded
#'
#' "myCustFuncR" <- function() {   ### <- Set this Code Editor Line Number "CELN"
#'                                 ###    as the "siStartCELN" argument value !!!
#'
#'    # Define your custom function code logic here !!!
#'
#'    # Classify your custom function ...
#'    ssFuncType <- MFMRutils::code.classify.func(1L, 14L)
#'    base::cat(
#'     base::paste0(
#'       " => My Custom R Function Class == ", ssFuncType, " !!!\n"
#'     )
#'    )
#'
#' }   ### <- Set this Code Editor Line Number "CELN" as the "siStopCELN" arg value !!!
#'
#' @export
#? ### ### ###
"code.classify.func" <- function(
  siStartCELN=NULL, siStopCELN=NULL
) {
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";            # <- R Library Identifier !!!
  rssTagFuncIDv01_      <- "Classify.Func";        # <- Function ID - SHORT !!!
  rssTagFuncIDv02_      <- "CODE.Classify.Func";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasRETURN <- base::return;
  rasIsNULL <- base::is.null;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  # NOTES: hand-over all func-args to func-local <internal> variables ...
  ssFuncRes_ <- NULL;   # -> The <final> function outputs <results> object.
  siStartCELN_ <- siStartCELN; siStopCELN_ <- siStopCELN;
  
  
  
  ####   STEP 04 - Calculate Function Code DELTA (START vs. END)   ####
  if (rasIsNULL(siStartCELN_)) {
    siStartCELN_ <- 1L;
  }
  if (rasIsNULL(siStopCELN_)) {
    siStopCELN_ <- 7L;
  }
  siCodeDELTA_ <- siStopCELN_ - siStartCELN_;
  
  
  
  ####   STEP 05 - Classify Function accordingly   ####
  if (siCodeDELTA_ <= 28L) {
    ssFuncRes_ <- "TNY";   # -> "TNY" == "TINY Function" !!!
  } else if (siCodeDELTA_ > 28L && siCodeDELTA_ <= 280L) {
    ssFuncRes_ <- "SML";   # -> "SML" == "SMALL Function" !!!
  } else if (siCodeDELTA_ > 280L && siCodeDELTA_ <= 2800L) {
    ssFuncRes_ <- "MED";   # -> "MED" == "MEDIUM Function" !!!
  } else if (siCodeDELTA_ > 2800L && siCodeDELTA_ <= 28000L) {
    ssFuncRes_ <- "LRG";   # -> "LRG" == "LARGE Function" !!!
  } else {   # <- ALL functions with greater than 28000 lines of code !!!
    ssFuncRes_ <- "MSV";   # -> "MSV" == "MASSIVE Function" !!!
  }
  
  
  
  ####   STEP 06 - Return Result to Function CALL   ####
  rasRETURN(ssFuncRes_);

}


