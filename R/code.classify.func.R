#? ### ### ### ### ### ### ###
#' @title Classify any custom R function via its Code-Base-Weight (CBW)
#' @description
#' A <tiny> Helper Function that classifies a custom R function on the basis
#' of the size of code that defines that functions (i.e. on the basis of the
#' number of lines of code that constitutes the function itself). This custom
#' function was intended to mainly support the MFMR Suite of R Functions, but
#' can be utilized as a standalone function in other R packages.
#'
#' @param siFuncStartCELN an integer that denotes the START of the function's
#'                        block of code. The Code Editor Line Number (`CELN`) of
#'                        the first line of code that defines a custom function.
#' @param siFuncStopCELN an integer that denotes the STOP (end) of the function's
#'                       block of code. The Code Editor Line Number (`CELN`) of
#'                       the last line of code that defines a custom function -
#'                       usually identified by a closing curly brace.
#'
#' @returns This function returns a single <abbreviated> character value that
#'          classifies five sizes (or classes) of custom R functions:
#'          * "TNY" -> a "Tiny" R Function (less than 50 lines of code);
#'          * "SML" -> a "Small" R Function (between 50 & 150 lines of code);
#'          * "MED" -> a "Medium" R Function (between 150 & 750 lines of code);
#'          * "LRG" -> a "Large" R Function (between 750 & 1500 lines of code);
#'          * "MSV" -> a "Massive" R Function (greater than 1500 lines of code);
#'
#' @examples
#' ### Classify your Custom R Function as follows:
#' require(MFMRutils)   # -> Ensures the "MFMRutils" library is installed & loaded
#'
#' "my_cust_r_func" <- function() {   # -> Set this Code Editor Line Number `CELN`
#'                                    #    as the `siFuncStartCELN` arg value !!!
#'
#'    # Define your custom function code logic here !!!
#'
#'    # Classify your custom function ...
#'    ssFuncType <- MFMRutils::code.classify.func(1, 9)
#'    base::cat(
#'     base::paste0(
#'       " => My Custom R Function Class == ", ssFuncType, " !!!\n"
#'     )
#'    )
#'
#' }   # -> Set this Code Editor Line Number `CELN`as the `siFuncStopCELN` arg value !!!
#'
#' @export
#? ### ### ###
"code.classify.func" <- function(
  siFuncStartCELN=NULL, siFuncStopCELN=NULL
) {

  ### STEP 1 - Define the "Function Self-ID" tag ... ####
  ssFuncSelfID_ <- "MFMR.Classify-Func";

  ### STEP 2 - Internalize ALL Function Arguments ... ####
  #            ( i.e. hand-over all to func-args to func-local variables )
  ssFuncRes_ <- NULL;   # -> The <final> function outputs <results> object.
  siFuncStartCELN_ <- siFuncStartCELN; siFuncStopCELN_ <- siFuncStopCELN;

  ### STEP 3 - Calculate Function Code DELTA ... ####
  if (base::is.null(siFuncStartCELN_)) {
    siFuncStartCELN_ <- 1L;
  }
  if (base::is.null(siFuncStopCELN_)) {
    siFuncStopCELN_ <- 7L;
  }
  siCodeDELTA_ <- siFuncStopCELN_ - siFuncStartCELN_;

  ### STEP 4 - Classify Function ... ####
  if (siCodeDELTA_ <= 50L) {
    ssFuncRes_ <- "TNY";   # -> "TNY" == "TINY Function" !!!
  } else if (siCodeDELTA_ > 50L && siCodeDELTA_ <= 150L) {
    ssFuncRes_ <- "SML";   # -> "SML" == "SMALL Function" !!!
  } else if (siCodeDELTA_ > 150L && siCodeDELTA_ <= 750L) {
    ssFuncRes_ <- "MED";   # -> "MED" == "MEDIUM Function" !!!
  } else if (siCodeDELTA_ > 750L && siCodeDELTA_ <= 1500L) {
    ssFuncRes_ <- "LRG";   # -> "LRG" == "LARGE Function" !!!
  } else {   # <- ALL functions with greater than 1500 lines of code !!!
    ssFuncRes_ <- "MSV";   # -> "MSV" == "MASSIVE Function" !!!
  }

  ### STEP 6 - Classify Function ... ####
  base::return(ssFuncRes_);

}


