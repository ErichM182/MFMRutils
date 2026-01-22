#? ### ### ### ### ### ### ###
#' @title Null-Coalescing Operator (the "SuiteMFMR" way)
#' @name %??%
#' @family CODE Functions (SuiteMFMR)
#' 
#' 
#' @description
#' The "SuiteMFMR" Null-Coalescing Operator (NCO) is similar to the "??" NCO of
#' the DART Programming Language. The operator evaluates whether the "Left-Hand" R
#' Object is NULL (in terms of its value, length & class <data type>) and returns
#' the "Right-Hand" R Object if the "Left-Hand" Object is <indeed> NULL.
#'
#'
#' @param coLHO a dynamic (complex) object that captures the "Left-Hand" R Object
#'              to be evaluated against the "NULL Criteria" (i.e. by way of value, 
#'              length or class <data type> NULL Checks).
#' @param coRHO a dynamic (complex) object that captures the "Right-Hand" R Object
#'              (to be returned if the "Left-Hand" R Object is NULL).
#'
#'
#' @examples
#' ### Use the Null-Coalescing Operator (NCO) as follows: ...
#' library(MFMRutils)   # <- Ensures the "MFMRutils" library is installed locally (loads library)...
#'
#' ### Then apply the NCO accordingly ...
#' NULL %??% "DeFAULT"            # -> returns "DeFAULT" !!!
#' "ACTual" %??% "DEFault"        # -> returns "ACTual" !!!
#' NULL %??% NULL %??% 1982       # -> returns 1982 !!!
#' NULL %??% FALSE %??% "FINal"   # -> returns FALSE !!!
#' "TEST" %??% NULL %??% TRUE     # -> returns "TEST" !!!
#'
#'
#' @export
#? ### ### ###
`%??%` <- function(coLHO=NULL, coRHO=NULL) {
  
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  RCT_TAG_FUNC_LIBR_ID_    <- "MFMRutils";                       # <- R Library Identifier !!!
  RCT_TAG_R_FUNC_ID_SHORT_ <- "Null.Coal.Oper";                  # <- Function ID - SHORT !!!
  RCT_TAG_R_FUNC_ID_LONG_  <- "CODE.Null.Coalescing.Operator";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseANY    <- base::any;
  rasBaseIsNA   <- base::is.na;
  rasBaseLENGTH <- base::length;
  rasBaseRETURN <- base::return;
  rasBaseIsNULL <- base::is.null;
  
  
  
  ####   STEP 03 - Execute MAIN <function> CODE LOGIC   ####
  if (rasBaseIsNULL(coLHO) || rasBaseLENGTH(coLHO) == 0 || rasBaseANY(rasBaseIsNA(coLHO))) {
    rasBaseRETURN(coRHO);
  } else {
    rasBaseRETURN(coLHO);
  }
  
}






#? ### ### ### ### ### ### ###
#' @title Check R Project Run-Time Mode
#' @name code.poll.r.runtime.mode
#' @family CODE Functions (SuiteMFMR)
#' 
#' 
#' @description
#' A Function Dot Argument Coalescing Function. This function greatly aides the unification (i.e. 
#' joining, collapsing or coalescing) two sets of DotsArgs groups into a single R List Object. This 
#' is a very important function within the "MFMR Suite of R Functions" (i.e. `SuiteMFMR`) 
#' considering almost all `SuiteMFMR` functions implement a dual-approach (i.e. more user-friendly 
#' approach) towards passing around "Fall-Through" (i.e. "DotsArgs") function arguments.
#'
#'
#' @examples
#' ### Use the R Run-time Mode Polling Function as follows: ...
#' library(MFMRutils)   # <- Ensure the "MFMRutils" library is installed locally (loads library) ...
#'
#' ### Then apply the NCO accordingly ...
#' ## STEP 1 - Create the `DEBUG` and `VERBOSE` Run-time Mode trackers in your <root> R Project ...
#' RCT_IS_DEBUG_RT_MODE_ <- TRUE      # <- Prime the DEBUG R Run-time Tracker accordingly (NB: the
#'                                    #    DEBUG variable must have the exact form specified here
#'                                    #    -> i.e. `RCT_IS_DEBUG_RT_MODE_`) !!!
#' RCT_IS_VERBOSE_RT_MODE_ <- FALSE   # <- Prime the VERBOSE R Run-time Tracker accordingly (NB: the
#'                                    #    VERBOSE variable must have the exact form specified here
#'                                    #    -> i.e. `RCT_IS_VERBOSE_RT_MODE_`) !!!
#' 
#' ## STEP 2 - Poll the Run-time Mode anywhere in your R Project (or custom R Funtions) ...
#' RCT_RT_MODE <- code.poll.r.runtime.mode()   # <- Polls the Run-time Mode where-ever called !!!
#' 
#' ## STEP 3 - Read or output the Run-time Mode (state or status) as needed ...
#' RCT_RT_MODE$IS_DEBUG     # Output: [TRUE]  -> Returns the DEBUG R Run-time Mode <result> ...  
#' RCT_RT_MODE$IS_VERBOSE   # Output: [FALSE] -> Returns the VERBOSE R Run-time Mode <result> ...  
#' 
#'
#' @export
#? ### ### ###
"code.poll.r.runtime.mode" <- function() {
  
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  RCT_TAG_FUNC_LIBR_ID_    <- "MFMRutils";                  # <- R Library Identifier !!!
  RCT_TAG_R_FUNC_ID_SHORT_ <- "Poll.RT.Mode";               # <- Function ID - SHORT !!!
  RCT_TAG_R_FUNC_ID_LONG_  <- "CODE.Poll.R.Runtime.Mode";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseGET0   <- base::get0;
  rasBaseLIST   <- base::list;
  rasBaseRETURN <- base::return;
  
  rasMfmrFSID   <- MFMRutils::RENV_FSID;
  
  
  
  ####   STEP 03 - Extract Run-time Modes   ####
  RAS_IS_MODE_DEBUG_   <- rasMfmrFSID$CONSTS_BOOL_IS_DEBUG;
  RAS_IS_MODE_VERBOSE_ <- rasMfmrFSID$CONSTS_BOOL_IS_VERBOSE;
  
  sbIsDEBUG_ <- rasBaseGET0(   # <- Searches the Global Environment of the Active R Session for the
    RAS_IS_MODE_DEBUG_,        #    <somewhat> uniquely named variable `RCT_IS_DEBUG_RT_MODE_` and
    envir = .GlobalEnv,        #    extracts its value.
    ifnotfound = FALSE         # -> Assigns a value of `FALSE` if the variable was NOT FOUND in
  );                           #    the Active R Session (i.e. active R run-time) !!!
  sbIsVERBOSE_ <- rasBaseGET0(   # <- Searches the Global Environment of the Active R Session for 
    RAS_IS_MODE_VERBOSE_,        #    the <somewhat> uniquely named variable
    envir = .GlobalEnv,          #    `RCT_IS_VERBOSE_RT_MODE_` and extracts its value.
    ifnotfound = FALSE           # -> Assigns a value of `FALSE` if the variable was NOT FOUND in
  );                             #    the Active R Session (i.e. active R run-time) !!!
  
  
  
  ####   STEP 04 - Return Results to Function Call   ####
  rasBaseRETURN(
    rasBaseLIST(
      "IS_DEBUG" = sbIsDEBUG_, "IS_VERBOSE" = sbIsVERBOSE_
    )
  );
  
}






#? ### ### ### ### ### ### ###
#' @title Dots-Args Coalescing Function
#' @name code.coalesce.dots.args
#' @family CODE Functions (SuiteMFMR)
#' 
#' 
#' @description
#' A Function Dots Arguments Coalescing Function. This function greatly aides the unification (i.e. 
#' joining, collapsing or coalescing) of two sets of "DotsArgs" groups into a single R List Object. 
#' This is a very important function within the "MFMR Suite of R Functions" (i.e. `SuiteMFMR`) 
#' considering almost all `SuiteMFMR` functions implement a dual-approach (i.e. more user-friendly 
#' approach) towards passing around "Fall-Through" (i.e. "DotsArgs") function arguments.
#'
#'
#' @param coArgsList01 ([list]) an R List (complex) object that captures the first group of Function 
#'                     Dot Arguments (i.e. "Fall-Through" Function Arguments) to be coalesced.
#' @param coArgsList02 ([list]) an R List (complex) object that captures the second group of 
#'                     Function Dot Arguments (i.e. "Fall-Through" Function Arguments) to be 
#'                     coalesced.
#'
#'
#' @examples
#' ### Use the DotsArgs-Coalescing Function as follows: ...
#' library(MFMRutils)   # <- Ensures the "MFMRutils" library is installed locally (loads library)...
#'
#' ### Then apply the NCO accordingly ...
#'
#'
#' @export
#? ### ### ###
"code.coalesce.dots.args" <- function(coArgsList01=NULL, coArgsList02=NULL) {
  
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  RCT_TAG_FUNC_LIBR_ID_    <- "MFMRutils";                 # <- R Library Identifier !!!
  RCT_TAG_R_FUNC_ID_SHORT_ <- "Coal.Dots.Args";            # <- Function ID - SHORT !!!
  RCT_TAG_R_FUNC_ID_LONG_  <- "CODE.Coalesce.Dots.Args";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseC      <- base::c;
  rasBaseRETURN <- base::return;
  rasBaseIsNULL <- base::is.null;
  
  
  
  ####   STEP 03 - Internalize All Function Arguments   ####
  coDotsArgsV01_ <- coArgsList01;
  coDotsArgsV02_ <- coArgsList02;
  
  
  
  ####   STEP 04 - Execute MAIN <function> CODE LOGIC   ####
  if (!rasBaseIsNULL(coDotsArgsV01_) && rasBaseIsNULL(coDotsArgsV02_)) {
    rasBaseRETURN(coDotsArgsV01_);
  } else if (rasBaseIsNULL(coDotsArgsV01_) && !rasBaseIsNULL(coDotsArgsV02_)) {
    rasBaseRETURN(coDotsArgsV02_);
  } else {   # <- This means both Function Arguments are NOT NULL !!!
    rasBaseRETURN(rasBaseC(coDotsArgsV01_, coDotsArgsV02_));
  }
  
}


