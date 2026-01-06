#? ### ### ### ### ### ### ###
#' @title Null-Coalescing Operator (the "SuiteMFMR" way)
#' @name %??%
#' @family SuiteMFMR CODE Functions
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
#' library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...
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
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";                       # <- R Library Identifier !!!
  rssTagFuncIDv01_      <- "Null.Coal.Oper";                  # <- Function ID - SHORT !!!
  rssTagFuncIDv02_      <- "CODE.Null.Coalescing.Operator";   # <- Function ID - LONG !!!
  
  
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


