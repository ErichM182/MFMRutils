#? ### ### ### ### ### ### ###
#' @title The Null-Coalescing Operator ("SuiteMFMR" helper)
#' @name %?!%
#' 
#' @description
#' The "SuiteMFMR" Null-Coalescing Operator (NCO) is similar to the "??" NCO of
#' the DART Programming Language. The operator evaluates whether the "Left-Hand" R
#' Object is NULL (in terms of its value or length or class <data type>) and 
#' returns the "Right-Hand" R Object if the "Left-Hand" Object is NULL.
#'
#' @param coLHO a dynamic (complex) object that captures the "Left-Hand" R Object
#'              to be evaluated against the "NULL Criteria" (i.e. by way of value, 
#'              length or class <data type> NULL Checks).
#' @param coRHO a dynamic (complex) object that captures the "Right-Hand" R Object
#'              (to be returned if the "Left-Hand" R Object is NULL).
#'
#' @examples
#' ### Use the Null-Coalescing Operator (NCO) as follows: ...
#' library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...
#'
#' ### Then apply the NCO accordingly ...
#' NULL %?!% "Default"                    # -> returns "Default" !!!
#' "ACTual" %?!% "DEFault"                # -> returns "ACTual" !!!
#' NULL %?!% NULL %?!% "FINal"            # -> returns "FINal" !!!
#' NULL %?!% "PENULTimate" %?!% "FINal"   # -> returns "PENULTimate" !!!
#'
#' @export
#? ### ### ###
`%?!%` <- function(coLHO=NULL, coRHO=NULL) {
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  rssTagFuncIDv01_ <- "Null.Coal.Oper";                  # <- Function ID - SHORT !!!
  rssTagFuncIDv02_ <- "CODE.Null.Coalescing.Operator";   # <- Function ID - LONG !!!
  ### rssTagFuncLibID_ <- MFMRutils::pkgs.get.lib.info()[["NAME"]];
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve R Session Memory Efficiency ...
  rasANY    <- base::any;
  rasIsNA   <- base::is.na;
  rasLENGTH <- base::length;
  rasRETURN <- base::return;
  rasIsNULL <- base::is.null;
  
  
  
  ####   STEP 03 - Execute MAIN <function> CODE LOGIC   ####
  if (rasIsNULL(coLHO) || rasLENGTH(coLHO) == 0 || rasANY(rasIsNA(coLHO))) {
    rasRETURN(coRHO);
  } else {
    rasRETURN(coLHO);
  }
  
}






