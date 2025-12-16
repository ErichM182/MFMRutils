#? ### ### ### ### ### ### ###
#' @title Sanitize String and Vector File Paths
#' @name code.patch.file.path
#' 
#' @description
#' The "SuiteMFMR" Null-Coalescing Operator (NCO) is similar to the "??" NCO of
#' the DART Programming Language. The operator evaluates whether the "Left-Hand" R
#' Object is NULL (in terms of its value or length or class <data type>) and returns
#' the "Right-Hand" R Object if the "Left-Hand" Object is <indeed> NULL.
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
#' NULL %??% "DeFAULT"                    # -> returns "Default" !!!
#' "ACTual" %??% "DEFault"                # -> returns "ACTual" !!!
#' NULL %??% NULL %??% "FINal"            # -> returns "FINal" !!!
#' NULL %??% "PENULTimate" %??% "FINal"   # -> returns "PENULTimate" !!!
#'
#' @export
#? ### ### ###
code.patch.file.path <- function(
  ssPathString=NULL, vsPathVector=NULL
) {
  
  
  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();         # <- Captures <real-time> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "MFMRutils";              # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "Patch.File.Path";        # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_LONG_   <- "CODE.Patch.File.Path";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseNCHAR    <- base::nchar;
  rasBaseSubSTR   <- base::substr;
  rasBaseLENGTH   <- base::length;
  rasBaseRETURN   <- base::return;
  rasBaseDoCALL   <- base::do.call;
  rasBaseAsLIST   <- base::as.list;
  rasBaseIsNULL   <- base::is.null;
  rasBaseStrSPLIT <- base::strsplit;
  rasBaseFilePATH <- base::file.path;
  
  `%??%` <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rvsPathVector_  <- vsPathVector;
  rssPathString_ <- ssPathString;
  
  
  
  ####   STEP 04 - Execute MAIN <function> CODE LOGIC   ####
  ####   4.1 - Prime Local <function> Variables & Constants   ####
  rssPathFINAL_    <- NULL;   # <- Character object for capturing the FINAL <output> Path value ...
  rlsPathPIECES_   <- NULL    # <- Vector used for capturing the Path split string values.
  rsbIsPathVECTOR_ <- TRUE;   # <- Boolean variable to track the state of the function code logic.
  
  
  ####   4.2 - Run Critical NULL Checks (on function arguments)   ####
  ## Handle scenario where both the `ssPathString` and `vsPathVector` arguments are NULL ...
  if (rasBaseIsNULL(rssPathString_) && rasBaseIsNULL(rvsPathVector_)) {
    rssPathString_ <- ".";      # <- Assigns <active> R Project Root as the default Path <value> !!!
    rsbIsPathVECTOR_ <- FALSE;   # <- Tells function to apply the "String" formatting code logic !!!
  }
  
  ## Handle scenario where both the `ssPathString` and `vsPathVector` arguments are NOT NULL ...
  if (!rasBaseIsNULL(rssPathString_) && !rasBaseIsNULL(rvsPathVector_)) {
    rsbIsPathVECTOR_ <- FALSE;   # <- Tells function to apply the "String" formatting code logic !!!
  }
  
  ## Handle scenario where only the `ssPathString` function argument is NOT NULL ...
  if (!rasBaseIsNULL(rssPathString_) && rasBaseIsNULL(rvsPathVector_)) {
    rsbIsPathVECTOR_ <- FALSE;   # <- Tells function to apply the "String" formatting code logic !!!
  }
  
  ## Handle scenario where only the `vsPathVector` function argument is NOT NULL ...
  if (rasBaseIsNULL(rssPathString_) && !rasBaseIsNULL(rvsPathVector_)) {
    rsbIsPathVECTOR_ <- TRUE;   # <- Tells function to apply the "Vector" formatting code logic !!!
  }
  
  
  ####   4.3 - Patch Path Format (accordingly)   ####
  if (rsbIsPathVECTOR_) {
    
    ### 4.3.1a - Validate each vector component against function format criteria <standard> ...
    for (snINDX in 1:rasBaseLENGTH(rvsPathVector_)) {
      
      ## Extract Vector Object located at <active> Vector Index ...
      rssObj_ <- rvsPathVector_[snINDX];
      
      ## Extract the FIRST Character of the returned Vector Object ...
      rssCharFirst_ <- rasBaseSubSTR(
        x = rssObj_, start = 1, stop = 1
      );
      
      ## Extract the LAST Character of the returned Vector Object ...
      rssCharLast_ <- rasBaseSubSTR(
        x = rssObj_, start = rasBaseNCHAR(rssObj_), stop = rasBaseNCHAR(rssObj_)
      );
      
      ## Clean BEGINNING of returned Vector Object appropriately ...
      rssObj_CLEAN_ <- rssObj_;   # <- Assign object to new variable for cleaning purposes ...
      if (rssCharFirst_ == "/" || rssCharFirst_ == "\\") {
        rssObj_CLEAN_ <- rasBaseSubSTR(
          x = rssObj_CLEAN_, start = 2, stop = rasBaseNCHAR(rssObj_CLEAN_)
        );
      }
      
      ## Clean END of returned Vector Object appropriately ...
      if (rssCharLast_ == "/" || rssCharLast_ == "\\") {
        rssObj_CLEAN_ <- rasBaseSubSTR(
          x = rssObj_CLEAN_, start = rasBaseNCHAR(rssObj_CLEAN_), 
          stop = rasBaseNCHAR(rssObj_CLEAN_) - 1
        );
      }
      
      ## Update <active> Vector Item (object) with updated <clean> string value ...
      rvsPathVector_[snINDX] <- rssObj_CLEAN_;
      rlsPathPIECES_ <- rasBaseAsLIST(rvsPathVector_);
      
    }
    
  } else {
    
    ### 4.3.1a - Extract terminal (last) character of the PATH string ...
    rssTermCharPATH_ <- rasBaseSubSTR(
      x = rssPathString_, start = rasBaseNCHAR(rssPathString_), stop = rasBaseNCHAR(rssPathString_)
    );
    
    ### 4.3.1b - Extract terminal (last) character of the PATH string ...
    if (rssTermCharPATH_ == "/") {
      # rssFilePath_ <- rasBaseSubSTR(   # <- Cut off the last character ("/") of the PATH string.
      #   x = rssFilePath_, 
      #   start = rasBaseNCHAR(rssFilePath_),
      #   stop = rasBaseNCHAR(rssFilePath_) - 1
      # );
    }
    
  }
  
  
  ####   4.4 - Compile FINAL Path <value>   ####
  rssPathFINAL_ <- rasBaseDoCALL(
    what = rasBaseFilePATH, args = rlsPathPIECES_
  );
  
  
  
  ####   STEP 05 - Return RESULT to Function CALL   ####
  rasBaseRETURN(rssPathFINAL_);
  
}


