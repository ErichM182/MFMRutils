#? ### ### ### ### ### ### ###
#' @title Sanitize String and Vector File Paths
#' @name code.clean.file.path
#' @family SuiteMFMR CODE Functions
#' 
#' 
#' 
#' @description
#' Easily standardize R Project File Paths (directories) with the use of this "SuiteMFMR" function.
#' This function employs the base R file path function (i.e. `base::file.path()`) under the hood and 
#' can handle R Project File Paths defined in the String and Vector formats/constructs.
#'
#'
#'
#' @param ssPathString ([character]) A string value containing the File Path (i.e. local directory
#'                     text value) that needs to be sanitized (standardized) by this function.
#' @param vsPathVector ([vector] of [character]s) A character vector containing the File Path (i.e. 
#'                     local directory text value) that needs to be sanitized (standardized) by 
#'                     this function.
#'
#'
#'
#' @examples
#' ### Use the "Path-Cleaning" Function as follows: ...
#' library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...
#' 
#' 
#' 
#' ### OPTION 1 - Run a NULL-ARGs execution of the "Path-Cleaning" Function ...
#' # NB: If no external values are passed to the function a default file path will be returned !!!
#' ssPathArgsNULL <- code.clean.file.path()   # <- No external args <values> passed to function ... 
#' ssPathArgsNULL                             # -> returns a default R Project Path (".") !!!
#' 
#' 
#' 
#' ### OPTION 2 - Use a STRING to define the File Path (R Project Directory, etc.) ...
#' # Prime the Function Inputs <arguments> (as needed) ...
#' ssCleanSTR_ <- "./rProjFiles/rData/TestDATA.csv"          # <- Clean PATH String !!!
#' ssMessySTR_ <- "./\\rProjFiles//rData/\\TestDATA.csv\\"   # <- Messy PATH String !!!
#' 
#' # Run the VECTOR-LOGIC execution of the "Path-Cleaning" Function ...
#' ssPathCleanSTRING <- MFMRutils::code.clean.file.path(   # <- Executes "STRING" Code Logic ...
#'   ssPathString = ssCleanSTR_                            # <- CLEAN "PATH String" supplied !!!
#' ); ssPathCleanSTRING                                    # -> returns clean Path STRING result !!!
#' 
#' ssPathMessySTRING <- MFMRutils::code.clean.file.path(   # <- Executes the "STRING" Code Logic ...
#'   ssPathString = ssMessySTR_                            # <- MESSY "PATH String" supplied !!!
#' ); ssPathMessySTRING                                    # -> returns clean Path STRING result !!!
#' 
#' 
#' 
#' ### OPTION 3 - Use a VECTOR to define the File Path (R Project Directory, etc.) ...
#' # Prime the Function Inputs <arguments> (as needed) ...
#' vsCleanVEC_ <- c(".", "rProjFiles", "rData", "TestDATA.csv")             # <- Clean PATH Vector.
#' vsMessyVEC_ <- c("./", "\\rProjFiles/", "/rData/", "\\TestDATA.csv\\")   # <- Messy PATH Vector.
#' 
#' # Run the VECTOR-LOGIC execution of the "Path-Cleaning" Function ...
#' ssPathCleanVECTOR <- MFMRutils::code.clean.file.path(   # <- Executes the "VECTOR" Code Logic ...
#'   vsPathVector = vsCleanVEC_                            # <- CLEAN "PATH Vector" supplied !!!
#' ); ssPathCleanVECTOR                                    # -> returns clean Path STRING result !!!
#' 
#' ssPathMessyVECTOR <- MFMRutils::code.clean.file.path(   # <- Executes the "VECTOR" Code Logic ...
#'   vsPathVector = vsMessyVEC_                            # <- MESSY "PATH Vector" supplied !!!
#' ); ssPathMessyVECTOR                                    # -> returns clean Path STRING result !!!
#' 
#' 
#' 
#' ### OPTION 4 - Provide BOTH the STRING and VECTOR Function Arguments ...
#' # NB: When both the String and Vector Function Arguments are supplied as function inputs, the
#' #     function will use the String argument instead of the Vector argument (i.e. defaults on the  
#' #     String function argument). 
#' 
#' # MESSY Example: run the "Path-Cleaning" Function with both the String & Vector MESSY values ...
#' ssPathBothArgsMESSY <- MFMRutils::code.clean.file.path(   # <- Executes "BOTH-ARGs" Code Logic...
#'   vsPathVector = vsMessyVEC_,                             # <- MESSY "PATH Vector" supplied !!!
#'   ssPathString = ssMessySTR_                              # <- MESSY "PATH String" supplied !!!
#' ); ssPathBothArgsMESSY                                    # -> returns clean Path STRING result.
#' 
#' # CLEAN Example: run the "Path-Cleaning" Function with both the String & Vector CLEAN values ...
#' ssPathBothArgsCLEAN <- MFMRutils::code.clean.file.path(   # <- Executes "BOTH-ARGs" Code Logic...
#'   vsPathVector = vsCleanVEC_,                             # <- CLEAN "PATH Vector" supplied !!!
#'   ssPathString = ssCleanSTR_                              # <- CLEAN "PATH String" supplied !!!
#' ); ssPathBothArgsCLEAN                                    # -> returns clean Path STRING result.
#' 
#' 
#'
#' @export
#? ### ### ###
"code.clean.file.path" <- function(
  ssPathString=NULL, vsPathVector=NULL
) {
  
  
  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();         # <- Captures <real-time> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "MFMRutils";              # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "Clean.File.Path";        # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_LONG_   <- "code.clean.file.path";   # <- Function ID - LONG !!!
  
  
  
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
  
  rasMfmrSplitOnVECTOR <- MFMRutils::code.split.string.on.vector;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rvsPathVector_ <- vsPathVector;
  rssPathString_ <- ssPathString;
  
  
  
  ####   STEP 04 - Execute MAIN <function> CODE LOGIC   ####
  ####   4.1 - Prime Local <function> Variables & Constants   ####
  rssPathFINAL_    <- NULL;   # <- Character object for capturing the FINAL <output> Path value ...
  rlsPathPIECES_   <- NULL;   # <- Vector used for capturing the Path split string values.
  rsbIsPathVECTOR_ <- TRUE;   # <- Boolean variable to track the state of the function code logic.
  
  
  ####   4.2 - Run Critical NULL Checks (on function arguments)   ####
  ## Handle scenario where BOTH the `ssPathString` and `vsPathVector` ARGUMENTS are NULL ...
  if (rasBaseIsNULL(rssPathString_) && rasBaseIsNULL(rvsPathVector_)) {
    rssPathString_ <- ".";       # <- Assigns a default Path value !!!
    rsbIsPathVECTOR_ <- FALSE;   # <- Tells function to apply the "String" patching code logic !!!
  }
  
  ## Handle scenario where BOTH the `ssPathString` and `vsPathVector` ARGUMENTS are NOT NULL ...
  if (!rasBaseIsNULL(rssPathString_) && !rasBaseIsNULL(rvsPathVector_)) {
    rsbIsPathVECTOR_ <- FALSE;   # <- Tells function to apply the "String" patching code logic !!!
  }
  
  ## Handle scenario where only the `ssPathString` function argument is NOT NULL ...
  if (!rasBaseIsNULL(rssPathString_) && rasBaseIsNULL(rvsPathVector_)) {
    rsbIsPathVECTOR_ <- FALSE;   # <- Tells function to apply the "String" patching code logic !!!
  }
  
  ## Handle scenario where only the `vsPathVector` function argument is NOT NULL ...
  if (rasBaseIsNULL(rssPathString_) && !rasBaseIsNULL(rvsPathVector_)) {
    rsbIsPathVECTOR_ <- TRUE;   # <- Tells function to apply the "Vector" patching code logic !!!
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
          x = rssObj_CLEAN_, start = 1, stop = rasBaseNCHAR(rssObj_CLEAN_) - 1
        );
      }
      
      ## Update <active> Vector Item (object) with updated <clean> string value ...
      rvsPathVector_[snINDX] <- rssObj_CLEAN_;
      
    }
    
    ### 4.3.1b - Return result in the form of an R List Object ...
    rlsPathPIECES_ <- rasBaseAsLIST(rvsPathVector_);
    
  } else {
    
    ### 4.3.2a - Split PATH String into its constituent parts ...
    rvsPathUnits_ <- rasMfmrSplitOnVECTOR(
      ssSplitString = rssPathString_,   # <- Provide the STRING Directory Path <value> ...    
      vsSplitVector = c("/", "\\"),     # <- Split PATH value on common OS Directory Delimiters ...
      sbExactSplits = FALSE             # <- Apply RegEx code parsing (if encountered in PATH) !!!
    )[["SPLITS"]];                      # <- VERY NB -> extracts only the String SPLITs values !!! 
    
    ### 4.3.2b - Return result in the form of an R List Object ...
    rlsPathPIECES_ <- rasBaseAsLIST(rvsPathUnits_);
    
  }
  
  
  ####   4.4 - Compile FINAL Path <value>   ####
  rssPathFINAL_ <- rasBaseDoCALL(
    what = rasBaseFilePATH, args = rlsPathPIECES_
  );
  
  
  
  ####   STEP 05 - Return RESULT to Function CALL   ####
  rasBaseRETURN(rssPathFINAL_);
  
}


