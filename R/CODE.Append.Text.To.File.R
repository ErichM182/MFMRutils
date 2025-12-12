#? ### ### ### ### ### ### ###
#' @title Append Text to File (the "SuiteMFMR" way)
#' @name code.append.text.to.file
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
#' NULL %??% "Default"                    # -> returns "Default" !!!
#' "ACTual" %??% "DEFault"                # -> returns "ACTual" !!!
#' NULL %??% NULL %??% "FINal"            # -> returns "FINal" !!!
#' NULL %??% "PENULTimate" %??% "FINal"   # -> returns "PENULTimate" !!!
#'
#' @export
#? ### ### ###
code.append.text.to.file <- function(
    ssFilePath=".", ssFileID=".Rbuildignore", 
    ssAppendText="^WIP$", sbMultiAppend=FALSE, sbPostPend=TRUE
    
) {
  
  
  ####   STEP 01 - Define "Function Self-ID" Tags   ####
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";                  # <- R Library Identifier !!!
  rssTagFuncIDv01_      <- "Text.To.File";               # <- Function ID - SHORT !!!
  rssTagFuncIDv02_      <- "code.append.text.to.file";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseANY        <- base::any;
  rasBaseGREP       <- base::grep;
  rasBaseLENGTH     <- base::length;
  rasBaseRETURN     <- base::return;
  rasBaseIfELSE     <- base::ifelse;
  rasBasePASTE0     <- base::paste0;
  rasBaseReadLINES  <- base::readLines;
  rasBaseFilePATH   <- base::file.path;
  rasBaseCHARACTER  <- base::character;
  rasBaseReadLINES  <- base::readLines;
  rasBaseDirCREATE  <- base::dir.create;
  rasBaseWriteLINES <- base::writeLines;
  rasBaseFileEXISTS <- base::file.exists;
  rasBaseFileCREATE <- base::file.create;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rssFileID_      <- ssFileID;
  rssFilePath_    <- ssFilePath;
  rsbPostPend_    <- sbPostPend;
  rssAppendText_  <- ssAppendText;
  rsbMultiAppend_ <- sbMultiAppend;
  
  
  
  ####   STEP 04 - Create DIRECTORY & FILE IF-NOT-EXISTS   ####
  rssIsNewlyCreatedFile_ <- FALSE;
  rssFullPathFile_ <- rasBaseFilePATH(rssFilePath_, rssFileID_);
  if (!rasBaseFileEXISTS(rssFullPathFile_)) {   # <- If TRUE -> then file DOES NOT EXIST !!!
    
    ### 4.1 - Create required directory or directories (if not already exists) ...
    rasBaseDirCREATE(
      path = rssFilePath_, recursive = T, showWarnings = F
    );
    
    ### 4.2 - Create the required File ...
    rssIsNewlyCreatedFile_ <- TRUE;   # <- NB for STEP 5 (below) !!!
    rasBaseFileCREATE(rssFullPathFile_);
  }
  
  
  
  ####   STEP 05 - Append TEXT to File IF-TEXT-NOT-EXISTS   ####
  rvsExtantFileContents_ <- rasBaseIfELSE(   # <- Extract existing (extant) file contents (NB) !!!
    rssIsNewlyCreatedFile_, rasBaseCHARACTER(0),       # <- Returns a NULL Character R Object !!!
    rasBaseReadLINES(rssFullPathFile_, warn = FALSE)   # <- Returns the actual contents of the file.
  );
  
  rcoFileOUT_ <- NULL;
  if (rsbMultiAppend_) {   # <- Append Text (even if already exists in the file - multiple times)!!!
    if (rsbPostPend_) {    # <- Append to END of file (i.e. AFTER existing file contents) !!!
      rcoFileOUT_ <- rasBasePASTE0(
        rvsExtantFileContents_, "\n", rssAppendText_
      );
      rasBaseWriteLINES(
        text = c(rvsExtantFileContents_, rssAppendText_), con = rssFullPathFile_
      );
    } else {   # < Append to START of file (i.e. BEFORE existing file contents) !!!
      rcoFileOUT_ <- rasBasePASTE0(
        rssAppendText_, "\n",rvsExtantFileContents_
      );
      rasBaseWriteLINES(
        text = c(rssAppendText_, rvsExtantFileContents_), con = rssFullPathFile_
      );
    }
  } else {   # <- Append Text ONLY IF it does NOT ALREADY EXISTS (in the file - ONLY ONCE EVER) !!!
    # Use grep to find if the exact pattern_to_add exists in any line
    # We use fixed=TRUE because we are checking for the literal string "^WIP$",
    # not whether the lines match the ^WIP$ pattern as a regex.
    rsbAlreadyExistsTEXT_ <- rasBaseANY(
      rasBaseGREP(pattern = rssAppendText_, x = rvsExtantFileContents_, fixed = TRUE)
    );
    if (!rsbAlreadyExistsTEXT_) {   # <- If Text DOES NOT ALREADY EXISTS (in file) ...
      if (rsbPostPend_) {    # <- Append to END of file (i.e. AFTER existing file contents) !!!
        rcoFileOUT_ <- rasBasePASTE0(
          rvsExtantFileContents_, "\n", rssAppendText_
        );
        rasBaseWriteLINES(
          text = c(rvsExtantFileContents_, rssAppendText_), con = rssFullPathFile_
        );
      } else {   # < Append to START of file (i.e. BEFORE existing file contents) !!!
        rcoFileOUT_ <- rasBasePASTE0(
          rssAppendText_, "\n",rvsExtantFileContents_
        );
        rasBaseWriteLINES(
          text = c(rssAppendText_, rvsExtantFileContents_), con = rssFullPathFile_
        );
      }
    }
  }
  
  
  
  ####   STEP 06 - Return Patched File to function call   ####
  rasBaseRETURN(rcoFileOUT_);
  
}


