#? ### ### ### ### ### ### ###
#' @title Append Text to File (the "SuiteMFMR" way)
#' @name code.append.text.to.file
#' @family SuiteMFMR Code Functions
#' 
#' 
#' @description
#' Append text character vectors (strings) to specified file. This function was originally intended 
#' to aide the "SuiteMFMR" R Library Developer Process (as part of the R Project init-phase). This
#' is a standalone function and as such can also be used in (and by) other (3rd Party) R Projects.
#'
#'
#' @param ssFilePath a character vector (string) that defines the path where the file that needs to
#'                   be appended to is located on the local directory.
#' @param ssFileID a character vector (string) that defines the file name (i.e. file ID), inclusive
#'                 of the file extension), of the file that needs to be appended to.
#' @param ssAppendText a character vector (string) that defines the actual text (i.e. the character 
#'                     vector or string) to be appended to the specified file.
#' @param sbMultiAppend a logical (boolean) function argument that specifies whether multiple append
#'                      actions are permitted (to the specified file) or not. When set to FALSE the 
#'                      function checks whether the "ssAppendText" value already exists within the
#'                      specified file and only appends if the text does not already exist (i.e. was
#'                      not found) in the contents of the specified file. When set to TRUE the 
#'                      function appends the "ssAppendText" value to the specified file irrespective
#'                      of whether the text already exists in the specified file or not.
#' @param sbPostPend a logical (boolean) function argument that specifies whether the "ssAppendText"
#'                   value should be appended to the start or end of the existing (extant) specified
#'                   file contents. When set to TRUE the "ssAppendText" value will be appended to 
#                    the end; and when set FALSE the "ssAppendText" value will be append to the 
#                    start of the existing content. 
#'
#'
#' @examples
#' ### Use the File-Text Append Function as follows: ...
#' library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...
#'
#' ### Then apply function accordingly ...
#' # Prime Function Arguments ...
#' sbMultiAppend_ <- TRUE
#' sbPostPend_    <- FALSE
#' ssFileID_      <- "File_for_Appending.txt"
#' ssAppendText_  <- "Text to APPEND to File !!!"
#' ssFilePath_    <- "~/path/to/file/for/text/appending"
#' 
#' # Apply Function (provide arguments to function & execute function) ...
#' code.append.text.to.file(
#'   ssFilePath = ssFilePath_,         # <- Full Path to File (to which text should be appended) ...
#'   ssFileID = ssFileID_,             # <- File name (File ID) -> must include file extension !!!
#'   ssAppendText = ssAppendText_,     # <- Text to be appended to file ...
#'   sbMultiAppend = sbMultiAppend_,   # <- `TRUE` -> append text even if it already exists in file.
#'   sbPostPend = sbPostPend_          # <- `FALSE` -> append text to START of file.
#' )
#'
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
  
  
  
  ####   STEP 02 - Define "Local Aliases" for ALL Internal Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseCAT        <- base::cat;
  rasBaseANY        <- base::any;
  rasBaseGREP       <- base::grep;
  rasBaseNCHAR      <- base::nchar;
  rasBaseLENGTH     <- base::length;
  rasBaseRETURN     <- base::return;
  rasBaseIfELSE     <- base::ifelse;
  rasBasePASTE0     <- base::paste0;
  rasBaseSubSTR     <- base::substr;
  rasBaseReadLINES  <- base::readLines;
  rasBaseFilePATH   <- base::file.path;
  rasBaseCHARACTER  <- base::character;
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
  rsbIsNewlyCreatedFile_ <- FALSE;
  rssTermCharPATH_ <- rasBaseSubSTR(   # <- Extract terminal (last) character of the PATH string.
    x = rssFilePath_, 
    start = rasBaseNCHAR(rssFilePath_),
    stop = rasBaseNCHAR(rssFilePath_)
  );
  
  ### Ensure the PATH string is correctly formatted (i.e. DOES NOT END with a "/" character) !!!
  if (rssTermCharPATH_ == "/") {
    rssFilePath_ <- rasBaseSubSTR(   # <- Cut off the last character ("/") of the PATH string.
      x = rssFilePath_, 
      start = rasBaseNCHAR(rssFilePath_),
      stop = rasBaseNCHAR(rssFilePath_) - 1
    );
  }
  
  rssFullPathFile_ <- rasBaseFilePATH(rssFilePath_, rssFileID_);
  if (!rasBaseFileEXISTS(rssFullPathFile_)) {   # <- If TRUE -> then file DOES NOT EXIST !!!
    
    ### 4.1 - Create required directory or directories (if not already exists) ...
    rasBaseDirCREATE(
      path = rssFilePath_, recursive = TRUE, showWarnings = FALSE
    );
    
    ### 4.2 - Create the required File ...
    rsbIsNewlyCreatedFile_ <- TRUE;   # <- NB for STEP 5 (below) !!!
    rasBaseFileCREATE(rssFullPathFile_);
  }
  
  
  
  ####   STEP 05 - Append TEXT to File IF-TEXT-NOT-EXISTS   ####
  rvsExtantFileContents_ <- NULL;
  rssNullSTRING_ <- rasBaseCHARACTER(0);
  
  ### Extract existing (extant) file contents (NB) !!!
  if (rsbIsNewlyCreatedFile_) {
    rvsExtantFileContents_ <- rssNullSTRING_;   # <- Returns a NULL Character R Object !!!
  } else {
    rvsExtantFileContents_ <- rasBaseReadLINES(
      con = rssFullPathFile_, warn = FALSE
    );   # <- Returns actual contents of file!!!
  }
  
  rcoFileOUT_ <- NULL;
  if (rsbMultiAppend_) {   # <- Append Text (even if already exists in the file - multiple times)!!!
    if (rsbPostPend_) {    # <- Append to END of file (i.e. AFTER existing file contents) !!!
      rcoFileOUT_ <- rasBasePASTE0(
        rasBasePASTE0(rvsExtantFileContents_, collapse = "\n"), "\n", rssAppendText_
      );
      rasBaseWriteLINES(
        text = c(rvsExtantFileContents_, rssAppendText_), con = rssFullPathFile_
      );
    } else {   # < Append to START of file (i.e. BEFORE existing file contents) !!!
      rcoFileOUT_ <- rasBasePASTE0(
        rssAppendText_, "\n", rasBasePASTE0(rvsExtantFileContents_, collapse = "\n")
      );
      rasBaseWriteLINES(
        text = c(rssAppendText_, rvsExtantFileContents_), con = rssFullPathFile_
      );
    }
  } else {   # <- Append Text ONLY IF it does NOT ALREADY EXISTS (in the file - ONLY ONCE EVER) !!!
    
    rcoFileOUT_ <- rvsExtantFileContents_;
    
    ### Use grep to find if the exact pattern (^WIP$) exists in any line. We use fixed=TRUE because 
    ### we are checking for the literal string "^WIP$", and not whether the lines match the ^WIP$ 
    ### pattern as a regex.
    rsbAlreadyExistsTEXT_ <- rasBaseANY(
      rasBaseGREP(
        pattern = rssAppendText_, 
        x = rvsExtantFileContents_, fixed = TRUE
      )
    );
    if (!rsbAlreadyExistsTEXT_) {   # <- If Text DOES NOT ALREADY EXISTS (in file) ...
      if (rsbPostPend_) {    # <- Append to END of file (i.e. AFTER existing file contents) !!!
        rcoFileOUT_ <- rasBasePASTE0(
          rasBasePASTE0(rvsExtantFileContents_, collapse = "\n"), 
          "\n", rssAppendText_
        );
        rasBaseWriteLINES(
          con = rssFullPathFile_,
          text = c(
            rvsExtantFileContents_, rssAppendText_
          )
        );
      } else {   # < Append to START of file (i.e. BEFORE existing file contents) !!!
        rcoFileOUT_ <- rasBasePASTE0(
          rssAppendText_, "\n",
          rasBasePASTE0(rvsExtantFileContents_, collapse = "\n") 
        );
        rasBaseWriteLINES(
          con = rssFullPathFile_,
          text = c(
            rssAppendText_, rvsExtantFileContents_
          )
        );
      }
    }
  }
  
  
  
  ####   STEP 06 - Return Patched File to function call   ####
  rasBaseRETURN(rcoFileOUT_);
  
}


