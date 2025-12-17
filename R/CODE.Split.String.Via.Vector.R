#? ### ### ### ### ### ### ###
#' @title Sanitize String and Vector File Paths
#' @name code.split.string.via.vector
#' 
#' @description
#' The "SuiteMFMR" Null-Coalescing Operator (NCO) is similar to the "??" NCO of
#' the DART Programming Language. The operator evaluates whether the "Left-Hand" R
#' Object is NULL (in terms of its value or length or class <data type>) and returns
#' the "Right-Hand" R Object if the "Left-Hand" Object is <indeed> NULL.
#'
#' @param ssStrToSplit a dynamic (complex) object that captures the "Left-Hand" R Object
#'              to be evaluated against the "NULL Criteria" (i.e. by way of value, 
#'              length or class <data type> NULL Checks).
#' @param vsSplitVector a dynamic (complex) object that captures the "Right-Hand" R Object
#'              (to be returned if the "Left-Hand" R Object is NULL).
#' @param sbExactSplits [logical] If TRUE match split exactly, otherwise use regular expressions.
#'
#' @examples
#' ### Use the Null-Coalescing Operator (NCO) as follows: ...
#' library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...
#'
#' ### Then apply the NCO accordingly ...
#' # Use Strings as File Paths (i.e. as R Project Directory Inputs) ...
#' 
#' 
#' # Use Vectors as File Paths (i.e. as R Project Directory Inputs) ...
#' vsPathCLEAN_ <- c(".", "rProjFiles", "rData", "InputDATA.txt");            # <- CLEAN Vector
#' vsPathMESSY_ <- c("./", "\\rProjFiles/", "/rData/", "\\TestDATA.txt\\");   # <- MESSY Vector
#'
#' @export
#? ### ### ###
code.split.string.via.vector <- function(
  ssStrToSplit="A-CRAZY//TEST\\String?for the?!'MFMRutils::split.string.via.vector()'_function!", 
  vsSplitVector=c("/", "_", "-", "?", "\\", " ", "'", "!"), sbExactSplits=FALSE
) {
  
  
  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();                 # <- Captures <active> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "MFMRutils";                      # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "Split.Via.Vector";               # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_LONG_   <- "code.split.string.via.vector";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseLIST     <- base::list;
  rasBaseNAMES    <- base::names;
  rasBaseNCHAR    <- base::nchar;
  rasBaseLENGTH   <- base::length;
  rasBasePASTE0   <- base::paste0;
  rasBaseRETURN   <- base::return;
  rasBaseStrSPLIT <- base::strsplit;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rssStrToSplit_  <- ssStrToSplit;
  rvsSplitVector_ <- vsSplitVector;
  rsbExactSplits_ <- sbExactSplits;
  
  
  
  ####   STEP 04 - Execute MAIN <function> CODE LOGIC   ####
  ####   4.1 - Prime Local <function> Variables & Constants   ####
  rvsVectOUT_ <- c();                # <- Vector for capturing FINAL <output> values (results) ...
  rvsSplitStringUnits_ <- c();       # <- Vector for capturing String Split Units (parts) ...
  rssMutagenSTR_ <- "";              # <- The "Hot POTATOE" String (for String Mutagenesis) !!!
  RCT_FUNC_DELIM_ <- "≡";            # <- A special delimiter needed by this function (only) !!!
  RCT_REGEX_CHARS_ <- rasBaseLIST(   # <- An exhaustive list of Regex Meta-Characters !!!
    "\t" = "\\t",   # <- Tab
    "\f" = "\\f",   # <- Form feed
    "\n" = "\\n",   # <- Newline (LF)
    "\v" = "\\v",   # <- Vertical tab
    "\a" = "\\a",   # <- Alert (bell)
    ### "\0" = "\\0",   # <- Null character   <- R appears to have issues with these Regex chars !!!
    ### "\e" = "\\e",   # <- Escape character <- R appears to have issues with these Regex chars !!!
    "\r" = "\\r",   # <- Carriage return (CR)
    "^"  = "\\^",   # <- Caret (if needed literally)
    "$"  = "\\$",   # <- Dollar sign (if needed literally)
    "."  = "\\.",   # <- Period (if needed literally)
    "|"  = "\\|",   # <- Pipe (if needed literally)
    "?"  = "\\?",   # <- Question mark (if needed literally)
    "*"  = "\\*",   # <- Asterisk (if needed literally)
    "+"  = "\\+",   # <- Plus sign (if needed literally)
    "("  = "\\(",   # <- Opening parenthesis (if needed literally)
    ")"  = "\\)",   # <- Closing parenthesis (if needed literally)
    "["  = "\\[",   # <- Opening bracket (if needed literally)
    "]"  = "\\]",   # <- Closing bracket (if needed literally)
    "{"  = "\\{",   # <- Opening brace (if needed literally)
    "}"  = "\\}",   # <- Closing brace (if needed literally)
    "\\" = "\\\\"   # <- Backslash
  );
  
  
  ####   4.2 - Iterate over Split Vector & Split String accordingly   ####
  for (rsnIndx in 1:rasBaseLENGTH(rvsSplitVector_)) {
    
    ### 4.3.1a - Sanitize any Regex Meta-Characters (when encountered) ...
    rcsSplitOBJ_ <- rvsSplitVector_[rsnIndx];   # <- Extract Split Vector Object @ current Index !!!
    if (rcsSplitOBJ_ %in% rasBaseNAMES(RCT_REGEX_CHARS_)) {
      rcsSplitOBJ_ <- RCT_REGEX_CHARS_[[rcsSplitOBJ_]];
    }
    
    ### 4.3.1b - Split the String on Split Vector Object ...
    if (rsnIndx == 1) {   # <- Use the original <user-specified> String when for-loop starts ...
      rvsSplitStringUnits_ <- rasBaseStrSPLIT(
        x = rssStrToSplit_, split = rcsSplitOBJ_, fixed = rsbExactSplits_
      );
    } else {   # <- ... then use the `Mutagenesis String` for all subsequent for-loop cycles ...
      rvsSplitStringUnits_ <- rasBaseStrSPLIT(
        x = rssMutagenSTR_, split = rcsSplitOBJ_, fixed = rsbExactSplits_
      );
    }
    
    ### 4.3.1c - Iterate over Split Units and compile output Vector ...
    for (rvsUnit in rvsSplitStringUnits_[[1]]) {
      if (rasBaseNCHAR(rvsUnit) >= 1) {
        rvsVectOUT_ <- c(
          rvsVectOUT_,      # <- Take existing vector (contents) and then ...
          rvsUnit #,        # <- Add only VALID Split Units to Vector (i.e. grow vector as needed).
          ### RCT_FUNC_DELIM_   # <- Denote end of vector with special delimiter (very NB) !!!
        );   
      }
    }
    
    ### 4.3.1c - VERY IMPORTANT -> collapse Split Units into the mutagenesis <local> String ...
    rssMutagenSTR_ <- rasBasePASTE0(
      rvsSplitStringUnits_[[1]], collapse = RCT_FUNC_DELIM_
    );
    
  }
  
  
  ####   4.3 - Clean-up String Split Process   ####
  
  
  
  
  ####   STEP 05 - Return RESULT to Function CALL   ####
  rasBaseRETURN(rvsVectOUT_);
  
}


