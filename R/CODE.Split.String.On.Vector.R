#? ### ### ### ### ### ### ###
#' @title Split String on Basis of Vector Elements 
#' @name code.split.string.on.vector
#' 
#' 
#' @description
#' Effortlessly split (cut up or snip) an R String (i.e. an R character vector) into the required 
#' pieces (snippets) with the help of a second character vector that specifies the locations (i.e. 
#' symbols or character values) where the main character vector (String) should be cut up at.
#'
#'
#' @param ssSplitString ([character]) A function argument that supplies the string or text (to be
#'                      cut up into its constituent parts) to the internal code of this function.
#' @param vsSplitVector ([vector] of [character]s) A vector of characters (strings) that defines the
#'                      characters (symbols) at which the `ssSplitString` value should be cut up at.
#' @param sbExactSplits ([logical]) A boolean function argument that defines the type of split to
#'                      apply (default: [FALSE]). If [TRUE] it interprets the `vsSplitVector` values
#'                      exactly (i.e. as is -> on their base or literal character symbolization), or 
#'                      else the function applies the `vsSplitVector` values as regular expressions.
#'
#'
#' @examples
#' ### Use the "Vector-Splitting" Function as follows: ...
#' library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...
#'
#'
#' ### Prime the Function Inputs (accordingly) ...
#' ssSplitString_ <- base::paste0(   # <- Compile a SUPER MESSED-UP <symbol-riddled> String.
#' "A+ ", " -", "CRAZY&", " // ", "+ String", " ?%? ", "to", "_ ", "& test", " \\ ", "} the", 
#' "/ ", "& SUPER{-%-}COOL#", "?!", "FUNCTIONALITY", " @ ", "of", " # ", "the {", "? ???&???", 
#' "+ 'MFMRutils::code.split.string.on.vector()'", " $ ", "^ + function", "!! !"
#' ); ssSplitString_   # <- Placing the variable here (after ";") merely prints it to the R Console.
#' 
#' vsSplitVector_ = c(   # <- Specify on which characters the MESSED-UP String should be cut up at.
#'   "/", "_", "-", "?", " ", "!", "'", "\\", "@", "#", "^", "$", "+", "%", "}", "&", "{"
#' ); vsSplitVector_   # <- Placing the variable here (after ";") merely prints it to the R Console.
#' 
#'
#' ### Then execute the "Vector-Splitting" Function ...
#' rlsResSplit <- code.split.string.on.vector(   # <- Runs the actual String Split process ...
#'   ssSplitString = ssSplitString_,
#'   vsSplitVector = vsSplitVector_, 
#' )
#' 
#' 
#' ### Finally -> view the results of the "Vector-Splitting" Function ...
#' rlsResSplit$SPLITS   # -> Returns the vector of String Splits (i.e. all parts of split string). 
#' rlsResSplit$STRING   # -> Returns a clean string version of the String Splits (splits combined).
#'
#'
#' @export
#? ### ### ###
"code.split.string.on.vector" <- function(
  ssSplitString=NULL, vsSplitVector=NULL, sbExactSplits=FALSE
) {
  
  
  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();                # <- Captures <active> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "MFMRutils";                     # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "Split.On.Vector";               # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_LONG_   <- "code.split.string.on.vector";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseLIST     <- base::list;
  rasBaseNAMES    <- base::names;
  rasBaseNCHAR    <- base::nchar;
  rasBaseLENGTH   <- base::length;
  rasBasePASTE0   <- base::paste0;
  rasBaseRETURN   <- base::return;
  rasBaseStrSPLIT <- base::strsplit;
  
  `%??%`             <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasMfmrRetRenvLIST <- MFMRutils::code.return.renv.locked.list;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rsbExactSplits_ <- sbExactSplits;
  rvsSplitVector_ <- vsSplitVector %??% c(" ");
  rssSplitString_ <- ssSplitString %??% "NULL-ERROR -> You DID NOT Provide a SPLIT STRING Value !!!"
  
  
  
  ####   STEP 04 - Execute MAIN <function> CODE LOGIC   ####
  ####   4.1 - Prime Local <function> Variables & Constants   ####
  rvsVectOUT_ <- c();                # <- Vector for capturing FINAL <output> values (results) ...
  rvsSplitStringUnits_ <- c();       # <- Vector for capturing String Split Units (parts) ...
  rssMutagenSTR_ <- "";              # <- The "Hot POTATOE" String (for String Mutagenesis) !!!
  RCT_FUNC_DELIM_ <- "   ";          # <- A special delimiter needed by this function (only) !!!
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
        x = rssSplitString_, split = rcsSplitOBJ_, fixed = rsbExactSplits_
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
          rvsVectOUT_,   # <- Take existing vector (contents) and then ...
          rvsUnit        # <- Add only VALID Split Units to Vector (i.e. grow vector as needed) !!!
        );   
      }
    }
    
    ### 4.3.1c - VERY IMPORTANT -> collapse Split Units into the mutagenesis <local> String ...
    rssMutagenSTR_ <- rasBasePASTE0(
      rvsSplitStringUnits_[[1]], collapse = RCT_FUNC_DELIM_
    );
    
  }
  
  
  ####   4.3 - Clean-up String Split Process   ####
  rvsCleanUpFINAL_ <- rasBaseStrSPLIT(
    x = rssMutagenSTR_, split = RCT_FUNC_DELIM_, fixed = rsbExactSplits_
  );
  
  rvsVectOUT_ <- NULL;   # <- Reset the VectorOUT back to a ZERO length (empty state) !!!
  for (rvsUnitFIN in rvsCleanUpFINAL_[[1]]) {
    if (rasBaseNCHAR(rvsUnitFIN) >= 1) {
      rvsVectOUT_ <- c(
        rvsVectOUT_,      # <- Take existing vector (contents) and then ...
        rvsUnitFIN        # <- Add only VALID Split Units to Vector (i.e. grow vector as needed).
      );   
    }
  }
  
  
  
  ####   STEP 05 - Return RESULT to Function CALL   ####
  rasBaseRETURN(
    rasBaseLIST(
      "SPLITS" = rvsVectOUT_,         # <- The String Splits (parts of the split string) ...
      "STRING" = rasBasePASTE0(
        rvsVectOUT_, collapse = " "   # <- Compiles a clean String from the split parts ...
      )
    )
  );
  
}


