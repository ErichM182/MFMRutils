#? ### ### ### ### ### ### ###
#' @title Locate Text or Regex Strings in R Functions
#' @name devs.find.text.instances
#' 
#' 
#' @description
#' Easily search and find text or regular expression (regex) snippets in any R Function.
#'
#'
#' @param csFindText ([character]) A string value containing the text or regular expression (regex) 
#'                   value or pattern to match against (search for) in the R Function code base.
#' @param ssSearchFunc ([character]) A string value containing the function name identifier of the
#'                     R Function where the search should be conducted or targeted at. This is an 
#'                     optional argument - which means the function will search all functions within 
#'                     the active Global R Environment ([.GlobalEnv]) if this function argument is 
#'                     set to a value of [NULL].
#' @param coRENV ([environment]) An R Object argument that defines the scope (i.e. R Environment) 
#'               where the text/regex search should be conducted (default: Global R Environment).
#' @param sbRetFullMatch ([logical]) A boolean function that defines whether the full matching line 
#'                       (i.e. text/string or code snippet) should be returned as a function output.
#'                       If set to [FALSE] then only the text/regex provided to the function via 
#'                       the `csFindText` function argument will be returned. If set to [TRUE] then
#'                       all <contiguous> text on the matching <code> line will be returned.
#'
#'
#' @return 
#' This function returns a Data Frame that contains the following variables <columns>:
#' * FUNC_NAME   -> a [character] identifier of the R Function where the matching regex was found.   
#' * LINE_NUMBER -> an [integer] variable denoting the R Function code line number where the 
#'                  matching text or regex value was found.   
#' * CODE_SNIP   -> a [character] variable that captures a code snippet from the matching line where 
#'                  the text or regex value was found.
#' * FILE_NAME   -> a [character] variable that captures the <parent> file identifier (i.e. name) of 
#'                  the R Function where the matching text or regex value was found.
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
"devs.find.text.instances" <- function(
    csFindText="unc_this", ssSearchFunc=NULL, coRENV=.GlobalEnv, sbRetFullMatch=TRUE
) {
  
  
  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();             # <- Captures <active> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "MFMRutils";                  # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "Find.Text.INSTs";            # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_LONG_   <- "devs.find.text.instances";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseLS         <- base::ls;
  rasBaseGET        <- base::get;
  rasBaseATTR       <- base::attr;
  rasBaseGREP       <- base::grep;
  rasBaseRBIND      <- base::rbind;
  rasBasePRINT      <- base::print;
  rasBaseLENGTH     <- base::length;
  rasBaseRETURN     <- base::return;
  rasBaseIsNULL     <- base::is.null;
  rasBaseINTEGER    <- base::integer;
  rasBaseREGEXPR    <- base::regexpr;
  rasBaseTryCATCH   <- base::tryCatch;
  rasBaseCHARACTER  <- base::character;
  rasBaseDataFRAME  <- base::data.frame;
  rasBaseIsFUNCTION <- base::is.function;
  
  rasBaseCaptureOUTPUT <- utils::capture.output;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rcoRENV_         <- coRENV;
  rcsFindText_     <- csFindText;
  rssSearchFunc_   <- ssSearchFunc;
  rsbRetFullMatch_ <- sbRetFullMatch;
  
  
  
  ####   STEP 04 - Get ALL R Objects   ####
  ## Extract all R Objects in the active <current> R Environment ...
  rcoRENV_OBJs_ <- rasBaseLS(envir = rcoRENV_);
  rdfOUT_ <- rasBaseDataFRAME(
    "FUNC_NAME"      = rasBaseCHARACTER(),
    "LINE_NUMBER"    = rasBaseINTEGER(),
    "CODE_SNIP"      = rasBaseCHARACTER(),
    "FILE_NAME"      = rasBaseCHARACTER(),
    stringsAsFactors = FALSE
  );
  
  
  
  ####   STEP 05 - Run Text Search Accordingly   ####
  for (rcoOBJ in rcoRENV_OBJs_) {
    
    # Skip if looking for specific function and this isn't it ...
    if (!rasBaseIsNULL(rssSearchFunc_) && rcoOBJ != rssSearchFunc_) next # <- Leave 'next' as is !!!
    
    rcoRENV_ObjVal_ <- rasBaseGET(rcoOBJ, envir = rcoRENV_);
    
    # Confirm that it is a function ...
    if (rasBaseIsFUNCTION(rcoRENV_ObjVal_)) {
      
      # Extract the function body as a character vector ...
      rcoFuncBody_ <- rasBaseCaptureOUTPUT(rasBasePRINT(rcoRENV_ObjVal_))
      
      # Find lines matching the `csFindText` value ...
      rvsMatchingLines_ <- rasBaseGREP(rcsFindText_, rcoFuncBody_, value = FALSE)
      
      if (rasBaseLENGTH(rvsMatchingLines_) > 0) {
        for (rsnLineNum in rvsMatchingLines_) {
          rssCodeSNIP_ <- if (rsbRetFullMatch_) {
            # Extract entire <contiguous> line of text linked to the matching part ...
            rcoFuncBody_[rsnLineNum]
          } else {
            # Extract just the matching part <snippet or search term> ...
            regmatches(
              rcoFuncBody_[rsnLineNum], 
              rasBaseREGEXPR(
                rcsFindText_, rcoFuncBody_[rsnLineNum]
              )
            );
          }
          
          # Try to get source file information
          rssFileSRC_ <- rasBaseTryCATCH(
            {
              rasBaseATTR(rcoRENV_ObjVal_, "srcref")[[1]]$srcfile$filename
            }, error = function(e) NA
          );
          
          rdfOUT_ <- rasBaseRBIND(
            rdfOUT_, rasBaseDataFRAME(
              "FUNC_NAME"      = rcoOBJ,
              "LINE_NUMBER"    = rsnLineNum,
              "CODE_SNIP"      = rssCodeSNIP_,
              "FILE_NAME"      = rssFileSRC_,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }
  
  
  
  ####   STEP 06 - Return Function Results <to func-call>   ####
  rasBaseRETURN(rdfOUT_);
  
}


