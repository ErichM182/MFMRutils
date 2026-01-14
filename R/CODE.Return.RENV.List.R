#? ### ### ### ### ### ### ###
#' @title Compile R-Environment Locked Lists
#' @name code.return.renv.list
#' @family CODE Functions (SuiteMFMR)
#' 
#' 
#' @description
#' A relatively small R function that compiles and returns R Environment Locked lists.
#'
#'
#' @param vsListNames ([vector] of [character]s) a Vector of String or Text ([character]) values 
#'                    that contain the "names" or "keys" of the R Environment Locked List. VERY NB:
#'                    The `vsListNames` function argument must match the `lsListVals` function 
#'                    argument in terms of length (i.e. number of vector elements) and order (i.e. 
#'                    sequence in accordance to the required NAME-VALUE or Key-Value pairing).
#' @param lsListVals ([list]) a List of R Objects that capture the "values" of the R Environment 
#'                   Locked List. VERY NB: The `lsListVals` function argument must match the 
#'                   `vsListNames` function argument in terms of length (i.e. number of vector 
#'                   elements) and order (i.e. sequence in accordance to the required NAME-VALUE or 
#'                   Key-Value pairing).
#' @param sbLockList ([logical]) a Boolean value that defines whether the returned <output> list
#'                   should be R Environment Locked or not (default: `FALSE`). If TRUE, individual 
#'                   list bindings (i.e. Key-Value Pairs) are locked, preventing modification of 
#'                   existing list elements -> making the returned <output> list immutable.
#' @param sbSetClass ([logical]) a Boolean value that denotes whether the class of the returned 
#'                   <output> list should be left as the default R type (i.e. [environment]) or 
#'                   not (default: `FALSE`). If set to `TRUE` the class of the output list will be 
#'                   set to the value of the `ssClassValue` function argument. 
#' @param ssClassValue ([character]) a String or Text (i.e. [character] [vector]) value that 
#'                     captures the <custom> class value of the returned <output> R Environment 
#'                     Locked List.
#' @param ... ([list]) a List of R Objects that captures the "Fall-Through" or "DotsArgs" values for
#'            functions that are nested within the [code.return.renv.list()] function. The [...]
#'            argument here has specific relevance (reference) to the following nested functions:<br>
#'               1. [MFMRutils::info.post.func.self.id()] , <br>
#'               2. [MFMRutils::info.post.note()] , and <br>
#'               3. [MFMRutils::code.get.celn()]
#'                   
#'                   
#' @return 
#' * This function returns a locked environment R Object.
#'
#'
#' @examples
#' ### Activate the "MFMRutils" R Library (if previously installed) ...
#' library(MFMRutils)   ### -> Loads the "MFMRutils" library !!!
#' 
#' ### Compile the R Locked List Information (for active R Library <project>) ...
#' vsListNames_ <- c("VAR_A", "VAR_B", "VAR_C", "VAR_X", "VAR_Y", "VAR_Z", "VAR_G")
#' lsListVals_  <- list(
#'   1982, "Value for VAR_B", "R-Object for VAR_C", FALSE, 
#'   "Value for VAR_Y", TRUE, "R-List for VAR_G"
#' )
#' 
#' 
#' 
#' ### Function-use OPTION 1 (main purpose) -> Create Immutable R List Objects ... 
#' rlsEnvLockdLIST <- code.return.renv.list(
#'   vsListNames = vsListNames_, lsListVals = lsListVals_, 
#'   sbLockList = TRUE   # <- Set to 'TRUE' to create an immutable (environment locked) R List !!!
#' )
#' 
#' rlsEnvLockdLIST$VAR_X        # -> Returns the value FALSE !!!
#' rlsEnvLockdLIST[["VAR_C"]]   # -> Returns the value 'R-Object for VAR_C' !!! 
#' rlsEnvLockdLIST$VAR_G        # -> Returns the value 'R-List for VAR_G' !!! 
#' 
#' \dontrun{   ### <- Code below does an "immutability" check (to ensure the custom function works
#'             ###    as intended) -> but this code creates a variable write error under the normal
#'             ###    "R_CMD_CHECK" scenario and as such should not be run during normal R library
#'             ###    development code check procedures.
#' ## Immutability test (OPTION 1 test) ...
#' rlsEnvLockdLIST$VAR_G <- "A NEW value for 'VAR_G' !!!"   # -> Will trigger an error !!!
#' 
#' }
#' 
#' 
#'  
#' ### Function-use OPTION 2 (secondary purpose) -> Create Mutable R List Objects ... 
#' rlsEnvLockdLIST <- code.return.renv.list(
#'   vsListNames = vsListNames_, lsListVals = lsListVals_, 
#'   sbLockList = FALSE   # <- Set to 'FALSE' to create a mutable R List !!!
#' )
#' 
#' ## Mutability test (OPTION 2 test) ...
#' rlsEnvLockdLIST$VAR_G <- "A NEW value for 'VAR_G' !!!"   # -> Assigns a new value to the "VAR_G"
#'                                                          #    element (name) of the R list.
#'
#'
#' @export
#? ### ### ###
"code.return.renv.list" <- function(
  vsListNames=NULL, lsListVals=NULL, sbLockList=FALSE, sbSetClass=FALSE, 
  ssClassValue="RENV-LOCKD-LIST", ...
) {
  
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_DBL_SYS_TIME_NOW_    <- base::Sys.time();          # <- Extract <active> System Date-Time !!!
  RCT_TAG_R_LIBR_ID_       <- "MFMRutils";               # <- R Library Identifier !!!
  RCT_TAG_R_FUNC_ID_SHORT_ <- "Ret.RENV.List";           # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_    <- "CODE.Return.RENV.List";   # <- Function ID - LONG !!! 
  
  RCT_INT_CELN_START_ <- 91L;    # <- The Code Editor Line Number (CELN) at which the function 
                                 #    OPENING <normal> brace/bracket "(" is located !!!
  RCT_INT_CELN_STOP_  <- 234L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    CLOSING <curly> brace/bracket "}" is located !!!
  coDotsArgs_ <- base::list(...);   # <- Capture all the "DotsArgs" values here !!!
  
  
  
  ###   Run Function Self-ID (as required)   ###
  MFMRutils::info.post.func.self.id(
    ssFuncSelfID = RCT_TAG_FUNC_ID_FULL_, siFuncMode01L = 1L, csTimeStart = RCT_DBL_SYS_TIME_NOW_,
    siStartCELN = RCT_INT_CELN_START_, siStopCELN = RCT_INT_CELN_STOP_, ...
  );
  
  
  
  ####   STEP 02 - Prime NB "Aliases" used locally (inside function)   ####
  rasBaseANY             <- base::any;
  rasBaseLIST            <- base::list;
  rasBaseSTOP            <- base::stop;
  rasBaseLENGTH          <- base::length;
  rasBaseRETURN          <- base::return;
  rasBasePASTE0          <- base::paste0;
  rasBaseIsLIST          <- base::is.list;
  rasBaseIsNULL          <- base::is.null;
  rasBaseList2ENV        <- base::list2env;
  rasBaseDUPLICATED      <- base::duplicated;
  rasBaseLockBINDING     <- base::lockBinding;
  rasBaseIsCHARACTER     <- base::is.character;
  rasBaseLockENVIRONMENT <- base::lockEnvironment;
  
  rasStatsSetNAMES <- stats::setNames;
  
  rasMfmrGetCELN  <- MFMRutils::code.get.celn;
  rasMfmrPostNOTE <- MFMRutils::info.post.note;
  
  ## Prime special values here ...
  ssFindCodeALIAS_ <- "rasMfmrPostNOTE\\(";
  sbShowTAIL_      <- FALSE;
  rasMfmrPostNOTE(
    ssHead = RCT_TAG_R_FUNC_ID_SHORT_, sbShowTail = sbShowTAIL_,
    ssBody = "Succesfully Primed Function Aliases ...",
    siCallCELN = rasMfmrGetCELN(
      ssFuncName = RCT_TAG_FUNC_ID_FULL_, 
      sbUseFuncAlias = TRUE, ssFuncAliasValue = ssFindCodeALIAS_,
      siCallIndex = 1L, sbRunByForce = coDotsArgs_[["sbRunByForce"]]
    )
  );
  
  
  
  ####   STEP 03 - Internalize Function Arguments   ####
  sbLockList_   <- sbLockList;
  lsListVals_   <- lsListVals;
  vsListNames_  <- vsListNames;
  sbSetClass_   <- sbSetClass;
  ssClassValue_ <- ssClassValue;
  rasMfmrPostNOTE(
    ssHead = RCT_TAG_R_FUNC_ID_SHORT_, sbShowTail = sbShowTAIL_,
    ssBody = "Internalized ALL Function Arguments ...", 
    siCallCELN = rasMfmrGetCELN(
      ssFuncName = RCT_TAG_FUNC_ID_FULL_, 
      sbUseFuncAlias = TRUE, ssFuncAliasValue = ssFindCodeALIAS_,
      siCallIndex = 2L, sbRunByForce = coDotsArgs_[["sbRunByForce"]]
    )
  );
  
  
  
  ####   STEP 04 - Run Input Arguments Validation   ####
  if (rasBaseIsNULL(vsListNames_) || rasBaseIsNULL(lsListVals_) ||
      rasBaseLENGTH(vsListNames_) != rasBaseLENGTH(lsListVals_)) {
    rasBaseSTOP(
      " -> Function NAMES vs. VALUES LENGTH MISS-MATCH !!!", "\n",
      " -> The length of 'vsListNames' (", rasBaseLENGTH(vsListNames_), ") ",
      "must match the length of 'lsListVals' (", rasBaseLENGTH(lsListVals_), ") !!!."
    );
  }
  
  if (!rasBaseIsCHARACTER(vsListNames_)) {
    rasBaseSTOP(
      " -> The 'vsListNames' function argument must be a character vector (character) !!!"
    );
  }
  
  if (!rasBaseIsLIST(lsListVals_)) {
    rasBaseSTOP(" -> The 'lsListVals' function argument must be an R List Object (list) !!!");
  }
  
  if (rasBaseANY(rasBaseDUPLICATED(vsListNames_))) {
    vsDuplicateNames_ <- vsListNames_[rasBaseDUPLICATED(vsListNames_)]
    rasBaseSTOP(
      " -> Duplicate name values found in 'vsListNames' function argument: ", 
      rasBasePASTE0(
        '[ ',
        rasBasePASTE0('"', unique(vsDuplicateNames_), collapse = '", '),
        '" ]'
      )
    );
  }
  rasMfmrPostNOTE(
    ssHead = RCT_TAG_R_FUNC_ID_SHORT_, sbShowTail = sbShowTAIL_,
    ssBody = "Completed Function Argument NULL-Checks ...",
    siCallCELN = rasMfmrGetCELN(
      ssFuncName = RCT_TAG_FUNC_ID_FULL_, 
      sbUseFuncAlias = TRUE, ssFuncAliasValue = ssFindCodeALIAS_,
      siCallIndex = 3L, sbRunByForce = coDotsArgs_[["sbRunByForce"]]
    )
  );
  
  
  
  ####   STEP 05 - Ensure List Names are CLEAN (not blanks)   ####
  if (rasBaseIsNULL(vsListNames_) || rasBaseANY(vsListNames_ == "") || 
      rasBaseANY(vsListNames_ == " ") || rasBaseANY(vsListNames_ == "  ") || 
      rasBaseANY(vsListNames_ == "   ")) {
    rasBaseSTOP(
      " -> Empty name values are not allowed (empty values found in 'vsListNames' argument) !!!"
    );
  }
  
  
  ####   STEP 06 - Combine Vectors into a List   ####
  # -> Combine vectors into a standard R list and convert to an R Environment Object. Here the
  #    "setNames()" function applies the character vector as names to the list of values.
  rcoDataList_ <- rasStatsSetNAMES(lsListVals_, vsListNames_);
  rcoLockedENV_ <- rasBaseList2ENV(rcoDataList_);
  
  
  ####   STEP 07 - Combine Vectors into a List   ####
  # -> Lock the environment structure ... this prevents adding or removing elements (bindings) 
  #    to or from (respectively) the environment object (i.e. list) post-creation !!!
  rasBaseLockENVIRONMENT(rcoLockedENV_);
  
  
  ####   STEP 08 - Lock List Bindings (Key-Val Pairs)   ####
  # -> Optional: Lock individual bindings (values) ... this prevents changing the value of 
  #    existing elements !!!
  if (sbLockList_) {
    for (ssName in vsListNames_) {
      rasBaseLockBINDING(ssName, rcoLockedENV_)
    }
  }
  
  
  ####   STEP 09 - Return List to Function Call   ####
  if (sbSetClass_) {   # <- If TRUE the function assigns a customized Class Identifier to list ...
    base::class(rcoLockedENV_) <- ssClassValue_;   # <- Assigns the <specified> Custom Class ID !!!
  }
  rasMfmrPostNOTE(
    ssHead = RCT_TAG_R_FUNC_ID_SHORT_, sbShowTail = sbShowTAIL_,
    ssBody = "Finalized Function Results <outputs> ...",
    siCallCELN = rasMfmrGetCELN(
      ssFuncName = RCT_TAG_FUNC_ID_FULL_, 
      sbUseFuncAlias = TRUE, ssFuncAliasValue = ssFindCodeALIAS_,
      siCallIndex = 4L, sbRunByForce = coDotsArgs_[["sbRunByForce"]]
    )
  );
  
  
  
  ###   Run Function Self-ID (as required)   ### 
  MFMRutils::info.post.func.self.id(
    ssFuncSelfID = RCT_TAG_FUNC_ID_FULL_, siFuncMode01L = 0L, csTimeStart = RCT_DBL_SYS_TIME_NOW_,
    siStartCELN = RCT_INT_CELN_START_, siStopCELN = RCT_INT_CELN_STOP_, 
  );
  
  
  
  rasBaseRETURN(rcoLockedENV_);   # <- Return FINAL result to Function Call ...
  
}
