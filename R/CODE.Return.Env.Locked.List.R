#? ### ### ### ### ### ### ###
#' @title Compile Environment Locked R Lists
#' @name code.return.env.locked.list
#' 
#' @description
#' A relatively small R function that compiles and returns R Environment Locked lists.
#'
#' @param vsListNames a vector of characters (strings) that captures the "names" of the environment
#'                    locked list. The "vsListNames" function argument must match the "lsListVals" 
#'                    function argument in terms of length (i.e. number of vector elements), as well 
#'                    as the required (correct) NAME-VALUE pairing (i.e. NAME-VALUE pairs).
#' @param lsListVals a list of R Objects that contains the "values" of the environment locked list.
#'                   The "lsListVals" function argument must match the "vsListNames" function
#'                   argument in terms of length (i.e. number of vector elements), as well as the
#'                   required (correct) NAME-VALUE pairing (i.e. NAME-VALUE pairs).
#' @param sbLockList a logical (boolean) value that denotes whether the returned <output> list
#'                   should be R environment locked <Default == FALSE>. If TRUE, individual list
#'                   bindings (values) are locked, preventing modification of existing list elements.
#'                   
#' @return 
#' * This function returns a locked environment R Object.
#'
#' @examples
#' ### Activate the "MFMRutils" R Library (if previously installed) ...
#' library(MFMRutils)   ### -> Loads the "MFMRutils" library !!!
#' 
#' ### Compile the R Locked List Information (for active R Library <project>) ...
#' vsListNames_ <- c("VAR_A", "VAR_B", "VAR_C", "VAR_X", "VAR_Y", "VAR_Z", "VAR_G")
#' vsListVals_  <- list(
#'   1982, "Value for VAR_B", "R-Object for VAR_C", FALSE, 
#'   "Value for VAR_Y", TRUE, "R-List for VAR_G"
#' )
#' 
#' 
#' 
#' ### Function-use OPTION 1 (main purpose) -> Create Immutable R List Objects ... 
#' rlsEnvLockdLIST <- code.return.env.locked.list(
#'   vsListNames = vsListNames_, lsListVals = vsListVals_, 
#'   sbLockList = TRUE   # <- Set to 'TRUE' to create an immutable (environment locked) R List !!!
#' )
#' 
#' rlsEnvLockdLIST$VAR_X        # -> Returns the value FALSE !!!
#' rlsEnvLockdLIST[["VAR_C"]]   # -> Returns the value 'R-Object for VAR_C' !!! 
#' rlsEnvLockdLIST$VAR_G        # -> Returns the value 'R-List for VAR_G' !!! 
#' 
#' ## Immutability test (OPTION 1 test) ...
#' rlsEnvLockdLIST$VAR_G <- "A NEW value for 'VAR_G' !!!"   # -> Will trigger an error !!!
#' 
#' 
#'  
#' ### Function-use OPTION 2 (secondary purpose) -> Create Mutable R List Objects ... 
#' rlsEnvLockdLIST <- code.return.env.locked.list(
#'   vsListNames = vsListNames_, lsListVals = vsListVals_, 
#'   sbLockList = FALSE   # <- Set to 'FALSE' to create a mutable R List !!!
#' )
#' 
#' ## Mutability test (OPTION 2 test) ...
#' rlsEnvLockdLIST$VAR_G <- "A NEW value for 'VAR_G' !!!"   # -> Assigns a new value to the "VAR_G"
#'                                                          #    element (name) of the R list.
#'
#' @export
#? ### ### ###
"code.return.env.locked.list" <- function(vsListNames, lsListVals, sbLockList=FALSE) {
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_ID_SHRT_ <- "Return.Lock.List";              # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "CODE.Return.Env.Locked.List";   # <- Function ID - LONG !!!
  RCT_TAG_FUNC_LIBR_ID_ <- MFMRutils::devs.pull.libr.info()[["NAME"]];
  
  
  ####   STEP 02 - Prime NB "Aliases" used locally (inside function)   ####
  rasBaseANY             <- base::any;
  rasBaseSTOP            <- base::stop;
  rasBaseLENGTH          <- base::length;
  rasBaseRETURN          <- base::return;
  rasBasePASTE0          <- base::paste0;
  rasBaseIsLIST          <- base::is.list;
  rasBaseList2ENV        <- base::list2env;
  rasStatsSetNAMES       <- stats::setNames;
  rasBaseDUPLICATED      <- base::duplicated;
  rasBaseLockBINDING     <- base::lockBinding;
  rasBaseIsCHARACTER     <- base::is.character;
  rasBaseLockENVIRONMENT <- base::lockEnvironment;
  
  
  ####   STEP 03 - Internalize Function Arguments   ####
  sbLockList_ <- sbLockList;
  lsListVals_ <- lsListVals;
  vsListNames_ <- vsListNames;
  
  
  ####   STEP 04 - Run Input Arguments Validation   ####
  if (rasBaseLENGTH(vsListNames_) != rasBaseLENGTH(lsListVals_)) {
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
  
  
  ####   STEP 05 - Ensure List Names are CLEAN (not blanks)   ####
  if (rasBaseANY(vsListNames_ == "") || rasBaseANY(vsListNames_ == " ") || 
      rasBaseANY(vsListNames_ == "  ") || rasBaseANY(vsListNames_ == "   ")) {
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
  base::class(rcoLockedENV_) <- "ENV-LOCKED-LIST";   # <- Assign a Class Identifier !!!
  rasBaseRETURN(rcoLockedENV_);
  
}
