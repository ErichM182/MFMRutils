#? ### ### ### ### ### ### ###
#' @title Compile Environment Locked R Lists
#' @name code.return.env.locked.list
#' 
#' @description
#' A tiny R function that extracts relevant package information from <internal> 
#' R Library Attributes.
#'
#' @param vsListNames a vector of characters (strings) that captures the "names" of the environment
#'                    locked list. The "vsListNames" vector length must match "lsListVals" length.
#' @param lsListVals a list of R Objects that contains the "values" of the environment locked list.
#'                    The "lsListVals" list length must match the "vsListNames" vector length.
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
#' rlsEnvLockdLIST <- code.return.env.locked.list(
#'   vsListNames = vsListNames_, vsListVals = vsListVals_, sbLockList = TRUE
#' )   ### <- Extracts Library Information ... 
#' 
#' rlsEnvLockdLIST$VAR_X        # -> Returns the value FALSE !!!
#' rlsEnvLockdLIST[["VAR_C"]]   # -> Returns the value 'R-Object for VAR_C' !!! 
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
  rasBasePASTE           <- base::paste;
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
  
  
  # 1. Input Validation ...
  if (rasBaseLENGTH(vsListNames_) != rasBaseLENGTH(lsListVals_)) {
    rasBaseSTOP(
      "The length of 'vsListNames' (", rasBaseLENGTH(vsListNames_), ") ",
      "must match the length of 'lsListVals' (", rasBaseLENGTH(vsListNames_), ")."
    );
  }
  
  if (!rasBaseIsCHARACTER(vsListNames_)) {
    rasBaseSTOP("The 'vsListNames' function argument must be a character vector (character) !!!");
  }
  
  if (!rasBaseIsLIST(lsListVals_)) {
    rasBaseSTOP("The 'lsListVals' function argument must be an R List Object (list) !!!");
  }
  
  if (rasBaseANY(rasBaseDUPLICATED(vsListNames_))) {
    vsDuplicateNames_ <- vsListNames_[rasBaseDUPLICATED(vsListNames_)]
    rasBaseSTOP(
      "Duplicate name values found in 'vsListNames' function argument: ", 
      rasBasePASTE(unique(vsDuplicateNames_), collapse = ", ")
    );
  }
  
  if (rasBaseANY(vsListNames_ == "")) {
    rasBaseSTOP(
      "Empty name values are not allowed (empty values found in 'vsListNames' argument) !!!"
    );
  }
  
  # 2. Combine vectors into a standard R list and convert to an environment
  # setNames() applies the character vector as names to the list of values.
  rcoDataList_ <- rasStatsSetNAMES(lsListVals_, vsListNames_);
  rcoLockedENV_ <- rasBaseList2ENV(rcoDataList_);
  
  # 3. Lock the environment structure
  # This prevents adding or removing elements (bindings) from the environment.
  rasBaseLockENVIRONMENT(rcoLockedENV_);
  
  # 4. Optional: Lock individual bindings (values)
  # This prevents changing the value of existing elements.
  if (sbLockList_) {
    for (ssName in vsListNames_) {
      rasBaseLockBINDING(ssName, rcoLockedENV_)
    }
  }
  
  rasBaseRETURN(rcoLockedENV_);
  
}
