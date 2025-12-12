#? ### ### ### ### ### ### ###
#' @title R Library Version Number Updater ("SuiteMFMR" DevTools)
#' @name devs.patch.libr.vers.number
#' 
#' @description
#' A Helper Function that updates (patches) the R Library Version Numbers accordingly (i.e. in 
#' terms of the DEVELOPMENT <debug> and PRODUCTION <alpha, beta & stable> code base releases). The 
#' function keeps track of the developmental progress of the code base and accurately updates the
#' <appropriate> version numbers as code is committed and upgraded on the developmental cycle.
#'
#' @param rvsVersNumVect a vector of characters (strings) that defines the <regent> R Library's
#'                       active development (or production) code release status (version number).
#'                    
#' @returns
#' * This function returns an environment locked list containing the updated version numbers for 
#'   the relevant development and production release code commits <debug, alpha, beta & stable>.
#'
#' @examples
#' ### Run R Package Version Updates easily as follows ...
#' library(MFMRutils)   # <- Loads "MFMRutils" library (if already installed) !!!
#'
#' ### Run 2 different types of code check/validation processes ...
#' rvsVersNumVect_ <- c("1", "2", "8", "999")
#' rlsLibrVers <- devs.patch.libr.vers.number(rvsVersNumVect_)   # -> Patches (updates) the library
#'                                                               #    <code commit> version number
#'                                                               #    accordingly ...
#'                                                               
#' rlsLibrVers$VERS_DEBUG    # <- Updated the <debug release> version number accordingly !!!
#' rlsLibrVers$VERS_ALPHA    # <- Updated the <alpha release> version number accordingly !!!
#' rlsLibrVers$VERS_BETA     # <- Updated the <beta release> version number accordingly !!!
#' rlsLibrVers$VERS_STABLE   # <- Updated the <stable release> version number accordingly !!!
#'
#' @keywords internal
#' @noRd
#? ### ### ###
"devs.patch.libr.vers.number" <- function(rvsVersNumVect=c("0", "0", "0", "001")) {
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";                     # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHRT_ <- "Patch.Libr.Vers";               # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Patch.Libr.Vers.Number";   # <- Function ID - LONG !!!
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve R Session Memory Efficiency ...
  rasBaseLIST             <- base::list;
  rasBaseLENGTH           <- base::length;
  rasBaseRETURN           <- base::return;
  rasBaseAsNUMERIC        <- base::as.numeric;
  rasMfmrReturnLockedLIST <- MFMRutils::code.return.env.locked.list;
  
  
  ####   STEP 03 - Internalize Function Arguments   ####
  rvsVersNumVect_ <- rvsVersNumVect;
  
  
  ####   STEP 04 - Initialize NB Variables & Constants   ####
  rsnVersDevsDEBUG_ <- NULL; rsnVersProdBETA_   <- NULL;
  rsnVersProdALPHA_ <- NULL; rsnVersProdSTABLE_ <- NULL;
  
  
  ####   STEP 05 - Patch Version Number (for PRODUCTION Builds)   ####
  if (rasBaseLENGTH(rvsVersNumVect_) == 3) {
    
    ####   STEP 5.1 - Extract ALL Version Stubs   ####
    rsnVersDevsDEBUG_  <- 0;  # <- The "DEBUG" Version Stub was not provided - so set it to ZERO !!!
    rsnVersProdALPHA_  <- rasBaseAsNUMERIC(rvsVersNumVect_[3]);
    rsnVersProdBETA_   <- rasBaseAsNUMERIC(rvsVersNumVect_[2]);
    rsnVersProdSTABLE_ <- rasBaseAsNUMERIC(rvsVersNumVect_[1]);
    
    
    ####   STEP 5.2 - Update the "ALPHA" Version Stub (always)   ####
    rsnVersProdALPHA_ <- rsnVersProdALPHA_ + 1;
    if (rsnVersProdALPHA_ >= 10) {   # <- The MAX Level for "PRODUCTION" stubs is 9 !!!
      
      ####   STEP 5.2.1a - Reset the "ALPHA" Version Stub to ZERO   ####
      rsnVersProdALPHA_ <- 0;
      
      ####   STEP 5.2.1b - Increment the "BETA" Version Stub accordingly   ####
      rsnVersProdBETA_ <- rsnVersProdBETA_ + 1;
      
    }
    
    
    ####   STEP 5.3 - Update the "BETA" Version Stub (as needed)   ####
    if (rsnVersProdBETA_ >= 10) {   # <- The MAX Level for "PRODUCTION" stubs is 9 !!!
      
      ####   STEP 5.3.1a - Reset the "BETA" Version Stub to ZERO   ####
      rsnVersProdBETA_ <- 0;
      
      ####   STEP 5.3.1b - Increment the "STABLE" Version Stub accordingly   ####
      rsnVersProdSTABLE_ <- rsnVersProdSTABLE_ + 1;
      
    }
    
  }
  
  
  ####   STEP 06 - Patch Version Number (for ACTIVE DEVELOPMENT <debug> Builds)   ####
  if (rasBaseLENGTH(rvsVersNumVect_) == 4) {
    
    ####   STEP 6.1 - Extract ALL Version Stubs   ####
    rsnVersDevsDEBUG_  <- rasBaseAsNUMERIC(rvsVersNumVect_[4]);
    rsnVersProdALPHA_  <- rasBaseAsNUMERIC(rvsVersNumVect_[3]);
    rsnVersProdBETA_   <- rasBaseAsNUMERIC(rvsVersNumVect_[2]);
    rsnVersProdSTABLE_ <- rasBaseAsNUMERIC(rvsVersNumVect_[1]);
    
    
    ####   STEP 6.2 - Update the "DEBUG" Version Stub (always)   ####
    rsnVersDevsDEBUG_ <- rsnVersDevsDEBUG_ + 1;
    if (rsnVersDevsDEBUG_ >= 1000) {   # <- The MAX Level for "DEBUG" stubs is 999 !!!
      
      ####   STEP 6.2.1a - Reset the "DEBUG" Version Stub to ZERO   ####
      rsnVersDevsDEBUG_ <- 0;
      
      ####   STEP 6.2.1b - Increment the "ALPHA" Version Stub accordingly   ####
      rsnVersProdALPHA_ <- rsnVersProdALPHA_ + 1;
      
    }
    
    
    ####   STEP 6.3 - Update the "ALPHA" Version Stub (as needed)   ####
    if (rsnVersProdALPHA_ >= 10) {   # <- The MAX Level for "PRODUCTION" stubs is 9 !!!
      
      ####   STEP 6.3.1a - Reset the "ALPHA" Version Stub to ZERO   ####
      rsnVersProdALPHA_ <- 0;
      
      ####   STEP 6.3.1b - Increment the "BETA" Version Stub accordingly   ####
      rsnVersProdBETA_ <- rsnVersProdBETA_ + 1;
      
    }
    
    
    ####   STEP 6.4 - Update the "BETA" Version Stub (as needed)   ####
    if (rsnVersProdBETA_ >= 10) {   # <- The MAX Level for "PRODUCTION" stubs is 9 !!!
      
      ####   STEP 6.4.1a - Reset the "BETA" Version Stub to ZERO   ####
      rsnVersProdBETA_ <- 0;
      
      ####   STEP 6.4.1b - Increment the "STABLE" Version Stub accordingly   ####
      rsnVersProdSTABLE_ <- rsnVersProdSTABLE_ + 1;
      
    }
    
  }
  
  
  ####   STEP 07 - Return LIST to Function Call   ####
  rasMfmrReturnLockedLIST(
    vsListNames = c(
      "VERS_DEBUG",   # <- The ACTIVE DEVELOPMENT <debug release> VERSION STUB !!!
      "VERS_ALPHA",   # <- The ALPHA PRODUCTION <alpha release> VERSION STUB !!!
      "VERS_BETA",    # <- The BETA PRODUCTION <beta release> VERSION STUB !!!
      "VERS_STABLE"   # <- The STABLE PRODUCTION <stable release> VERSION STUB !!!
    ),
    lsListVals = rasBaseLIST(
      rsnVersDevsDEBUG_,   # <- The ACTIVE DEVELOPMENT <debug release> VERSION VALUE !!! 
      rsnVersProdALPHA_,   # <- The ALPHA PRODUCTION <alpha release> VERSION VALUE !!!
      rsnVersProdBETA_,    # <- The BETA PRODUCTION <beta release> VERSION VALUE !!!
      rsnVersProdSTABLE_   # <- The STABLE PRODUCTION <stable release> VERSION VALUE !!!
    ),
    sbLockList = TRUE
  );
  
}
