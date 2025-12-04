#? ### ### ### ### ### ### ###
#' @title R Library Version Number Updater ("SuiteMFMR" DevTools)
#' @name devs.patch.libr.vers.number
#' 
#' @description
#' A Helper Function that formats and prints (to console) the CRAN Code Validation
#' results as returned from the DevTools Code Check function. This function merely 
#' receives the output from the DevTools Code Check function and applies ANSI text
#' formatting to the results.
#'
#' @param rvsVersNumVect a vector of characters (strings) that defines the <regent> R Library's
#'                       active development (or production) code release status (version number).
#'                    
#' @returns
#' * This function returns the programmatically amended or updated (real-time or
#'   active) version number for the active R Library Project as a list of character
#'   objects.
#'
#' @examples
#' ### Run R Package DevCode easily as follows ...
#' library(MFMRutils)   # <- Loads "MFMRutils" library (if already installed) !!!
#'
#' ### Run 2 different types of code check/validation processes ...
#' devs.check.code.specs(sbCheckDocs = TRUE)   # -> Executes only the DevTools Documentation 
#'                                             #    Process.
#'
#' @keywords internal
#' @noRd
#? ### ### ###
"devs.patch.libr.vers.number" <- function(rvsVersNumVect=c("0", "0", "0", "001")) {
  
  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_ID_SHRT_ <- "Patch.Libr.Vers";               # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Patch.Libr.Vers.Number";   # <- Function ID - LONG !!!
  RCT_TAG_FUNC_LIBR_ID_ <- MFMRutils::devs.pull.libr.info()[["NAME"]];
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve R Session Memory Efficiency ...
  rasBaseLIST      <- base::list;
  rasBaseLENGTH    <- base::length;
  rasBaseRETURN    <- base::return;
  rasBaseAsNUMERIC <- base::as.numeric;
  
  
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
  rasBaseRETURN(
    rasBaseLIST(
      "VERS_DEVS_DEBUG"  = rsnVersDevsDEBUG_,
      "VERS_PROD_ALPHA"  = rsnVersProdALPHA_,
      "VERS_PROD_BETA"   = rsnVersProdBETA_,
      "VERS_PROD_STABLE" = rsnVersProdSTABLE_
    )
  );
  
}
