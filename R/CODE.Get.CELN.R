#? ### ### ### ### ### ### ###
#' @title Extract the Code Editor Gutter Line Number (CELN)
#' @name code.get.celn
#' @family CODE Functions (`SuiteMFMR`)
#' 
#' 
#' @description
#' A SPECIAL HELPER Function that extracts the Code Editor Gutter Line Number (CEGLN a.k.a. CELN) at 
#' the code editor line location from which the `code.get.celn()` function was called. This function 
#' was created for the purpose of enhancing the debugging, R Package development and real-time (i.e.
#' interactive) code documentation processes from within the MFMR Suite of R Functions. However, 
#' this function may also be used as a stand-alone function by (or in) other, 3rd Party, libraries 
#' and custom R functions - as long as the `MFMRutils` library is linked (i.e. previously installed
#' and/or specified as a library dependency).
#'
#'
#' @param ssFuncName ([character]) a String or Text (i.e. [character] [vector]) value that specifies 
#'                   the Function Identifier (i.e. name) where the R Code Search or Function ID 
#'                   look-up should be conducted (i.e. directed at).
#' @param sbRunByForce ([logical]) a Boolean value that specifies whether the function must execute
#'                     its standard code logic even if the R Project Development DEBUG tracker (
#'                     `RCT_IS_DEBUG_RT_MODE_`) was activated (i.e. found in the call stack & set to 
#'                     a value of `TRUE`) or not (default: `FALSE`).
#' @param siCallIndex ([integer]) an Integer value that defines the "Call Index" or "Call Sequence" 
#'                    in the localized (function <internal>) call stack in reference to when (i.e. 
#'                    in which order) the `code.get.celn()` function was called (i.e. from inside a 
#'                    function).
#' @param sbUseAlias ([logical]) a Boolean value that specifies whether the function should use the
#'                   the standard `code.get.celn()` function identifier or not (default: `FALSE`).
#'                   If set to `TRUE` the function will NOT use standard function ID (i.e.
#'                   "code\\.get\\.celn"), but rather the value supplied to the function via the
#'                   `ssAliasValue` function argument.
#' @param ssAliasValue ([character]) a String or Text (i.e. [character] [vector]) value that 
#'                     specifies the alias (i.e. re-assigned function identifier) of the default or
#'                     standard `code.get.celn()` function identifier to search for in the R
#'                     function <internal> code.
#'
#'
#' @examples
#' ### Easily debug custom R function code with this <cool> helper function ...
#' library(MFMRutils)   # <- Ensures the `MFMRutils` library is <already> installed locally.
#'
#'
#'
#' ### Use with any custom R function as follows ...
#' "my.custom.r.func" <- function(x=7, y=3, z=28) {
#' 
#'   RCT_TAG_FUNC_ID <- "my.custom.r.func";  # <- ALWAYS TAG (ID) Custom R Functions accordingly !!!
#' 
#'   valSUM <- sum(x, y, z);
#'   cat(
#'     paste0(
#'       " \u279C ", RCT_TAG_FUNC_ID, " " , 
#'       MFMRutils::code.get.celn(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         1L                 # <- Define the call order <call sequence> for the `CODE.Get.CELN()` 
#'       ),                   #    function calls inside the custom function code. This is the 1st 
#'                            #    time the `CODE.Get.CELN()` function is called inside this custom 
#'                            #    function so the `siCallIndex` value here should == `1L`.
#'       " | Summed all 3 input values < result: ", valSUM," > !!! \n"
#'     )
#'   );
#' 
#'   valMEAN <- sum(x, y, z) / 3;
#'   cat(
#'     paste0(
#'       " \u279C ", RCT_TAG_FUNC_ID, " " ,
#'       MFMRutils::code.get.celn(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         2L                 # <- Define the call order <call sequence> for the `CODE.Get.CELN()` 
#'       ),                   #    function calls inside the custom function code. This is the 2nd 
#'                            #    time the `CODE.Get.CELN()` function is called inside this custom 
#'                            #    function so the `siCallIndex` value here should == `2L`.
#'       " | Averaged the 3 input values < result: ", round(valMEAN, 1)," > !!! \n"
#'     )
#'   );
#'   
#'   # Notify function's SUCCESSFUL COMPLETION ( <- is only needed for code illustration) ...
#'   cat(
#'     paste0(
#'       " \u279C ", RCT_TAG_FUNC_ID, " " ,
#'       MFMRutils::code.get.celn(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         3L                 # <- Define the call order <call sequence> for the `CODE.Get.CELN()` 
#'       ),                   #    function calls inside the custom function code. This is the 3rd 
#'                            #    time the `CODE.Get.CELN()` function is called inside this custom 
#'                            #    function so the `siCallIndex` value here should == `3L`.
#'       " | CUSTOM R-FUNC `", tolower(RCT_TAG_FUNC_ID), "()` SUCESSFULLY EXECUTED !!! \n\n"
#'     )
#'   );
#' 
#'   return(
#'     list("SUM" = valSUM, "MEAN" = valMEAN)
#'   );
#' }
#' 
#' ## Execute the custom R Function ...
#' my.custom.r.func()
#' 
#' ## Outputs from "my.custom.r.func()" should be as follows ...
#' # ➜ My.Cust.R.FUNC 9 | Summed all 3 input values < result: 38 > !!!
#' # ➜ My.Cust.R.FUNC 24 | Averaged the 3 input values < result: 12.7 > !!!
#' # ➜ My.Cust.R.FUNC 39 | CUSTOM R-FUNC `my.custom.r.func()` SUCESSFULLY EXECUTED !!!
#' 
#' # $SUM
#' # [1] 38
#' 
#' # $MEAN
#' # [1] 12.66667
#' 
#'
#' @export
#? ### ### ###
"code.get.celn" <- function(
  ssFuncName="code.get.celn\\(", sbRunByForce=FALSE, siCallIndex=1L,
  sbUseAlias=FALSE, ssAliasValue="rasMfmrGetCELN\\("
) {
  
  
  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();   # <- Captures <active> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "MFMRutils";        # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "Get.CELN";         # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_LONG_   <- "CODE.Get.CELN";    # <- Function ID - LONG !!!
  
  RCT_INT_CELN_START_ <- 117L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    OPENING <normal> brace/bracket "(" is located !!!
  RCT_INT_CELN_STOP_  <- 212L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    CLOSING <curly> brace/bracket "}" is located !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  ## NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseGET0       <- base::get0;
  rasBaseNROW       <- base::nrow;
  rasBaseORDER      <- base::order;
  rasBaseRETURN     <- base::return;
  rasBaseSubSET     <- base::subset;
  rasBaseIfELSE     <- base::ifelse;
  rasBaseToLOWER    <- base::tolower;
  rasBaseSeqALONG   <- base::seq_along;
  rasBaseAsNUMERIC  <- base::as.numeric;
  rasBaseDUPLICATED <- base::duplicated;
  
  rasMfmrFuncSID  <- MFMRutils::RENV_FSID;
  rasMfmrFindCODE <- MFMRutils::devs.find.code.instances;
  
  ## SPECIAL - Constant - TAG - Aliases ...
  RAS_IS_DEBUG_MODE_   <- rasMfmrFuncSID$CONSTS_BOOL_IS_DEBUG;
  RAS_IS_VERBOSE_MODE_ <- rasMfmrFuncSID$CONSTS_BOOL_IS_VERBOSE;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  ssFuncName_   <- ssFuncName;
  siCallIndex_  <- siCallIndex;
  sbRunByForce_ <- sbRunByForce;
  sbUseAlias_   <- sbUseAlias;
  ssAliasValue_ <- ssAliasValue;
  
  ## SPECIAL: Try to locate & extract the 'isDebugMode' logical (boolean) variable 
  ##          <if set or primed elsewhere> in the current <active> R Project ... 
  sbIsDEBUG_ <- rasBaseGET0(   # <- Searches the Global Environment of the Active R Session for
    RAS_IS_DEBUG_MODE_,        #    the <somewhat> uniquely named variable `RCT_IS_DEBUG_MODE_`
    envir = .GlobalEnv,        #    and extracts its value.
    ifnotfound = FALSE         # -> Assigns a value of `FALSE` if the variable was NOT FOUND in
  );                           #    the Active R Session !!!
  
  
  if (sbRunByForce_ || sbIsDEBUG_) {   # <- Run standard code logic if either of these is TRUE !!!
    
    ####   STEP 04 - Trace Function Call Stack Location   ####
    ## 4.1 - Run an R Function Code Search to locate all instances of this `code.get.celn()` 
    ##       function in the specified <active> R Function <internal function code> ...
    rdfFuncCalls_ <- rasMfmrFindCODE(
      vsTargetLibs = c(RCT_TAG_FUNC_LIBR_ID_),
      sbVerboseSearch = FALSE, sbIgnoreCase = FALSE,
      ssFindText = rasBaseIfELSE(
        sbUseAlias_,
        ssAliasValue_,           # <- This is the user-specified "Alias" value to search for !!!
        "code\\.get\\.celn\\("   # <- NEVER CHANGE THIS (it is the 100% CORRECT default value) !!!
      )
    );
    
    ## 4.2 - Extract only results that match specified Function ID ...
    rdfFuncCalls_v02_ <- rasBaseSubSET(
      rdfFuncCalls_, rdfFuncCalls_[["FUNC_NAME"]] == rasBaseToLOWER(ssFuncName_)
    );
    rdfFuncCalls_UNIQUE_LNs_ <- rdfFuncCalls_v02_[
      !rasBaseDUPLICATED(rdfFuncCalls_v02_[["LINE_NUMBER"]]), 
    ];
    
    ## 4.3 - Create a new INDEX Variable for the subset results Data Frame ...
    rdfFuncCalls_v03_ <- rdfFuncCalls_UNIQUE_LNs_[
      rasBaseORDER(rasBaseAsNUMERIC(rdfFuncCalls_UNIQUE_LNs_[["MATCH_ID"]]), decreasing = FALSE), 
    ];
    rdfFuncCalls_v03_$INDEX <- rasBaseSeqALONG(1:rasBaseNROW(rdfFuncCalls_v03_));
    
    ## 4.5 - Extract the Code Editor Line Number (CELN) accordingly ...
    rdfAtCELN_ <- rasBaseSubSET(rdfFuncCalls_v03_, rdfFuncCalls_v03_[["INDEX"]] == siCallIndex_);
    
    
    ####   STEP 05 - Return Function Outputs   ####
    rasBaseRETURN(rasBaseAsNUMERIC(rdfAtCELN_[["LINE_NUMBER"]]));
    
  }
  
}
