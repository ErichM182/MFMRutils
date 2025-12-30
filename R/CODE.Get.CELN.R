#? ### ### ### ### ### ### ###
#' @title Extract the Code Editor Gutter Line Number (CELN)
#' @name code.get.celn
#' @family SuiteMFMR Code Functions
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
#' @param ssFuncName ([character]) A String value that specifies the Function Identifier (i.e. name)
#'                   where the R Code Search should be conducted (directed at).
#' @param siStartCELN ([integer]) An Integer value that specifies the Code Editor Gutter Line Number
#'                    (abbreviated as `CELN`) where the function's opening (i.e. starting) curly 
#'                    brace is located (i.e. the code editor line number at which the function 
#'                    body's block of code starts !!!).
#' @param siCallIndex ([integer]) An Integer value that defines the "Call Index" in the localized (
#'                    function <internal>) call stack in reference to when (i.e. in which order) the
#'                    `code.get.celn()` function was called (i.e. from inside a function).
#'
#'
#' @examples
#' ### Easily debug custom R function code with this <cool> helper function ...
#' library(MFMRutils)   # <- Ensures the `MFMRutils` library is <already> installed locally.
#'
#'
#'
#' ### Use with any custom R function as follows ...
#' "my.cust.r.func" <- function(x=7, y=3, z=28) {   # <- Use the Code Editor Line Number (CELN)
#'                                                  #    at which the `function()` call's opening 
#'                                                  #    brace or bracket <i.e. `(`> is located at 
#'                                                  #    as the `siStartCELN` input argument value.
#'   siStartCELN <- 19L; # <- 👆 assumes this opening brace above (denoting the start of
#'                       #    the <custom> function's arguments code block) is located
#'                       #    at line 19 of the code editor (i.e. the opening brace is
#'                       #    located at the 19th CELN).
#' 
#'   RCT_TAG_FUNC_ID <- "My.Cust.R.FUNC";  # <- ALWAYS TAG (ID) Custom R Functions accordingly !!!
#' 
#'   valSUM <- sum(x, y, z);
#'   cat(
#'     paste0(
#'       " \u279C ", RCT_TAG_FUNC_ID, " " , 
#'       MFMRutils::code.get.celn(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         siStartCELN,       # <- Specify at which line the opening function brace is located !!! 
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
#'         siStartCELN,       # <- Specify at which line the opening function brace is located !!! 
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
#'         siStartCELN,       # <- Specify at which line the opening function brace is located !!! 
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
#' my.cust.r.func()
#' 
#' ## Outputs from "my.cust.r.func()" ...
#' # ➜ My.Cust.R.FUNC 34 | Summed all 3 input values < result: 38 > !!!
#' # ➜ My.Cust.R.FUNC 49 | Averaged the 3 input values < result: 12.7 > !!!
#' # ➜ My.Cust.R.FUNC 64 | CUSTOM R-FUNC `my.cust.r.func()` SUCESSFULLY EXECUTED !!!
#' 
#' # $SUM
#' # [1] 45
#' 
#' # $MEAN
#' # [1] 15
#' 
#'
#' @export
#? ### ### ###
"code.get.celn" <- function(ssFuncName="code.get.celn", siStartCELN=1L, siCallIndex=1L) {
  
  
  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();  # <- Captures <active> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "MFMRutils";       # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "Get.CELN";        # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_LONG_   <- "CODE.Get.CELN";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseNROW       <- base::nrow;
  rasBaseORDER      <- base::order;
  rasBaseRETURN     <- base::return;
  rasBaseSubSET     <- base::subset;
  rasBaseToLOWER    <- base::tolower;
  rasBaseSeqALONG   <- base::seq_along;
  rasBaseAsNUMERIC  <- base::as.numeric;
  rasBaseDUPLICATED <- base::duplicated;
  
  rasMfmrDevsFindCODE <- MFMRutils::devs.find.code.instances;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rssFuncName_  <- "my.cust.r.func"; # ssFuncName;
  rsiStartCELN_ <- siStartCELN;
  rsiCallIndex_ <- siCallIndex;
  
  
  
  ####   STEP 04 - Trace Function Call Stack Location   ####
  # 4.1 - Run an R Function Code Search to locate all instances of this `code.get.celn()` function 
  #       in the specified <active> R Function <internal function code> ...
  rdfFuncCalls_ <- rasMfmrDevsFindCODE(
    ssFindText = "code\\.get\\.celn\\(", 
    vsTargetLibs = c(RCT_TAG_FUNC_LIBR_ID_), 
    sbVerboseSearch = FALSE, sbIgnoreCase = FALSE
  );
  
  # 4.2 - Extract only results that match specified Function ID ...
  rdfFuncCalls_v02_ <- rasBaseSubSET(
    rdfFuncCalls_, rdfFuncCalls_[["FUNC_NAME"]] == rasBaseToLOWER(rssFuncName_)
  );
  rdfFuncCalls_UNIQUE_LNs_ <- rdfFuncCalls_v02_[
    !rasBaseDUPLICATED(rdfFuncCalls_v02_[["LINE_NUMBER"]]), 
  ];
  
  # 4.3 - Create a new INDEX Variable for the subset results Data Frame ...
  rdfFuncCalls_v03_ <- rdfFuncCalls_UNIQUE_LNs_[
    rasBaseORDER(rasBaseAsNUMERIC(rdfFuncCalls_UNIQUE_LNs_[["MATCH_ID"]]), decreasing = FALSE), 
  ];
  rdfFuncCalls_v03_$INDEX <- rasBaseSeqALONG(1:rasBaseNROW(rdfFuncCalls_v03_));
  
  # 4.5 - Extract the Code Editor Line Number (CELN) accordingly ...
  rdfAtCELN_ <- rasBaseSubSET(rdfFuncCalls_v03_, rdfFuncCalls_v03_[["INDEX"]] == rsiCallIndex_);
  
  
  ####   STEP 05 - Return Function Outputs   ####
  rasBaseRETURN(rasBaseAsNUMERIC(rdfAtCELN_[["LINE_NUMBER"]]) + rsiStartCELN_);
}
