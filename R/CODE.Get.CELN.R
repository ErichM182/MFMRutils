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
#' library(MFMRutils)   # <- Loads the `MFMRutils` library (if previously installed).
#'
#'
#'
#' ### Pass a starting value to the function via the "siStartCELN" argument ...
#' code.get.celn(28L)   # <- Pass a starting value as the first positional argument to the function
#'                      #    and it will return the correct (active/real-time) CELN as extracted
#'                      #    from the R function you want to tersely document.
#'
#'
#'
#' ### Use the function with the direct-access R operator `::` from anywhere ...
#' MFMRutils::code.get.celn(28L)   # <- Pass the starting Editor Gutter Line Number (CELN) as the
#'                                 #    first positional argument to the function and it will return
#'                                 #    the correct (active /real-time) CELN as extracted from the
#'                                 #    the R function you want to interactively & tersely document.
#'
#'
#'
#' ### SPECIAL HACK - extract the CELN without a direct argument reference ...
#' rsiStartCELN_ <- 7L    # <- ALWAYS ensure this special "rsiStartCELN" variable name ends
#'                        #    with an underscore `_` character !!!
#' code.get.celn()        # <- ... then simply execute the `code.get.celn()` function without
#'                        #    passing any arguments to the function itself.
#'
#'
#'
#' ### Use with any custom R function as follows ...
#' "myCustFuncR" <- function(x=7, y=3, z=28) {   # <- Use the Editor Gutter Line Number (CELN)
#'                                               #    at which this opening curly brace is
#'                                               #    located as the "siStartCELN" value.
#'   rsiStartCELN_ <- 3L   # <- assumes this 👆 opening curly brace above (denoting the start
#'                         #    of the <custom> function body block of code) is located at
#'                         #    line 3 of the code editor (i.e. the curly brace is located
#'                         #    at the 3rd CELN).
#'
#'   ssTagFuncID <- "myCustFuncR"   # <- ALWAYS TAG Large Custom R Functions accordingly !!!
#'
#'   valueSUM <- sum(x, y, z)
#'   cat(paste0(" \u279C ", ssTagFuncID, " " , MFMRutils::code.get.celn(),
#'     " | Summed all 3 function arguments < result: ", valueSUM," > !!! \n")
#'   )
#'
#'   valueMEAN <- sum(x, y, z) / 3
#'   cat(paste0(" \u279C ", ssTagFuncID, " " , MFMRutils::code.get.celn(),
#'     " | Took the average of the 3 function arguments < result: ",
#'     round(valueMEAN, 3)," > !!! \n\n")
#'   )
#'
#'   return(
#'     list("SUM" = valueSUM, "MEAN" = valueMEAN)
#'   )
#' }
#'
#' ## Execute the custom R Function ...
#' myCustFuncR()
#'
#' ## Outputs from "myCustFuncR()" ...
#' # ➜ myCustFuncR 11 | Summed all 3 function arguments < result: 45 > !!!
#' # ➜ myCustFuncR 16 | Took the average of the 3 function arguments < result: 15 > !!!
#'
#' # $SUM
#' # [1] 45
#'
#' # $MEAN
#' # [1] 15
#'
#'
#'
#' ### Return the source frame of the special hack  "rsiStartCELN_" variable as follows ...
#' ## Set the special hack variable accordingly ...
#' rsiStartCELN_ <- 7L   # <- Ensure the "rsiStartCELN" variable name ends with
#'                       #    an underscore "_" character !!!
#'
#' ## Enable the 'return source' function argument ...
#' vsRes <- MFMRutils::code.get.celn(sbRetSRC = TRUE)   # <- Assign the outputs of the
#'                                                      #    `code.get.celn()` function to a
#'                                                      #    variable and set the `sbRetSRC`
#'                                                      #    function argument to a value of TRUE.
#'
#' ## Extract the Editor Gutter Line Number (CELN) and CELN Source (scope) ...
#' cat(paste0(" \u279C Code Editor Line ", vsRes["CELN"],   # <= returns the CELN ...
#'       " | ", vsRes["EnvSRC"], "\n"))   # <- outputs a sentence specifying where
#'                                        #    the "rsiStartCELN_" value used in
#'                                        #    the `code.get.celn()` function was
#'                                        #    obtained (i.e. sourced) from.
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
  rssFuncName_  <- "My.Cust.R.FUNC"; ##ssFuncName;
  rsiStartCELN_ <- siStartCELN;
  rsiCallIndex_ <- siCallIndex;
  
  
  
  ####   STEP 04 - Trace Function Call Stack Location   ####
  # 4.1 - Run an R Function Code Search to locate all instances of this `code.get.celn()` function 
  #       in the specified <active> R Function <internal function code> ...
  rdfFuncCalls_ <- rasMfmrDevsFindCODE(
    ssFindText = "code\\.get\\.celn\\(", 
    vsTargetLibs = c(RCT_TAG_FUNC_LIBR_ID_), 
    sbVerboseSearch = FALSE
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
