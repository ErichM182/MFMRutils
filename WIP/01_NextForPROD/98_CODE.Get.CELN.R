#? ### ### ### ### ### ### ###
#' @title Extract the Code Editor Gutter Line Number (CELN)
#' @name code.get.celn
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
#' @param siStartCELN ([integer]) An integer value that specifies the Code Editor Gutter Line Number
#'                    (abbreviated as `CELN`) where the function's opening (i.e. starting) curly 
#'                    brace is located (i.e. the code editor line number at which the function 
#'                    body's block of code starts !!!).
#' @param sbRetSRC ([logical]) A boolean value that specifies whether the R Environment Frame (i.e. 
#'                 scope: `LOCAL`, `PARENT` or `GLOBAL`) where the special `siStartCELN_` variable 
#'                 was sourced from should be returned as a function output [TRUE] or not [FALSE].
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
"code.get.celn" <- function(siStartCELN=NULL, sbRetSRC=FALSE) {
  
  
  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();  # <- Captures <active> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "MFMRutils";       # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "Get.CELN";        # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_LONG_   <- "CODE.Get.CELN";   # <- Function ID - LONG !!!
  
  
  
  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  # NOTES: This is a <NEW> approach to improve the R Session Memory Efficiency ...
  rasBaseGET       <- base::get;
  rasBaseATTR      <- base::attr;
  rasBaseLIST      <- base::list;
  rasBaseGREPL     <- base::grepl;
  rasBaseEXISTS    <- base::exists;
  rasBaseRETURN    <- base::return;
  rasBaseIsNULL    <- base::is.null;
  rasBaseDEPARSE   <- base::deparse;
  rasBaseSysCALLS  <- base::sys.calls;
  rasBaseSeqALONG  <- base::seq_along;
  rasBaseAsINTEGER <- base::as.integer;
  
  rasBaseFrameGLOBAL <- .GlobalEnv;
  rasBaseFrameLOCAL  <- base::environment;
  rasBaseFramePARENT <- base::parent.frame;
  
  
  
  ####   STEP 03 - Internalize ALL Function Arguments   ####
  rsbRetSRC_    <- sbRetSRC;
  rsiStartCELN_ <- siStartCELN;
  
  
  
  ####   STEP 04 - Set Func-Call FRAME   ####
  # STEP 1 - A special hack to check if the "siStartCELN_" variable has already
  #          been defined in the global or calling function environments !!!
  rssActSRC_ <- "The 'siStartCELN_' value was set directly via the FUNC-ARGS !!!";
  if (rasBaseIsNULL(rsiStartCELN_)) {

    # Create a list of all Active R Session Environment Scopes ...
    rvsActENVs_ <- rasBaseLIST(
      # GLOBAL Frame (the top-most R Session Environment or Scope) => akin to
      # the main room of a house ...
      "GLOBAL" = rasBaseFrameGLOBAL,
      # PARENT or Caller Frame (env. or scope where function was called from)
      # => akin to a room within the house ...
      "PARENT" = rasBaseFramePARENT(),
      # LOCAL, Current or Executer Frame (env. or scope where function was
      # executed from) => akin to a temporary workspace within the room ...
      "LOCAL" = rasBaseFrameLOCAL()
    );

    # Locate & extract the "rsiStartCELN_" variable accordingly ...
    if (   # <= Current or LOCAL (i.e. active R Session) Environment !!!
      rasBaseEXISTS("rsiStartCELN_", envir = rvsActENVs_$LOCAL, inherits = FALSE)
    ) {
      rsiStartCELN_ <- rasBaseGET("rsiStartCELN_", envir = rvsActENVs_$LOCAL);
      rssActSRC_ <- "The 'rsiStartCELN_' value was sourced from the LOCAL Frame !!!";
    } else if (    # <= PARENT (i.e. active R Session) Environment !!!
      rasBaseEXISTS("rsiStartCELN_", envir = rvsActENVs_$PARENT, inherits = FALSE)
    ) {
      rsiStartCELN_ <- rasBaseGET("rsiStartCELN_", envir = rvsActENVs_$PARENT);
      rssActSRC_ <- "The 'rsiStartCELN_' value was sourced from the PARENT Frame !!!";
    } else if (   # <= GLOBAL (i.e. active R Session) Environment !!!
      rasBaseEXISTS("rsiStartCELN_", envir = rvsActENVs_$GLOBAL, inherits = FALSE)
    ) {
      rsiStartCELN_ <- rasBaseGET("rsiStartCELN_", envir = rvsActENVs_$GLOBAL);
      rssActSRC_ <- "The 'rsiStartCELN_' value was sourced from the GLOBAL Frame !!!";
    } else {   # <- The "rsiStartCELN_" variable was not found anywhere !!!
      rsiStartCELN_ <- 1L;
      rssActSRC_ <- "The 'rsiStartCELN_' value was set to 1L by the `Get.CELN` FUNCTION itself !!!";
    }
  }
  
  
  
  ####   STEP 05 - Trace Function Call Stack Location   ####
  # STEP 2 - Run a call stack analysis to locate where this `code.get.celn()`
  #          function was called in the Active Stack Trace !!!
  rcoActSysCalls_ <- rasBaseSysCALLS();
  if (rasBaseIsNULL(rcoActSysCalls_)) {
    rasBaseRETURN(base::cat(base::paste0(" -> FUNCTION CALL STACK is EMPTY !!!\n")));
  } else {
    for (sysCall in rasBaseSeqALONG(rcoActSysCalls_)) {
      rcoRefSRC_ <- rasBaseATTR(rcoActSysCalls_[[sysCall]], "srcref");
      if (!rasBaseIsNULL(rcoRefSRC_)) {
        rsbCodeMatchFound_ <- rasBaseGREPL(
          "code\\.get\\.celn\\(", rasBaseDEPARSE(rcoActSysCalls_[[sysCall]])
          ### "code\\.get\\.celn\\s*\\(", rasBaseDEPARSE(rcoActSysCalls_[[sysCall]])
        )[1];   # <= Returns a result object of length 3 - so select only first entry !!!
        if (rsbCodeMatchFound_) {
          if (!rsbRetSRC_) {
            rasBaseRETURN(
              (rsiStartCELN_ - 1L) + rasBaseAsINTEGER(rcoRefSRC_[[1]][1])
            );
          } else {
            rasBaseRETURN(
              rasBaseLIST(
                "EnvSRC" = rssActSRC_,
                "CELN" = (rsiStartCELN_ - 1L) + rasBaseAsINTEGER(rcoRefSRC_[[1]][1])
              )
            )
          }
        }
      }
    }
  }
  
  
  
  ### ####   STEP 06 - Return Function Outputs   ####
  ### ## STEP 3 - If the call stack trace fails ... return "NA" to function call !!!
  ### rasBaseRETURN(NA_integer_);
}
