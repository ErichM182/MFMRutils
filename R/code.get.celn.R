#? ### ### ### ### ### ### ###
#' @title Fetch the active Code Editor Gutter Line Number (CEGLN a.k.a. CELN)
#' 
#' @description
#' A SPECIAL HELPER Function that extracts the Code Editor Gutter Line Number
#' (CEGLN a.k.a. CELN) at the code editor line location from which the
#' `code.get.celn()` function was called. This function was created for the
#' purpose of enhancing the debugging, R Package development and real-time
#' (i.e. interactive) code documentation processes from within the MFMR Suite
#' of R Functions. However, this function may also be used as a stand-alone
#' function by (or in) other, 3rd Party, libraries and custom R functions - as
#' long as the `MFMRutils` library is linked accordingly (i.e. previously
#' installed and/or specified as a library dependency).
#'
#' @param siStartCLN the Editor Gutter Line Number (CEGLN) where the function's
#' curly opening brace is located (i.e. the code editor line number at which
#' the function body's block of code starts !!!).
#' @param sbRetSRC a logical (boolean) value that specifies whether the
#' Environment Frame (i.e. scope: `LOCAL`, `PARENT` or `GLOBAL`) where the
#' special "siStartCEGLN_" variable was sourced from should be returned as a
#' function output [TRUE] or not [FALSE].
#'
#' @examples
#' ### Easily debug custom R function code with this <cool> helper function ...
#' library(MFMRutils)   # <= Load the `MFMRutils` library (if previously installed).
#'
#'
#'
#' ### Pass a starting value to the function via the "siStartCLN" argument ...
#' { code.get.celn(28L) }   # <= Pass a starting value as the first positional argument to the
#'                          #    function & it will return the correct (active/real-time) CEGLN
#'                          #    as extracted from the R function you want to tersely document.
#'
#'
#'
#' ### Use the function with the direct-access R operator `::` from anywhere ...
#' { MFMRutils::code.get.celn(28L) }   # <= Pass the starting Editor Gutter Line Number (CEGLN)
#'                                     #    as the first positional argument to the function
#'                                     #    and it will return the correct (active /real-time)
#'                                     #    CEGLN as extracted from the R function you want to
#'                                     #    interactively & tersely document.
#'
#'
#'
#' ### SPECIAL HACK - extract the CEGLN without a direct argument reference ...
#' siStartCEGLN_ <- 7L   # <= ALWAYS snsure this special "siStartCEGLN" variable name ends
#'                       # with an underscore `_` character !!!
#' { code.get.celn() }   # <= ... then simply execute the `code.get.celn()` function without
#'                       #    passing any arguments to the function itself.
#'
#'
#'
#' ### Use with any custom R function as follows ...
#' "myCustFuncR" <- function(x=7, y=3, z=28) {   # <= Use the Editor Gutter Line Number (CEGLN)
#'                                               #    at which this opening curly brace is
#'                                               #    located as the "siStartCLN" value.
#'   siStartCEGLN_ <- 3L   # <= assumes this 👆 opening curly brace above (denoting the start
#'                         #    of the <custom> function body block of code) is located at
#'                         #    line 3 of the code editor (i.e. the curly brace is located
#'                         #    at the 3rd CEGLN).
#'
#'   ssTagFuncID <- "myCustFuncR"   # <= ALWAYS TAG Large Custom R Functions accordingly !!!
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
#' ### Return the source frame of the special hack  "siStartCEGLN_" variable as follows ...
#' {
#'   ## Set the special hack variable accordingly ...
#'   siStartCEGLN_ <- 7L   # <= Ensure the "siStartCEGLN" variable name ends with
#'                         #    an underscore "_" character !!!
#'
#'   ## Enable the 'return source' function argument ...
#'   vsRes <- MFMRutils::code.get.celn(sbRetSRC = TRUE)   # <= Assign the outputs of the
#'                                                        #    `code.get.celn()` function
#'                                                        #    to a variable and set the
#'                                                        #    'sbRetSRC' function argument
#'                                                        #    to a value of TRUE.
#'
#'   ## Extract the Editor Gutter Line Number (CEGLN) and CEGLN Source (scope) ...
#'   cat(paste0(" \u279C Code Editor Line ", vsRes["CEGLN"],   # <= returns the CEGLN ...
#'       " | ", vsRes["EnvSRC"], "\n"))   # <= outputs a sentence specifying where
#'                                        #    the "siStartCEGLN_" value used in
#'                                        #    the `code.get.celn()` function was
#'                                        #    obtained (i.e. sourced) from.
#' }
#'
#' @export
#? ### ### ###
"code.get.celn" <- function(siStartCLN=NULL, sbRetSRC=FALSE) {

  # STEP 1 - A special hack to check if the "siStartCEGLN_" variable has already
  #          been defined in the global or calling function environments !!!
  ssActSRC <- "The 'siStartCEGLN_' value was set directly via the FUNC-ARGS !!!";
  if (base::is.null(siStartCLN)) {

    # Create a list of all Active R Session Environment Scopes ...
    vsActENVs <- base::list(
      # GLOBAL Frame (the top-most R Session Environment or Scope) => akin to
      # the main room of a house ...
      "GLOBAL" = .GlobalEnv,
      # PARENT or Caller Frame (env. or scope where function was called from)
      # => akin to a room within the house ...
      "PARENT" = base::parent.frame(),
      # LOCAL, Current or Executer Frame (env. or scope where function was
      # executed from) => akin to a temporary workspace within the room ...
      "LOCAL" = base::environment()
    );

    # Locate & extract the "siStartCEGLN_" variable accordingly ...
    if (   # <= Current or LOCAL (i.e. active R Session) Environment !!!
      base::exists("siStartCEGLN_", envir = vsActENVs$LOCAL, inherits = FALSE)
    ) {
      siStartCLN <- base::get("siStartCEGLN_", envir = vsActENVs$LOCAL);
      ssActSRC <- "The 'siStartCEGLN_' value was sourced from the LOCAL Frame !!!";
    } else if (    # <= PARENT (i.e. active R Session) Environment !!!
      base::exists("siStartCEGLN_", envir = vsActENVs$PARENT, inherits = FALSE)
    ) {
      siStartCLN <- base::get("siStartCEGLN_", envir = vsActENVs$PARENT);
      ssActSRC <- "The 'siStartCEGLN_' value was sourced from the PARENT Frame !!!";
    } else if (   # <= GLOBAL (i.e. active R Session) Environment !!!
      base::exists("siStartCEGLN_", envir = vsActENVs$GLOBAL, inherits = FALSE)
    ) {
      siStartCLN <- base::get("siStartCEGLN_", envir = vsActENVs$GLOBAL);
      ssActSRC <- "The 'siStartCEGLN_' value was sourced from the GLOBAL Frame !!!";
    } else {   # <- The "siStartCEGLN_" variable was not found anywhere !!!
      siStartCLN <- 1L;
      ssActSRC <- "The 'siStartCEGLN_' value was set to 1L by the FUNCTION itself !!!";
    }
  }

  # STEP 2 - Run a call stack analysis to locate where this `code.get.celn()`
  #          function was called in the active stack trace !!!
  actSysCalls <- base::sys.calls();
  for (i in base::seq_along(actSysCalls)) {
    srcRef <- base::attr(actSysCalls[[i]], "srcref");
    if (!base::is.null(srcRef)) {
      sbCodeMatchFound <- base::grepl(
        "code\\.get\\.celn\\s*\\(", base::deparse(actSysCalls[[i]])
      )[1];   # <= Returns a result object of length 3 - so select only first entry !!!
      if (sbCodeMatchFound) {
        if (!sbRetSRC) {
          base::return(
            (siStartCLN - 1L) + base::as.integer(srcRef[[1]][1])
          );
        } else {
          base::return(
            base::list(
              "EnvSRC" = ssActSRC,
              "CEGLN" = (siStartCLN - 1L) + base::as.integer(srcRef[[1]][1])
            )
          )
        }
      }
    }
  }

  # STEP 3 - If the call stack trace fails ... return "NA" to function call !!!
  base::return(NA_integer_);
}
