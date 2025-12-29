#######|       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Test-Pad > R SCRIPT   <-       |#######
###  -> Use this R Script <file> purely for Testing Purposes during Code Development Cycle !!!  ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######



### Use the "Path-Cleaning" Function as follows: ...
library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...



#' @examples
#' ### Easily debug custom R function code with this <cool> helper function ...
#' library(MFMRutils)   # <- Loads the `MFMRutils` library (if previously installed).
#'
#'
#'
### Use with any custom R function as follows ...
"my.cust.r.func" <- function(x=7, y=3, z=28) {   # <- Use the Editor Gutter Line Number (CELN)
                                              #    at which this opening curly brace is
                                              #    located as the "siStartCELN" value.
  rsiStartCELN_ <- 19L;   # <- assumes this 👆 opening curly brace above (denoting the start
                          #    of the <custom> function body block of code) is located at
                          #    line 3 of the code editor (i.e. the curly brace is located
                          #    at the 3rd CELN).

  ssTagFuncID <- "My.Cust.R.FUNC"   # <- ALWAYS TAG Large Custom R Functions accordingly !!!

  valueSUM <- sum(x, y, z);
  cat(
    paste0(
      " \u279C ", ssTagFuncID, " " , 
      MFMRutils::code.get.celn(ssFuncName = ssTagFuncID, rsiStartCELN_, 1L ),
      " | Summed all 3 function arguments < result: ", valueSUM," > !!! \n"
    )
  );

  valueMEAN <- sum(x, y, z) / 3;
  cat(
    paste0(
      " \u279C ", ssTagFuncID, " " , 
      MFMRutils::code.get.celn(ssTagFuncID, rsiStartCELN_, 2L),
      " | Took the average of the 3 function arguments < result: ",
      round(valueMEAN, 3)," > !!! \n\n"
    )
  )

  return(
    list("SUM" = valueSUM, "MEAN" = valueMEAN)
  )
}

## Execute the custom R Function ...
my.cust.r.func()

## Outputs from "myCustFuncR()" ...
# ➜ myCustFuncR 11 | Summed all 3 function arguments < result: 45 > !!!
# ➜ myCustFuncR 16 | Took the average of the 3 function arguments < result: 15 > !!!

# $SUM
# [1] 45

# $MEAN
# [1] 15



### Return the source frame of the special hack  "rsiStartCELN_" variable as follows ...
## Set the special hack variable accordingly ...
rsiStartCELN_ <- 7L   # <- Ensure the "rsiStartCELN" variable name ends with
                      #    an underscore "_" character !!!

## Enable the 'return source' function argument ...
vsRes <- MFMRutils::code.get.celn(sbRetSRC = TRUE)   # <- Assign the outputs of the
                                                     #    `code.get.celn()` function to a
                                                     #    variable and set the `sbRetSRC`
                                                     #    function argument to a value of TRUE.

## Extract the Editor Gutter Line Number (CELN) and CELN Source (scope) ...
cat(paste0(" \u279C Code Editor Line ", vsRes["CELN"],   # <= returns the CELN ...
      " | ", vsRes["EnvSRC"], "\n"))   # <- outputs a sentence specifying where
                                       #    the "rsiStartCELN_" value used in
                                       #    the `code.get.celn()` function was
                                       #    obtained (i.e. sourced) from.

#' 
#' 
#'
