#? ### ### ### ### ### ### ###
#' @title Extract the RStudio Editor Gutter Line Number ...
#' @description
#' A SPECIAL HELPER Function that extracts the Editor Gutter Line Number - EGLN (at the line location from which this function was called). This function only works in the RStudio IDE since it makes direct use of the "rstudioapi" library. This function was created with the main purpose of enhancing the debugging and R Package development process from within the MFMR Suite of R Functions.
#'
#' @usage code.get.egln()   ### <- if "MFMRutils" library is already installed & loaded !!!
#'
#' @import rstudioapi
#'
#' @examples
#' ### Easily debug your R code with the help of this <special> function ...
#' code.get.egln()   ### <- prints the active script editor line number to the R console !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::code.get.egln()   ### <- prints the active script editor line number to the R console !!!
#'
#' @export
#? ### ### ###
###   SPECIAL HELPER Function - extracts the Editor Gutter Line Number - EGLN (at the line location from which the function was called) ...   ###
"code.get.egln" <- function() {

  # Check if running in RStudio with "rstudioapi" library available ...
  if (
    base::requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()
  ) {

    base::tryCatch(
      {

        # Get active document context ...
        ctx <- rstudioapi::getActiveDocumentContext();

        # Find which line contains the function call ...
        content <- ctx$contents;
        call_line <- base::which(
          base::grepl("get_editor_line\\s*\\(\\s*\\)", content)
        )[1]

        if (!base::is.na(call_line)) base::return(call_line)
      },
      error = function(e) NULL
    )
  }

  # Fallback method using source references ...
  frames <- base::sys.calls();
  for (i in base::seq_along(frames)) {
    srcref <- base::attr(frames[[i]], "srcref");
    if (!base::is.null(srcref)) {
      base::return(base::as.integer(srcref[[1]]));
    }
  }

  base::warning("Line number detection failed");
  base::return(NA_integer_);
}


