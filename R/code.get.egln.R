#? ### ### ### ### ### ### ###
#' @title Extract the RStudio Editor Gutter Line Number ...
#' @description
#' A SPECIAL HELPER Function that extracts the Editor Gutter Line Number - EGLN (at the line location from which this function was called). This function only works in the RStudio IDE since it makes direct use of the "rstudioapi" library. This function was created with the main purpose of enhancing the debugging, R Package development and real-time (interactive) code documentation processes from within the MFMR Suite of R Functions.
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
"code.get.egln" <- function() {
  # RStudio API method (most accurate)
  if (
    base::requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    base::tryCatch(
      {
        ctx <- rstudioapi::getActiveDocumentContext()

        # Get exact cursor position of function call
        if (base::length(ctx$selection) > 0) {
          cursor_pos <- ctx$selection[[1]]$range$start
          base::return(base::as.integer(cursor_pos[1]))  # Return line number only ...
        }
      },
      error = function(e) NULL
    )
  }

  # Call stack analysis (fallback method) ...
  frame_idx <- base::sys.nframe() - 1L  # Immediate parent frame ...
  while (frame_idx > 0L) {
    srcref <- base::attr(base::sys.frame(frame_idx), "srcref")

    if (!base::is.null(srcref)) {
      base::return(base::as.integer(srcref[[1]]))
    }
    frame_idx <- frame_idx - 1L
  }

  # Failure case ...
  base::warning("EGLN Err !!!")
  return(NA_integer_)
}

