#? ### ### ### ### ### ### ###
#' A Helper Function that standardizes the User / Project Information Posting (i.e. notification) Processes.
#'
#' @param ssPostNote the character vector (i.e. string or text or note) to be printed to the console. This text note is also returned as a function output under the "value" option of this function's results list.
#' @param ssFuncSelfID a character vector (i.e. string or text) serving as a self-identifier (tag) for this "info.post.note()" function.
#' @param sbRunSelfID a logical (i.e. boolean) value that specifies whether the "info.post.note()" function should output its own identifier information alongside the user specified info (notification).
#' @param ssFuncCallerID a character vector (string or text) serving as the identifier (tag) for the R Function or R Project that called (invoked) this "info.post.note()" function.
#' @param ssFuncType a character vector (string or text) that specifies the function type. OPTIONS: "Helper" (small to medium custom functions; less than 300 lines of code) and "LARGE" (massive custom function with >300 lines of code). If set to NULL the function type will be assigned the default value of "Helper".
#' @param siPostMode123 an integer value that defines the form of the output (notification) message to be printed to the console. OPTIONS: 1L (applies the "START" or "HEADER" form to the printed message), 2L (applies the "Normal" or "DEFAULT" form to the printed message) and 3L (applies the "STOP" or "TERMINAL" form to the printed message). This function argument is extremely useful for defining the beginning and terminal sections of custom R Functions and/or R Projects.
#' @param sbRetFuncInfo a logical (boolean) value that specifies whether to output or return the function (i.e. internally processed) information in the results of the current function. This function argument can be useful when nesting multiple custom functions.
#' @param ssPreStub a character vector (string or text) that defines the starting text of the output message (notification). This function argument is useful in standardizing the notification format for an entire R Project.
#' @param ssMidStub a character vector (string or text) that defines the middle (ID "tag" and Messages) text of the output message (notification). This function argument is useful in standardizing the notification format for an entire R Project.
#' @param sbPrePendNewLine a logical (boolean) value that specifies whether a new line (blank space or row) should be added to the START of a posted note.
#' @param sbPostPendNewLine a logical (boolean) value that specifies whether a new line (blank space or row) should be added to the END of a posted note.
#' @param sbEndExtraNewLine a logical (boolean) value that specifies whether a second line (blank space or row) should be added to the END of a posted note. This function argument can be useful for clearly delineating sections within a custom R Function and/or R Project.
#'
#' @returns => This function prints the specified text (notification) directly to the console even if the function outputs are assigned to a variable.
#' @returns => This function also outputs additional function information (i.e. internally computed function information) in a list.
#' @export
#'
#' @examples info.post.note()   # <- Prints a dummy notification ...
#' @examples MFMRutils::info.post.note()   # <- Prints a dummy notification ...
#' @examples info.post.note(siPostMode123 = 1L)   # <- Prints a START notification ...
#' @examples info.post.note(siPostMode123 = 2L)   # <- Prints a Normal (default) notification ...
#' @examples info.post.note(siPostMode123 = 3L)   # <- Prints an END notification ...
#' @examples info.post.note(sbRetFuncInfo = F, siPostMode123 = 2L)   # <- Outputs additional function information in list form ...
#? ### ### ###
"info.post.note" <- function(
  ssPostNote="NOTE to POST !!!", ssFuncSelfID="Post Note",
  sbRunSelfID=FALSE, ssFuncCallerID=NULL, ssFuncType=NULL,
  siPostMode123=2L, sbRetFuncInfo=FALSE, ssPreStub=" => ", ssMidStub=" | ",
  sbPrePendNewLine=FALSE, sbPostPendNewLine=TRUE, sbEndExtraNewLine=FALSE
) {

  rssPreSTUB_ <- ssPreStub;   # <- A standardized start to all posted notes ...

  rdtFuncSTART <- base::Sys.time();      # <- Extract Function START Time ...
  rssFormatDTI <- "%a, %b %d %Y @ %X";   # <- DateTime Format for "FuncSelfID" Process ...

  if (base::is.null(ssFuncCallerID)) {
    ssFuncCallerID <- base::get0(
      "rssTagProjID_",
      envir = .GlobalEnv,
      ifnotfound = "UNK. Proj. ID"
    );
  }

  if (base::is.null(ssFuncType)) {
    ssFuncType <- "Helper";   # <- Options: "LARGE" ...or... "Helper" Function !!!
  }

  if (sbRunSelfID) {
    base::cat(base::paste0(rssPreSTUB_, ssFuncSelfID, ssMidStub, "START  { F-Type: '", ssFuncType, "', Caller: '", ssFuncCallerID, "', Time: ", base::format(rdtFuncSTART, rssFormatDTI), " }\n"));
  }

  rdtFuncSTOP <- base::Sys.time();   # <- Extract Function STOP Time ...
  rcoFuncINFO <- base::list(         # <- Collate Key Function SelfID Information ...
    "FuncID" = ssFuncSelfID, "CallerID" = ssFuncCallerID,
    "FuncSTART" = rdtFuncSTART, "FuncSTOP" = rdtFuncSTOP, "FuncType" = ssFuncType
  )
  if (sbRunSelfID) {
    base::cat(base::paste0(rssPreSTUB_, ssFuncSelfID, ssMidStub, "STOP  { F-Type: '", ssFuncType, "', Caller: '", ssFuncCallerID, "', Time: ", base::format(rdtFuncSTOP, rssFormatDTI), " }\n"));
  }

  if (!sbRetFuncInfo) {
    if (siPostMode123 == 1L) {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(sbPrePendNewLine, "\n", ""),
            rssPreSTUB_, ssFuncSelfID, ssMidStub, "START  { F-Type: '", ssFuncType, "', Caller: '", ssFuncCallerID, "', Time: ", base::format(rdtFuncSTART, rssFormatDTI), " }",
            base::ifelse(sbPostPendNewLine, "\n", ""),
            base::ifelse(sbEndExtraNewLine, "\n", "")
          )
        )
      );
    } else if (siPostMode123 == 2L) {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(sbPrePendNewLine, "\n", ""),
            rssPreSTUB_, ssFuncCallerID, ssMidStub, ssPostNote,
            base::ifelse(sbPostPendNewLine, "\n", ""),
            base::ifelse(sbEndExtraNewLine, "\n", "")
          )
        )
      );
    } else {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(sbPrePendNewLine, "\n", ""),
            rssPreSTUB_, ssFuncSelfID, ssMidStub, "STOP  { F-Type: '", ssFuncType, "', Caller: '", ssFuncCallerID, "', Time: ", base::format(rdtFuncSTOP, rssFormatDTI), " }",
            base::ifelse(sbPostPendNewLine, "\n", ""),
            base::ifelse(sbEndExtraNewLine, "\n", "")
          )
        )
      );
    }
  } else {
    ssPostNoteFINAL <- NULL;
    if (siPostMode123 == 1L) {
      ssPostNoteFINAL <- base::paste0(
        rssPreSTUB_, ssFuncSelfID, ssMidStub, "START  { F-Type: '", ssFuncType, "', Caller: '", ssFuncCallerID, "', Time: ", base::format(rdtFuncSTART, rssFormatDTI), " }"
      );
    } else if (siPostMode123 == 2L) {
      ssPostNoteFINAL <- base::paste0("> ", ssFuncCallerID, " | ", ssPostNote, "");
    } else {
      ssPostNoteFINAL <- base::paste0(
        rssPreSTUB_, ssFuncSelfID, ssMidStub, "STOP  { F-Type: '", ssFuncType, "', Caller: '", ssFuncCallerID, "', Time: ", base::format(rdtFuncSTOP, rssFormatDTI), " }"
      );
    }
    base::return(base::list("Value" = ssPostNoteFINAL, "FI" = rcoFuncINFO));
  }
}

# info.post.note(sbRetFuncInfo = F, sbRunSelfID = F)

