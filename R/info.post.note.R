

"info.post.note" <- function(
  siPostMode123=2, sbRetFI=FALSE,
) {

}


# VERY NB -> Helper Function that standardizes the User INFO Posting Processes ...
"rcf_utils.post.note" <- function(
    rsiPostMode123=2, rsbRetFI=FALSE,
    rsbRunSelfID=FALSE, rssFuncCallerID=NULL, rssFuncType=NULL,
    rssFuncSelfID="Func Self-ID", rssPostNote="=> ADD a VALID POST NOTE !!!",
    rsbPrePendNewLine=FALSE, rsbPostPendNewLine=TRUE, rsbEndExtraNewLine=FALSE
) {

  rssPreSTUB_ <- " => ";   # <- A standardized start to all posted notes ...

  rdtFuncSTART <- base::Sys.time();      # <- Extract Function START Time ...
  rssFormatDTI <- "%a, %b %d %Y @ %X";   # <- DateTime Format for "FuncSelfID" Process ...

  if (base::is.null(rssFuncCallerID)) {
    rssFuncCallerID <- base::get0(
      "rssTagProjID_",
      envir = .GlobalEnv,
      ifnotfound = "UNDEFINED"
    );
  }

  if (base::is.null(rssFuncType)) {
    rssFuncType <- "Helper";   # <- Options: "LARGE" ...or... "Helper" Function !!!
  }

  if (rsbRunSelfID) {
    base::cat(base::paste0(rssPreSTUB_, "Post Note | START  { F-Type: '", rssFuncType, "', Caller: '", rssFuncCallerID, "', Time: ", base::format(rdtFuncSTART, rssFormatDTI), " }\n"));
  }

  rdtFuncSTOP <- base::Sys.time();   # <- Extract Function STOP Time ...
  rcoFuncINFO <- base::list(         # <- Collate Key Function SelfID Information ...
    "FuncID" = rssFuncSelfID, "CallerID" = rssFuncCallerID,
    "FuncSTART" = rdtFuncSTART, "FuncSTART" = rdtFuncSTOP, "FuncType" = rssFuncType
  )
  if (rsbRunSelfID) {
    base::cat(base::paste0(rssPreSTUB_, "Post Note | STOP  { F-Type: '", rssFuncType, "', Caller: '", rssFuncCallerID, "', Time: ", base::format(rdtFuncSTOP, rssFormatDTI), " }\n"));
  }

  if (!rsbRetFI) {
    if (rsiPostMode123 == 1) {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(rsbPrePendNewLine, "\n", ""),
            rssPreSTUB_, rssFuncSelfID, " | START  { F-Type: '", rssFuncType, "', Caller: '", rssFuncCallerID, "', Time: ", base::format(rdtFuncSTART, rssFormatDTI), " }",
            base::ifelse(rsbPostPendNewLine, "\n", ""),
            base::ifelse(rsbEndExtraNewLine, "\n", "")
          )
        )
      );
    } else if (rsiPostMode123 == 2) {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(rsbPrePendNewLine, "\n", ""),
            rssPreSTUB_, rssFuncCallerID, " | ", rssPostNote,
            base::ifelse(rsbPostPendNewLine, "\n", ""),
            base::ifelse(rsbEndExtraNewLine, "\n", "")
          )
        )
      );
    } else {
      base::return(
        base::cat(
          base::paste0(
            base::ifelse(rsbPrePendNewLine, "\n", ""),
            rssPreSTUB_, rssFuncSelfID, " | STOP  { F-Type: '", rssFuncType, "', Caller: '", rssFuncCallerID, "', Time: ", base::format(rdtFuncSTOP, rssFormatDTI), " }",
            base::ifelse(rsbPostPendNewLine, "\n", ""),
            base::ifelse(rsbEndExtraNewLine, "\n", "")
          )
        )
      );
    }
  } else {
    rssPostNoteFINAL <- NULL;
    if (rsiPostMode123 == 1) {
      rssPostNoteFINAL <- base::paste0(
        rssPreSTUB_, rssFuncSelfID, " | START  { F-Type: '", rssFuncType, "', Caller: '", rssFuncCallerID, "', Time: ", base::format(rdtFuncSTART, rssFormatDTI), " }"
      );
    } else if (rsiPostMode123 == 2) {
      rssPostNoteFINAL <- base::paste0("> ", rssFuncCallerID, " | ", rssPostNote, "");
    } else {
      rssPostNoteFINAL <- base::paste0(
        rssPreSTUB_, rssFuncSelfID, " | STOP  { F-Type: '", rssFuncType, "', Caller: '", rssFuncCallerID, "', Time: ", base::format(rdtFuncSTOP, rssFormatDTI), " }"
      );
    }
    base::return(base::list("Value" = rssPostNoteFINAL, "FI" = rcoFuncINFO));
  }
}
