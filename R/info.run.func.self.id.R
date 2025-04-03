


"info.run.func.self.id" <- function(
  sbRunSelfID=NULL, ssFuncCallerID=NULL, siFuncMode12=NULL, ssFormatDT=NULL,
  ssTimeStart=NULL, ssTimeStop=NULL, ssFuncType=NULL, sbRetFuncInfo=NULL, ...
) {

  ### STEP 1 - Define the "Function Self-ID" tag ... ####
  #            ( this👆 is THE ONLY FUNCTION [in the MFMR Suite of R Functions]
  #            THAT DOES NOT SELF-IDENTIFY !!! )
  ssFuncSelfID_ <- "Info-Run-Func-Self-ID";

  ### STEP 2 - Capture the Dots Function Arguments ... ####
  #            ( the "dots-args" will be handed over in subsequent steps )
  vsDotsArgs_ <- base::list(...);

  ### STEP 3 - Internalize ALL Function Arguments ... ####
  #            ( i.e. hand-over all to func-args to func-local variables )
  coFuncResList_ <- NULL;   # -> The <final> function outputs <results> object.
  ssFuncType_ <- ssFuncType; siFuncMode12_ <- siFuncMode12;
  sbRunSelfID_ <- sbRunSelfID; ssFuncCallerID_ <- ssFuncCallerID;
  ssTimeStart_ <- ssTimeStart; ssTimeStop_ <- ssTimeStop; ssFormatDT_ <- ssFormatDT;

  ### STEP 4 - Check if "Function Self-ID" was indeed requested ... ####
  if (!base::is.null(sbRunSelfID_) && sbRunSelfID_) {   # -> Checks if "sbRunSelfID"
                                                        #    is NOT NULL and if it
    # 4.1 - Check if "Func. Caller" was defined ...     #    is set to `TRUE` !!!
    if (base::is.null(ssFuncCallerID_)) {
      ssFuncCallerID_ <- base::get0(   # -> Searches the Global Environment of the
        "rssTagProjID_",               #    Active R Session for the <somewhat>
        envir = .GlobalEnv,            #    unique variable name "rssTagProjID_"
        ifnotfound = "UNK. Proj. ID"   #    and extracts the value contained in
      );                               #    that variable (if it exists) ... or
    }                                  #    else returns the "NOT-FOUND" value.

    # 4.2 - Check the "Function Mode" (1L: `ENTER` or 2L: `EXIT`) ...
    if (base::is.null(siFuncMode12_)) {
      siFuncMode12_ <- 1L;   # -> Sets the default Function mode to `ENTER` !!!
    }

    # 4.3 - Compile the "Func-Self-ID" notification text accordingly ...
    ssNoteSelfID_ <- NULL;   # -> Init the output notification variable !!!
    ssFormatDT_ <- base::ifelse(   # -> Ensures that DateTime Format is properly set !!!
      !base::is.null(ssFormatDT_), ssFormatDT_, MFMRDates[["LONGv03"]]
    );
    if (siFuncMode12_ == 1L) {   # -> Run the `ENTER` func. mode code logic ...

      # 4.3.1.1 - Compile the `ENTER` notification (Func-Self-ID) text ...
      if (base::is.null(vsDotsArgs_[["sbPrintPretty"]])) {
        ssNoteSelfID_ <- base::paste0(
          "START { F-Type: '",
          ssFuncType_, "' < Caller: '", ssFuncCallerID_, "' > Time: ",
          base::format(ssTimeStart_, ssFormatDT_), " }\n"
        );
      } else {
        if (vsDotsArgs_[["sbPrintPretty"]]) {
          ssNoteSelfID_ <- base::paste0(
            "START { F-Type: '",
            ssFuncType_, "' < Caller: '", ssFuncCallerID_, "' > Time: ",
            base::format(ssTimeStart_, ssFormatDT_), " }\n"
          );
        } else {
          ssNoteSelfID_ <- base::paste0(
            "START { F-Type: '",
            ssFuncType_, "' < Caller: '", ssFuncCallerID_, "' > Time: ",
            base::format(ssTimeStart_, ssFormatDT_), " }\n"
          );
        }
      }

    } else {   # -> Run the `EXIT` func. mode code logic ...

    }

    # 4.3.1.2 - Post the `ENTER` notification (Func-Self-ID) text ...
    MFMRutils::info.post.note(
      ssPostNote = ssNoteSelfID_,

      sbPrintPretty = vsDotsArgs_[["sbPrintPretty"]],
      ssFuncCallerID = vsDotsArgs_[["ssFuncCallerID"]]
    );

    # 4.3.1.3 - Output the `ENTER` "Func-Self-ID' properties ...
    coFuncResList_ <- base::list(
      "FuncType" = ssFuncType,
      "FuncSTART" = ssTimeStart_, "FuncSTOP" = ssTimeStop_,
      "FuncID" = ssFuncSelfID_, "CallerID" = ssFuncCallerID_
    );
    base::invisible(base::list("FuncInfo" = coFuncResList_));

  }

}

### rssTagProjID_ <- "Test R ProJ"

coResLst_ <- info.run.func.self.id(
  ssFuncType = code.classify.func(4, 94),
  ssFormatDT = MFMRDates$LONGv03, ssTimeStart = base::Sys.time(),
  sbRunSelfID = T, sbPrintPretty = F, ssFuncCallerID = "Test-Func-ID"
);
base::cat(base::paste0(" > Func ID -> ", coResLst_$FuncInfo$FuncID, "\n"))


