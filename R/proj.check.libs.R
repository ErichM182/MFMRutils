
"proj.check.libs" <- function(
  sbFixLibs=FALSE, sbQuietInstall=FALSE, ssFormatDT="%a, %b %d %Y %X",
  vsReqLibs=NULL, ssFuncType=NULL, sbShowLibs=TRUE, sbUpdateLibs=TRUE,
  ssFuncSelfID="Check Proj. Libs", sbRunSelfID=FALSE, ssFuncCallerID=NULL
) {

  dtFuncSTART <- base::Sys.time();      # <- Extract Function START Time ...
  ssFormatDTI <- "%a, %b %d %Y @ %X";   # <- DateTime Format for "FuncSelfID" Process ...

  if (base::is.null(ssFuncCallerID)) {
    ssFuncCallerID <- base::get0(
      "rssTagProjID_",
      envir = .GlobalEnv,
      ifnotfound = "UNDEFINED"
    );
  }

  if (base::is.null(ssFuncType)) {
    ssFuncType <- "Helper";   # <- Options: "LARGE" ...or... "Helper" Function !!!
  }

  if (sbRunSelfID) {
    MFMRutils::info.post.note(
      siPostMode123 = 1, ssFuncSelfID = ssFuncSelfID,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );
  }

  # !!! ... ADD 'Helper Func' CODE LOGIC here ... !!!
  rvsMissingProjectLibs <- base::c();    # <- Creates an empty vector ...
  rvsOutdatedProjectLibs <- base::c();   # <- Creates an empty vector ...
  rvsLibsUpdateSpecifics <- base::c();   # <- Creates an empty vector ...
  if (base::is.null(vsReqLibs)) {

    rssReqLibsNONE <- "No Libraries were defined for this R Project !!!"
    MFMRutils::info.post.note(
      rssPostNote = rssReqLibsNONE,
      siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );

    rssReqLibsVERIFY <- "Please VERIFY that the R Project DOES NOT USE any R Libraries before continuing ..."
    MFMRutils::info.post.note(
      rssPostNote = rssReqLibsVERIFY,
      siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );

  } else {

    # Notify user about package installation 'ACTIVE' state ...
    rssNoteLibsListCRAN <- base::paste0("Querying CRAN Repository for registered R Libraries ...");
    MFMRutils::info.post.note(
      rssPostNote = rssNoteLibsListCRAN,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
      siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID, rsbPrePendNewLine = T
    );

    # Extract ALL R Libraries installed locally and listed on the CRAN Repo ...
    rcoMtrxArryLibsINSTALLED <- utils::installed.packages();
    rcoMtrxArryLibsCRANREGIS <- utils::available.packages();

    if (base::length(vsReqLibs) >= 1) {

      for (lib in vsReqLibs) {   # <- Iterates through "vsReqLibs" assessing list items one-by-one with the code below ...

        # Checks if library is not already installed on local machine & if TRUE (i.e. not installed), then appends it to the 'rvsMissingProjectLibs' vector ...
        if (!(lib %in% rcoMtrxArryLibsINSTALLED[ , "Package"])) {
          rvsMissingProjectLibs <- base::c(rvsMissingProjectLibs, lib);
        } else {
          # Checks if newer versions of already installed libraries exist on the <remote/online> CRAN Repository ...
          if (sbUpdateLibs) {   # <- ... but does this ONLY IF the 'sbUpdateLibs' argument is set to TRUE !!!
            rsdLibsVersLOCL <- rcoMtrxArryLibsINSTALLED[lib, "Version"];
            rsdLibsVersCRAN <- rcoMtrxArryLibsCRANREGIS[lib, "Version"];
            if (rsdLibsVersLOCL != rsdLibsVersCRAN) {   # <- If there is a miss-match between the LOCAL vs. CRAN results, then it appends the lib to the 'rvsOutdatedProjectLibs' vector ...
              rvsOutdatedProjectLibs <- base::c(rvsOutdatedProjectLibs, lib);
              rvsLibsUpdateSpecifics <- base::c(rvsLibsUpdateSpecifics, base::paste0(lib, "  v", rsdLibsVersLOCL, "  =>  v", rsdLibsVersCRAN));
            }
          }
        }
      }

      if (sbFixLibs && base::length(rvsMissingProjectLibs) >= 1 ||
          sbFixLibs && base::length(rvsOutdatedProjectLibs) >= 1) {

        for (lib in rvsMissingProjectLibs) {   # <- Iterates through "rvsMissingProjectLibs" assessing list items one-by-one with the code below ...

          # Notify user about package installation start ...
          rssNoteLibsInstallSTARTED <- base::paste0("INSTALL of R Package [ ", lib, " ] => STARTED: ", base::format(base::Sys.time(), ssFormatDT), " !!!");
          MFMRutils::info.post.note(
            rsbPrePendNewLine = TRUE,
            rssPostNote = rssNoteLibsInstallSTARTED,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );

          # Notify user about package installation 'ACTIVE' state ...
          rssNoteLibsInstallACTIVE <- base::paste0("Please wait => installation of [ ", lib, " ] is currently underway ...");
          MFMRutils::info.post.note(
            rssPostNote = rssNoteLibsInstallACTIVE,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
          utils::install.packages(
            lib, quiet = sbQuietInstall
          );   # <- Installs <specified> 3rd Party Library on local machine ...

          # Notify user about package installation completion ...
          rssNoteLibsInstallACTIVE <- base::paste0("INSTALL of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), ssFormatDT), " !!!\n");
          MFMRutils::info.post.note(
            rssPostNote = rssNoteLibsInstallACTIVE,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
        }

        if (sbUpdateLibs && base::length(rvsOutdatedProjectLibs) >= 1) {

          for (lib in rvsOutdatedProjectLibs) {   # <- Iterates through "rvsOutdatedProjectLibs" assessing list items one-by-one with the code below ...

            rsvVersNEW <- rcoMtrxArryLibsCRANREGIS[lib, 'Version'];
            rsvVersOLD <- rcoMtrxArryLibsINSTALLED[lib, 'Version'];

            # Notify user about package installation start ...
            rssNoteLibsInstallSTARTED <- base::paste0("UPDATE of R Package [ ", lib, " v", rsvVersOLD, " => v", rsvVersNEW," ] => STARTED: ", base::format(base::Sys.time(), ssFormatDT), " !!!");
            MFMRutils::info.post.note(
              rsbPrePendNewLine = TRUE,
              rssPostNote = rssNoteLibsInstallSTARTED,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
            );

            # Notify user about package installation 'ACTIVE' state ...
            rssNoteLibsInstallACTIVE <- base::paste0("Please wait => updating of [ ", lib, " ] is currently underway ...");
            MFMRutils::info.post.note(
              rssPostNote = rssNoteLibsInstallACTIVE,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
            );
            utils::install.packages(
              lib, quiet = sbQuietInstall
            );   # <- Installs <specified> 3rd Party Library on local machine ...

            # Notify user about package installation completion ...
            rssNoteLibsInstallACTIVE <- base::paste0("UPDATE of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), ssFormatDT), " !!!\n");
            MFMRutils::info.post.note(
              rssPostNote = rssNoteLibsInstallACTIVE,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
            );
          }
        }

      } else {

        rsiVarLenMissingLibs <- base::length(rvsMissingProjectLibs);
        rsiVarLenOutdatedLibs <- base::length(rvsOutdatedProjectLibs);
        if (rsiVarLenMissingLibs >= 1) {

          # Notify user about the missing R libraries ...
          rssNoteLibsMISSING <- base::paste0(
            "The following ", base::ifelse(rsiVarLenMissingLibs == 1, "library is", "libraries are"), " required by this R Project, but currently NOT INSTALLED: -> [\n",
            ' "', base::paste0(rvsMissingProjectLibs, collapse = '", "'), '"\n', "] <-"
          );
          MFMRutils::info.post.note(
            rssPostNote = rssNoteLibsMISSING,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );

          # Notify user how to install the missing R libraries ...
          rssNoteLibsINSTALL <- base::paste0(
            "To install missing libraries, re-run this function and set the `sbFixLibs` argument to TRUE.\n"
          );
          MFMRutils::info.post.note(
            rssPostNote = rssNoteLibsINSTALL,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
        }

        if (rsiVarLenOutdatedLibs >= 1) {

          # Notify user about the missing R libraries ...
          rssNoteLibsOUTDATED <- base::paste0(
            "The following already installed Project ", base::ifelse(rsiVarLenOutdatedLibs == 1, "library", "libraries"), " can be updated: -> [\n",
            ' ', base::paste0(rvsLibsUpdateSpecifics, collapse = ',\n '), '\n', "] <-"
          );
          MFMRutils::info.post.note(
            rssPostNote = rssNoteLibsOUTDATED,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );

          # Notify user how to install the missing R libraries ...
          rssNoteLibsUPDATE <- base::paste0(
            "To update installed libraries, re-run this function and set both the `sbFixLibs` & `sbUpdateLibs` arguments to TRUE.\n"
          );
          MFMRutils::info.post.note(
            rssPostNote = rssNoteLibsUPDATE,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
        }
      }

      # Notify user that ALL REQUIRED Project Libraries are already installed ...
      if (base::length(rvsMissingProjectLibs) == 0 && base::length(rcoMtrxArryLibsCRANREGIS) >= 1) {
        rssNoteLibsINSTALL <- base::paste0(
          "All libraries required for this R Project are properly installed ..."
        );
        MFMRutils::info.post.note(
          rssPostNote = rssNoteLibsINSTALL,
          siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
          ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
          rsbPrePendNewLine = TRUE, rsbEndExtraNewLine = base::ifelse(sbShowLibs, FALSE, TRUE)
        );

        if (sbShowLibs) {
          rssNoteLibsREQUIRED <- base::paste0(
            "Libraries defined for this R Project are as follows: -> [\n",
            ' "', base::paste0(vsReqLibs, collapse = '", "'), '"',
            "\n ] <-"
          );
          MFMRutils::info.post.note(
            rssPostNote = rssNoteLibsREQUIRED,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            rsbPrePendNewLine = FALSE, rsbEndExtraNewLine = TRUE,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
        }
      }
    }
  }

  dtFuncSTOP <- base::Sys.time();   # <- Extract Function STOP Time ...
  coFuncINFO <- base::list(         # <- Collate Key Function Self-ID Information ...
    "FuncID" = ssFuncSelfID, "CallerID" = ssFuncCallerID,
    "FuncSTART" = dtFuncSTART, "FuncSTART" = dtFuncSTOP, "FuncType" = ssFuncType
  )
  if (sbRunSelfID) {
    MFMRutils::info.post.note(
      siPostMode123 = 3, ssFuncSelfID = ssFuncSelfID,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );
  }

  base::return(
    base::invisible(base::list("ReqLibs" = vsReqLibs, "FI" = coFuncINFO))
  );
}


