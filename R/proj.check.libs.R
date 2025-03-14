
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
  vsMissingProjectLibs <- base::c();    # <- Creates an empty vector ...
  vsOutdatedProjectLibs <- base::c();   # <- Creates an empty vector ...
  vsLibsUpdateSpecifics <- base::c();   # <- Creates an empty vector ...
  if (base::is.null(vsReqLibs)) {

    ssReqLibsNONE <- "No Libraries were defined for this R Project !!!"
    MFMRutils::info.post.note(
      ssPostNote = ssReqLibsNONE,
      siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );

    ssReqLibsVERIFY <- "Please VERIFY that the R Project DOES NOT USE any R Libraries before continuing ..."
    MFMRutils::info.post.note(
      ssPostNote = ssReqLibsVERIFY,
      siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );

  } else {

    # Notify user about package installation 'ACTIVE' state ...
    ssNoteLibsListCRAN <- base::paste0("Querying CRAN Repository for registered R Libraries ...");
    MFMRutils::info.post.note(
      ssPostNote = ssNoteLibsListCRAN,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
      siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID, sbPrePendNL = T
    );

    # Extract ALL R Libraries installed locally and listed on the CRAN Repo ...
    coMtrxArryLibsINSTALLED <- utils::installed.packages();
    coMtrxArryLibsCRANREGIS <- utils::available.packages();

    if (base::length(vsReqLibs) >= 1) {

      for (lib in vsReqLibs) {   # <- Iterates through "vsReqLibs" assessing list items one-by-one with the code below ...

        # Checks if library is not already installed on local machine & if TRUE (i.e. not installed), then appends it to the 'vsMissingProjectLibs' vector ...
        if (!(lib %in% coMtrxArryLibsINSTALLED[ , "Package"])) {
          vsMissingProjectLibs <- base::c(vsMissingProjectLibs, lib);
        } else {
          # Checks if newer versions of already installed libraries exist on the <remote/online> CRAN Repository ...
          if (sbUpdateLibs) {   # <- ... but does this ONLY IF the 'sbUpdateLibs' argument is set to TRUE !!!
            sdLibsVersLOCL <- coMtrxArryLibsINSTALLED[lib, "Version"];
            sdLibsVersCRAN <- coMtrxArryLibsCRANREGIS[lib, "Version"];
            if (sdLibsVersLOCL != sdLibsVersCRAN) {   # <- If there is a miss-match between the LOCAL vs. CRAN results, then it appends the lib to the 'vsOutdatedProjectLibs' vector ...
              vsOutdatedProjectLibs <- base::c(vsOutdatedProjectLibs, lib);
              vsLibsUpdateSpecifics <- base::c(vsLibsUpdateSpecifics, base::paste0(lib, "  v", sdLibsVersLOCL, "  =>  v", sdLibsVersCRAN));
            }
          }
        }
      }

      if (sbFixLibs && base::length(vsMissingProjectLibs) >= 1 ||
          sbFixLibs && base::length(vsOutdatedProjectLibs) >= 1) {

        for (lib in vsMissingProjectLibs) {   # <- Iterates through "vsMissingProjectLibs" assessing list items one-by-one with the code below ...

          # Notify user about package installation start ...
          ssNoteLibsINSTALLSTARTED <- base::paste0("INSTALL of R Package [ ", lib, " ] => STARTED: ", base::format(base::Sys.time(), ssFormatDT), " !!!");
          MFMRutils::info.post.note(
            sbPrePendNL = TRUE,
            ssPostNote = ssNoteLibsINSTALLSTARTED,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );

          # Notify user about package installation 'ACTIVE' state ...
          ssNoteLibsINSTALLACTIVE <- base::paste0("Please wait => installation of [ ", lib, " ] is currently underway ...");
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsINSTALLACTIVE,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
          utils::install.packages(
            lib, quiet = sbQuietInstall
          );   # <- Installs <specified> 3rd Party Library on local machine ...

          # Notify user about package installation completion ...
          ssNoteLibsINSTALLACTIVE <- base::paste0("INSTALL of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), ssFormatDT), " !!!\n");
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsINSTALLACTIVE,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
        }

        if (sbUpdateLibs && base::length(vsOutdatedProjectLibs) >= 1) {

          for (lib in vsOutdatedProjectLibs) {   # <- Iterates through "vsOutdatedProjectLibs" assessing list items one-by-one with the code below ...

            svVersNEW <- coMtrxArryLibsCRANREGIS[lib, 'Version'];
            svVersOLD <- coMtrxArryLibsINSTALLED[lib, 'Version'];

            # Notify user about package installation start ...
            ssNoteLibsINSTALLSTARTED <- base::paste0("UPDATE of R Package [ ", lib, " v", svVersOLD, " => v", svVersNEW," ] => STARTED: ", base::format(base::Sys.time(), ssFormatDT), " !!!");
            MFMRutils::info.post.note(
              sbPrePendNL = TRUE,
              ssPostNote = ssNoteLibsINSTALLSTARTED,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
            );

            # Notify user about package installation 'ACTIVE' state ...
            ssNoteLibsINSTALLACTIVE <- base::paste0("Please wait => updating of [ ", lib, " ] is currently underway ...");
            MFMRutils::info.post.note(
              ssPostNote = ssNoteLibsINSTALLACTIVE,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
            );
            utils::install.packages(
              lib, quiet = sbQuietInstall
            );   # <- Installs <specified> 3rd Party Library on local machine ...

            # Notify user about package installation completion ...
            ssNoteLibsINSTALLACTIVE <- base::paste0("UPDATE of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), ssFormatDT), " !!!\n");
            MFMRutils::info.post.note(
              ssPostNote = ssNoteLibsINSTALLACTIVE,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
            );
          }
        }

      } else {

        siVarLenMissingLibs <- base::length(vsMissingProjectLibs);
        siVarLenOutdatedLibs <- base::length(vsOutdatedProjectLibs);
        if (siVarLenMissingLibs >= 1) {

          # Notify user about the missing R libraries ...
          ssNoteLibsMISSING <- base::paste0(
            "The following ", base::ifelse(siVarLenMissingLibs == 1, "library is", "libraries are"), " required by this R Project, but currently NOT INSTALLED: -> [\n",
            ' "', base::paste0(vsMissingProjectLibs, collapse = '", "'), '"\n', "] <-"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsMISSING,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );

          # Notify user how to install the missing R libraries ...
          ssNoteLibsINSTALL <- base::paste0(
            "To install missing libraries, re-run this function and set the `sbFixLibs` argument to TRUE.\n"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsINSTALL,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
        }

        if (siVarLenOutdatedLibs >= 1) {

          # Notify user about the missing R libraries ...
          ssNoteLibsOUTDATED <- base::paste0(
            "The following already installed Project ", base::ifelse(siVarLenOutdatedLibs == 1, "library", "libraries"), " can be updated: -> [\n",
            ' ', base::paste0(vsLibsUpdateSpecifics, collapse = ',\n '), '\n', "] <-"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsOUTDATED,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );

          # Notify user how to install the missing R libraries ...
          ssNoteLibsUPDATE <- base::paste0(
            "To update installed libraries, re-run this function and set both the `sbFixLibs` & `sbUpdateLibs` arguments to TRUE.\n"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsUPDATE,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );
        }
      }

      # Notify user that ALL REQUIRED Project Libraries are already installed ...
      if (base::length(vsMissingProjectLibs) == 0 && base::length(coMtrxArryLibsCRANREGIS) >= 1) {
        ssNoteLibsINSTALL <- base::paste0(
          "All libraries required for this R Project are properly installed ..."
        );
        MFMRutils::info.post.note(
          ssPostNote = ssNoteLibsINSTALL,
          siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
          ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
          sbPrePendNL = TRUE, sbPostPend2ndNL = base::ifelse(sbShowLibs, FALSE, TRUE)
        );

        if (sbShowLibs) {
          ssNoteLibsREQUIRED <- base::paste0(
            "Libraries defined for this R Project are as follows: -> [\n",
            ' "', base::paste0(vsReqLibs, collapse = '", "'), '"',
            "\n ] <-"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsREQUIRED,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            sbPrePendNL = FALSE, sbPostPend2ndNL = TRUE,
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


proj.check.libs(vsReqLibs = c("ggplot2", "tidyr", "abind"))

