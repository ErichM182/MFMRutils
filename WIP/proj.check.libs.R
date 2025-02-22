



# Function to install required R Project Libraries that are not already installed ...
"proj.check.libs" <- function(
  vsReqLibs=NULL, ssFuncSelfID="Check Proj. Libs", sbRunSelfID=FALSE,
  sbFixLibs=FALSE, sbQuietInstall=FALSE, ssFormatDT="%a, %b %d %Y @ %X",
  ssFuncCallerID=NULL, ssFuncType=NULL, sbShowLibs=TRUE, sbUpdateLibs=TRUE
) {

  rdtFuncSTART <- base::Sys.time();   # <- Extract Function START Time ...
  ssFormatDTI <- ssFormatDT;          # <- DateTime Format for "FuncSelfID" Process ...

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
    MFMRutils::info.post.note(
      ssFormatDT = ssFormatDTI,
      siPostMode123 = 1L, ssFuncSelfID = ssFuncSelfID,
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
      siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
      ssPostNote = rssReqLibsNONE, ssFormatDT = ssFormatDTI,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );

    rssReqLibsVERIFY <- "Please VERIFY which R Libraries this R Project uses before continuing ..."
    MFMRutils::info.post.note(
      siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
      ssPostNote = rssReqLibsVERIFY, ssFormatDT = ssFormatDTI,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );

  } else {

    # Notify user about package installation 'ACTIVE' state ...
    rssNoteLibsListCRAN <- base::paste0("Querying CRAN Repository for registered R Libraries ...");
    MFMRutils::info.post.note(
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
      ssPostNote = rssNoteLibsListCRAN, ssFormatDT = ssFormatDTI,
      siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID, sbPrePendNewLine = T
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
            ssPostNote = rssNoteLibsInstallSTARTED,
            siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
            sbPrePendNewLine = TRUE, ssFormatDT = ssFormatDTI,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
          );

          # Notify user about package installation 'ACTIVE' state ...
          rssNoteLibsInstallACTIVE <- base::paste0("Please wait => installation of [ ", lib, " ] is currently underway ...");
          MFMRutils::info.post.note(
            siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
            ssPostNote = rssNoteLibsInstallACTIVE, ssFormatDT = ssFormatDTI
          );
          utils::install.packages(
            lib, quiet = sbQuietInstall
          );   # <- Installs <specified> 3rd Party Library on local machine ...

          # Notify user about package installation completion ...
          rssNoteLibsInstallACTIVE <- base::paste0("INSTALL of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), ssFormatDT), " !!!\n");
          MFMRutils::info.post.note(
            siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
            ssPostNote = rssNoteLibsInstallACTIVE, ssFormatDT = ssFormatDTI
          );
        }

        if (sbUpdateLibs && base::length(rvsOutdatedProjectLibs) >= 1) {

          for (lib in rvsOutdatedProjectLibs) {   # <- Iterates through "rvsOutdatedProjectLibs" assessing list items one-by-one with the code below ...

            rsvVersNEW <- rcoMtrxArryLibsCRANREGIS[lib, 'Version'];
            rsvVersOLD <- rcoMtrxArryLibsINSTALLED[lib, 'Version'];

            # Notify user about package installation start ...
            rssNoteLibsInstallSTARTED <- base::paste0("UPDATE of R Package [ ", lib, " v", rsvVersOLD, " => v", rsvVersNEW," ] => STARTED: ", base::format(base::Sys.time(), ssFormatDT), " !!!");
            MFMRutils::info.post.note(
              ssPostNote = rssNoteLibsInstallSTARTED,
              siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
              sbPrePendNewLine = TRUE, ssFormatDT = ssFormatDTI,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
            );

            # Notify user about package installation 'ACTIVE' state ...
            rssNoteLibsInstallACTIVE <- base::paste0("Please wait => updating of [ ", lib, " ] is currently underway ...");
            MFMRutils::info.post.note(
              siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
              ssPostNote = rssNoteLibsInstallACTIVE, ssFormatDT = ssFormatDTI
            );
            utils::install.packages(
              lib, quiet = sbQuietInstall
            );   # <- Installs <specified> 3rd Party Library on local machine ...

            # Notify user about package installation completion ...
            rssNoteLibsInstallACTIVE <- base::paste0("UPDATE of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), ssFormatDT), " !!!\n");
            MFMRutils::info.post.note(
              siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
              ssPostNote = rssNoteLibsInstallACTIVE, ssFormatDT = ssFormatDTI
            );
          }
        }

      } else {

        rsiVarLenMissingLibs <- base::length(rvsMissingProjectLibs);
        rsiVarLenOutdatedLibs <- base::length(rvsOutdatedProjectLibs);
        if (rsiVarLenMissingLibs >= 1) {

          # Notify user about the missing R libraries ...
          rssNoteLibsMISSING <- base::paste0(
            "The following R ", base::ifelse(rsiVarLenMissingLibs == 1, "library was listed, but is", "libraries were listed, but are"), " currently NOT INSTALLED: -> [\n",
            ' "', base::paste0(rvsMissingProjectLibs, collapse = '", "'), '"\n', "] <-"
          );
          MFMRutils::info.post.note(
            siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
            ssPostNote = rssNoteLibsMISSING, ssFormatDT = ssFormatDTI
          );

          # Notify user how to install the missing R libraries ...
          rssNoteLibsINSTALL <- base::paste0(
            "To install missing libraries, re-run this function and set the `sbFixLibs` argument to TRUE.\n"
          );
          MFMRutils::info.post.note(
            siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
            ssPostNote = rssNoteLibsINSTALL, ssFormatDT = ssFormatDTI
          );
        }

        if (rsiVarLenOutdatedLibs >= 1) {

          # Notify user about the missing R libraries ...
          rssNoteLibsOUTDATED <- base::paste0(
            "The following already installed R ", base::ifelse(rsiVarLenOutdatedLibs == 1, "library", "libraries"), " can be updated: -> [\n",
            ' ', base::paste0(rvsLibsUpdateSpecifics, collapse = ',\n '), '\n', "] <-"
          );
          MFMRutils::info.post.note(
            siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
            ssPostNote = rssNoteLibsOUTDATED, ssFormatDT = ssFormatDTI
          );

          # Notify user how to install the missing R libraries ...
          rssNoteLibsUPDATE <- base::paste0(
            "To update installed libraries, re-run this function and set both the `sbFixLibs` & `sbUpdateLibs` arguments to TRUE.\n"
          );
          MFMRutils::info.post.note(
            siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
            ssPostNote = rssNoteLibsUPDATE, ssFormatDT = ssFormatDTI
          );
        }
      }

      if (rsiVarLenMissingLibs == 0 && rsiVarLenOutdatedLibs == 0) {
        # Notify user that ALL REQUIRED Project Libraries are already installed ...
        if (base::length(rvsMissingProjectLibs) == 0 && base::length(rcoMtrxArryLibsCRANREGIS) >= 1) {
          rssNoteLibsINSTALL <- base::paste0(
            "All required libraries are properly installed ..."
          );
          MFMRutils::info.post.note(
            siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
            ssPostNote = rssNoteLibsINSTALL, ssFormatDT = ssFormatDTI,
            sbPrePendNewLine = TRUE, sbEndExtraNewLine = base::ifelse(sbShowLibs, FALSE, TRUE)
          );

          if (sbShowLibs) {
            rssNoteLibsREQUIRED <- base::paste0(
              "The specified", base::ifelse(base::length(vsReqLibs) >= 2, "libraries were", "library was"), " identified as follows: -> [\n",
              ' "', base::paste0(vsReqLibs, collapse = '", "'), '"',
              "\n ] <-"
            );
            MFMRutils::info.post.note(
              siPostMode123 = 2L, ssFuncSelfID = ssFuncSelfID,
              sbPrePendNewLine = FALSE, sbEndExtraNewLine = TRUE,
              ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
              ssPostNote = rssNoteLibsREQUIRED, ssFormatDT = ssFormatDTI
            );
          }
        }
      }
    }
  }

  rdtFuncSTOP <- base::Sys.time();   # <- Extract Function STOP Time ...
  rcoFuncINFO <- base::list(         # <- Collate Key Function SelfID Information ...
    "FuncID" = ssFuncSelfID, "CallerID" = ssFuncCallerID,
    "FuncSTART" = rdtFuncSTART, "FuncSTART" = rdtFuncSTOP, "FuncType" = ssFuncType
  )
  if (sbRunSelfID) {
    MFMRutils::info.post.note(
      ssFormatDT = ssFormatDTI,
      siPostMode123 = 3L, ssFuncSelfID = ssFuncSelfID,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType
    );
  }

  base::return(
    base::invisible(base::list("ProjLibs" = vsReqLibs, "FI" = rcoFuncINFO))
  );
}



proj.check.libs(
  ssFuncCallerID = "UBER Project",
  vsReqLibs = c("doBy", "bench", "cli", "llama", "LLM", "LLSR")
)
