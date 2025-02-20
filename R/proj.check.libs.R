



# Function to install required R Project Libraries that are not already installed ...
"proj.check.libs" <- function(
    rsbFixLibs=FALSE, rsbQuietInstall=FALSE, rssFormatDT="%a, %b %d %Y %X",
    rvsReqLibs=NULL, rssFuncType=NULL, rsbShowLibs=TRUE, rsbUpdateLibs=TRUE,
    rssFuncSelfID="Check Proj. Libs", rsbRunSelfID=FALSE, rssFuncCallerID=NULL
) {

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
    rcf_utils.post.note(
      rsiPostMode123 = 1, rssFuncSelfID = rssFuncSelfID,
      rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
    );
  }

  # !!! ... ADD 'Helper Func' CODE LOGIC here ... !!!
  rvsMissingProjectLibs <- base::c();    # <- Creates an empty vector ...
  rvsOutdatedProjectLibs <- base::c();   # <- Creates an empty vector ...
  rvsLibsUpdateSpecifics <- base::c();   # <- Creates an empty vector ...
  if (base::is.null(rvsReqLibs)) {

    rssReqLibsNONE <- "No Libraries were defined for this R Project !!!"
    rcf_utils.post.note(
      rssPostNote = rssReqLibsNONE,
      rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
      rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
    );

    rssReqLibsVERIFY <- "Please VERIFY that the R Project DOES NOT USE any R Libraries before continuing ..."
    rcf_utils.post.note(
      rssPostNote = rssReqLibsVERIFY,
      rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
      rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
    );

  } else {

    # Notify user about package installation 'ACTIVE' state ...
    rssNoteLibsListCRAN <- base::paste0("Querying CRAN Repository for registered R Libraries ...");
    rcf_utils.post.note(
      rssPostNote = rssNoteLibsListCRAN,
      rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType,
      rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID, rsbPrePendNewLine = T
    );

    # Extract ALL R Libraries installed locally and listed on the CRAN Repo ...
    rcoMtrxArryLibsINSTALLED <- utils::installed.packages();
    rcoMtrxArryLibsCRANREGIS <- utils::available.packages();

    if (base::length(rvsReqLibs) >= 1) {

      for (lib in rvsReqLibs) {   # <- Iterates through "rvsReqLibs" assessing list items one-by-one with the code below ...

        # Checks if library is not already installed on local machine & if TRUE (i.e. not installed), then appends it to the 'rvsMissingProjectLibs' vector ...
        if (!(lib %in% rcoMtrxArryLibsINSTALLED[ , "Package"])) {
          rvsMissingProjectLibs <- base::c(rvsMissingProjectLibs, lib);
        } else {
          # Checks if newer versions of already installed libraries exist on the <remote/online> CRAN Repository ...
          if (rsbUpdateLibs) {   # <- ... but does this ONLY IF the 'rsbUpdateLibs' argument is set to TRUE !!!
            rsdLibsVersLOCL <- rcoMtrxArryLibsINSTALLED[lib, "Version"];
            rsdLibsVersCRAN <- rcoMtrxArryLibsCRANREGIS[lib, "Version"];
            if (rsdLibsVersLOCL != rsdLibsVersCRAN) {   # <- If there is a miss-match between the LOCAL vs. CRAN results, then it appends the lib to the 'rvsOutdatedProjectLibs' vector ...
              rvsOutdatedProjectLibs <- base::c(rvsOutdatedProjectLibs, lib);
              rvsLibsUpdateSpecifics <- base::c(rvsLibsUpdateSpecifics, base::paste0(lib, "  v", rsdLibsVersLOCL, "  =>  v", rsdLibsVersCRAN));
            }
          }
        }
      }

      if (rsbFixLibs && base::length(rvsMissingProjectLibs) >= 1 ||
          rsbFixLibs && base::length(rvsOutdatedProjectLibs) >= 1) {

        for (lib in rvsMissingProjectLibs) {   # <- Iterates through "rvsMissingProjectLibs" assessing list items one-by-one with the code below ...

          # Notify user about package installation start ...
          rssNoteLibsInstallSTARTED <- base::paste0("INSTALL of R Package [ ", lib, " ] => STARTED: ", base::format(base::Sys.time(), rssFormatDT), " !!!");
          rcf_utils.post.note(
            rsbPrePendNewLine = TRUE,
            rssPostNote = rssNoteLibsInstallSTARTED,
            rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
            rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
          );

          # Notify user about package installation 'ACTIVE' state ...
          rssNoteLibsInstallACTIVE <- base::paste0("Please wait => installation of [ ", lib, " ] is currently underway ...");
          rcf_utils.post.note(
            rssPostNote = rssNoteLibsInstallACTIVE,
            rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
            rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
          );
          utils::install.packages(
            lib, quiet = rsbQuietInstall
          );   # <- Installs <specified> 3rd Party Library on local machine ...

          # Notify user about package installation completion ...
          rssNoteLibsInstallACTIVE <- base::paste0("INSTALL of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), rssFormatDT), " !!!\n");
          rcf_utils.post.note(
            rssPostNote = rssNoteLibsInstallACTIVE,
            rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
            rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
          );
        }

        if (rsbUpdateLibs && base::length(rvsOutdatedProjectLibs) >= 1) {

          for (lib in rvsOutdatedProjectLibs) {   # <- Iterates through "rvsOutdatedProjectLibs" assessing list items one-by-one with the code below ...

            rsvVersNEW <- rcoMtrxArryLibsCRANREGIS[lib, 'Version'];
            rsvVersOLD <- rcoMtrxArryLibsINSTALLED[lib, 'Version'];

            # Notify user about package installation start ...
            rssNoteLibsInstallSTARTED <- base::paste0("UPDATE of R Package [ ", lib, " v", rsvVersOLD, " => v", rsvVersNEW," ] => STARTED: ", base::format(base::Sys.time(), rssFormatDT), " !!!");
            rcf_utils.post.note(
              rsbPrePendNewLine = TRUE,
              rssPostNote = rssNoteLibsInstallSTARTED,
              rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
              rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
            );

            # Notify user about package installation 'ACTIVE' state ...
            rssNoteLibsInstallACTIVE <- base::paste0("Please wait => updating of [ ", lib, " ] is currently underway ...");
            rcf_utils.post.note(
              rssPostNote = rssNoteLibsInstallACTIVE,
              rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
              rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
            );
            utils::install.packages(
              lib, quiet = rsbQuietInstall
            );   # <- Installs <specified> 3rd Party Library on local machine ...

            # Notify user about package installation completion ...
            rssNoteLibsInstallACTIVE <- base::paste0("UPDATE of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), rssFormatDT), " !!!\n");
            rcf_utils.post.note(
              rssPostNote = rssNoteLibsInstallACTIVE,
              rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
              rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
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
          rcf_utils.post.note(
            rssPostNote = rssNoteLibsMISSING,
            rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
            rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
          );

          # Notify user how to install the missing R libraries ...
          rssNoteLibsINSTALL <- base::paste0(
            "To install missing libraries, re-run this function and set the `rsbFixLibs` argument to TRUE.\n"
          );
          rcf_utils.post.note(
            rssPostNote = rssNoteLibsINSTALL,
            rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
            rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
          );
        }

        if (rsiVarLenOutdatedLibs >= 1) {

          # Notify user about the missing R libraries ...
          rssNoteLibsOUTDATED <- base::paste0(
            "The following already installed Project ", base::ifelse(rsiVarLenOutdatedLibs == 1, "library", "libraries"), " can be updated: -> [\n",
            ' ', base::paste0(rvsLibsUpdateSpecifics, collapse = ',\n '), '\n', "] <-"
          );
          rcf_utils.post.note(
            rssPostNote = rssNoteLibsOUTDATED,
            rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
            rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
          );

          # Notify user how to install the missing R libraries ...
          rssNoteLibsUPDATE <- base::paste0(
            "To update installed libraries, re-run this function and set both the `rsbFixLibs` & `rsbUpdateLibs` arguments to TRUE.\n"
          );
          rcf_utils.post.note(
            rssPostNote = rssNoteLibsUPDATE,
            rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
            rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
          );
        }
      }

      # Notify user that ALL REQUIRED Project Libraries are already installed ...
      if (base::length(rvsMissingProjectLibs) == 0 && base::length(rcoMtrxArryLibsCRANREGIS) >= 1) {
        rssNoteLibsINSTALL <- base::paste0(
          "All libraries required for this R Project are properly installed ..."
        );
        rcf_utils.post.note(
          rssPostNote = rssNoteLibsINSTALL,
          rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
          rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType,
          rsbPrePendNewLine = TRUE, rsbEndExtraNewLine = base::ifelse(rsbShowLibs, FALSE, TRUE)
        );

        if (rsbShowLibs) {
          rssNoteLibsREQUIRED <- base::paste0(
            "Libraries defined for this R Project are as follows: -> [\n",
            ' "', base::paste0(rvsReqLibs, collapse = '", "'), '"',
            "\n ] <-"
          );
          rcf_utils.post.note(
            rssPostNote = rssNoteLibsREQUIRED,
            rsiPostMode123 = 2, rssFuncSelfID = rssFuncSelfID,
            rsbPrePendNewLine = FALSE, rsbEndExtraNewLine = TRUE,
            rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
          );
        }
      }
    }
  }

  rdtFuncSTOP <- base::Sys.time();   # <- Extract Function STOP Time ...
  rcoFuncINFO <- base::list(         # <- Collate Key Function SelfID Information ...
    "FuncID" = rssFuncSelfID, "CallerID" = rssFuncCallerID,
    "FuncSTART" = rdtFuncSTART, "FuncSTART" = rdtFuncSTOP, "FuncType" = rssFuncType
  )
  if (rsbRunSelfID) {
    rcf_utils.post.note(
      rsiPostMode123 = 3, rssFuncSelfID = rssFuncSelfID,
      rssFuncCallerID = rssFuncCallerID, rssFuncType = rssFuncType
    );
  }

  base::return(
    base::invisible(base::list("ProjLibs" = rvsReqLibs, "FI" = rcoFuncINFO))
  );
}
