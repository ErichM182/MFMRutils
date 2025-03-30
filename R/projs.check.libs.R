#? ### ### ### ### ### ### ###
#' @title Install required R Project Libraries
#' @description
#' A Helper Function to install 3rd Party R Libraries required by an R Project
#' but that are not currently installed. This custom function was intended to
#' mainly support the MFMR Suite of R Functions, but can be utilized as a
#' standalone function in other R packages.
#'
#' @param vsReqLibs a vector argument containing the discrete (i.e. comma-separated)
#'                  character values (i.e. strings or texts) of the 3rd Party R
#'                  Libraries required for the active R project.
#' @param sbFixLibs a logical (boolean) argument that specifies whether to fix (
#'                  i.e. install or update) any missing required R libraries.
#' @param sbQuietInstall a logical (boolean) argument that specifies whether the
#'                       R library install or update should be verbose or not.
#' @param sbShowLibs a logical (boolean) argument specifies whether the input R
#'                   libraries (i.e. required R libraries) should be printed out
#'                   to the R Console or returned to the function output (result)
#'                   at the completion of the function code execution.
#' @param sbUpdateLibs a logical (boolean) argument that specifies whether any
#'                     outdated R libraries should be updated or not. If set to
#'                     [FALSE] the function will merely post all outdated R libs
#'                     to the R Console and not take any further action towards
#'                     "fixing" the outdated R libraries.
#' @param ssFuncType a character vector (string or text) that specifies the
#'                   function type. OPTIONS: "Helper" (small to medium custom
#'                   functions; less than 300 lines of code) and "LARGE" (massive
#'                   custom function with >300 lines of code). If set to NULL
#'                   the function type will be assigned the default value of "Helper".
#' @param ssFormatDT a character vector (string or text) that specifies the
#'                   DateTime format to be used for displaying date-times in
#'                   the console.
#' @param ssFuncSelfID a character vector (i.e. string or text) serving as a
#'                     self-identifier (tag) for this "projs.check.libs()" function.
#' @param sbRunSelfID a logical (boolean) argument that specifies whether the
#'                    "projs.check.libs()" function should output its own identifier
#'                    information alongside the user specified info (notification).
#' @param ssFuncCallerID a character vector (string or text) serving as the
#'                       identifier (tag) for the R Function or R Project that
#'                       called (invoked) this "projs.check.libs()" function.
#' @param sbPrintPretty a logical (boolean) argument that specifies whether the
#'                      ANSI text font formatting should be applied to the
#'                      printed notification or not.
#'
#' @returns
#' The function outputs a list containing to list items ...
#'    - ReqLibs: a vector containing the R library IDs supplied via the
#'                 "vsReqLibs" function argument.
#'    - FI: additional information on the functions internal code execution
#'            process.
#'
#' @examples
#' ### Easily install missing and update outdated R Project Libraries ...
#' library(MFMRutils)   # -> Load the `MFMRutils` library (if previously installed).
#'
#'
#' ## Explicitly set a CRAN mirror ... (<- only needed for CRAN-CODE-CHECKS) !!!
#' options(repos = c(CRAN = "https://cloud.r-project.org"))
#'
#'
#' ### Install the required R Project libraries as follows ...
#' vsReqLibs_ <- c("ggplot2", "dplyr")   # <- Specify the required R Libraries accordingly !!!
#' projs.check.libs(           # -> Run the function with the "vsReqLibs" function argument
#'   vsReqLibs = vsReqLibs_,   #    provided accordingly ...
#'   sbFixLibs = TRUE   # -> Set the "sbFixLibs" function argument to TRUE to ensure all
#' )                    #    missing R Libraries are installed !!!
#'
#'
#'
#' ### Update outdated R Project libraries as follows ...
#' vsReqLibs_ <- c("bench", "zoo")   # -> Specify the required R Libraries accordingly !!!
#' projs.check.libs(           # -> Run the function with the "vsReqLibs" function argument
#'   vsReqLibs = vsReqLibs_,   #    provided accordingly !!!
#'   sbFixLibs = TRUE,     # -> Set both the "sbFixLibs" and "sbUpdateLibs" function
#'   sbUpdateLibs = TRUE   #    arguments to TRUE to ensure all outdated R libraries
#' )
#'
#' @export
#? ### ### ###
"projs.check.libs" <- function(
  vsReqLibs=NULL, sbFixLibs=FALSE, sbQuietInstall=FALSE, sbShowLibs=TRUE,
  ssFuncSelfID="Check Proj. Libs", sbRunSelfID=FALSE, ssFuncCallerID=NULL,
  sbUpdateLibs=TRUE, ssFuncType=NULL, ssFormatDT="%a, %b %d %Y %X", sbPrintPretty=FALSE
) {

  dtFuncSTART <- base::Sys.time();      # -> Extract Function START Time ...
  ssFormatDTI <- "%a, %b %d %Y @ %X";   # -> DateTime Format for "FuncSelfID" Process ...

  if (base::is.null(ssFuncCallerID)) {
    ssFuncCallerID <- base::get0(
      "rssTagProjID_",
      envir = .GlobalEnv,
      ifnotfound = "UNDEFINED"
    );
  }

  if (base::is.null(ssFuncType)) {
    ssFuncType <- "Helper";   # -> Options: "LARGE" ...or... "Helper" Function !!!
  }

  if (sbRunSelfID) {
    MFMRutils::info.post.note(
      siPostMode123 = 1, ssFuncSelfID = ssFuncSelfID, ssFuncType = ssFuncType,
      sbPrintPretty = sbPrintPretty, ssFuncCallerID = ssFuncCallerID
    );
  }

  # !!! ... ADD 'Helper Func' CODE LOGIC here ... !!!
  vsMissingProjectLibs <- base::c();    # -> Creates an empty vector ...
  vsOutdatedProjectLibs <- base::c();   # -> Creates an empty vector ...
  vsLibsUpdateSpecifics <- base::c();   # -> Creates an empty vector ...
  if (base::is.null(vsReqLibs)) {

    ssReqLibsNONE <- "No Libraries were defined for this R Project !!!"
    MFMRutils::info.post.note(
      ssPostNote = ssReqLibsNONE, ssFuncSelfID = ssFuncSelfID,
      ssFuncCallerID = ssFuncCallerID, ssFuncType = ssFuncType,
      siPostMode123 = 2, sbPrintPretty = sbPrintPretty
    );

    ssReqLibsVERIFY <- "Please VERIFY that the R Project DOES NOT USE any R Libraries before continuing ..."
    MFMRutils::info.post.note(
      ssPostNote = ssReqLibsVERIFY, ssFuncSelfID = ssFuncSelfID,
      ssFuncType = ssFuncType, sbPrintPretty = sbPrintPretty,
      siPostMode123 = 2, ssFuncCallerID = ssFuncCallerID
    );

  } else {

    # Notify user about package installation 'ACTIVE' state ...
    ssNoteLibsListCRAN <- base::paste0("Querying CRAN Repository for registered R Libraries ...");
    MFMRutils::info.post.note(
      ssPostNote = ssNoteLibsListCRAN, ssFuncCallerID = ssFuncCallerID,
      ssFuncType = ssFuncType, sbPrintPretty = sbPrintPretty,
      siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID, sbPrePendNL = T
    );

    # Extract ALL R Libraries installed locally and listed on the CRAN Repo ...
    coMtrxArryLibsINSTALLED <- utils::installed.packages();
    coMtrxArryLibsCRANREGIS <- utils::available.packages();

    if (base::length(vsReqLibs) >= 1) {

      for (lib in vsReqLibs) {   # -> Iterates through "vsReqLibs" assessing list items one-by-one with the code below ...

        # Checks if library is not already installed on local machine & if TRUE (i.e. not installed), then appends it to the 'vsMissingProjectLibs' vector ...
        if (!(lib %in% coMtrxArryLibsINSTALLED[ , "Package"])) {
          vsMissingProjectLibs <- base::c(vsMissingProjectLibs, lib);
        } else {
          # Checks if newer versions of already installed libraries exist on the <remote/online> CRAN Repository ...
          if (sbUpdateLibs) {   # -> ... but does this ONLY IF the 'sbUpdateLibs' argument is set to TRUE !!!
            sdLibsVersLOCL <- coMtrxArryLibsINSTALLED[lib, "Version"];
            sdLibsVersCRAN <- coMtrxArryLibsCRANREGIS[lib, "Version"];
            if (sdLibsVersLOCL != sdLibsVersCRAN) {   # -> If there is a miss-match between the LOCAL vs. CRAN results, then it appends the lib to the 'vsOutdatedProjectLibs' vector ...
              vsOutdatedProjectLibs <- base::c(vsOutdatedProjectLibs, lib);
              vsLibsUpdateSpecifics <- base::c(vsLibsUpdateSpecifics, base::paste0(lib, "  v", sdLibsVersLOCL, "  =>  v", sdLibsVersCRAN));
            }
          }
        }
      }

      if (sbFixLibs && base::length(vsMissingProjectLibs) >= 1 ||
          sbFixLibs && base::length(vsOutdatedProjectLibs) >= 1) {

        for (lib in vsMissingProjectLibs) {   # -> Iterates through "vsMissingProjectLibs" assessing list items one-by-one with the code below ...

          # Notify user about package installation start ...
          ssNoteLibsINSTALLSTARTED <- base::paste0("INSTALL of R Package [ ", lib, " ] => STARTED: ", base::format(base::Sys.time(), ssFormatDT), " !!!");
          MFMRutils::info.post.note(
            sbPrePendNL = TRUE, ssPostNote = ssNoteLibsINSTALLSTARTED,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID, ssFuncType = ssFuncType,
            ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
          );

          # Notify user about package installation 'ACTIVE' state ...
          ssNoteLibsINSTALLACTIVE <- base::paste0("Please wait => installation of [ ", lib, " ] is currently underway ...");
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsINSTALLACTIVE, ssFuncType = ssFuncType,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
          );
          utils::install.packages(
            lib, quiet = sbQuietInstall
          );   # -> Installs <specified> 3rd Party Library on local machine ...

          # Notify user about package installation completion ...
          ssNoteLibsINSTALLACTIVE <- base::paste0("INSTALL of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), ssFormatDT), " !!!\n");
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsINSTALLACTIVE, ssFuncType = ssFuncType,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
          );
        }

        if (sbUpdateLibs && base::length(vsOutdatedProjectLibs) >= 1) {

          for (lib in vsOutdatedProjectLibs) {   # -> Iterates through "vsOutdatedProjectLibs" assessing list items one-by-one with the code below ...

            svVersNEW <- coMtrxArryLibsCRANREGIS[lib, 'Version'];
            svVersOLD <- coMtrxArryLibsINSTALLED[lib, 'Version'];

            # Notify user about package installation start ...
            ssNoteLibsINSTALLSTARTED <- base::paste0("UPDATE of R Package [ ", lib, " v", svVersOLD, " => v", svVersNEW," ] => STARTED: ", base::format(base::Sys.time(), ssFormatDT), " !!!");
            MFMRutils::info.post.note(
              sbPrePendNL = TRUE,
              ssPostNote = ssNoteLibsINSTALLSTARTED, ssFuncType = ssFuncType,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
            );

            # Notify user about package installation 'ACTIVE' state ...
            ssNoteLibsINSTALLACTIVE <- base::paste0("Please wait => updating of [ ", lib, " ] is currently underway ...");
            MFMRutils::info.post.note(
              ssPostNote = ssNoteLibsINSTALLACTIVE, ssFuncType = ssFuncType,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
            );
            utils::install.packages(
              lib, quiet = sbQuietInstall
            );   # -> Installs <specified> 3rd Party Library on local machine ...

            # Notify user about package installation completion ...
            ssNoteLibsINSTALLACTIVE <- base::paste0("UPDATE of R Package [ ", lib, " ] => COMPLETED: ", base::format(base::Sys.time(), ssFormatDT), " !!!\n");
            MFMRutils::info.post.note(
              ssPostNote = ssNoteLibsINSTALLACTIVE, ssFuncType = ssFuncType,
              siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
              ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
            );
          }
        }

      } else {

        siVarLenMissingLibs <- base::length(vsMissingProjectLibs);
        siVarLenOutdatedLibs <- base::length(vsOutdatedProjectLibs);
        if (siVarLenMissingLibs >= 1) {

          # Notify user about the missing R libraries ...
          ssNoteLibsMISSING <- base::paste0(
            "The following ", base::ifelse(siVarLenMissingLibs == 1, "LIBRARY is", "LIBRARIES are"), " required, but currently NOT INSTALLED: -> [\n",
            ' "', base::paste0(vsMissingProjectLibs, collapse = '", "'), '"\n', "] <-"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsMISSING, ssFuncType = ssFuncType,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
          );

          # Notify user how to install the missing R libraries ...
          ssNoteLibsINSTALL <- base::paste0(
            "To install missing libraries, re-run the function and set the `sbFixLibs` argument to TRUE.\n"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsINSTALL, ssFuncType = ssFuncType,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
          );
        }

        if (siVarLenOutdatedLibs >= 1) {

          # Notify user about the missing R libraries ...
          ssNoteLibsOUTDATED <- base::paste0(
            "The following already installed ", base::ifelse(siVarLenOutdatedLibs == 1, "LIBRARY", "LIBRARIES"), " can be UPDATED: -> [\n",
            ' ', base::paste0(vsLibsUpdateSpecifics, collapse = ',\n '), '\n', "] <-"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsOUTDATED, ssFuncType = ssFuncType,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
          );

          # Notify user how to install the missing R libraries ...
          ssNoteLibsUPDATE <- base::paste0(
            "To update installed libraries, re-run this function and set both the `sbFixLibs` & `sbUpdateLibs` arguments to TRUE.\n"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsUPDATE, ssFuncType = ssFuncType,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
          );
        }
      }

      # Notify user that ALL REQUIRED Project Libraries are already installed ...
      if (base::length(vsMissingProjectLibs) == 0 && base::length(coMtrxArryLibsCRANREGIS) >= 1) {
        ssNoteLibsINSTALL <- base::paste0(
          "All libraries required for this R Project are properly installed ..."
        );
        MFMRutils::info.post.note(
          ssPostNote = ssNoteLibsINSTALL, ssFuncType = ssFuncType,
          siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
          ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty,
          sbPrePendNL = TRUE, sbPostPend2ndNL = base::ifelse(sbShowLibs, FALSE, TRUE)
        );

        if (sbShowLibs) {
          ssNoteLibsREQUIRED <- base::paste0(
            "Libraries defined for this R Project are as follows: -> [\n",
            ' "', base::paste0(vsReqLibs, collapse = '", "'), '"',
            "\n ] <-"
          );
          MFMRutils::info.post.note(
            ssPostNote = ssNoteLibsREQUIRED, ssFuncType = ssFuncType,
            siPostMode123 = 2, ssFuncSelfID = ssFuncSelfID,
            sbPrePendNL = FALSE, sbPostPend2ndNL = TRUE,
            ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
          );
        }
      }
    }
  }

  dtFuncSTOP <- base::Sys.time();   # -> Extract Function STOP Time ...
  coFuncINFO <- base::list(         # -> Collate Key Function Self-ID Information ...
    "FuncID" = ssFuncSelfID, "CallerID" = ssFuncCallerID,
    "FuncSTART" = dtFuncSTART, "FuncSTART" = dtFuncSTOP, "FuncType" = ssFuncType
  )
  if (sbRunSelfID) {
    MFMRutils::info.post.note(
      siPostMode123 = 3, ssFuncSelfID = ssFuncSelfID, ssFuncType = ssFuncType,
      ssFuncCallerID = ssFuncCallerID, sbPrintPretty = sbPrintPretty
    );
  }

  base::return(
    base::invisible(base::list("ReqLibs" = vsReqLibs, "FI" = coFuncINFO))
  );
}


