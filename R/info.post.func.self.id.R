#? ### ### ### ### ### ### ###
#' @title Compile & print custom function Self-ID Information
#' @description
#' A <tiny> Helper Function that compiles and prints the self-identification 
#' information (i.e. self-id, type, caller, run-time duration etc.) of a custom 
#' R function. This custom function was intended to mainly support the MFMR Suite 
#' of R Functions, but can be utilized as a standalone function in other R packages.
#'
#' @param ssProjID a character (string) identifier of the R Project <script>
#'                 file that called the custom R function execution.
#' @param ssFuncSelfID a character (text string) identifier for the active R 
#'                     Function being called (or being executed). If set to NULL
#'                     the hard-coded "info.post.func.self.id()" function ID 
#'                     (i.e. "MFMR-Self.ID") will be used in the printed result. 
#' @param ssFuncCallerID a character (string) value that identifies the parent
#'                       function that invoked that execution of the active or
#'                       current function.
#' @param siFuncMode01 an integer (long) value that identifies the two run-time 
#'                     states of this `Self-ID` custom function. A value of `1L` 
#'                     sets the function mode to the `ENTER` (start) state; while 
#'                     a value of `0L` sets the function mode to the `EXIT` (stop) 
#'                     run-time state.
#' @param csColorProjID a character (string) value that sets the `ProjID` text
#'                      colour in the formatted text output.
#' @param csColorFuncType a character (string) value that sets the `FuncType` text
#'                        colour in the formatted text output.
#' @param csColorCallerID a character (string) value that sets the `CallerID` text
#'                        colour in the formatted text output.
#' @param csTimeStart a character (string) value that defines the `START-Time` 
#'                    of the active function.
#' @param csTimeStop a character (string) value that defines the `STOP-Time` 
#'                   of the active function.
#' @param sbPrintPretty a logical (boolean) value that defines whether the output
#'                      text (Self-ID Function Info) should be formatted when
#'                      printed to the R Console.
#' @param csFormatDT a character (string) value that specifies how `DateTime` info 
#'                   should be formatted when printed out to the R Console.
#' @param csIconCarat a character (string) value that defines the leading icon 
#'                    (carat) object to be used when printing the `Self-ID` 
#'                    information to the R Console.
#' @param csColorCarat a character (string) value that sets the `Carat Icon` object
#'                     colour in the formatted text output.
#' @param csIconSplit a character (string) value that defines the middle <split> 
#'                    icon (object) that should be used for printing the `Self-ID` 
#'                    information to the R Console.
#' @param csColorMain a character (string) value that sets the `Main` information
#'                    text colour in the formatted text output.
#' @param csColorSplit a character (string) value that sets the `Split` information
#'                     text colour in the formatted text output.
#' @param csColorTimeStamp a character (string) value that sets the `TimeStamp` 
#'                         information text colour in the formatted text output.
#' @param siStartCELN an integer that denotes the START of the function's block of
#'                    code. The Code Editor Line Number (`CELN`) of the first line 
#'                    of code that defines a custom function.
#' @param siStopCELN an integer that denotes the STOP (end) of the function's
#'                   block of code. The Code Editor Line Number (`CELN`) of the
#'                   last line of code that defines a custom function - usually
#'                   identified by a closing curly brace.
#'
#' @returns This function returns a single <abbreviated> character value that
#'          classifies five sizes (or classes) of custom R functions: ...
#'          * "TNY" -> a "Tiny" R Function (less than 50 lines of code);
#'
#' @examples
#' ### Classify your Custom R Function as follows:
#' require(MFMRutils)   # -> Ensures the "MFMRutils" library is installed & loaded
#'
#' info.post.func.self.id()   # -> Set this Code Editor Line Number `CELN`as the 
#'                            #    `siFuncStopCELN` arg value !!!
#'
#' @export
#? ### ### ###
"info.post.func.self.id" <- function(
  ssProjID=NULL, ssFuncSelfID=NULL, siFuncMode01=NULL, ssFuncCallerID=NULL, 
  csIconCarat=NULL, csColorCarat=NULL, csIconSplit=NULL, csColorSplit=NULL,
  csTimeStart=NULL, csTimeStop=NULL, csFormatDT=NULL, csColorTimeStamp=NULL,
  csColorProjID=NULL, csColorFuncType=NULL, csColorCallerID=NULL, csColorMain=NULL,
  sbPrintPretty=NULL, siStartCELN=NULL, siStopCELN=NULL
) {

  ### STEP 01 - Define the "Function Self-ID" tag ... ####
  # NB: This👆 is THE ONLY FUNCTION [in the MFMR Suite of R Functions] THAT DOES
  #     NOT SELF-IDENTIFY (i.e. Self-ID here causes infinite recursion) !!!
  ssFuncTAG_ <- base::paste0(
    MFMRutils::pkgs.get.lib.info()[['PkgNAME']], "_", "Func.Self.ID"
  );
  
  
  ### Assign "Local Aliases" for frequently used functions !!!
  # NOTES: This is a <NEW> approach to improve R Session Memory Efficiency ...
  rasANY         <- base::any;
  rasCAT         <- base::cat;
  rasLIST        <- base::list;
  rasIsNA        <- base::is.na;
  rasTRUNC       <- base::trunc;
  rasLENGTH      <- base::length;
  rasRETURN      <- base::return;
  rasIfELSE      <- base::ifelse;
  rasPASTE0      <- base::paste0;
  rasIsNULL      <- base::is.null;
  rasStrFormTIME <- base::strftime;
  rasAsNUMERIC   <- base::as.numeric;
  rasSysTimeNOW_ <- base::Sys.time();
  rasMfmrDATES   <- MFMRutils::ENVDates
  rasMfmrICONS   <- MFMRutils::ENVIcons
  rasMfmrCOLORS  <- MFMRutils::ENVColors
  rcsAnsiBOLD_   <- MFMRutils::ENVFormats$BOLD;
  rcsAnsiRESET_  <- MFMRutils::ENVFormats$RESET;
  
  
  ### Compile Useful <internal> Custom Functions here !!!
  # Define custom null-coalescing operator ...
  `%?!%` <- function(e1, e2) {
    if (rasIsNULL(e1) || rasLENGTH(e1) == 0 || rasANY(rasIsNA(e1))) {
      rasRETURN(e2);
    } else {
      rasRETURN(e1)
    }
  }
  
  # Define a custom function to Extract the String Formatting Setting ... ####
  rcf_calc.time.delta <- function(csTimeStart, csTimeStop) {
    csTimeDeltaRAW_ <- rasAsNUMERIC(
      csTimeStop - csTimeStart, units = "secs"
    );
    csTimeDelta_ <- rasAsNUMERIC(csTimeDeltaRAW_[[1]]);
    csTimeDeltaRESULT_ <- NULL;
    csTimeDeltaROUND_ <- base::round(csTimeDelta_, 3);
    if (csTimeDeltaROUND_ <= 0.999) {
      ssFloatVals_ <- base::abs(
        csTimeDeltaROUND_ - rasTRUNC(csTimeDeltaROUND_)
      );
      ssFloatsAsInts_ <- base::sub(
        "^0\\.", "", base::format(ssFloatVals_, scientific = FALSE)
      );
      csTimeDeltaRESULT_ <- rasPASTE0(
        ssFloatsAsInts_, " milli-secs"
      );
    } else if (csTimeDeltaROUND_ > 0.999 && csTimeDeltaROUND_ <= 60.0) {
      ssIntsONLY_ <- rasTRUNC(csTimeDeltaROUND_);
      csTimeDeltaRESULT_ <- rasPASTE0(
        ssIntsONLY_, " secs"
      );
    } else if (csTimeDeltaROUND_ > 60.0 && csTimeDeltaROUND_ <= 3600) {
      ssIntsONLY_ <- rasTRUNC(csTimeDeltaROUND_);
      ssDeltaSecs_ <- ssIntsONLY_ %% 60;
      ssDeltaMins_ <- rasTRUNC(ssIntsONLY_ / 60);
      csTimeDeltaRESULT_ <- rasPASTE0(
        ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
      );
    } else if (csTimeDeltaROUND_ > 3600 && csTimeDeltaROUND_ <= 216000) {
      ssIntsONLY_ <- rasTRUNC(csTimeDeltaROUND_);
      ssDeltaSecs_ <- ssIntsONLY_ %% 60;
      ssDeltaMins_ <- rasTRUNC(ssIntsONLY_ / 60);
      ssDeltaHrs_ <- rasTRUNC(ssIntsONLY_ / (60 * 60));
      csTimeDeltaRESULT_ <- rasPASTE0(
        ssDeltaHrs_, " hrs, ", ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
      );
    } else if (csTimeDeltaROUND_ > 216000 && csTimeDeltaROUND_ <= 5184000) {
      ssIntsONLY_ <- rasTRUNC(csTimeDeltaROUND_);
      ssDeltaSecs_ <- ssIntsONLY_ %% 60;
      ssDeltaMins_ <- rasTRUNC(ssIntsONLY_ / 60);
      ssDeltaHrs_ <- rasTRUNC(ssIntsONLY_ / (60 * 60));
      ssDeltaDays_ <- rasTRUNC(ssIntsONLY_ / (60 * 60 * 24));
      csTimeDeltaRESULT_ <- rasPASTE0(
        ssDeltaDays_, " days, ", ssDeltaHrs_, " hrs, ", 
        ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
      );
    }
    base::return(csTimeDeltaRESULT_);
  }

  
  
  ### STEP 02 - Internalize ALL Function Arguments here ... ####
  # NOTES: hand-over all func-args to func-local <internal> variables ...
  csTimeStamp_      <- NULL;
  coListFuncRes_    <- NULL;   # -> The <final> function outputs <results> object.
  ssProjID_         <- ssProjID         %?!% "UNDEFINED";
  ssFuncCallerID_   <- ssFuncCallerID   %?!% "UNDEFINED";
  ssFuncSelfID_     <- ssFuncSelfID     %?!% "UNDEFINED";
  siFuncMode01_     <- siFuncMode01     %?!% 1L;
  csIconCarat_      <- csIconCarat      %?!% rasMfmrICONS$FireFlame;
  csColorCarat_     <- csColorCarat     %?!% rasMfmrCOLORS$YellowFORE;
  csIconSplit_      <- csIconSplit      %?!% " | ";
  csColorSplit_     <- csColorSplit     %?!% rasMfmrCOLORS$YellowFORE;
  csTimeStart_      <- csTimeStart      %?!% rasSysTimeNOW_;
  csTimeStop_       <- csTimeStop       %?!% rasSysTimeNOW_;
  csFormatDT_       <- csFormatDT       %?!% rasMfmrDATES$LONGv03;
  csColorTimeStamp_ <- csColorTimeStamp %?!% rasMfmrCOLORS$YellowFORE;
  csColorProjID_    <- csColorProjID    %?!% rasMfmrCOLORS$GreenFORE;
  csColorFuncType_  <- csColorFuncType  %?!% rasMfmrCOLORS$YellowFORE;
  csColorCallerID_  <- csColorCallerID  %?!% rasMfmrCOLORS$MagentaFORE;
  csColorMain_      <- csColorMain      %?!% rasMfmrCOLORS$CyanFORE;
  sbPrintPretty_    <- sbPrintPretty    %?!% TRUE;
  siStartCELN_      <- siStartCELN      %?!% 1L;
  siStopCELN_       <- siStopCELN       %?!% 28L;
  
  
  
  ### STEP 04 - Prime the DateTime Values ... ####
  csTimeStartFORMATTED_ <- rasStrFormTIME(
    x = csTimeStart_, format = csFormatDT_
  );
  csTimeStopFORMATTED_ <- rasStrFormTIME(
    x = csTimeStop_, format = csFormatDT_
  );
  
  
  
  ### STEP 08 - Apply the Carat Icon Setting ... ####
  if (!rasIsNULL(csIconCarat_)) {   # <- Check that "IconCarat" is NOT NULL !!!
    if (sbPrintPretty_) {
      if (csIconCarat_ == "=>" || csIconCarat_ == " => " ||
          csIconCarat_ == "->" || csIconCarat_ == " -> ") {
        csIconCarat_ <- rasPASTE0(
          " ",                               # -> Add "leading" white space ...
          csColorCarat_,                     # -> Apply specified text colour ...
          rasMfmrICONS$ArrowRIGHT,   # -> Assign the MFMR Arrow Icon !!!
          " ",                               # -> Add "trailing" white space ...
          rcsAnsiRESET_                       # -> Deactivate text formatting !!!
        );
      } else {
        csIconCarat_ <- rasPASTE0(
          " ",             # -> Add "leading" white space ...
          csColorCarat_,   # -> Apply specified text colour ...
          csIconCarat_,    # -> Assign the specified Carat Icon !!!
          " ",             # -> Add "trailing" white space ...
          rcsAnsiRESET_     # -> Deactivate text formatting !!!
        );
      }
    }
  } else {
    if (sbPrintPretty_) {
      csIconCarat_ <- rasPASTE0(
        " ",                               # -> Add "leading" white space ...
        csColorCarat_,                     # -> Apply specified text colour ...
        rasMfmrICONS$ArrowRIGHT,   # -> Assign the MFMR Arrow Icon !!!
        " ",                               # -> Add "trailing" white space ...
        rcsAnsiRESET_                       # -> Deactivate text formatting !!!
      );
    } else {
      csIconCarat_ <- " => ";   # -> Apply a simple <default> Carat Icon !!!
    }
  }
  
  
  
  ### STEP 09 - Apply the "Project-ID" Text Formatting ... ####
  if (rasIsNULL(ssProjID_)) {
    ssProjID_ <- base::get0(         # -> Searches the Global Environment of the
      "rssTagProjID_",               #    Active R Session for the <somewhat>
      envir = .GlobalEnv,            #    unique variable name "rssTagProjID_"
      ifnotfound = "UNK. Proj. ID"   #    and extracts the value contained in
    );                               #    that variable (if it exists) ... or
  }                                  #    else returns the "NOT-FOUND" value.
  if (sbPrintPretty_) {
    ssProjID_ <- rasPASTE0(
      rcsAnsiBOLD_,      # -> Apply a BOLD text formatting ... 
      csColorProjID_,   # -> Apply the specified text colour ... 
      ssProjID_,        # -> Add the "Caller-ID" string value !!!
      rcsAnsiRESET_      # -> Deactivate text formatting !!!
    );
  }
  
  
  
  ### STEP 10 - Apply the "Split-Icon" Text Formatting ... ####
  if (!rasIsNULL(csIconSplit_)) {
    if (sbPrintPretty_) {
      csIconSplit_ <- rasPASTE0(
        rcsAnsiBOLD_,     # -> Apply a BOLD text formatting ... 
        csColorSplit_,   # -> Apply the specified text colour ... 
        csIconSplit_,    # -> Add the "Split-Icon" string value !!!
        rcsAnsiRESET_     # -> Deactivate text formatting !!!
      );
    }
  } else {
    if (sbPrintPretty_) {
      csIconSplit_ <- rasPASTE0(
        rcsAnsiBOLD_,     # -> Apply a BOLD text formatting ... 
        csColorSplit_,   # -> Apply the specified text colour ... 
        " | ",           # -> Add the <default> "Split-Icon" string value !!!
        rcsAnsiRESET_     # -> Deactivate text formatting !!!
      );
    } else {
      csIconSplit_ <- " | ";   # -> Add a <basic> "Split-Icon" string value !!!
    }
  }
  
  
  
  ### STEP 11 - Apply the "Func-Type" Text Formatting ... ####
  ssFuncType_ <- MFMRutils::code.classify.func(
    siFuncStartCELN = siStartCELN_, siFuncStopCELN = siStopCELN_
  );
  if (!rasIsNULL(ssFuncType_)) {
    if (sbPrintPretty_) {
      ssFuncType_ <- rasPASTE0(
        rcsAnsiBOLD_,        # -> Apply a BOLD text formatting ... 
        csColorFuncType_,   # -> Apply the specified text colour ... 
        ssFuncType_,        # -> Add the "Func-Type" string value !!!
        rcsAnsiRESET_        # -> Deactivate text formatting !!!
      );
    }
  } else {
    if (sbPrintPretty_) {
      ssFuncType_ <- rasPASTE0(
        rcsAnsiBOLD_,     # -> Apply a BOLD text formatting ... 
        csColorSplit_,   # -> Apply the specified text colour ... 
        "UNK.",          # -> Add the <default> "Func-Type" string value !!!
        rcsAnsiRESET_     # -> Deactivate text formatting !!!
      );
    } else {
      ssFuncType_ <- "UNK.";   # -> Add a <basic> "Func-Type" string value !!!
    }
  }
  
  
  
  ### STEP 12 - Apply the "Caller-ID" Text Formatting ... ####
  if (!rasIsNULL(ssFuncCallerID_)) {
    if (sbPrintPretty_) {
      ssFuncCallerID_ <- rasPASTE0(
        rcsAnsiBOLD_,        # -> Apply a BOLD text formatting ... 
        csColorCallerID_,   # -> Apply the specified text colour ... 
        ssFuncCallerID_,    # -> Add the "Caller-ID" string value !!!
        rcsAnsiRESET_        # -> Deactivate text formatting !!!
      );
    }
  } else {
    if (sbPrintPretty_) {
      ssFuncCallerID_ <- rasPASTE0(
        rcsAnsiBOLD_,        # -> Apply a BOLD text formatting ... 
        csColorCallerID_,   # -> Apply the specified text colour ... 
        "UNK.",             # -> Add the <default> "Caller-ID" string value !!!
        rcsAnsiRESET_        # -> Deactivate text formatting !!!
      );
    } else {
      ssFuncCallerID_ <- "UNK.";   # -> Add a <basic> "Caller-ID" string value !!!
    }
  }
  
  
  
  ### STEP 13 - Apply the "Time-Stamp" Text Formatting ... ####
  if (siFuncMode01_ == 1L) {   # -> Apply the ENTER function Info !!!
    if (sbPrintPretty_) {
      csTimeStamp_ <- rasPASTE0(
        rcsAnsiBOLD_,             # -> Apply a BOLD text formatting ... 
        csColorTimeStamp_,       # -> Apply the specified text colour ... 
        csTimeStartFORMATTED_,   # ...
        rcsAnsiRESET_             # -> Deactivate text formatting !!!
      );
    } else {
      csTimeStamp_ <- csTimeStartFORMATTED_;
    }
  } else if (siFuncMode01_ == 0L) {   # -> Apply the EXIT function Info !!!
    if (sbPrintPretty_) {
      csTimeStamp_ <- rasPASTE0(
        rcsAnsiBOLD_,            # -> Apply a BOLD text formatting ... 
        csColorTimeStamp_,      # -> Apply the specified text colour ... 
        csTimeStopFORMATTED_,   # ...
        rcsAnsiRESET_            # -> Deactivate text formatting !!!
      );
    } else {
      csTimeStamp_ <- csTimeStopFORMATTED_;
    }
  }
  
  
  
  # 4.3.1.2 - Post the `ENTER` notification (Func-Self-ID) text ...
  if (siFuncMode01_ == 1L) {   # -> Apply the ENTER function Info Post !!!
    if (sbPrintPretty_) {
      rasCAT(
        rasPASTE0(
          csIconCarat_, ssProjID_, csIconSplit_,
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, "F-START { <F-SID: '", rcsAnsiRESET_
          ),
          rasPASTE0(
            rcsAnsiBOLD_, csColorCallerID_, ssFuncSelfID_, rcsAnsiRESET_
          ),
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, "'  F-Type: '", rcsAnsiRESET_
          ), 
          ssFuncType_,
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, "'>  F-Caller: '", rcsAnsiRESET_
          ), 
          ssFuncCallerID_,
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, "'  <Time: ", rcsAnsiRESET_
          ), 
          csTimeStamp_,
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, ">  }\n", rcsAnsiRESET_
          )
        )
      );
    } else {
      rasCAT(
        rasPASTE0(
          csIconCarat_, ssProjID_, csIconSplit_,
          "F-START {  <F-SID: '", ssFuncSelfID_, 
          "'  F-Type: '", ssFuncType_, "'> ",
          " F-Caller: '", ssFuncCallerID_,
          "'  <Time: ", csTimeStamp_, ">  }\n"
        )
      );
    }
  } else if (siFuncMode01_ == 0L) {   # -> Apply the EXIT function Info Post !!!
     csDeltaTIME_ <- rcf_calc.time.delta(csTimeStart_, csTimeStop_);
     if (sbPrintPretty_) {
       rasCAT(
        rasPASTE0(
          csIconCarat_, ssProjID_, csIconSplit_,
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, "F-STOP { <F-SID: '", rcsAnsiRESET_
          ),
          rasPASTE0(
            rcsAnsiBOLD_, csColorCallerID_, ssFuncSelfID_, rcsAnsiRESET_
          ),
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, "'  F-Caller: '", rcsAnsiRESET_
          ), 
          ssFuncCallerID_,
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, "'>  Time: ", rcsAnsiRESET_
          ), 
          csTimeStamp_,
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, " ( F-Dur: ", rcsAnsiRESET_
          ),
          rasPASTE0(
            rcsAnsiBOLD_, csColorTimeStamp_, csDeltaTIME_, rcsAnsiRESET_
          ),
          rasPASTE0(
            rcsAnsiBOLD_, csColorMain_, " ) }\n", rcsAnsiRESET_
          )
        )
      );
    } else {
      rasCAT(
        rasPASTE0(
          csIconCarat_, ssProjID_, csIconSplit_,
          "F-STOP { <F-SID: '", ssFuncSelfID_, 
          "'  F-Caller: '", ssFuncCallerID_, "'> ",
          " Time: ", csTimeStamp_, "",
          " ( F-Dur: ", csDeltaTIME_, " ) }\n"
        )
      );
    }
  }
  
  # 4.3.1.3 - Output the `ENTER` "Func-Self-ID' properties ...
  coListFuncRes_ <- rasLIST(
    "FuncID" = ssFuncSelfID_, "FuncType" = ssFuncType_, 
    "ProjID" = ssProjID_, "CallerID" = ssFuncCallerID_,
    "FuncSTART" = csTimeStart_, "FuncSTOP" = csTimeStop_
  );
  base::invisible(rasLIST("SelfID" = coListFuncRes_));
}


