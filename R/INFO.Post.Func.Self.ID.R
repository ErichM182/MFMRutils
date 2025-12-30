#? ### ### ### ### ### ### ###
#' @title Print Self-ID Information for Custom R Functions
#' @name info.post.func.self.id
#' @family SuiteMFMR INFO Functions
#' 
#' 
#' @description
#' A simple Helper Function that compiles and prints the self-identification (i.e. self-id, type, 
#' caller, run-time duration etc.) information of a custom R function. This custom function was 
#' intended to mainly support the MFMR Suite of R Functions, but can be utilized as a standalone 
#' function in other (i.e. 3rd Party) R packages.
#'
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
#'                     states of this "Self-ID" custom function. A value of "1L" 
#'                     sets the function mode to the "ENTER" (start) state; while 
#'                     a value of "0L" sets the function mode to the "EXIT" (stop) 
#'                     run-time state.
#' @param csColorProjID a character (string) value that sets the "ProjID" text
#'                      colour in the formatted text output.
#' @param csColorFuncType a character (string) value that sets the "FuncType" text
#'                        colour in the formatted text output.
#' @param csColorCallerID a character (string) value that sets the "CallerID" text
#'                        colour in the formatted text output.
#' @param csTimeStart a character (string) value that defines the "START-Time" 
#'                    of the active function.
#' @param csTimeStop a character (string) value that defines the "STOP-Time" 
#'                   of the active function.
#' @param sbPrintPretty a logical (boolean) value that defines whether the output
#'                      text (Self-ID Function Info) should be formatted when
#'                      printed to the R Console.
#' @param csFormatDT a character (string) value that specifies how "DateTime" info 
#'                   should be formatted when printed out to the R Console.
#' @param csIconCarat a character (string) value that defines the leading icon 
#'                    (carat) object to be used when printing the "Self-ID" 
#'                    information to the R Console.
#' @param csColorCarat a character (string) value that sets the "Carat Icon" object
#'                     colour in the formatted text output.
#' @param csIconSplit a character (string) value that defines the middle <split> 
#'                    icon (object) that should be used for printing the "Self-ID" 
#'                    information to the R Console.
#' @param csColorMainText a character (string) value that sets the "Main" information
#'                    text colour in the formatted text output.
#' @param csColorSplit a character (string) value that sets the "Split" information
#'                     text colour in the formatted text output.
#' @param csColorTimeStamp a character (string) value that sets the "TimeStamp" 
#'                         information text colour in the formatted text output.
#' @param siStartCELN an integer that denotes the START of the function's block of
#'                    code. The Code Editor Line Number ("CELN") of the first line 
#'                    of code that defines a custom function.
#' @param siStopCELN an integer that denotes the STOP (end) of the function's
#'                   block of code. The Code Editor Line Number ("CELN") of the
#'                   last line of code that defines a custom function - usually
#'                   identified by a closing curly brace.
#' @param sbRunSelfID a logical (boolean) value that defines whether the SELF-ID
#'                    procedure should be executed (TRUE) or not (FALSE). 
#'
#'
#' @returns 
#' This function returns a single <abbreviated> character value that classifies five function size
#' (code base size) classes for custom R functions: ...
#'    * "TNY" -> a "Tiny" R Function (less than 50 lines of code);
#'
#'
#' @examples
#' ### Run Self-Identification (Self-ID) on your Custom R Function as follows:
#' library(MFMRutils)         # -> Loads the "MFMRutils" R Library ...
#' info.post.func.self.id()   # -> Runs the default <NULL> function state ...
#' 
#' ### Prime the relevant function arguments as needed ...
#' info.post.func.self.id(
#'  ssProjID = "rTestProject",
#'  ssFuncSelfID = "rcfTextFUNC", 
#'  siFuncMode01 = 1L
#' )
#'
#'
#' @export
#? ### ### ###
"info.post.func.self.id" <- function(
  ssProjID=NULL, ssFuncSelfID=NULL, siFuncMode01=NULL, ssFuncCallerID=NULL, csIconCarat=NULL, 
  csColorCarat=NULL, csIconSplit=NULL, csColorSplit=NULL, csTimeStart=NULL, csTimeStop=NULL, 
  csFormatDT=NULL, csColorTimeStamp=NULL, csColorProjID=NULL, csColorFuncType=NULL, 
  csColorCallerID=NULL, siStopCELN=NULL, siStartCELN=NULL, csColorMainText=NULL, 
  sbPrintPretty=NULL, sbRunSelfID=FALSE
) {
  
  ####   STEP 01 - Prime "Function Self-ID" CONSTANTS   ####
  # NB: This 👆 is THE ONLY FUNCTION [in the MFMR Suite of R Functions] THAT DOES
  #     NOT SELF-IDENTIFY (since Self-ID here causes infinite recursion) !!!
  RCT_SYS_TIME_NOW_     <- base::Sys.time();           # <- Extract the active System Date-Time !!!
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";                # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHRT_ <- "Func-SID";                 # <- FSID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "INFO.Post.Func.Self.ID";   # <- FSID - LONG !!!
  
  
  
  ####   STEP 02 - Internalize ALL Function Arguments   ####
  # NOTES: hand-over all func-args to func-local <internal> variables ...
  rssProjID_         <- ssProjID;
  rssFuncCallerID_   <- ssFuncCallerID;
  rssFuncSelfID_     <- ssFuncSelfID;
  rsiFuncMode01_     <- siFuncMode01;
  rcsColorCarat_     <- csColorCarat;
  rcsIconSplit_      <- csIconSplit;
  rcsColorSplit_     <- csColorSplit;
  rcsTimeStart_      <- csTimeStart;
  rcsTimeStop_       <- csTimeStop;
  rcsFormatDT_       <- csFormatDT;
  rcsColorTimeStamp_ <- csColorTimeStamp;
  rcsColorProjID_    <- csColorProjID;
  rcsColorFuncType_  <- csColorFuncType;
  rcsColorCallerID_  <- csColorCallerID;
  rcsColorMainText_  <- csColorMainText;
  rsbPrintPretty_    <- sbPrintPretty;
  rsiStartCELN_      <- siStartCELN;
  rsiStopCELN_       <- siStopCELN;
  rcsIconCarat_      <- csIconCarat;
  rsbRunSelfID_      <- sbRunSelfID;
  
  ## SPECIAL: Try to locate & extract the 'isDebugMode' logical (boolean) variable 
  ##          <if set or primed elsewhere> in the current <active> R Project ... 
  rsbRunModeDEBUG_ <- base::get0(   # <- Searches the Global Environment of the Active R Session for
    "rsbRunModeDEBUG_",             #    the <somewhat> uniquely named variable `RCT_IS_DEBUG_MODE_`
    envir = .GlobalEnv,             #    and extracts its value.
    ifnotfound = FALSE              # -> Assigns a value of `FALSE` if the variable was NOT FOUND in
  );                                #    the Active R Session !!!
  
  
  ### ONLY RUN the Function SELF-ID Process if the following condition is TRUE !!! 
  if (rsbRunSelfID_ || rsbRunModeDEBUG_) {
    
    ####   STEP 03 - Alias ALL Required Functions   ####
    ## Assign "Local Aliases" for frequently used functions !!!
    ## NB: This is a <NEW> approach to improve R Session Memory Efficiency ...
    rasABS           <- base::abs;
    rasANY           <- base::any;
    rasCAT           <- base::cat;
    rasSUB           <- base::sub;
    rasGET0          <- base::get0;
    rasLIST          <- base::list;
    rasIsNA          <- base::is.na;
    rasTRUNC         <- base::trunc;
    rasROUND         <- base::round;
    rasLENGTH        <- base::length;
    rasRETURN        <- base::return;
    rasIfELSE        <- base::ifelse;
    rasPASTE0        <- base::paste0;
    rasFORMAT        <- base::format;
    rasIsNULL        <- base::is.null;
    rasStrFormTIME   <- base::strftime;
    rasINVISIBLE     <- base::invisible;
    rasAsNUMERIC     <- base::as.numeric;
    
    `%??%`           <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
    rasMfmrICONS     <- MFMRutils::RENV_ICONS;
    rasMfmrCOLORS    <- MFMRutils::RENV_COLOURS;
    rasMfmrFORMATS   <- MFMRutils::RENV_FORMATS;
    rasMfmrClassFUNC <- MFMRutils::code.classify.func;
    
    
    
    ####   STEP 04 - Define Critical Constants   ####
    ### Prime selected variables (akin to constants) ...
    rcsIconSPARK_     <- rasMfmrICONS$SparkRed;
    rcsIconSKULL_     <- rasMfmrICONS$SkullOnly;
    rcsColorsCYAN_    <- rasMfmrCOLORS$CyanFORE;
    rcsColorsGREEN_   <- rasMfmrCOLORS$GreenFORE;
    rcsColorsYELLOW_  <- rasMfmrCOLORS$YellowFORE;
    rcsAnsiBOLD_      <- rasMfmrFORMATS$ANSI_BOLD;
    rcsAnsiRESET_     <- rasMfmrFORMATS$ANSI_RESET;
    rcsColorsMAGENTA_ <- rasMfmrCOLORS$MagentaFORE;
    
    
    ####### ### Compile Useful <internal> Custom Functions here !!!
    # Define a custom function to Extract the String Formatting Setting ... ####
    rcf_calc.time.delta <- function(csTimeStart, csTimeStop) {
      rcsTimeDeltaRAW_ <- rasAsNUMERIC(
        csTimeStop - csTimeStart, units = "secs"
      );
      rcsTimeDelta_ <- rasAsNUMERIC(rcsTimeDeltaRAW_[[1]]);
      rcsTimeDeltaRESULT_ <- NULL;
      rcsTimeDeltaROUND_ <- rasROUND(rcsTimeDelta_, 3);
      if (rcsTimeDeltaROUND_ <= 0.999) {
        rssFloatVals_ <- rasABS(
          rcsTimeDeltaROUND_ - rasTRUNC(rcsTimeDeltaROUND_)
        );
        rssFloatsAsInts_ <- rasSUB(
          "^0\\.", "", rasFORMAT(rssFloatVals_, scientific = FALSE)
        );
        rcsTimeDeltaRESULT_ <- rasPASTE0(
          rssFloatsAsInts_, " milli-secs"
        );
      } else if (rcsTimeDeltaROUND_ > 0.999 && rcsTimeDeltaROUND_ <= 60.0) {
        rssIntsONLY_ <- rasTRUNC(rcsTimeDeltaROUND_);
        rcsTimeDeltaRESULT_ <- rasPASTE0(
          rssIntsONLY_, " secs"
        );
      } else if (rcsTimeDeltaROUND_ > 60.0 && rcsTimeDeltaROUND_ <= 3600) {
        rssIntsONLY_ <- rasTRUNC(rcsTimeDeltaROUND_);
        rssDeltaSecs_ <- rssIntsONLY_ %% 60;
        rssDeltaMins_ <- rasTRUNC(rssIntsONLY_ / 60);
        rcsTimeDeltaRESULT_ <- rasPASTE0(
          rssDeltaMins_, " mins, ", rssDeltaSecs_, " secs"
        );
      } else if (rcsTimeDeltaROUND_ > 3600 && rcsTimeDeltaROUND_ <= 216000) {
        rssIntsONLY_ <- rasTRUNC(rcsTimeDeltaROUND_);
        rssDeltaSecs_ <- rssIntsONLY_ %% 60;
        rssDeltaMins_ <- rasTRUNC(rssIntsONLY_ / 60);
        rssDeltaHrs_ <- rasTRUNC(rssIntsONLY_ / (60 * 60));
        rcsTimeDeltaRESULT_ <- rasPASTE0(
          rssDeltaHrs_, " hrs, ", rssDeltaMins_, " mins, ", rssDeltaSecs_, " secs"
        );
      } else if (rcsTimeDeltaROUND_ > 216000 && rcsTimeDeltaROUND_ <= 5184000) {
        rssIntsONLY_ <- rasTRUNC(rcsTimeDeltaROUND_);
        rssDeltaSecs_ <- rssIntsONLY_ %% 60;
        rssDeltaMins_ <- rasTRUNC(rssIntsONLY_ / 60);
        rssDeltaHrs_ <- rasTRUNC(rssIntsONLY_ / (60 * 60));
        rssDeltaDays_ <- rasTRUNC(rssIntsONLY_ / (60 * 60 * 24));
        rcsTimeDeltaRESULT_ <- rasPASTE0(
          rssDeltaDays_, " days, ", rssDeltaHrs_, " hrs, ", 
          rssDeltaMins_, " mins, ", rssDeltaSecs_, " secs"
        );
      }
      rasRETURN(rcsTimeDeltaRESULT_);
    }
    
    
    
    ####   STEP 05 - Run NULL Checks & Prime NB Variables   ####
    # NOTES: hand-over all func-args to func-local <internal> variables ...
    rcsTimeStamp_      <- NULL;
    rcoListFuncRes_    <- NULL;   # -> The <final> function output <results> object.
    rssProjID_         <- rssProjID_         %??% NULL;
    rssFuncSelfID_     <- rssFuncSelfID_     %??% RCT_TAG_FUNC_ID_FULL_;
    rssFuncCallerID_   <- rssFuncCallerID_   %??% "UNDEFINED";
    rsiFuncMode01_     <- rsiFuncMode01_     %??% 0L;
    rcsColorCarat_     <- rcsColorCarat_     %??% rcsColorsYELLOW_;
    rcsIconSplit_      <- rcsIconSplit_      %??% " | ";
    rcsColorSplit_     <- rcsColorSplit_     %??% rcsColorsYELLOW_;
    rcsTimeStart_      <- rcsTimeStart_      %??% RCT_SYS_TIME_NOW_;
    rcsTimeStop_       <- rcsTimeStop_       %??% RCT_SYS_TIME_NOW_;
    rcsFormatDT_       <- rcsFormatDT_       %??% rasMfmrFORMATS$DATE_LONG_V03;
    rcsColorTimeStamp_ <- rcsColorTimeStamp_ %??% rcsColorsYELLOW_;
    rcsColorProjID_    <- rcsColorProjID_    %??% rcsColorsGREEN_;
    rcsColorFuncType_  <- rcsColorFuncType_  %??% rcsColorsYELLOW_;
    rcsColorCallerID_  <- rcsColorCallerID_  %??% rcsColorsMAGENTA_;
    rcsColorMainText_  <- rcsColorMainText_  %??% rcsColorsCYAN_;
    rsbPrintPretty_    <- rsbPrintPretty_    %??% TRUE;
    rsiStartCELN_      <- rsiStartCELN_      %??% 1L;
    rsiStopCELN_       <- rsiStopCELN_       %??% 28L;
    rcsIconCarat_      <- rcsIconCarat_      %??% rasIfELSE(rsiFuncMode01_ == 1L,
                                                            rcsIconSPARK_, rcsIconSKULL_);
    
    
    
    ### STEP 04 - Prime the DateTime Values ... ####
    rcsTimeStartFORMATTED_ <- rasStrFormTIME(
      x = rcsTimeStart_, format = rcsFormatDT_
    );
    rcsTimeStopFORMATTED_ <- rasStrFormTIME(
      x = rcsTimeStop_, format = rcsFormatDT_
    );
    
    
    
    ### STEP 08 - Apply the Carat Icon Setting ... ####
    if (!rasIsNULL(rcsIconCarat_)) {   # <- Check that "IconCarat" is NOT NULL !!!
      if (rsbPrintPretty_) {
        if (rcsIconCarat_ == "=>" || rcsIconCarat_ == " => " ||
            rcsIconCarat_ == "->" || rcsIconCarat_ == " -> ") {
          rcsIconCarat_ <- rasPASTE0(
            " ",                       # -> Add "leading" white space ...
            rcsColorCarat_,            # -> Apply specified text colour ...
            rasMfmrICONS$ArrowRIGHT,   # -> Assign the MFMR Arrow Icon !!!
            " ",                       # -> Add "trailing" white space ...
            rcsAnsiRESET_              # -> Deactivate text formatting !!!
          );
        } else {
          rcsIconCarat_ <- rasPASTE0(
            " ",              # -> Add "leading" white space ...
            rcsColorCarat_,   # -> Apply specified text colour ...
            rcsIconCarat_,    # -> Assign the specified Carat Icon !!!
            " ",              # -> Add "trailing" white space ...
            rcsAnsiRESET_     # -> Deactivate text formatting !!!
          );
        }
      }
    } else {
      if (rsbPrintPretty_) {
        rcsIconCarat_ <- rasPASTE0(
          " ",                       # -> Add "leading" white space ...
          rcsColorCarat_,            # -> Apply specified text colour ...
          rasMfmrICONS$ArrowRIGHT,   # -> Assign the MFMR Arrow Icon !!!
          " ",                       # -> Add "trailing" white space ...
          rcsAnsiRESET_              # -> Deactivate text formatting !!!
        );
      } else {
        rcsIconCarat_ <- " => ";   # -> Apply a simple <default> Carat Icon !!!
      }
    }
    
    
    
    ### STEP 09 - Apply the "Project-ID" Text Formatting ... ####
    if (rasIsNULL(rssProjID_)) {
      rssProjID_ <- base::get0(        # -> Searches the Global Environment of the
        "RCT_TAG_PROJ_ID_",            #    Active R Session for the <somewhat>
        envir = .GlobalEnv,            #    unique variable name "rssTagProjID_"
        ifnotfound = "UNK. Proj. R"    #    and extracts the value contained in
      );                               #    that variable (if it exists) ... or
    }                                  #    else returns the "NOT-FOUND" value.
    if (rsbPrintPretty_) {
      rssProjID_ <- rasPASTE0(
        rcsAnsiBOLD_,      # -> Apply a BOLD text formatting ... 
        rcsColorProjID_,   # -> Apply the specified text colour ... 
        rssProjID_,        # -> Add the "Caller-ID" string value !!!
        rcsAnsiRESET_      # -> Deactivate text formatting !!!
      );
    }
    
    
    
    ### STEP 10 - Apply the "Split-Icon" Text Formatting ... ####
    if (!rasIsNULL(rcsIconSplit_)) {
      if (rsbPrintPretty_) {
        rcsIconSplit_ <- rasPASTE0(
          rcsAnsiBOLD_,     # -> Apply a BOLD text formatting ... 
          rcsColorSplit_,   # -> Apply the specified text colour ... 
          rcsIconSplit_,    # -> Add the "Split-Icon" string value !!!
          rcsAnsiRESET_     # -> Deactivate text formatting !!!
        );
      }
    } else {
      if (rsbPrintPretty_) {
        rcsIconSplit_ <- rasPASTE0(
          rcsAnsiBOLD_,     # -> Apply a BOLD text formatting ... 
          rcsColorSplit_,   # -> Apply the specified text colour ... 
          " | ",            # -> Add the <default> "Split-Icon" string value !!!
          rcsAnsiRESET_     # -> Deactivate text formatting !!!
        );
      } else {
        rcsIconSplit_ <- " | ";   # -> Add a <basic> "Split-Icon" string value !!!
      }
    }
    
    
    
    ###   STEP 11 - Apply the "Func-Type" Text Formatting   ####
    rssFuncType_ <- rasMfmrClassFUNC(
      siStartCELN = rsiStartCELN_, siStopCELN = rsiStopCELN_
    );
    if (!rasIsNULL(rssFuncType_)) {
      if (rsbPrintPretty_) {
        rssFuncType_ <- rasPASTE0(
          rcsAnsiBOLD_,        # -> Apply a BOLD text formatting ... 
          rcsColorFuncType_,   # -> Apply the specified text colour ... 
          rssFuncType_,        # -> Add the "Func-Type" string value !!!
          rcsAnsiRESET_        # -> Deactivate text formatting !!!
        );
      }
    } else {
      if (rsbPrintPretty_) {
        rssFuncType_ <- rasPASTE0(
          rcsAnsiBOLD_,     # -> Apply a BOLD text formatting ... 
          rcsColorSplit_,   # -> Apply the specified text colour ... 
          "UNK.",           # -> Add the <default> "Func-Type" string value !!!
          rcsAnsiRESET_     # -> Deactivate text formatting !!!
        );
      } else {
        rssFuncType_ <- "UNK.";   # -> Add a <basic> "Func-Type" string value !!!
      }
    }
    
    
    
    ### STEP 12 - Apply the "Caller-ID" Text Formatting ... ####
    if (!rasIsNULL(rssFuncCallerID_)) {
      if (rsbPrintPretty_) {
        rssFuncCallerID_ <- rasPASTE0(
          rcsAnsiBOLD_,        # -> Apply a BOLD text formatting ... 
          rcsColorCallerID_,   # -> Apply the specified text colour ... 
          rssFuncCallerID_,    # -> Add the "Caller-ID" string value !!!
          rcsAnsiRESET_        # -> Deactivate text formatting !!!
        );
      }
    } else {
      if (rsbPrintPretty_) {
        rssFuncCallerID_ <- rasPASTE0(
          rcsAnsiBOLD_,        # -> Apply a BOLD text formatting ... 
          rcsColorCallerID_,   # -> Apply the specified text colour ... 
          "UNK.",              # -> Add the <default> "Caller-ID" string value !!!
          rcsAnsiRESET_        # -> Deactivate text formatting !!!
        );
      } else {
        rssFuncCallerID_ <- "UNK.";   # -> Add a <basic> "Caller-ID" string value !!!
      }
    }
    
    
    
    ### STEP 13 - Apply the "Time-Stamp" Text Formatting ... ####
    if (rsiFuncMode01_ == 1L) {   # -> Apply the ENTER function Info !!!
      if (rsbPrintPretty_) {
        rcsTimeStamp_ <- rasPASTE0(
          rcsAnsiBOLD_,             # -> Apply a BOLD text formatting ... 
          rcsColorTimeStamp_,       # -> Apply the specified text colour ... 
          rcsTimeStartFORMATTED_,   # ...
          rcsAnsiRESET_             # -> Deactivate text formatting !!!
        );
      } else {
        rcsTimeStamp_ <- rcsTimeStartFORMATTED_;
      }
    } else if (rsiFuncMode01_ == 0L) {   # -> Apply the EXIT function Info !!!
      if (rsbPrintPretty_) {
        rcsTimeStamp_ <- rasPASTE0(
          rcsAnsiBOLD_,            # -> Apply a BOLD text formatting ... 
          rcsColorTimeStamp_,      # -> Apply the specified text colour ... 
          rcsTimeStopFORMATTED_,   # ...
          rcsAnsiRESET_            # -> Deactivate text formatting !!!
        );
      } else {
        rcsTimeStamp_ <- rcsTimeStopFORMATTED_;
      }
    }
    
    
    
    # 4.3.1.2 - Post the `ENTER` notification (Func-Self-ID) text ...
    if (rsiFuncMode01_ == 1L) {   # -> Apply the ENTER function Info Post !!!
      if (rsbPrintPretty_) {
        rasCAT(
          rasPASTE0(
            rcsIconCarat_, rssProjID_, rcsIconSplit_,
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, "F-START { <F-SID: '", rcsAnsiRESET_
            ),
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorCallerID_, rssFuncSelfID_, rcsAnsiRESET_
            ),
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, "'  F-Type: '", rcsAnsiRESET_
            ), 
            rssFuncType_,
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, "'>  F-Caller: '", rcsAnsiRESET_
            ), 
            rssFuncCallerID_,
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, "'  <Time: ", rcsAnsiRESET_
            ), 
            rcsTimeStamp_,
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, ">  }\n", rcsAnsiRESET_
            )
          )
        );
      } else {
        rasCAT(
          rasPASTE0(
            rcsIconCarat_, rssProjID_, rcsIconSplit_,
            "F-START {  <F-SID: '", rssFuncSelfID_, 
            "'  F-Type: '", rssFuncType_, "'> ",
            " F-Caller: '", rssFuncCallerID_,
            "'  <Time: ", rcsTimeStamp_, ">  }\n"
          )
        );
      }
    } else if (rsiFuncMode01_ == 0L) {   # -> Apply the EXIT function Info Post !!!
      rcsDeltaTIME_ <- rcf_calc.time.delta(rcsTimeStart_, rcsTimeStop_);
      if (rsbPrintPretty_) {
        rasCAT(
          rasPASTE0(
            rcsIconCarat_, rssProjID_, rcsIconSplit_,
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, "F-STOP { <F-SID: '", rcsAnsiRESET_
            ),
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorCallerID_, rssFuncSelfID_, rcsAnsiRESET_
            ),
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, "'  F-Caller: '", rcsAnsiRESET_
            ), 
            rssFuncCallerID_,
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, "'>  Time: ", rcsAnsiRESET_
            ), 
            rcsTimeStamp_,
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, " ( F-RunTime: ", rcsAnsiRESET_
            ),
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorTimeStamp_, rcsDeltaTIME_, rcsAnsiRESET_
            ),
            rasPASTE0(
              rcsAnsiBOLD_, rcsColorMainText_, " ) }\n", rcsAnsiRESET_
            )
          )
        );
      } else {
        rasCAT(
          rasPASTE0(
            rcsIconCarat_, rssProjID_, rcsIconSplit_,
            "F-STOP { <F-SID: '", rssFuncSelfID_, 
            "'  F-Caller: '", rssFuncCallerID_, "'> ",
            " Time: ", rcsTimeStamp_, "",
            " ( F-RunTime: ", rcsDeltaTIME_, " ) }\n"
          )
        );
      }
    }
    
    # 4.3.1.3 - Output the `ENTER` "Func-Self-ID' properties ...
    rcoListFuncRes_ <- rasLIST(
      "FuncID" = rssFuncSelfID_, "FuncType" = rssFuncType_, 
      "ProjID" = rssProjID_, "CallerID" = rssFuncCallerID_,
      "FuncSTART" = rcsTimeStart_, "FuncSTOP" = rcsTimeStop_
    );
    rasINVISIBLE(rasLIST("SelfID" = rcoListFuncRes_));
  }
  
}


