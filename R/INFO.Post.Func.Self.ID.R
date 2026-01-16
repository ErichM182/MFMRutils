#? ### ### ### ### ### ### ###
#' @title Print R Function Self-ID Information
#' @name info.post.func.self.id
#' @family INFO Functions (SuiteMFMR)
#' 
#' 
#' @description
#' A simple Helper Function that compiles and prints the self-identification (i.e. self-ID, type, 
#' caller-ID, run-time duration etc.) information of a custom R function. This function was intended
#' to mainly support the MFMR Suite of R Functions (aka SuiteMFMR), but can be utilized as a 
#' standalone function in other (i.e. 3rd Party) R packages.
#'
#'
#' @param ssProjID ([character]) a String or Text (i.e. [character] [vector]) value that identifies 
#'                 the R Project <script> file that called <invoked> the custom R function (default:
#'                 "UNK-Proj-R").
#' @param ssFuncSelfID ([character]) a String or Text ([character] [vector]) value that identifies
#'                     the active R Function being called (or being executed). This is a SELF-ID tag
#'                     that is meant as recursive reference to the function called <invoked>. If set
#'                     to a value of `NULL` the hard-coded "info.post.func.self.id()" function ID 
#'                     (i.e. "MFMR-Self.ID") will be used in the function outputs (function result).
#' @param ssFuncCallerID ([character]) a String or Text ([character] [vector]) value that identifies 
#'                       the parent function that invoked the execution of the active function (
#'                       default: "UNDEFINED").
#' @param siFuncMode01L ([integer]) a Numeric (i.e. long) value that identifies the two run-time 
#'                      states of this "Self-ID" custom function. A value of `1L` sets the function 
#'                      mode to the `ENTER` (start/open) state; while a value of `0L` sets the 
#'                      function mode to the `EXIT` (stop/close) run-time state (default: `1L`).
#' @param csTimeStart ([double]) the R DateTime (POSIXt) value that defines the "START-Time" (i.e.
#'                    realtime invocation start time) of when the <active> function was invoked.
#' @param csTimeStop ([double]) the R DateTime (POSIXt) value that defines the "STOP-Time" (i.e.
#'                    realtime invocation exit time) of when the <active> function was exited or 
#'                    stopped.
#' @param siStartCELN ([integer]) a Numeric (i.e. long) value that denotes the START of the custom 
#'                    function block of code. The Code Editor Line Number (`CELN`) of the first 
#'                    line of code that defines a custom function - usually identified by an opening 
#'                    curly brace immediately following a Base-R `function()` code call or code
#'                    segment.
#' @param siStopCELN ([integer]) a Numeric (i.e. long) value that denotes the START of the custom 
#'                    function block of code. The Code Editor Line Number (`CELN`) of the first 
#'                    line of code that defines a custom function - usually identified by a closing 
#'                    curly brace signifying the closure of a Base-R `function()` code call or code
#'                    segment.
#' @param sbRunSelfID ([logical]) a Boolean value that defines whether the SELF-ID procedure should 
#'                    be executed (`TRUE`) or not (`FALSE`). This function argument overrides both 
#'                    of the R Project Development DEBUG and VERBOSE trackers (default: `TRUE`).
#' @param ... ([list]) a KV-List (i.e. Key-Value Pair R List) of dynamic R Objects that specify 
#'            values for specific "Fall-Through" function arguments (aka "DotsArgs"). The DotsArgs
#'            applicable here are defined in the `MFMRutils::RENV_FSID` immutable (i.e. R 
#'            Environment-Locked) List. In the `MFMRutils::RENV_FSID` immutable list applicable 
#'            "Fall-Through" options are those with the form "F_ARGS_*".
#'
#'
#' @returns 
#' * This function prints a preconfigured text (Function Self-ID message) directly to the active R 
#'   Session Console when the function argument `sbRunSelfID` or either of the R Project DEBUG and 
#'   VERBOSE R <active> Runtime Trackers (i.e. `RCT_IS_DEBUG_RT_MODE_` & `RCT_IS_VERBOSE_RT_MODE_`, 
#'   respectively) are set to the boolean value of `TRUE`.
#' * This function also outputs key components of the Function Self-ID information in a list as an 
#'   invisible function return value (function result or output).
#'
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
  ssProjID=NULL, ssFuncSelfID=NULL, ssFuncCallerID=NULL, siFuncMode01L=NULL, csTimeStart=NULL, 
  csTimeStop=NULL, siStartCELN=NULL, siStopCELN=NULL, sbRunSelfID=NULL, ...
) {
  
  
  ####   STEP 01 - Prime "Function Self-ID" CONSTANTS   ####
  ## NB: This 👆  is THE 2nd OF ONLY 2 FUNCTIONS [in the entire MFMR Suite of R Functions] THAT DO
  ##     NOT SELF-IDENTIFY (since a Self-ID implementation here will cause infinite recursion) !!!
  RCT_DBL_SYS_TIME_NOW_ <- base::Sys.time();           # <- Extract the <active> System Date-Time.
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";                # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_LONG_ <- "INFO.Post.Func.Self.ID";   # <- FSID - LONG !!!
  RCT_TAG_FUNC_ID_NSID_ <- "Func.SID";                 # <- This Func DOES NOT SELF-ID (NSID) !!!
  
  RCT_INT_CELN_START_ <- 79L;    # <- The Code Editor Line Number (CELN) at which the function 
                                 #    OPENING <normal> brace/bracket "(" is located !!!
  RCT_INT_CELN_STOP_  <- 533L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    CLOSING <curly> brace/bracket "}" is located !!!
  coDotsArgs_ <- base::list(...);   # <- Capture all the "DotsArgs" values here !!!
  
  
  
  ####   STEP 02 - Alias ALL <required> Functions   ####
  ## NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasABS         <- base::abs;
  rasANY         <- base::any;
  rasCAT         <- base::cat;
  rasSUB         <- base::sub;
  rasBaseGET0    <- base::get0;
  rasLIST        <- base::list;
  rasIsNA        <- base::is.na;
  rasTRUNC       <- base::trunc;
  rasROUND       <- base::round;
  rasLENGTH      <- base::length;
  rasRETURN      <- base::return;
  rasIfELSE      <- base::ifelse;
  rasPASTE0      <- base::paste0;
  rasFORMAT      <- base::format;
  rasIsNULL      <- base::is.null;
  rasStrFormTIME <- base::strftime;
  rasINVISIBLE   <- base::invisible;
  rasAsNUMERIC   <- base::as.numeric;
  
  `%??%`           <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasMfmrFSID      <- MFMRutils::RENV_FSID;
  rasMfmrICONS     <- MFMRutils::RENV_ICONS;
  rasMfmrCOLORS    <- MFMRutils::RENV_COLOURS;
  rasMfmrFORMATS   <- MFMRutils::RENV_FORMATS;
  rasMfmrClassFUNC <- MFMRutils::code.classify.func;
  
  # SPECIAL - Constant - TAG - Aliases (NB for the `INFO.Post.*` functions) ...
  RAS_TAG_PROJ_ID_       <- rasMfmrFSID$CONSTS_PROJ_ID_SHORT;
  RAS_TAG_CALLER_ID_     <- rasMfmrFSID$CONSTS_PROJ_ID_SHORT;
  RAS_TAG_FUNC_ID_SHORT_ <- rasMfmrFSID$CONSTS_FUNC_ID_SHORT;
  RAS_IS_MODE_DEBUG_     <- rasMfmrFSID$CONSTS_BOOL_IS_DEBUG;
  RAS_IS_MODE_VERBOSE_   <- rasMfmrFSID$CONSTS_BOOL_IS_VERBOSE;
  
  
  ## SPECIAL: Try to locate & extract the 'isDebugMode' logical (boolean) variable 
  ##          <if set or primed elsewhere> in the current <active> R Project ... 
  sbIsDEBUG_ <- rasBaseGET0(   # <- Searches the Global Environment of the Active R Session for
    RAS_IS_MODE_DEBUG_,        #    the <somewhat> uniquely named variable `RCT_IS_DEBUG_MODE_`
    envir = .GlobalEnv,        #    and extracts its value.
    ifnotfound = FALSE         # -> Assigns a value of `FALSE` if the variable was NOT FOUND in
  );                           #    the Active R Session !!!
  sbIsVERBOSE_ <- rasBaseGET0(   # <- Searches the Global Environment of the Active R Session for
    RAS_IS_MODE_VERBOSE_,        #    the <somewhat> uniquely named variable `RCT_IS_VERBOSE_MODE_`
    envir = .GlobalEnv,          #    and extracts its value.
    ifnotfound = FALSE           # -> Assigns a value of `FALSE` if the variable was NOT FOUND in
  );                             #    the Active R Session !!!
  
  
  ### ONLY RUN the Function SELF-ID Process if the following condition is TRUE !!!
  sbRunSelfID_ <- coDotsArgs_[[rasMfmrFSID$F_ARGS_BOOL_RUN_SELF_ID]] %??% FALSE;   # <- IMPORANT !!!
  if (sbRunSelfID_ || sbIsDEBUG_ || sbIsVERBOSE_) {
    
    ####   STEP 03 - Internalize ALL Function Arguments   ####
    # NOTES: hand-over all func-args to func-local <internal> variables ...
    ssProjID_       <- ssProjID;
    ssFuncSelfID_   <- ssFuncSelfID;
    ssFuncCallerID_ <- ssFuncCallerID;
    siFuncMode01L_  <- siFuncMode01L;
    csTimeStart_    <- csTimeStart;
    csTimeStop_     <- csTimeStop;
    siStartCELN_    <- siStartCELN;
    siStopCELN_     <- siStopCELN;
    
    ####   STEP 04 - Define Critical Constants   ####
    ##   Prime selected variables (akin to constants) ...
    csIconSPARK_     <- rasMfmrICONS$SparkRed;
    csIconSKULL_     <- rasMfmrICONS$SkullOnly;
    csColorsCYAN_    <- rasMfmrCOLORS$CyanFORE;
    csColorsGREEN_   <- rasMfmrCOLORS$GreenFORE;
    csColorsYELLOW_  <- rasMfmrCOLORS$YellowFORE;
    csAnsiBOLD_      <- rasMfmrFORMATS$ANSI_BOLD;
    csAnsiRESET_     <- rasMfmrFORMATS$ANSI_RESET;
    csColorsMAGENTA_ <- rasMfmrCOLORS$MagentaFORE;
    
    
    ####### ### Compile Useful <internal> Custom Functions here !!!
    ## Define a custom function to Extract the String Formatting Setting ... ####
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
    
    
    
    ####   STEP 04 - Run NULL Checks & Prime NB Variables   ####
    ## NOTES: hand-over all func-args to func-local <internal> variables ...
    csTimeStamp_    <- NULL;
    coListFuncRes_  <- NULL;   # -> The <final> function output <results> object.
    ssProjID_       <- ssProjID_       %??% coDotsArgs_[[rasMfmrFSID$F_ARGS_PROJ_ID]] %??% NULL;
    ssFuncSelfID_   <- ssFuncSelfID_   %??% RCT_TAG_FUNC_ID_LONG_;
    ssFuncCallerID_ <- ssFuncCallerID_ %??% NULL;
    siFuncMode01L_  <- siFuncMode01L_  %??% 1L;
    csTimeStart_    <- csTimeStart_    %??% RCT_DBL_SYS_TIME_NOW_;
    csTimeStop_     <- csTimeStop_     %??% RCT_DBL_SYS_TIME_NOW_;
    siStartCELN_    <- siStartCELN_    %??% 1L;
    siStopCELN_     <- siStopCELN_     %??% 28L;
    sbRunSelfID_    <- sbRunSelfID_    %??% FALSE;
    
    ## Prime all "Fall-Through" Function Arguments or values (parameters or variables) ...
    csIconSplit_      <- coDotsArgs_[[rasMfmrFSID$F_ARGS_ICON_SPLIT]]        %??% " | ";
    sbPrintPretty_    <- coDotsArgs_[[rasMfmrFSID$F_ARGS_BOOL_PRINT_PRETTY]] %??% TRUE;
    csColorCarat_     <- coDotsArgs_[[rasMfmrFSID$F_ARGS_COLOR_CARAT]]       %??% csColorsYELLOW_;
    csColorSplit_     <- coDotsArgs_[[rasMfmrFSID$F_ARGS_COLOR_SPLIT]]       %??% csColorsYELLOW_;
    csColorTimeStamp_ <- coDotsArgs_[[rasMfmrFSID$F_ARGS_COLOR_TIME_STAMP]]  %??% csColorsYELLOW_;
    csColorProjID_    <- coDotsArgs_[[rasMfmrFSID$F_ARGS_COLOR_PROJ_ID]]     %??% csColorsGREEN_;
    csColorFuncType_  <- coDotsArgs_[[rasMfmrFSID$F_ARGS_COLOR_FUNC_TYPE]]   %??% csColorsYELLOW_;
    csColorCallerID_  <- coDotsArgs_[[rasMfmrFSID$F_ARGS_COLOR_FUNC_CALLER]] %??% csColorsMAGENTA_;
    csColorFuncSID_   <- coDotsArgs_[[rasMfmrFSID$F_ARGS_COLOR_FUNC_CALLER]] %??% csColorsGREEN_;
    csColorMainText_  <- coDotsArgs_[[rasMfmrFSID$F_ARGS_COLOR_MAIN_TEXT]]   %??% csColorsCYAN_;
    csFormatDT_       <- coDotsArgs_[[rasMfmrFSID$F_ARGS_TIME_FORMAT]] %??% 
                                                                       rasMfmrFORMATS$DATE_LONG_V03;
    csIconCarat_      <- coDotsArgs_[[rasMfmrFSID$F_ARGS_ICON_CARAT]] %??% 
                                        rasIfELSE(siFuncMode01L_ == 1L, csIconSPARK_, csIconSKULL_);
    
    
    
    ### STEP 04 - Prime the DateTime Values ... ####
    rcsTimeStartFORMATTED_ <- rasStrFormTIME(
      x = csTimeStart_, format = csFormatDT_
    );
    rcsTimeStopFORMATTED_ <- rasStrFormTIME(
      x = csTimeStop_, format = csFormatDT_
    );
    
    
    
    ### STEP 08 - Apply the Carat Icon Setting ... ####
    if (!rasIsNULL(csIconCarat_)) {   # <- Check that "IconCarat" is NOT NULL !!!
      if (sbPrintPretty_) {
        if (csIconCarat_ == "=>" || csIconCarat_ == " => " ||
            csIconCarat_ == "->" || csIconCarat_ == " -> ") {
          csIconCarat_ <- rasPASTE0(
            " ",                      # <- Add "leading" white space ...
            csColorCarat_,            # <- Apply specified text colour ...
            rasMfmrICONS$ArrowRIGHT,  # <- Assign the MFMR Arrow Icon !!!
            " ",                      # <- Add "trailing" white space ...
            csAnsiRESET_              # <- Deactivate text formatting !!!
          );
        } else {
          csIconCarat_ <- rasPASTE0(
            " ",             # <- Add "leading" white space ...
            csColorCarat_,   # <- Apply specified text colour ...
            csIconCarat_,    # <- Assign the specified Carat Icon !!!
            " ",             # <- Add "trailing" white space ...
            csAnsiRESET_     # <- Deactivate text formatting !!!
          );
        }
      }
    } else {
      if (sbPrintPretty_) {
        csIconCarat_ <- rasPASTE0(
          " ",                      # <- Add "leading" white space ...
          csColorCarat_,            # <- Apply specified text colour ...
          rasMfmrICONS$ArrowRIGHT,  # <- Assign the MFMR Arrow Icon !!!
          " ",                      # <- Add "trailing" white space ...
          csAnsiRESET_              # <- Deactivate text formatting !!!
        );
      } else {
        csIconCarat_ <- " => ";   # <- Apply a simple <default> Carat Icon !!!
      }
    }
    
    
    
    ### STEP 09 - Apply the "Project-ID" Text Formatting ... ####
    if (rasIsNULL(ssProjID_)) {
      ssProjID_ <- rasBaseGET0(     # <- Searches the Global Environment of the
        RAS_TAG_PROJ_ID_,           #    Active R Session for the <somewhat>
        envir = .GlobalEnv,         #    unique variable name "rssTagProjID_"
        ifnotfound = "UNK-Proj-R"   #    and extracts the value contained in
      );                            #    that variable (if it exists) ... or
    }                               #    else returns the "NOT-FOUND" value.
    if (sbPrintPretty_) {
      ssProjID_ <- rasPASTE0(
        csAnsiBOLD_,      # <- Apply a BOLD text formatting ... 
        csColorProjID_,   # <- Apply the specified text colour ... 
        ssProjID_,        # <- Add the "Caller-ID" string value !!!
        csAnsiRESET_      # <- Deactivate text formatting !!!
      );
    }
    
    
    
    ### STEP 10 - Apply the "Split-Icon" Text Formatting ... ####
    if (!rasIsNULL(csIconSplit_)) {
      if (sbPrintPretty_) {
        csIconSplit_ <- rasPASTE0(
          csAnsiBOLD_,     # <- Apply a BOLD text formatting ... 
          csColorSplit_,   # <- Apply the specified text colour ... 
          csIconSplit_,    # <- Add the "Split-Icon" string value !!!
          csAnsiRESET_     # <- Deactivate text formatting !!!
        );
      }
    } else {
      if (sbPrintPretty_) {
        csIconSplit_ <- rasPASTE0(
          csAnsiBOLD_,     # <- Apply a BOLD text formatting ... 
          csColorSplit_,   # <- Apply the specified text colour ... 
          " | ",           # <- Add the <default> "Split-Icon" string value !!!
          csAnsiRESET_     # <- Deactivate text formatting !!!
        );
      } else {
        csIconSplit_ <- " | ";   # <- Add a <basic> "Split-Icon" string value !!!
      }
    }
    
    
    
    ###   STEP 11 - Apply the "Func-Type" Text Formatting   ####
    rssFuncType_ <- rasMfmrClassFUNC(
      siStartCELN = siStartCELN_, siStopCELN = siStopCELN_
    );
    if (!rasIsNULL(rssFuncType_)) {
      if (sbPrintPretty_) {
        rssFuncType_ <- rasPASTE0(
          csAnsiBOLD_,        # <- Apply a BOLD text formatting ... 
          csColorFuncType_,   # <- Apply the specified text colour ... 
          rssFuncType_,       # <- Add the "Func-Type" string value !!!
          csAnsiRESET_        # <- Deactivate text formatting !!!
        );
      }
    } else {
      if (sbPrintPretty_) {
        rssFuncType_ <- rasPASTE0(
          csAnsiBOLD_,     # <- Apply a BOLD text formatting ... 
          csColorSplit_,   # <- Apply the specified text colour ... 
          "UNK.",          # <- Add the <default> "Func-Type" string value !!!
          csAnsiRESET_     # <- Deactivate text formatting !!!
        );
      } else {
        rssFuncType_ <- "UNK.";   # -> Add a <basic> "Func-Type" string value !!!
      }
    }
    
    
    
    ### STEP 12 - Apply the "Caller-ID" Text Formatting ... ####
    if (!rasIsNULL(ssFuncCallerID_)) {
      if (sbPrintPretty_) {
        ssFuncCallerID_ <- rasPASTE0(
          csAnsiBOLD_,        # <- Apply a BOLD text formatting ... 
          csColorCallerID_,   # <- Apply the specified text colour ... 
          ssFuncCallerID_,    # <- Add the "Caller-ID" string value !!!
          csAnsiRESET_        # <- Deactivate text formatting !!!
        );
      }
    } else {
      if (sbPrintPretty_) {
        ssFuncCallerID_GET0_ <- rasBaseGET0(
          RAS_TAG_FUNC_ID_SHORT_,          # <- Find the parent <caller> Function ID (if defined)...
          envir = base::pos.to.env(-1L),   # <- The R environment the function was called from !!!
          ifnotfound = "UNDEFINED"         # <- Set a DEFAULT <caller> Function Identifier <UNKNOWN> 
        );
        ssFuncCallerID_ <- rasPASTE0(
          csAnsiBOLD_,            # <- Apply a BOLD text formatting ... 
          csColorCallerID_,       # <- Apply the specified text colour ... 
          ssFuncCallerID_GET0_,   # <- Add the <default> "Caller-ID" string value !!!
          csAnsiRESET_            # <- Deactivate text formatting !!!
        );
      } else {
        ssFuncCallerID_GET0_ <- rasBaseGET0(
          RAS_TAG_FUNC_ID_SHORT_,          # <- Find the parent <caller> Function ID (if defined)...
          envir = base::pos.to.env(-1L),   # <- The R environment the function was called from !!!
          ifnotfound = "UNDEFINED"         # <- Set a DEFAULT <caller> Function Identifier <UNKNOWN> 
        );
        ssFuncCallerID_ <- ssFuncCallerID_GET0_;   # <- Add a <basic> "Caller-ID" string value !!!
      }
    }
    
    
    
    ### STEP 13 - Apply the "Time-Stamp" Text Formatting ... ####
    if (siFuncMode01L_ == 1L) {   # -> Apply the ENTER function Info !!!
      if (sbPrintPretty_) {
        csTimeStamp_ <- rasPASTE0(
          csAnsiBOLD_,              # -> Apply a BOLD text formatting ... 
          csColorTimeStamp_,        # -> Apply the specified text colour ... 
          rcsTimeStartFORMATTED_,   # ...
          csAnsiRESET_              # -> Deactivate text formatting !!!
        );
      } else {
        csTimeStamp_ <- rcsTimeStartFORMATTED_;
      }
    } else if (siFuncMode01L_ == 0L) {   # -> Apply the EXIT function Info !!!
      if (sbPrintPretty_) {
        csTimeStamp_ <- rasPASTE0(
          csAnsiBOLD_,             # -> Apply a BOLD text formatting ... 
          csColorTimeStamp_,       # -> Apply the specified text colour ... 
          rcsTimeStopFORMATTED_,   # ...
          csAnsiRESET_             # -> Deactivate text formatting !!!
        );
      } else {
        csTimeStamp_ <- rcsTimeStopFORMATTED_;
      }
    }
    
    
    
    # 4.3.1.2 - Post the `ENTER` notification (Func-Self-ID) text ...
    if (siFuncMode01L_ == 1L) {   # -> Apply the ENTER function Info Post !!!
      if (sbPrintPretty_) {
        rasCAT(
          rasPASTE0(
            csIconCarat_, ssProjID_, csIconSplit_,
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, "F-START { <F-SID: '", csAnsiRESET_
            ),
            rasPASTE0(
              csAnsiBOLD_, csColorFuncSID_, ssFuncSelfID_, csAnsiRESET_
            ),
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, "'  F-Type: '", csAnsiRESET_
            ), 
            rssFuncType_,
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, "'>  F-Caller: '", csAnsiRESET_
            ), 
            ssFuncCallerID_,
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, "'  <Time: ", csAnsiRESET_
            ), 
            csTimeStamp_,
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, ">  }\n", csAnsiRESET_
            )
          )
        );
      } else {
        rasCAT(
          rasPASTE0(
            csIconCarat_, ssProjID_, csIconSplit_,
            "F-START {  <F-SID: '", ssFuncSelfID_, 
            "'  F-Type: '", rssFuncType_, "'> ",
            " F-Caller: '", ssFuncCallerID_,
            "'  <Time: ", csTimeStamp_, ">  }\n"
          )
        );
      }
    } else if (siFuncMode01L_ == 0L) {   # -> Apply the EXIT function Info Post !!!
      rcsDeltaTIME_ <- rcf_calc.time.delta(csTimeStart_, csTimeStop_);
      if (sbPrintPretty_) {
        rasCAT(
          rasPASTE0(
            csIconCarat_, ssProjID_, csIconSplit_,
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, "F-STOP { <F-SID: '", csAnsiRESET_
            ),
            rasPASTE0(
              csAnsiBOLD_, csColorFuncSID_, ssFuncSelfID_, csAnsiRESET_
            ),
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, "'  F-Caller: '", csAnsiRESET_
            ), 
            ssFuncCallerID_,
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, "'>  Time: ", csAnsiRESET_
            ), 
            csTimeStamp_,
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, " ( F-RunTime: ", csAnsiRESET_
            ),
            rasPASTE0(
              csAnsiBOLD_, csColorTimeStamp_, rcsDeltaTIME_, csAnsiRESET_
            ),
            rasPASTE0(
              csAnsiBOLD_, csColorMainText_, " ) }\n", csAnsiRESET_
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
            " ( F-RunTime: ", rcsDeltaTIME_, " ) }\n"
          )
        );
      }
    }
    
    # 4.3.1.3 - Output the `ENTER` "Func-Self-ID' properties ...
    coListFuncRes_ <- rasLIST(
      "FuncID" = ssFuncSelfID_, "FuncType" = rssFuncType_, 
      "ProjID" = ssProjID_, "CallerID" = ssFuncCallerID_,
      "FuncSTART" = csTimeStart_, "FuncSTOP" = csTimeStop_
    );
    rasINVISIBLE(rasLIST("SelfID" = coListFuncRes_));
  }
  
}


