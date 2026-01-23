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
#'                 the R Project (script) file that called <invoked> the custom R function (default:
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
#'  siFuncMode01 = 1L, sbRunSelfID = TRUE
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
  ##  NB: This 👆 is THE 2nd OF ONLY 2 FUNCTIONS [in the entire MFMR Suite of R Functions] THAT DO
  ##      NOT SELF-IDENTIFY (since a Self-ID implementation here will cause infinite recursion) !!!
  RCT_DBL_SYS_TIME_NOW_    <- base::Sys.time();           # <- Extract the <active> System Date-Time.
  RCT_TAG_FUNC_LIBR_ID_    <- "MFMRutils";                # <- R Library Identifier !!!
  RCT_TAG_R_FUNC_ID_NSIDS_ <- "Func.SID";                 # <- This Func DOES NOT SELF-ID (NSID) !!!
  RCT_TAG_R_FUNC_ID_NSIDL_ <- "INFO.Post.Func.Self.ID";   # <- FSID - LONG !!!
  
  RCT_INT_CELN_START_ <- 79L;    # <- The Code Editor Line Number (CELN) at which the function 
                                 #    OPENING <normal> brace/bracket "(" is located !!!
  RCT_INT_CELN_STOP_  <- 572L;   # <- The Code Editor Line Number (CELN) at which the function 
                                 #    CLOSING <curly> brace/bracket "}" is located !!!
  coDotsArgs_ <- base::list(...);   # <- Capture all the "DotsArgs" values here !!!
  
  
  
  ####   STEP 02 - Alias ALL <required> Functions   ####
  ##  NOTES: This is a NEW approach to improve R Session Memory Efficiency ...
  rasBaseABS         <- base::abs;
  rasBaseANY         <- base::any;
  rasBaseCAT         <- base::cat;
  rasBaseSUB         <- base::sub;
  rasBaseGET0        <- base::get0;
  rasBaseLIST        <- base::list;
  rasBaseIsNA        <- base::is.na;
  rasBaseTRUNC       <- base::trunc;
  rasBaseROUND       <- base::round;
  rasBaseLENGTH      <- base::length;
  rasBaseRETURN      <- base::return;
  rasBaseIfELSE      <- base::ifelse;
  rasBasePASTE0      <- base::paste0;
  rasBaseFORMAT      <- base::format;
  rasBaseIsNULL      <- base::is.null;
  rasBaseStrFormTIME <- base::strftime;
  rasBaseINVISIBLE   <- base::invisible;
  rasBaseAsNUMERIC   <- base::as.numeric;
  
  `%??%`            <- MFMRutils::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasMfmrFSID       <- MFMRutils::RENV_FSID;
  rasMfmrICONS      <- MFMRutils::RENV_ICONS;
  rasMfmrCOLORS     <- MFMRutils::RENV_COLOURS;
  rasMfmrFORMATS    <- MFMRutils::RENV_FORMATS;
  rasMfmrClassFUNC  <- MFMRutils::code.classify.func;
  rasMfmrPollRTMODE <- MFMRutils::code.poll.r.runtime.mode;
  
  ## SPECIAL - Constant - TAG - Aliases (NB for the `INFO.Post.*` functions) ...
  RCT_RCO_RT_MODE_              <- rasMfmrPollRTMODE();                   # <- R Run-Time Mode/State !!!
  RCT_TAG_PROJ_ID_              <- rasMfmrFSID$F_ARGS_PROJ_ID;
  RCT_TAG_FUNC_SELF_ID_         <- rasMfmrFSID$F_ARGS_FUNC_SID;
  RCT_TAG_FUNC_RT_STOP_         <- rasMfmrFSID$F_ARGS_TIME_STOP;
  RCT_TAG_FUNC_RT_START_        <- rasMfmrFSID$F_ARGS_TIME_START;
  RCT_TAG_FUNC_CALLER_ID_       <- rasMfmrFSID$F_ARGS_FUNC_CALLER;
  RCT_TAG_FUNC_MODE_01L_        <- rasMfmrFSID$F_ARGS_FUNC_MODE_01L;
  RCT_TAG_FUNC_CELN_STOP_       <- rasMfmrFSID$F_ARGS_FUNC_CELN_STOP;
  RCT_TAG_FUNC_CELN_START_      <- rasMfmrFSID$F_ARGS_FUNC_CELN_START;
  RCT_TAG_RUN_SELF_ID_          <- rasMfmrFSID$F_ARGS_BOOL_RUN_SELF_ID;
  RCT_TAG_ICON_SPLIT_           <- rasMfmrFSID$F_ARGS_ICON_SPLIT;
  RCT_TAG_ICON_CARAT_           <- rasMfmrFSID$F_ARGS_ICON_CARAT;
  RCT_TAG_IS_PRINT_PRETTY_      <- rasMfmrFSID$F_ARGS_BOOL_PRINT_PRETTY;
  RCT_TAG_COLOR_CARAT_          <- rasMfmrFSID$F_ARGS_COLOR_CARAT;
  RCT_TAG_COLOR_SPLIT_          <- rasMfmrFSID$F_ARGS_COLOR_SPLIT;
  RCT_TAG_COLOR_TIME_STAMP_     <- rasMfmrFSID$F_ARGS_COLOR_TIME_STAMP;
  RCT_TAG_COLOR_PROJ_ID_        <- rasMfmrFSID$F_ARGS_COLOR_PROJ_ID;
  RCT_TAG_COLOR_FUNC_TYPE_      <- rasMfmrFSID$F_ARGS_COLOR_FUNC_TYPE;
  RCT_TAG_COLOR_FUNC_CALLER_    <- rasMfmrFSID$F_ARGS_COLOR_FUNC_CALLER;
  RCT_TAG_COLOR_FUNC_SELF_ID_   <- rasMfmrFSID$F_ARGS_COLOR_FUNC_SELF_ID;
  RCT_TAG_COLOR_MAIN_NOTE_TEXT_ <- rasMfmrFSID$F_ARGS_COLOR_MAIN_TEXT;
  
  
  
  ####   STEP 03 - Run Function Code Logic ( accordingly )   ####
  ##  ONLY RUN the Function SELF-ID Process if the following condition is TRUE !!!
  sbIsDEBUG_   <- RCT_RCO_RT_MODE_$IS_DEBUG;
  sbIsVERBOSE_ <- RCT_RCO_RT_MODE_$IS_VERBOSE;
  sbRunSelfID_ <- sbRunSelfID %??% coDotsArgs_[[RCT_TAG_RUN_SELF_ID_]] %??% FALSE;   # <- IMPORANT !
  if (sbRunSelfID_ || sbIsDEBUG_ || sbIsVERBOSE_) {
    
    
    ####   3.01 - Define Critical Constants   ####
    ##   Prime selected variables (akin to constants) ...
    csAnsiBOLD_  <- rasMfmrFORMATS$ANSI_BOLD;
    csAnsiRESET_ <- rasMfmrFORMATS$ANSI_RESET;
    
    csIconSPARK_ <- rasMfmrICONS$SparkRed;
    csIconSKULL_ <- rasMfmrICONS$SkullOnly;
    
    csColorsCYAN_    <- rasMfmrCOLORS$CyanFORE;
    csColorsGREEN_   <- rasMfmrCOLORS$GreenFORE;
    csColorsYELLOW_  <- rasMfmrCOLORS$YellowFORE;
    csColorsMAGENTA_ <- rasMfmrCOLORS$MagentaFORE;
    
    csFormatDateLONGv03_ <- rasMfmrFORMATS$DATE_LONG_V03;
    
    
    
    ####   3.02 - Internalize ALL Function Arguments   ####
    ##  NOTES: hand-over all func-args to func-local <internal> variables ...
    csTimeStamp_    <- NULL;
    coListFuncRes_  <- NULL;          # -> The <final> function output <results> object.
    ssProjID_       <- ssProjID       %??% coDotsArgs_[[RCT_TAG_PROJ_ID_]];
    ssFuncSelfID_   <- ssFuncSelfID   %??% coDotsArgs_[[RCT_TAG_FUNC_SELF_ID_]];
    ssFuncCallerID_ <- ssFuncCallerID %??% coDotsArgs_[[RCT_TAG_FUNC_CALLER_ID_]];
    siFuncMode01L_  <- siFuncMode01L  %??% coDotsArgs_[[RCT_TAG_FUNC_MODE_01L_]];
    csTimeStop_     <- csTimeStop     %??% coDotsArgs_[[RCT_TAG_FUNC_RT_STOP_]];
    csTimeStart_    <- csTimeStart    %??% coDotsArgs_[[RCT_TAG_FUNC_RT_START_]];
    siStopCELN_     <- siStopCELN     %??% coDotsArgs_[[RCT_TAG_FUNC_CELN_STOP_]];
    siStartCELN_    <- siStartCELN    %??% coDotsArgs_[[RCT_TAG_FUNC_CELN_START_]];
    
    ##  Prime all "Fall-Through" Function Arguments or values (parameters or variables) ...
    csIconSplit_      <- coDotsArgs_[[RCT_TAG_ICON_SPLIT_]]            %??% " | ";
    sbPrintPretty_    <- coDotsArgs_[[RCT_TAG_IS_PRINT_PRETTY_]]       %??% TRUE;
    csColorCarat_     <- coDotsArgs_[[RCT_TAG_COLOR_CARAT_]]           %??% csColorsYELLOW_;
    csColorSplit_     <- coDotsArgs_[[RCT_TAG_COLOR_SPLIT_]]           %??% csColorsYELLOW_;
    csColorTimeStamp_ <- coDotsArgs_[[RCT_TAG_COLOR_TIME_STAMP_]]      %??% csColorsYELLOW_;
    csColorProjID_    <- coDotsArgs_[[RCT_TAG_COLOR_PROJ_ID_]]         %??% csColorsGREEN_;
    csColorFuncType_  <- coDotsArgs_[[RCT_TAG_COLOR_FUNC_TYPE_]]       %??% csColorsYELLOW_;
    csColorCallerID_  <- coDotsArgs_[[RCT_TAG_COLOR_FUNC_CALLER_]]     %??% csColorsMAGENTA_;
    csColorFuncSID_   <- coDotsArgs_[[RCT_TAG_COLOR_FUNC_SELF_ID_]]    %??% csColorsGREEN_;
    csColorMainText_  <- coDotsArgs_[[RCT_TAG_COLOR_MAIN_NOTE_TEXT_]]  %??% csColorsCYAN_;
    csFormatDT_       <- coDotsArgs_[[rasMfmrFSID$F_ARGS_TIME_FORMAT]] %??% csFormatDateLONGv03_;
    csIconCarat_      <- coDotsArgs_[[RCT_TAG_ICON_CARAT_]]            %??% 
                                    rasBaseIfELSE(siFuncMode01L_ == 1L, csIconSPARK_, csIconSKULL_);
    
    
    
    ####   3.03 - Run NULL Checks & Prime NB Variables   ####
    ##  NOTES: hand-over all func-args to func-local <internal> variables ...
    ssProjID_       <- ssProjID_       %??% NULL;
    ssFuncSelfID_   <- ssFuncSelfID_   %??% RCT_TAG_R_FUNC_ID_NSIDL_;
    ssFuncCallerID_ <- ssFuncCallerID_ %??% NULL;
    siFuncMode01L_  <- siFuncMode01L_  %??% 01L;
    csTimeStop_     <- csTimeStop_     %??% RCT_DBL_SYS_TIME_NOW_;
    csTimeStart_    <- csTimeStart_    %??% RCT_DBL_SYS_TIME_NOW_;
    siStopCELN_     <- siStopCELN_     %??% 01L;
    siStartCELN_    <- siStartCELN_    %??% 28L;
    sbRunSelfID_    <- sbRunSelfID_    %??% FALSE;
    
    
    
    ####   ### Compile Useful <internal> Custom Functions here !!!
    ##  Define a custom function to Extract the String Formatting Setting ... ####
    "rcf_calc.time.delta" <- function(csTimeStart, csTimeStop) {
      
      RCT_1_MINUTE_IN_SECS_ <- 60;
      RCT_1_HOUR_IN_SECS_   <- 60 * 60;
      RCT_1_DAY_IN_SECS_    <- 60 * 60 * 24;
      RCT_1_WEEK_IN_SECS_   <- 60 * 60 * 24 * 7;
      
      
      RCT_MILLIs_1_SEC_ <- 0.999;                   # <- SECOND - Maximum Limit per Time Category !
      RCT_MILLIs_1_MIN_ <- RCT_1_MINUTE_IN_SECS_;   # <- MINUTE - Maximum Limit per Time Category !
      RCT_MILLIs_1_HOR_ <- RCT_1_HOUR_IN_SECS_;     # <- HOUR   - Maximum Limit per Time Category !
      RCT_MILLIs_1_DAY_ <- RCT_1_DAY_IN_SECS_;      # <- DAY    - Maximum Limit per Time Category !
      RCT_MILLIs_1_WEK_ <- RCT_1_WEEK_IN_SECS_;     # <- WEEK   - Maximum Limit per Time Category !
      
      
      csTimeDeltaRAW_ <- rasBaseAsNUMERIC(
        csTimeStop - csTimeStart, units = "secs"
      );
      csTimeDelta_ <- rasBaseAsNUMERIC(csTimeDeltaRAW_[[1]]);
      csTimeDeltaRESULT_ <- NULL;
      csTimeDeltaROUND_ <- rasBaseROUND(csTimeDelta_, 3);
      if (csTimeDeltaROUND_ <= RCT_MILLIs_1_SEC_) {
        ssFloatVals_ <- rasBaseABS(
          csTimeDeltaROUND_ - rasBaseTRUNC(csTimeDeltaROUND_)
        );
        ssFloatsAsInts_ <- rasBaseSUB(
          "^0\\.", "", rasBaseFORMAT(ssFloatVals_, scientific = FALSE)
        );
        csTimeDeltaRESULT_ <- rasBasePASTE0(
          ssFloatsAsInts_, " milli-secs"
        );
      } else if (csTimeDeltaROUND_ > RCT_MILLIs_1_SEC_ && csTimeDeltaROUND_ <= RCT_MILLIs_1_MIN_) {
        ssIntsONLY_ <- rasBaseTRUNC(csTimeDeltaROUND_);
        csTimeDeltaRESULT_ <- rasBasePASTE0(
          ssIntsONLY_, " secs"
        );
      } else if (csTimeDeltaROUND_ > RCT_MILLIs_1_MIN_ && csTimeDeltaROUND_ <= RCT_MILLIs_1_HOR_) {
        ssIntsONLY_  <- rasBaseTRUNC(csTimeDeltaROUND_);
        ssDeltaSecs_ <- ssIntsONLY_ %% RCT_1_MINUTE_IN_SECS_;
        ssDeltaMins_ <- rasBaseTRUNC(ssIntsONLY_ / RCT_1_MINUTE_IN_SECS_);
        csTimeDeltaRESULT_ <- rasBasePASTE0(
          ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
        );
      } else if (csTimeDeltaROUND_ > RCT_MILLIs_1_HOR_ && csTimeDeltaROUND_ <= RCT_MILLIs_1_DAY_) {
        ssIntsONLY_  <- rasBaseTRUNC(csTimeDeltaROUND_);
        ssDeltaSecs_ <- ssIntsONLY_ %% RCT_1_MINUTE_IN_SECS_;
        ssDeltaMins_ <- rasBaseTRUNC(ssIntsONLY_ / RCT_1_MINUTE_IN_SECS_);
        ssDeltaHrs_  <- rasBaseTRUNC(ssIntsONLY_ / RCT_1_HOUR_IN_SECS_);
        csTimeDeltaRESULT_ <- rasBasePASTE0(
          ssDeltaHrs_, " hrs, ", ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
        );
      } else if (csTimeDeltaROUND_ > RCT_MILLIs_1_DAY_ && csTimeDeltaROUND_ <= RCT_MILLIs_1_WEK_) {
        ssIntsONLY_  <- rasBaseTRUNC(csTimeDeltaROUND_);
        ssDeltaSecs_ <- ssIntsONLY_ %% RCT_1_MINUTE_IN_SECS_;
        ssDeltaMins_ <- rasBaseTRUNC(ssIntsONLY_ / RCT_1_MINUTE_IN_SECS_);
        ssDeltaHrs_  <- rasBaseTRUNC(ssIntsONLY_ / RCT_1_HOUR_IN_SECS_);
        rssDeltaDays_ <- rasBaseTRUNC(ssIntsONLY_ / RCT_1_DAY_IN_SECS_);
        csTimeDeltaRESULT_ <- rasBasePASTE0(
          rssDeltaDays_, " days, ", ssDeltaHrs_, " hrs, ", 
          ssDeltaMins_, " mins, ", ssDeltaSecs_, " secs"
        );
      }
      rasBaseRETURN(csTimeDeltaRESULT_);
    }
    
    
    
    ####   3.04 - Prime the DateTime Values   ####
    csTimeStartFORMATTED_ <- rasBaseStrFormTIME(
      x = csTimeStart_, format = csFormatDT_
    );
    csTimeStopFORMATTED_ <- rasBaseStrFormTIME(
      x = csTimeStop_, format = csFormatDT_
    );
    
    
    
    ####   3.05 - Apply the Carat Icon Setting   ####
    if (!rasBaseIsNULL(csIconCarat_)) {   # <- Check that "IconCarat" is NOT NULL !!!
      if (sbPrintPretty_) {
        if (csIconCarat_ == "=>" || csIconCarat_ == " => " ||
            csIconCarat_ == "->" || csIconCarat_ == " -> ") {
          csIconCarat_ <- rasBasePASTE0(
            " ",                      # <- Add "leading" white space ...
            csColorCarat_,            # <- Apply specified text colour ...
            rasMfmrICONS$ArrowRIGHT,  # <- Assign the MFMR Arrow Icon !!!
            " ",                      # <- Add "trailing" white space ...
            csAnsiRESET_              # <- Deactivate text formatting !!!
          );
        } else {
          csIconCarat_ <- rasBasePASTE0(
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
        csIconCarat_ <- rasBasePASTE0(
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
    
    
    
    ####   3.06 - Apply the "Project-ID" Text Formatting   ####
    if (rasBaseIsNULL(ssProjID_)) {
      ssProjID_ <- rasBaseGET0(             # <- Searches the Global Environment of the
        rasMfmrFSID$CONSTS_PROJ_ID_SHORT,   #    Active R Session for the <somewhat>
        envir = .GlobalEnv,                 #    unique variable name "ssProjID"
        ifnotfound = "UNK-Proj-R"           #    and extracts the value contained in
      );                                    #    that variable (if it exists) ... or
    }                                       #    else returns the "NOT-FOUND" value.
    if (sbPrintPretty_) {
      ssProjID_ <- rasBasePASTE0(
        csAnsiBOLD_,      # <- Apply a BOLD text formatting ... 
        csColorProjID_,   # <- Apply the specified text colour ... 
        ssProjID_,        # <- Add the "Caller-ID" string value !!!
        csAnsiRESET_      # <- Deactivate text formatting !!!
      );
    }
    
    
    
    ####   3.07 - Apply the "Split-Icon" Text Formatting   ####
    if (!rasBaseIsNULL(csIconSplit_)) {
      if (sbPrintPretty_) {
        csIconSplit_ <- rasBasePASTE0(
          csAnsiBOLD_,     # <- Apply a BOLD text formatting ... 
          csColorSplit_,   # <- Apply the specified text colour ... 
          csIconSplit_,    # <- Add the "Split-Icon" string value !!!
          csAnsiRESET_     # <- Deactivate text formatting !!!
        );
      }
    } else {
      if (sbPrintPretty_) {
        csIconSplit_ <- rasBasePASTE0(
          csAnsiBOLD_,     # <- Apply a BOLD text formatting ... 
          csColorSplit_,   # <- Apply the specified text colour ... 
          " | ",           # <- Add the <default> "Split-Icon" string value !!!
          csAnsiRESET_     # <- Deactivate text formatting !!!
        );
      } else {
        csIconSplit_ <- " | ";   # <- Add a <basic> "Split-Icon" string value !!!
      }
    }
    
    
    
    ####   3.08 - Apply the "Func-Type" Text Formatting   ####
    rssFuncType_ <- rasMfmrClassFUNC(
      siStartCELN = siStartCELN_, siStopCELN = siStopCELN_
    );
    if (!rasBaseIsNULL(rssFuncType_)) {
      if (sbPrintPretty_) {
        rssFuncType_ <- rasBasePASTE0(
          csAnsiBOLD_,        # <- Apply a BOLD text formatting ... 
          csColorFuncType_,   # <- Apply the specified text colour ... 
          rssFuncType_,       # <- Add the "Func-Type" string value !!!
          csAnsiRESET_        # <- Deactivate text formatting !!!
        );
      }
    } else {
      if (sbPrintPretty_) {
        rssFuncType_ <- rasBasePASTE0(
          csAnsiBOLD_,     # <- Apply a BOLD text formatting ... 
          csColorSplit_,   # <- Apply the specified text colour ... 
          "UNK.",          # <- Add the <default> "Func-Type" string value !!!
          csAnsiRESET_     # <- Deactivate text formatting !!!
        );
      } else {
        rssFuncType_ <- "UNK.";   # -> Add a <basic> "Func-Type" string value !!!
      }
    }
    
    
    
    ####   3.09 - Apply the "Caller-ID" Text Formatting   ####
    if (!rasBaseIsNULL(ssFuncCallerID_)) {
      if (sbPrintPretty_) {
        ssFuncCallerID_ <- rasBasePASTE0(
          csAnsiBOLD_,        # <- Apply a BOLD text formatting ... 
          csColorCallerID_,   # <- Apply the specified text colour ... 
          ssFuncCallerID_,    # <- Add the "Caller-ID" string value !!!
          csAnsiRESET_        # <- Deactivate text formatting !!!
        );
      }
    } else {
      ssTagTopLVL_ <- "TOP-LVL (rsProjMAIN)";
      RCT_TAG_R_FUNC_SELF_ID_ <- rasMfmrFSID$CONSTS_FUNC_ID_LONG;
      if (sbPrintPretty_) {
        ssFuncCallerID_GET0_ <- rasBaseGET0(   
          RCT_TAG_R_FUNC_SELF_ID_,         # <- Find the parent <caller> Function ID (if defined) ...
          envir = base::pos.to.env(-1L),   # <- The R environment the function was called from !!!
          ifnotfound = ssTagTopLVL_        # <- Set a DEFAULT <caller> Function Identifier <UNKNOWN> 
        );
        ssFuncCallerID_ <- rasBasePASTE0(
          csAnsiBOLD_,            # <- Apply a BOLD text formatting ... 
          csColorCallerID_,       # <- Apply the specified text colour ... 
          ssFuncCallerID_GET0_,   # <- Add the <default> "Caller-ID" string value !!!
          csAnsiRESET_            # <- Deactivate text formatting !!!
        );
      } else {
        ssFuncCallerID_GET0_ <- rasBaseGET0(
          RCT_TAG_R_FUNC_SELF_ID_,         # <- Find the parent <caller> Function ID (if defined)...
          envir = base::pos.to.env(-1L),   # <- The R environment the function was called from !!!
          ifnotfound = ssTagTopLVL_        # <- Set a DEFAULT <caller> Function Identifier <UNKNOWN> 
        );
        ssFuncCallerID_ <- ssFuncCallerID_GET0_;   # <- Add a <basic> "Caller-ID" string value !!!
      }
    }
    
    
    
    ####   3.10 - Apply the "Time-Stamp" Text Formatting   ####
    if (siFuncMode01L_ == 1L) {   # <- Apply the ENTER function Info !!!
      if (sbPrintPretty_) {
        csTimeStamp_ <- rasBasePASTE0(
          csAnsiBOLD_,             # -> Apply a BOLD text formatting ... 
          csColorTimeStamp_,       # -> Apply the specified text colour ... 
          csTimeStartFORMATTED_,   # ...
          csAnsiRESET_             # -> Deactivate text formatting !!!
        );
      } else {
        csTimeStamp_ <- csTimeStartFORMATTED_;
      }
    } else if (siFuncMode01L_ == 0L) {   # <- Apply the EXIT function Info !!!
      if (sbPrintPretty_) {
        csTimeStamp_ <- rasBasePASTE0(
          csAnsiBOLD_,            # -> Apply a BOLD text formatting ... 
          csColorTimeStamp_,      # -> Apply the specified text colour ... 
          csTimeStopFORMATTED_,   # ...
          csAnsiRESET_            # -> Deactivate text formatting !!!
        );
      } else {
        csTimeStamp_ <- csTimeStopFORMATTED_;
      }
    }
    
    
    
    ####   3.11 - Post the `ENTER` notification (Func-Self-ID) text   ####
    if (siFuncMode01L_ == 1L) {   # <- Apply the ENTER function Info Post !!!
      if (sbPrintPretty_) {
        rasBaseCAT(
          rasBasePASTE0(
            csIconCarat_, ssProjID_, csIconSplit_,
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, "F-START { <F-SID: '", csAnsiRESET_
            ),
            rasBasePASTE0(
              csAnsiBOLD_, csColorFuncSID_, ssFuncSelfID_, csAnsiRESET_
            ),
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, "'  F-Type: '", csAnsiRESET_
            ), 
            rssFuncType_,
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, "'>  F-Caller: '", csAnsiRESET_
            ), 
            ssFuncCallerID_,
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, "'  <Time: ", csAnsiRESET_
            ), 
            csTimeStamp_,
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, ">  }\n", csAnsiRESET_
            )
          )
        );
      } else {
        rasBaseCAT(
          rasBasePASTE0(
            csIconCarat_, ssProjID_, csIconSplit_,
            "F-START {  <F-SID: '", ssFuncSelfID_, 
            "'  F-Type: '", rssFuncType_, "'> ",
            " F-Caller: '", ssFuncCallerID_,
            "'  <Time: ", csTimeStamp_, ">  }\n"
          )
        );
      }
    } else if (siFuncMode01L_ == 0L) {   # <- Apply the EXIT function Info Post !!!
      csDeltaTIME_ <- rcf_calc.time.delta(csTimeStart_, csTimeStop_);
      if (sbPrintPretty_) {
        rasBaseCAT(
          rasBasePASTE0(
            csIconCarat_, ssProjID_, csIconSplit_,
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, "F-STOP  { <F-SID: '", csAnsiRESET_
            ),
            rasBasePASTE0(
              csAnsiBOLD_, csColorFuncSID_, ssFuncSelfID_, csAnsiRESET_
            ),
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, "'  F-Caller: '", csAnsiRESET_
            ), 
            ssFuncCallerID_,
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, "'>  Time: ", csAnsiRESET_
            ), 
            csTimeStamp_,
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, " ( F-RunTime: ", csAnsiRESET_
            ),
            rasBasePASTE0(
              csAnsiBOLD_, csColorTimeStamp_, csDeltaTIME_, csAnsiRESET_
            ),
            rasBasePASTE0(
              csAnsiBOLD_, csColorMainText_, " ) }\n\n", csAnsiRESET_
            )
          )
        );
      } else {
        rasBaseCAT(
          rasBasePASTE0(
            csIconCarat_, ssProjID_, csIconSplit_,
            "F-STOP { <F-SID: '", ssFuncSelfID_, 
            "'  F-Caller: '", ssFuncCallerID_, "'> ",
            " Time: ", csTimeStamp_, "",
            " ( F-RunTime: ", csDeltaTIME_, " ) }\n\n"
          )
        );
      }
    }
    
    
    
    ####   3.12 - Output the `ENTER` "Func-Self-ID' properties   ####
    coListFuncRes_ <- rasBaseLIST(
      "FuncID" = ssFuncSelfID_,   "FuncType" = rssFuncType_, 
      "ProjID" = ssProjID_,       "CallerID" = ssFuncCallerID_,
      "FuncSTART" = csTimeStart_, "FuncSTOP" = csTimeStop_
    );
    rasBaseINVISIBLE(coListFuncRes_);
  }
  
}


