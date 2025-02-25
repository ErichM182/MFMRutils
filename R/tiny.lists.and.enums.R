#? ### ### ### ### ### ### ###
#
#  This R Script collates a collection of tiny, loose (i.e. non-contained) and
#  standalone lists and enumerated data objects (enums) that support the broader
#  MFMR Suite of R Functions.
#
#? ### ### ###



#? ### ### ### ### ### ### ###
#' @title ANSI Text Formats for use in R ...
#' @description
#' A collection of frequently used ANSI (American National Standards Institute) Text Font Formats to support the MFMR Suite of R Functions.
#'
#' @usage ANSIsFormat   ### <- if "MFMRutils" library is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' ANSIsFormat$BOLD      ### <- sets the text font format to BOLD !!!
#' ANSIsFormat$ITALICS   ### <- sets the text font format to ITALICS !!!
#' ANSIsFormat$RESET     ### <- removes any actively applied ANSI Text Formatting !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::ANSIsFormat$BOLD   ### <- sets the text font format to BOLD !!!
#'
#' @export
#? ### ### ###
ANSIsFormat <- {   # <- "ANSIs" == ... !!!
  envList <- base::list2env(
    base::list(
      RESET     = "\033[0m",   ### Output: Normal text !!!
      BOLD      = "\033[1m",   ### Output: BOLD text !!!
      ITALICS   = "\033[3m",   ### Output: Italicized text !!!
      UNDERLINE = "\033[4m",   ### Output: Underlined text !!!
      REVERSE   = "\033[7m"    ### Output: Inverted text (character order) !!!
    )
  );
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  envList;   # <- return (export) the env-locked list !!!
}







#? ### ### ### ### ### ### ###
#' @title ANSI Colours for use in R ...
#' @description
#' A collection of frequently used ANSI (American National Standards Institute) Colours to support the MFMR Suite of R Functions.
#'
#' @usage ANSIsColors   ### <- if "MFMRutils" library is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' ANSIsColors$BlackFORE     ### <- prints the text in black font colour !!!
#' ANSIsColors$CyanFORE      ### <- prints the text in cyan font colour !!!!!!
#' ANSIsColors$MagentaBACK   ### <- prints the text background in black font colour !!! !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::ANSIsColors$OoglyEyes   ### <- prints "👀" to the R console !!!
#'
#' @export
#? ### ### ###
ANSIsColors <- {   # <- "ANSIs" == ... !!!
  envList <- base::list2env(
    base::list(
      BlackFORE   = "\033[30m",   ### Output: "➜"
      RedFORE     = "\033[31m",   ### Output: "➜"
      GreenFORE   = "\033[32m",   ### Output: "➜"
      YellowFORE  = "\033[33m",   ### Output: "➜"
      BlueFORE    = "\033[34m",   ### Output: "➜"
      MagentaFORE = "\033[35m",   ### Output: "➜"
      CyanFORE    = "\033[36m",   ### Output: "➜"
      WhiteFORE   = "\033[37m",   ### Output: "➜"
      BlackBACK   = "\033[40m",   ### Output: "➜"
      RedBACK     = "\033[41m",   ### Output: "➜"
      GreenBACK   = "\033[42m",   ### Output: "➜"
      YellowBACK  = "\033[43m",   ### Output: "➜"
      BlueBACK    = "\033[44m",   ### Output: "➜"
      MagentaBACK = "\033[45m",   ### Output: "➜"
      CyanBACK    = "\033[46m",   ### Output: "➜"
      WhiteBACK   = "\033[47m",   ### Output: "➜"

      BlackBrightFORE   = "\033[90m",   ### Output: "➜"
      RedBrightFORE     = "\033[91m",   ### Output: "➜"
      GreenBrightFORE   = "\033[92m",   ### Output: "➜"
      YellowBrightFORE  = "\033[93m",   ### Output: "➜"
      BlueBrightFORE    = "\033[94m",   ### Output: "➜"
      MagentaBrightFORE = "\033[95m",   ### Output: "➜"
      CyanBrightFORE    = "\033[96m",   ### Output: "➜"
      WhiteBrightFORE   = "\033[97m",   ### Output: "➜"
      BlackBrightBACK   = "\033[100m",   ### Output: "➜"
      RedBrightBACK     = "\033[101m",   ### Output: "➜"
      GreenBrightBACK   = "\033[102m",   ### Output: "➜"
      YellowBrightBACK  = "\033[103m",   ### Output: "➜"
      BlueBrightBACK    = "\033[104m",   ### Output: "➜"
      MagentaBrightBACK = "\033[105m",   ### Output: "➜"
      CyanBrightBACK    = "\033[106m",   ### Output: "➜"
      WhiteBrightBACK   = "\033[107m"   ### Output: "➜"
    )
  );
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  envList;   # <- return (export) the env-locked list !!!
}







#? ### ### ### ### ### ### ###
#' @title Unicode Icons for use in R ...
#' @description
#' A collection of frequently used icons (unicode characters) meant to support the MFMR Suite of R Functions.
#'
#' @usage IconsLUCCs   ### <- if "MFMRutils" library is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' IconsLUCCs$OoglyEyes      ### <- prints "👀" to the R console !!!
#' IconsLUCCs$CheckMark      ### <- prints "✔" to the R console !!!
#' IconsLUCCs$SmileyPonder   ### <- prints "🤔" to the R console !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::IconsLUCCs$OoglyEyes   ### <- prints "👀" to the R console !!!
#'
#' @export
#? ### ### ###
IconsLUCCs <- {   # <- "LUCCs" == List [L] of UniCode Characters [UCC] !!!
  envList <- base::list2env(
    base::list(
      ArrowRIGHT   = "\u279C",       ### Output: "➜"
      CheckMark    = "\u2714",       ### Output: "✔"
      FireFlame    = "\U0001F525",   ### Output: "🔥"
      SmileyPonder = "\U0001F914",   ### Output: "🤔"
      SmileySad    = "\U0001F622",   ### Output: "😢"
      OoglyEyes    = "\U0001F440",   ### Output: "👀"
      PointUP      = "\U0001F446",   ### Output: "👆"
      XSlanted     = "\u2716"        ### Output: "✖"
    )
  );
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  envList;   # <- return (export) the env-locked list !!!
}


