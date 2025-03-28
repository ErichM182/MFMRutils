#? ### ### ### ### ### ### ###
#
#  This R Script collates a collection of tiny, loose (i.e. non-contained) and
#  standalone lists and enumerated data objects (enums) that support the broader
#  MFMR Suite of R Functions.
#
#? ### ### ###



#? ### ### ### ### ### ### ###
#' @title ANSI Text Formats for use in R (the `MFMRutils` selection)
#' @description
#' A collection of frequently used ANSI (American National Standards Institute)
#' Text Font Formats to support the MFMR Suite of R Functions.
#'
#' @usage MFMRFormat   ### -> if "MFMRutils" is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' MFMRFormat$BOLD      ### -> sets the text font format to BOLD !!!
#' MFMRFormat$ITALICS   ### -> sets the text font format to ITALICS !!!
#' MFMRFormat$RESET     ### -> removes actively applied ANSI Text Formatting !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::MFMRFormat$BOLD   ### -> sets the text font format to BOLD !!!
#'
#' @export
#? ### ### ###
MFMRFormat <- {   # -> Define a static list of "ANSI" Text Formats !!!
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
  envList;   # -> return (export) the env-locked list !!!
}







#? ### ### ### ### ### ### ###
#' @title ANSI Colours for use in R (the `MFMRutils` selection)
#' @description
#' A collection of frequently used ANSI (American National Standards Institute)
#' Colours to support the MFMR Suite of R Functions.
#'
#' @usage MFMRColors   ### -> if [MFMRutils] is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' MFMRColors$BlackFORE     ### -> outputs the text in black font colour !!!
#' MFMRColors$CyanFORE      ### -> outputs the text in cyan font colour !!!
#' MFMRColors$MagentaBACK   ### -> outputs the text background in black font colour !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::MFMRColors$CyanFORE   ### -> outputs the text in cyan font colour !!!
#'
#' @export
#? ### ### ###
MFMRColors <- {   # -> Define a static list of "ANSI" Text & Object Colours !!!
  envList <- base::list2env(
    base::list(
      BlackFORE   = "\033[30m",   ### Output: "black-text"
      RedFORE     = "\033[31m",   ### Output: "red-text"
      GreenFORE   = "\033[32m",   ### Output: "green-text"
      YellowFORE  = "\033[33m",   ### Output: "yellow-text"
      BlueFORE    = "\033[34m",   ### Output: "blue-text"
      MagentaFORE = "\033[35m",   ### Output: "magenta-text"
      CyanFORE    = "\033[36m",   ### Output: "cyan-text"
      WhiteFORE   = "\033[37m",   ### Output: "white-text"
      BlackBACK   = "\033[40m",   ### Output: "black-background"
      RedBACK     = "\033[41m",   ### Output: "red-background"
      GreenBACK   = "\033[42m",   ### Output: "green-background"
      YellowBACK  = "\033[43m",   ### Output: "yellow-background"
      BlueBACK    = "\033[44m",   ### Output: "blue-background"
      MagentaBACK = "\033[45m",   ### Output: "magenta-background"
      CyanBACK    = "\033[46m",   ### Output: "cyan-background"
      WhiteBACK   = "\033[47m",   ### Output: "white-background"

      BlackBrightFORE   = "\033[90m",   ### Output: "black-bright-text"
      RedBrightFORE     = "\033[91m",   ### Output: "red-bright-text"
      GreenBrightFORE   = "\033[92m",   ### Output: "green-bright-text"
      YellowBrightFORE  = "\033[93m",   ### Output: "yellow-bright-text"
      BlueBrightFORE    = "\033[94m",   ### Output: "blue-bright-text"
      MagentaBrightFORE = "\033[95m",   ### Output: "magenta-bright-text"
      CyanBrightFORE    = "\033[96m",   ### Output: "cyan-bright-text"
      WhiteBrightFORE   = "\033[97m",   ### Output: "white-bright-text"
      BlackBrightBACK   = "\033[100m",   ### Output: "black-bright-background"
      RedBrightBACK     = "\033[101m",   ### Output: "red-bright-background"
      GreenBrightBACK   = "\033[102m",   ### Output: "green-bright-background"
      YellowBrightBACK  = "\033[103m",   ### Output: "yellow-bright-background"
      BlueBrightBACK    = "\033[104m",   ### Output: "blue-bright-background"
      MagentaBrightBACK = "\033[105m",   ### Output: "magenta-bright-background"
      CyanBrightBACK    = "\033[106m",   ### Output: "cyan-bright-background"
      WhiteBrightBACK   = "\033[107m"    ### Output: "white-bright-background"
    )
  );
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  envList;   # -> return (export) the env-locked list !!!
}







#? ### ### ### ### ### ### ###
#' @title Unicode Icons for use in R (the `MFMRutils` selection)
#' @description
#' A collection of frequently used icons (Unicode characters) meant to support
#' the MFMR Suite of R Functions.
#'
#' @usage MFMRIcons   ### -> if "MFMRutils" library is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' MFMRIcons$OoglyEyes      ### -> prints "👀" to the R console !!!
#' MFMRIcons$CheckMark      ### -> prints "✔" to the R console !!!
#' MFMRIcons$SmileyPonder   ### -> prints "🤔" to the R console !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::MFMRIcons$OoglyEyes   ### -> prints "👀" to the R console !!!
#'
#' @export
#? ### ### ###
MFMRIcons <- {   # -> Define a static list of Unicode Character Icons !!!
  envList <- base::list2env(
    base::list(
      ArrowLEFT    = "\u2B05",       ### Output: "->"
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
  envList;   # -> return (export) the env-locked list !!!
}


