#? ### ### ### ### ### ### ###
#' @title Unicode Icons for use in R (the `MFMRutils` selection)
#' @name EnvICONS
#' @description
#' A collection of frequently used icons (Unicode characters) meant to support
#' the MFMR Suite of R Functions.
#'
#' @usage EnvICONS   ### -> if "MFMRutils" library is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' EnvICONS$OoglyEyes      ### -> prints "👀" to the R console !!!
#' EnvICONS$CheckMark      ### -> prints "✔" to the R console !!!
#' EnvICONS$SmileyPonder   ### -> prints "🤔" to the R console !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::EnvICONS$OoglyEyes   ### -> prints "👀" to the R console !!!
#'
#' @export
#? ### ### ###
"EnvICONS" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of Unicode Character Icons !!!
  envList <- base::list2env(
    base::list(
      ArrowLEFT      = "\U00002B05",   ### Output: "⬅"
      ArrowRIGHT     = "\U0000279C",   ### Output: "➜"
      CheckMark      = "\U00002714",   ### Output: "✔"
      FireFlame      = "\U0001F525",   ### Output: "🔥"
      SmileyPonder   = "\U0001F914",   ### Output: "🤔"
      SmileySad      = "\U0001F614",   ### Output: "😔"
      SmileyHurting  = "\U0001F622",   ### Output: "😢"
      SmileyGrinning = "\U0001F600",   ### Output: "😀"
      SmileyWinking  = "\U0001F609",   ### Output: "😉"
      SmileyNeutral  = "\U0001F610",   ### Output: "😐"
      SmileyNormal   = "\U0001F60A",   ### Output: "😊"
      SmileyShocked  = "\U0001F627",   ### Output: "😧"
      OoglyEyes      = "\U0001F440",   ### Output: "👀"
      PointUP        = "\U0001F446",   ### Output: "👆"
      X_White        = "\U00002716",   ### Output: "✖"
      X_Red          = "\U0000274C",   ### Output: "❌"
      WaterSplash    = "\U0001F4A6",   ### Output: "💦"
      FireHelmet     = "\U000026D1",   ### Output: "⛑"
      SparkRed       = "\U0001F4A5",   ### Output: "💥"
      GustOfAir      = "\U0001F4A8",   ### Output: "💨"
      Gravestone     = "\U0001FAA6",   ### Output: "🪦"
      SkullOnly      = "\U0001F480",   ### Output: "💀"
      SkullBones     = "\U00002620",   ### Output: "☠"
      SignSTOP       = "\U0001F6D1",   ### Output: "🛑"
      SignNO_ENTRY   = "\U000026D4",   ### Output: "⛔"
      SignPROHIBITED = "\U0001F6AB",   ### Output: "🚫"
      HourGlassFLOW  = "\U000023F3",   ### Output: "⏳"
      HourGlassDONE  = "\U0000231B",   ### Output: "⌛"
      BowAndArrow    = "\U0001F3F9",   ### Output: "🏹"
      SwordsCrossed  = "\U00002694",   ### Output: "⚔"
      PersonWalking  = "\U0001F6B6",   ### Output: "🚶"
      HeartArrow     = "\U0001F498",   ### Output: "💘"
      HeartWhite     = "\U0001F90D"    ### Output: "🤍"
    )
  );
  
  # Set R Environment Bindings = TRUE makes the bindings immutable ...
  base::lockEnvironment(envList, bindings = TRUE);
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
