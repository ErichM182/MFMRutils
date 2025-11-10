#? ### ### ### ### ### ### ###
#' @title Unicode Icons for use in R (the `MFMRutils` selection)
#' @name MFMRIcons
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
"MFMRIcons" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of Unicode Character Icons !!!
  envList <- base::list2env(
    base::list(
      ArrowLEFT    = "\u2B05",       ### Output: "<-"
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
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
