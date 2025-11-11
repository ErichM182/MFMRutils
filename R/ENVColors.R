#? ### ### ### ### ### ### ###
#' @title ANSI Colours for use in R (the `MFMRutils` selection)
#' @name EnvCOLORS
#' @description
#' A collection of frequently used ANSI (American National Standards Institute)
#' Colours to support the MFMR Suite of R Functions.
#'
#' @usage EnvCOLORS   ### -> if [MFMRutils] is already installed & loaded !!!
#'
#' @examples
#' ### Easily print & assign icons as follows ...
#' EnvCOLORS$BlackFORE     ### -> outputs the text in black font colour !!!
#' EnvCOLORS$CyanFORE      ### -> outputs the text in cyan font colour !!!
#' EnvCOLORS$MagentaBACK   ### -> outputs the text background in black font colour !!!
#'
#' ### Use with the direct-access R operator "::" from anywhere ...
#' MFMRutils::EnvCOLORS$CyanFORE   ### -> outputs the text in cyan font colour !!!
#'
#' @export
#? ### ### ###
"EnvCOLORS" <- {   # <- MUST BE LIKE THIS ... DO NOT use `function(){}` !!!
  
  # -> Define a static list of "ANSI" Text & Object Colours !!!
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
  
  # Return (export) the env-locked list ...
  envList;   # <- MUST BE LIKE THIS ... DO NOT use `base::return(envList)` !!!
}
