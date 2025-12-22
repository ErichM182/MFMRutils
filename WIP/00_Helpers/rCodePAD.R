#######|       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Code-Pad > R SCRIPT   <-       |#######
####`   -> Use this R Script <file> for Random Coding Tasks during Code Development Cycle !!!   ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######

{
  
  MFMRutils::code.classify.func(
    # siStartCELN = 28L, siStopCELN = 7450L
  )
  
  MFMRutils::devs.check.code.specs(sbCheckCRAN = T)
  
  myNewFUNC <- function() {
    rssTestSTR_ <- "@param csFindText Regex pattern to match in the code"
  }
  
  
  # Find line numbers where specific code patterns were executed
  #
  # @param csFindText Regex pattern to match in the code
  # @param ssSearchFunc Function name to search within (optional)
  # @param coRENV Environment to search (defaults to global environment)
  # @param sbRetFullMatch Whether to return full matching line
  # @return Data frame with line numbers and code snippets
  # @export
  # 
  
  
  {
    
    myCustFuncR <- function(x=7, y=3, z=28) {   # <- Use the Code Editor Gutter Line Number (CELN) at
      #    which this opening curly brace is located as the
      #    `siStartCELN` value.
      rsiStartCELN_ <- 67L   # <- assumes this 👆 opening curly brace above (denoting the start
      #    of the <custom> function body block of code) is located at
      #    line 3 of the code editor (i.e. the curly brace is located
      #    at the 3rd CELN).
      
      ssTagFuncID <- "myCustFuncR"   # <- ALWAYS TAG Large Custom R Functions accordingly !!!
      
      rasBaseSysCALLS  <- base::sys.calls;
      
      valueSUM <- sum(x, y, z)
      cat(paste0(" \u279C ", ssTagFuncID, " " , 
                 code.find.text.instances("\\.inst")$LINE_NUMBER, ###
                 ### MFMRutils::code.get.celn(),
                 " | Summed all 3 function arguments < result: ", valueSUM," > !!! \n")
      )
      
      valueMEAN <- sum(x, y, z) / 3
      cat(paste0(" \u279C ", ssTagFuncID, " " ,  
                 code.find.text.instances("test")$LINE_NUMBER, ##
                 ### MFMRutils::code.get.celn(),
                 " | Took the average of the 3 function arguments < result: ",
                 round(valueMEAN, 3)," > !!! \n\n")
      )
      
      return(
        list("SUM" = valueSUM, "MEAN" = valueMEAN)
      )
    }
    
    ## Execute the custom R Function ...
    myCustFuncR()
    
  }
  
  
}

  code.find.text.instances("Classify\\.Func", ssSearchFunc = "Classify.Func")
  
  
  
  
