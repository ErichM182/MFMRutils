#######`       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Test-Pad > R SCRIPT   <-        #######
###  -> Use this R Script <file> purely for Testing Purposes during Code Development Cycle !!!  ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######



### Use the "Path-Cleaning" Function as follows: ...
library(MFMRutils)   # <- Loads the "MFMRutils" library (if already installed) ...



### OPTION 1 - Run a NULL-ARGs execution of the "Path-Cleaning" Function ...
# NB: If no external values are passed to the function a default file path will be returned !!!
ssPathArgsNULL <- code.clean.file.path()   # <- No external args <values> passed to function ... 
ssPathArgsNULL                             # -> returns a default R Project Path (".") !!!



### OPTION 2 - Use a STRING to define the File Path (R Project Directory, etc.) ...
# Prime the Function Inputs <arguments> (as needed) ...
ssCleanSTR_ <- "./rProjFiles/rData/TestDATA.txt"          # <- Clean PATH String !!!
ssMessySTR_ <- "./\\rProjFiles//rData/\\TestDATA.txt\\"   # <- Messy PATH String !!!

# Run the VECTOR-LOGIC execution of the "Path-Cleaning" Function ...
ssPathCleanSTRING <- MFMRutils::code.clean.file.path(   # <- Executes the "STRING" Code Logic ...
  ssPathString = ssCleanSTR_                            # <- CLEAN "PATH String" supplied !!!
); ssPathCleanSTRING                                    # -> returns a Clean Path STRING result !!!

ssPathMessySTRING <- MFMRutils::code.clean.file.path(   # <- Executes the "STRING" Code Logic ...
  ssPathString = ssMessySTR_                            # <- MESSY "PATH String" supplied !!!
); ssPathMessySTRING                                    # -> returns a Clean Path STRING result !!!
