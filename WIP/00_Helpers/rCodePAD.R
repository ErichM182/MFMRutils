#######|       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Code-Pad > R SCRIPT   <-       |#######
####`   -> Use this R Script <file> for Random Coding Tasks during Code Development Cycle !!!   ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######


###   R Project Development <Run-Time Mode> Trackers   ###
RCT_IS_DEBUG_RT_MODE_   <- F;   # <- The `DEBUG` <dev> Run-Time Mode Tracker ...
RCT_IS_VERBOSE_RT_MODE_ <- F;   # <- The `VERBOSE` <dev> Run-Time Mode Tracker ...


### R Library CRAN Code and Documentation Checking Function ...
MFMRutils::devs.check.code.specs(
  sbCheckCRAN = T,   # <- Set this to `FALSE` if you only want to update the documentation !!!
  sbIsProdRel = F    # <- Set this to `TRUE` if you are running a PRODUCTION Release <test> !!!
);







base::assign(MFMRutils::RENV_FSID$CONSTS_BOOL_IS_DEBUG, FALSE)

base::get0(MFMRutils::RENV_FSID$CONSTS_BOOL_IS_DEBUG)







MFMRutils::info.post.note(sbPostAlways = F)
MFMRutils::info.post.func.self.id(
  sbRunSelfID = T, siFuncMode01L = 0L, sbPrintPretty = T
)

ssTestVAR_ <- "129";
nchar(ssTestVAR_)



vsListNames_ <- c("VAR_A", "VAR_B", "VAR_C", "VAR_X", "VAR_Y", "VAR_Z", "VAR_G")
lsListVals_  <- list(
  1982, "Value for VAR_B", "R-Object for VAR_C", FALSE,
  "Value for VAR_Y", TRUE, "R-List for VAR_G"
)



### Function-use OPTION 1 (main purpose) -> Create Immutable R List Objects ...
rlsListEnvLOCKD_ <- MFMRutils::code.return.renv.list(
  vsListNames = vsListNames_, 
  lsListVals = lsListVals_,
  sbLockList = TRUE,   # <- Set to 'TRUE' to create an immutable (environment locked) R List !!!
  sbRunByForce = T, sbPostAlways = T, sbRunSelfID = T    
)




