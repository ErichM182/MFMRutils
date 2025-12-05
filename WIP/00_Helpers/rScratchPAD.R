


# Testing the ENVIRO-LOCKED R List Helper Function ...
rvsListNames_ <- c("VAR_A", "VAR_B", "VAR_C", "VAR_X", "VAR_Y", "VAR_Z", "VAR_G")
rlsListVals_  <- list(
  1982, "Value for VAR_B", "R-Object for VAR_C", FALSE, 
  "Value for VAR_Y", TRUE, "R-List for VAR_G"
)
class(rlsListVals_)
class(rvsListNames_)

tstLEN_ <- "";
nchar(tstLEN_)



rlsEnvLockdLIST <- code.return.env.locked.list(
  vsListNames = rvsListNames_, lsListVals = rlsListVals_, 
  sbLockList = TRUE
)   ### <- Extracts Library Information ... 

rlsEnvLockdLIST$VAR_X        # -> Returns the value FALSE !!!
rlsEnvLockdLIST[["VAR_C"]]   # -> Returns the value 'R-Object for VAR_C' !!! 
rlsEnvLockdLIST$`128`        # -> Returns the value FALSE !!!

rlsEnvLockdLIST$VAR_X <- FALSE







### Run 2 different types of code check/validation processes ...
rvsVersNumVect_ <- c("9", "9", "9")
rlsLibrVers <- MFMRutils:::devs.patch.libr.vers.number(
  rvsVersNumVect = rvsVersNumVect_
)
names(rlsLibrVers)

rlsLibrVers$VERS_DEBUG
rlsLibrVers$VERS_ALPHA
rlsLibrVers$VERS_BETA
rlsLibrVers$VERS_STABLE
