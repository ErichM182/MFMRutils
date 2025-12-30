#######|       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Code-Pad > R SCRIPT   <-       |#######
####`   -> Use this R Script <file> for Random Coding Tasks during Code Development Cycle !!!   ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######

MFMRutils::devs.check.code.specs(sbCheckCRAN = T)


results <- devs.find.code.instances(
  ssFindText = "plot",
  vsTargetLibs = c("base", "utils", "graphics"),
  sbSearchInternals = FALSE
)

