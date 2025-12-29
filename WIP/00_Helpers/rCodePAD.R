#######|       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Code-Pad > R SCRIPT   <-       |#######
####`   -> Use this R Script <file> for Random Coding Tasks during Code Development Cycle !!!   ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######

MFMRutils::devs.check.code.specs(sbCheckCRAN = T)



results <- devs.find.code.instances(
  ssFindText = "plot",
  vsTargetLibs = c("utils", "graphics"),
  sbSearchInternals = FALSE
)



rdfFuncCalls_ <- rasMfmrDevsFindCODE(
  ssFindText = "code\\.get\\.celn\\()", vsTargetLibs = c(RCT_TAG_FUNC_LIBR_ID_)
);

# 4.2 - Extract only results that match specified Function ID ...
rdfFuncCalls_ <- rasBaseSubSET(results, FUNC_NAME == rasBaseToLOWER("barplot.default"));

# 4.3 - Create a new INDEX Variable for the subset results Data Frame ...
rdfFuncCalls_$INDEX <- rasBaseSeqALONG(1:rasBaseNROW(rdfFuncCalls_))

