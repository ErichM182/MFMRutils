

rlsCodeVERS_ <- MFMRutils:::devs.patch.code.dev.trckr.file(sbIsProdRel = F)
rlsCodeVERS_$CODE_NAME_TAG
rlsCodeVERS_$CODE_PUSH_DATE
rlsCodeVERS_$CODE_PUSH_TYPE
rlsCodeVERS_$CODE_VERS_PROD
rlsCodeVERS_$CODE_VERS_DEVS


lstTEST_ <- MFMRutils:::devs.patch.libr.vers.number()
class(lstTEST_)
lstTEST_$VERS_DEBUG
lstTEST_$VERS_BETA
lstTEST_$VERS_STABLE
lstTEST_$VERS_ALPHA


MFMRutils:::devs.check.code.specs(sbCheckCRAN = T, sbIsProdRel = F)


beepr::beep(2)

base::R.version[["version.string"]]



rvsLibrDEPENDENCIES_ <- MFMRutils::devs.pull.libr.info()[["DEPENDENCIES"]];
rvsLibrSplitDEPs_ <- base::strsplit(rvsLibrDEPENDENCIES_, ", ")
rssLibrDepsLIST_ <- "";   # <- Compile a list of 3rd Party Dependencies for this R-Library !!!
for (libr in rvsLibrSplitDEPs_[[1]]) {
  rssLibrDepsLIST_ <- base::paste0(
    rssLibrDepsLIST_,   # <- Take the whatever already exists -> and add (concat) to it ...
    "> ", libr, " v", utils::packageVersion(pkg = libr), "\n"
  )
}
base::length(rvsLibrDEPsUNLISTED_[[1]])
