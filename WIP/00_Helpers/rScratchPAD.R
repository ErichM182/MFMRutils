

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

MFMRutils:::devs.check.code.specs(sbCheckCRAN = F, sbIsProdRel = T)


beepr::beep(2)

