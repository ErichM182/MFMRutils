

MFMRutils:::devs.patch.code.dev.trckr.file(sbIsProdRel = F)

lstTEST_ <- MFMRutils:::devs.patch.libr.vers.number()
class(lstTEST_)
lstTEST_$VERS_DEBUG
lstTEST_$VERS_BETA
lstTEST_$VERS_STABLE
lstTEST_$VERS_ALPHA

MFMRutils:::devs.check.code.specs(sbCheckCRAN = F)


beepr::beep(2)

