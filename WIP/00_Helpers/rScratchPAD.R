

lstTEST_ <- MFMRutils:::devs.pull.libr.info()
class(lstTEST_)
lstTEST_$NAME
lstTEST_[["VERSION"]]
lstTEST_$VERS_STABLE
lstTEST_$VERS_ALPHA

rcoRbuildIgnoreTEST_ <- MFMRutils::code.append.text.to.file(
  
)
