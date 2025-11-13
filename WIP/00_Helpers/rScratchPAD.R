


MFMRutils::info.post.func.self.id(
  ssProjID = "rProjTESTr",
  ssFuncSelfID = "rcFuncSelfID",
  ssFuncCallerID = "rcf_TEST_FUNC", 
  siFuncMode01 = 1L
)

MFMRutils::info.post.func.self.id(
  ssProjID = "rProjTESTr",
  ssFuncSelfID = "rcFuncSelfID",
  ssFuncCallerID = "rcf_TEST_FUNC", 
  siFuncMode01 = 0L
)

info.post.func.self.id(
  ssProjID = "rProjTESTr",
  ssFuncSelfID = "rcFuncSelfID",
  ssFuncCallerID = "rcf_TEST_FUNC", 
  siFuncMode01 = 1L
)


rlsLibINFO_ <- MFMRutils::pkgs.pull.libr.info()
rlsLibINFO_$NAME
rlsLibINFO_[['NAME']]
rlsLibINFO_$VERSION
rlsLibINFO_$DESC
rlsLibINFO_[['DESC']]
rlsLibINFO_$AUTHORS



pkgs.check.code.specs(sbCheckCRAN=T)



FALSE | c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
c(FALSE, TRUE) | c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE) | FALSE
