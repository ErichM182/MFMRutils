


MFMRutils::info.post.func.self.id(
  ssProjID = "rProjTESTr",
  ssFuncSelfID = "rcFuncSelfID",
  ssFuncCallerID = "rcf_TEST_FUNC", 
  siFuncMode01 = 1L, sbRunSelfID = T
);

MFMRutils::info.post.func.self.id(
  ssProjID = "rProjTESTr",
  ssFuncSelfID = "rcFuncSelfID",
  ssFuncCallerID = "rcf_TEST_FUNC", 
  siFuncMode01 = 0L, sbRunSelfID = T
);

info.post.func.self.id(
  ssProjID = "rProjTESTr",
  ssFuncSelfID = "rcFuncSelfID",
  ssFuncCallerID = "rcf_TEST_FUNC", 
  siFuncMode01 = 1L
)


rlsLibINFO_ <- MFMRutils::devs.pull.libr.info()
rlsLibINFO_$NAME
rlsLibINFO_[['NAME']]
rlsLibINFO_$VERSION
rlsLibINFO_[['VERSION']]
rlsLibINFO_$DESC
rlsLibINFO_[['DESC']]
rlsLibINFO_$AUTHORS


RCT_TAG_PROJ_ID_ <- "rTestPROJ";
MFMRutils::pkgs.check.code.specs(
  sbCheckCRAN = T
);


"rcf_null.args.test" <- function(w, x=NULL, y, z=NULL) {
  cat(
    paste0(
      # " -> Value of Func-Arg 'w' == ", w, " !!!\n",
      " -> Value of Func-Arg 'x' == ", x, " !!!\n",
      " -> Value of Func-Arg 'y' == ", y, " !!!\n",
      " -> Value of Func-Arg 'z' == ", z, " !!!\n"
    )
  )
}
rcf_null.args.test()


FALSE | c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
c(FALSE, TRUE) | c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE) | FALSE
