

rcoRbuildIgnoreTEST_ <- MFMRutils::code.append.text.to.file(
  ssAppendText = "^WIP$", sbMultiAppend = T, sbPostPend = F
)
base::cat(rcoRbuildIgnoreTEST_)



base::readLines(con = "./.Rbuildignore", warn = FALSE)
