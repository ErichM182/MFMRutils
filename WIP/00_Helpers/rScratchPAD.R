

rvsPathVectCLEAN_ <- c(".", "rProjFiles", "rData", "InputDATA.txt");             # CLEAN Vector
rvsPathVectMESSY_ <- c("./", "\\rProjFiles/", "/rData/", "\\TestDATA.txt\\");   # MESSY Vector

rssDelimSTR_ <- paste0(
  rvsPathVectCLEAN_, collapse = "‡≡‡"
); rssDelimSTR_

strsplit(
  x = rssDelimSTR_, split = "‡≡‡"
)


rssPathCleanV00_ <- MFMRutils::code.clean.file.path(
  # vsPathVector = rvsPathVectV01_
); rssPathCleanV00_;

rssPathCleanV01_ <- MFMRutils::code.clean.file.path(
  vsPathVector = rvsPathVectV01_
); rssPathCleanV01_;

rssPathCleanV02_ <- MFMRutils::code.clean.file.path(
  vsPathVector = rvsPathVectV02_
); rssPathCleanV02_


RCT_REGEX_CHARS_[["\\"]]
"\\" %in% names(RCT_REGEX_CHARS_)



