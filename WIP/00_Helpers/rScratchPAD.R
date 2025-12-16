

rvsPathVectV01_ <- c(".", "rProjFiles", "rData", "RAW", "InputDATA.txt");             # CLEAN Vector
rvsPathVectV02_ <- c("./", "\\rProjFiles/", "/rData/", "/RAW", "\\TestDATA.txt\\");   # MESSY Vector


rssPathCleanV00_ <- code.patch.file.path(
  # vsPathParts = rvsPathVectV01_
); rssPathCleanV00_;

rssPathCleanV01_ <- code.patch.file.path(
  vsPathParts = rvsPathVectV01_
); rssPathCleanV01_;

rssPathCleanV02_ <- code.patch.file.path(
  vsPathParts = rvsPathVectV02_
); rssPathCleanV02_
