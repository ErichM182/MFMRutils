

rvsPathVectV01_ <- c(".", "rProjFiles", "rData", "RAW", "InputDATA.txt");             # CLEAN Vector
rvsPathVectV02_ <- c("./", "\\rProjFiles/", "/rData/", "/RAW", "\\TestDATA.txt\\");   # MESSY Vector


rssPathCleanV00_ <- MFMRutils::code.clean.file.path(
  # vsPathVector = rvsPathVectV01_
); rssPathCleanV00_;

rssPathCleanV01_ <- MFMRutils::code.clean.file.path(
  vsPathVector = rvsPathVectV01_
); rssPathCleanV01_;

rssPathCleanV02_ <- MFMRutils::code.clean.file.path(
  vsPathVector = rvsPathVectV02_
); rssPathCleanV02_


nchar(rvsIterVECT_[1])
substr(
  rvsIterVECT_[1], nchar(rvsIterVECT_[1]), nchar(rvsIterVECT_[1])
)

rvsIterVECT_ <- rvsPathVectV02_;
rvsIterVECT_v02_ <- rvsIterVECT_;
for (snINDX in 1:nchar(rvsIterVECT_)) {
  
  ## Extract Vector Object located at <active> Vector Index ...
  rssObj_ <- rvsIterVECT_[snINDX];
  base::cat(
    base::paste0(" -> EXTRACTED Object @ Index ", snINDX, " == ", rssObj_, "\n")
  )
  
  ## Extract the FIRST Character of the returned Vector Object ...
  rssCharFirst_ <- rasBaseSubSTR(
    x = rssObj_, start = 1, stop = 1
  );
  base::cat(
    base::paste0(" -> FIRST Object CHAR @ Index ", snINDX, " == ", rssCharFirst_, "\n")
  )
  
  ## Extract the LAST Character of the returned Vector Object ...
  rssCharLast_ <- rasBaseSubSTR(
    x = rssObj_, start = rasBaseNCHAR(rssObj_), stop = rasBaseNCHAR(rssObj_)
  );
  base::cat(
    base::paste0(" -> LAST Object CHAR @ Index ", snINDX, " == ", rssCharLast_, "\n")
  )
  
  ## Clean BEGINNING of returned Vector Object appropriately ...
  rssObj_CLEAN_ <- rssObj_;   # <- Assign object to new variable for cleaning purposes ...
  if (rssCharFirst_ == "/" || rssCharFirst_ == "\\") {
    rssObj_CLEAN_ <- rasBaseSubSTR(
      x = rssObj_CLEAN_, start = 2, stop = rasBaseNCHAR(rssObj_CLEAN_)
    );
  }
  
  ## Clean END of returned Vector Object appropriately ...
  if (rssCharLast_ == "/" || rssCharLast_ == "\\") {
    rssObj_CLEAN_ <- rasBaseSubSTR(
      x = rssObj_CLEAN_, start = 1, 
      stop = rasBaseNCHAR(rssObj_CLEAN_) - 1
    );
  }
  
  ## Update <active> Vector Item (object) with updated <clean> string value ...
  rvsIterVECT_v02_[snINDX] <- rssObj_CLEAN_;
  base::cat(
    base::paste0(" -> CLEANED Object @ Index ", snINDX, " == ", rvsIterVECT_v02_[snINDX], "\n")
  )
}
