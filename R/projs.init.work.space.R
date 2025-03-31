






"projs.init.work.space" <- function(
  rnbIsNewProject_,
  rnsAuthorName_, rnsAuthorSocMedInfo_,
  rnsProjectPath_, rnsProjectDescription_
) {
  rniStartEGLN_ <- 107;   # <- DO NOT DELETE OR MOVE !!! <- Must ALWAYS BE on the very 1st line of the function body !!!
  rniFuncSignLines_ <- 5;   # <- The number of lines taken up by the function signature (Func ID & Arguments) ...


  ### INFO - Function Arguments ####################################################################################### ###
  #   'rnbIsNewProject_'       - [boolean] 'TRUE' or 'FALSE' value indicating if workspace has been previously created.   #
  #   'rnsAuthorName_'         - [string] preferred PROJECT CREATOR NAME for the newly created R Project workspace.       #
  #   'rnsAuthorSocMedInfo_'   - [string] preferred PROJECT CREATOR SOCIAL MEDIA ID for the newly created R Project.      #
  #   'rnsProjectPath_'        - [string] absolute folder <director> path of where the newly created R Project (folder)   #
  #                                       is saved on the local machine (PC).                                             #
  #   'rnsProjectDescription_' - [string] the 128 character <succinct> description (title) of the current R Project.      #
  ## ################################################################################################################### ##



  ####---   Step 1 - Validate the Project Description length ...   ---####
  # > Verify that 'rnsProjectDescription_' is NOT LONGER than 90 characters in length !!!
  base::message(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | Template Script generation STARTED !!!"));
  rniLenTitle_ = base::nchar(rnsProjectDescription_);
  if (rniLenTitle_ > 90) {
    base::message(base::paste0("WARNING: ", TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_), " | Project Description length = ", rniLenTitle_, " characters !!!"));
    base::message(base::paste0("ERROR: ", TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_), " | Project Description is longer than 90 characters !!!"));
    base::stop();   # Stops the function from executing any further code beyond this point (i.e. kills the code execution right here !!)
  }



  ####---   Step 2 - Create the <standardized> R Project Structure ...   ---####
  # > Create the (standardized) directory structure for the current R Project workspace ...
  rnsDirScripts_ <- base::file.path(rnsProjectPath_, "rScripts");          # The R SCRIPTS Folder Path (relative to the Project ROOT) ...
  if (rnbIsNewProject_) {   # Checks if the current R project (workspace) has not been created previously.
    ###---   2.1 - Define NB Project Directories   ---###
    base::cat(base::paste0(" > ", TAG_SCRIPT_ID, ' ==> Current R Project Path:\n  => ', rnsProjectPath_, '\n'));   # <- Check where the R Project is being executed from ...
    rnsDirMaps_    <- base::file.path(rnsProjectPath_, "rPlots/rMaps");      # The R MAPS Folder Path (relative to the Project ROOT) ...
    rnsDirCharts_  <- base::file.path(rnsProjectPath_, "rPlots/rCharts");    # The R CHARTS Folder Path (relative to the Project ROOT) ...
    rnsDirDataIN_  <- base::file.path(rnsProjectPath_, "rData/rInputs");     # The DATA Input Folder Path (relative to the Project ROOT) ...
    rnsDirDataOUT_ <- base::file.path(rnsProjectPath_, "rData/rOutputs");    # The DATA Outputs Folder Path (relative to the Project ROOT) ...
    sDirUtils_     <- base::file.path(rnsProjectPath_, "rScripts/rUtils");   # The R SCRIPT UTILS Folder Path (relative to the Project ROOT) ...

    ###---   2.2 - Create the NB Project Directories   ---###
    rcf_init.proj.dirs(
      ssFuncSelfID = TAG_SCRIPT_ID,
      ssProjRoot = rnsProjectPath_,
      siCELN = rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_),
      svProjDirs = c(
        rnsDirMaps_, rnsDirCharts_, rnsDirScripts_,
        sDirUtils_, rnsDirDataIN_, rnsDirDataOUT_
      )
    );
  }



  ####---   Step 3 - Compile the Important R Project File Identifiers ...   ---####
  # > 3.1 - Compile the '.Rprofile' Project File Identifier ...
  rnsFileNamePROFILE_ <- ".Rprofile";   # <- Compiles the R Project ".Rprofile" Script file name ... (NB: the pre-pended '.' character indicates this is a hidden file !!!)

  # > 3.2 - Compile the 'Main' Project File Identifier ...
  rvsProjPathNodes_ <- base::strsplit(rnsProjectPath_, "/");
  rnsProjectName_ <- rvsProjPathNodes_[[1]][base::length(rvsProjPathNodes_[[1]])];
  rnsFileNameMAIN_ <- base::paste0("Main", rnsProjectName_, ".R");   # <- Compiles the R Project "Main" Script file name ...



  ####---   Step 4 - Create the ".Rprofile" R Project File & Launcher Hook ...   ---####
  if (rnbIsNewProject_) {   # <- Checks if the current R project (workspace) has not been created previously.

    # > 4.1 - Check if the file exists before creating it ...
    rnsAbsFilePathPROFILE_ <- base::file.path(rnsProjectPath_, rnsFileNamePROFILE_);   # <- Compiles an absolute <full> File Path to the '.Rprofile' file in the R Project <root> Folder !!!
    rniLinesFilePROFILE_ <- base::length(base::readLines(rnsAbsFilePathPROFILE_, warn = FALSE));
    if (rniLinesFilePROFILE_ < 1) {   # <- Checks if the current R Project '.Rprofile" file contains any data in it ...

      # > 4.1.1 - Notify the user on the content size of the file ...
      base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | The '", rnsFileNamePROFILE_, "' file contains ", rniLinesFilePROFILE_," lines of code ...\n"));

      # > 4.1.2 - Define content to be written to new file here ...
      rnsRprofileLine01_ <- "\n\n\n";
      rnsRprofileLine02_ <- "# Load the Project 'MAIN' <root> Script File AFTER the";
      rnsRprofileLine03_ <- "# R-Studio Session Initialization (via a user-defined Hook) ...";
      rnsRprofileLine04_ <- "base::setHook(";
      rnsRprofileLine05_ <- "  'rstudio.sessionInit',";
      rnsRprofileLine06_ <- "  function(newSession) {";
      rnsRprofileLine07_ <- "    if (newSession) {";
      rnsRprofileLine08_ <- "      rstudioapi::navigateToFile(";
      rnsRprofileLine09_ <- paste0("        './rScripts/", rnsFileNameMAIN_, "',");
      rnsRprofileLine10_ <- "        line = -1L, column = -1L";
      rnsRprofileLine11_ <- "      )";
      rnsRprofileLine12_ <- "    }";
      rnsRprofileLine13_ <- "  },";
      rnsRprofileLine14_ <- "  action = 'append'";
      rnsRprofileLine15_ <- ")";

      # > 4.1.3 - Compile the contents define above (Line01 - Line15) into a vector ...
      rvsInputTextFilePROFILE_ <- c(
        rnsRprofileLine01_, rnsRprofileLine02_, rnsRprofileLine03_,
        rnsRprofileLine04_, rnsRprofileLine05_, rnsRprofileLine06_,
        rnsRprofileLine07_, rnsRprofileLine08_, rnsRprofileLine09_,
        rnsRprofileLine10_, rnsRprofileLine11_, rnsRprofileLine12_,
        rnsRprofileLine13_, rnsRprofileLine14_, rnsRprofileLine15_
      );

      # > 4.1.4 - Insert the compiled contents into the ".Rprofile" file ...
      base::writeLines(rvsInputTextFilePROFILE_, rnsAbsFilePathPROFILE_);

      # > 4.1.5 - Output some code-execution success info ...
      base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | Wrote ", base::length(rvsInputTextFilePROFILE_), " lines of template code to newly created '.Rprofile' file.\n"));
      base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | A new '", rnsFileNamePROFILE_, "' file was successfully created and populated with data !!! \n"));
      base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | Newly created file path -> ", rnsAbsFilePathPROFILE_, "\n"));

      # > 4.1.6 - Open the newly created file in the R-Studio Scripts Window ...
      rstudioapi::navigateToFile(
        file = rnsAbsFilePathPROFILE_,
        line = -1L, column = -1L
      );
      ###! utils::file.edit(rnsAbsFilePathMAIN_, editor = getOption());   # <- OLD Approach ... doesn't work so well anymore !!!

    } else {

      # > 4.2.1 - Notify the user on the content size of the file ...
      base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | The '", rnsFileNamePROFILE_, "' file contains ", rniLinesFilePROFILE_," lines of code ...\n"));
      base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | An '", rnsFileNamePROFILE_, "' file already exists ( thus NO CHANGES were made to existing file ) !!! \n"));

    }
  }



  ####---   Step 5 - Create the "MAIN" R Project Script File ...   ---####
  # > 5.1 - Check if the file exists before creating it ...
  rnsAbsFilePathMAIN_ <- base::file.path(rnsDirScripts_, rnsFileNameMAIN_);   # <- Compiles an absolute <full> File Path to the 'Main' script file in the R Project <rScripts> Folder !!!
  rniLinesFileMAIN_ <- base::length(base::readLines(rnsAbsFilePathMAIN_, warn = FALSE));
  if (rniLinesFileMAIN_ < 1) {   # <- Checks if the current R project (workspace) has not been created previously.

    # > 5.1.1 - Notify the user on the content size of the file ...
    base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | The 'Main' R Project file contains ", rniLinesFileMAIN_," lines of content.\n"));

    # > 5.1.2 - Define content to be written to new file here ...
    rniMaxHeaderChars_ <- 107;
    rdtCurrDate_ <- base::format(base::Sys.Date(), "%d %b %Y");   # <- Extracts today's date and formats it as '06 Sep 2019'.
    rniLine1Dashes_ <- rniMaxHeaderChars_ - (37 + base::nchar(rnsAuthorName_) + base::nchar(rdtCurrDate_));
    rniLine3Spaces_ <- rniMaxHeaderChars_ - (12 + base::nchar(rnsProjectDescription_));
    rniLeftRigthTitleSpaces_ <- base::strrep(" ", (rniLine3Spaces_ / 2));
    rniLine5Dashes_ <- rniMaxHeaderChars_ - (30 + base::nchar(rnsAuthorSocMedInfo_) + base::nchar(rdtCurrDate_));
    line01 <- base::paste0("#--- [ Author: ", rnsAuthorName_, " ] ", base::strrep("-", rniLine1Dashes_), " [ Created: ", rdtCurrDate_, " ] ---#");
    line02 <- "##                                                                                                       ##";
    line03 <- base::paste0("#### > ", rniLeftRigthTitleSpaces_, rnsProjectDescription_ , rniLeftRigthTitleSpaces_," ####");
    line04 <- "##                                                                                                       ##";
    line05 <- base::paste0("#--- [ ", rnsAuthorSocMedInfo_, " ] ", base::strrep("-", rniLine5Dashes_), " [ Modified: ", rdtCurrDate_, " ] ---#");
    line06 <- "#### ~~~ ~~~ ~~~ ####";
    line07 <- "# ======================================================================================================= #";
    line08 <- "#   SCRIPT NOTES - Input data, Variables, Arguments explained:                                         ####";
    line09 <- "#    - Capture any important notes about the use of this script file here.                                #";
    line10 <- "#    - The 'notes' section can be composed of multiple lines (as is clear from this starter example       #";
    line11 <- "#       here) - just make the 'notes' section as user friendly [and concise] as possible. This will       #";
    line12 <- "#       ensure the correct use of the script by 3rd party users (and yourself - in the future).           #";
    line13 <- "#    - A good use of the 'notes' section is to outline the requirements for [or conditions to be met      #";
    line14 <- "#       prior to] using this script (e.g. input data requirements - if there are special requirements).   #";
    line15 <- "#    - The 'notes' section is also the best place to describe input variables [i.e. function arguments].  #";
    line16 <- "# ======================================================================================================= #";
    line17 <- "";
    line18 <- "#### ~~~ ~~~ ~~~ ####";
    line19 <- "####--- SECTION 1 - Working Directory, Libraries & Data Imports ---####";
    line20 <- "####--- 1.1 - Working Directory ---####";
    line21 <- base::paste0("myWorkDir = '", rnsProjectPath_,"'   # Update path accordingly.");
    line22 <- "setwd(myWorkDir)";
    line23 <- "";
    line24 <- "####--- 1.2 - Required Libraries ---####";
    line25 <- "####  | - NOTES: Install or activate required libraries (packages)   ####";
    line26 <- "reqPacks <- c('MASS', 'splines', 'car', 'ggplot2', 'beepr')   # Update 'reqPacks' list accordingly.";
    line27 <- "for (p in reqPacks) {   # Iterates through 'reqPacks' assessing list items one-by-one with the code below.";
    line28 <- "  if (!require(p, character.only=TRUE)) { install.packages(p) }   # Checks if library is not already installed & executes a new install if required.";
    line29 <- "  library(p, character.only=TRUE)    # Loads each library after installation.";
    line30 <- "}";
    line31 <- "";
    line32 <- "####--- 1.3 - Source Helper Scripts [if needed] ---####";
    line33 <- "# dirToolsR <- '/home/erich/Tresors/Work Related/R Stuff/R_Tools/'       # ...full path to the root directory for all useful R functions.";
    line34 <- "# dirData <- base::paste0(dirToolsR, 'rData/')                                 # ...full path to 'Data Manipulation' functions.";
    line35 <- "# dirMaps <- base::paste0(dirToolsR, 'rMaps/')                                 # ...full path to 'GIS & Mapping' functions.";
    line36 <- "# dirStats <- base::paste0(dirToolsR, 'rStats/')                               # ...full path to 'Statistical Analyses' functions.";
    line37 <- "# dirUtils <- base::paste0(dirToolsR, 'rUtils/')                               # ...full path to 'R Utility' functions.";
    line38 <- "# dirPlots <- base::paste0(dirToolsR, 'rPlots/')                               # ...full path to 'Plotting & Graph' functions.";
    line39 <- "# dir3rdPartyScripts <- base::paste0(dirToolsR, 'r_3rd_Party_Scripts/')        # ...full path to '3rd Party Sourced' functions.";
    line40 <- "# f.utils.source.all.funcs.in.dir(dirUtils)                       # The code that actually imports all the 'helper functions' from the specified directory.";
    line41 <- ""
    line42 <- "####--- 1.4 - Data Imports ---####";
    line43 <- "myDataFile <- 'SRL_Obs_LF_Data_R_Format_1999_To_Current.txt'   # The data file must be located in the root of the 'myWorkDir' file path.";
    line44 <- "myBiolData <- read.delim(file.path(myWorkDir, myDataFile), header=T, sep='\\t')";
    line45 <- "# myBiolData = read.delim(file.choose(), header=T, sep='\\t')   # OLD METHOD [frowned upon!!] | Browse your local file directory and select the required data file.";
    line46 <- "";
    line47 <- "#### ~~~ ~~~ ~~~ ####";
    line48 <- "####--- SECTION 2 - Data Exploration & Data Wrangling ---####";
    line49 <- "####--- 2.1 - Data Exploration [high-level summaries] ---####";
    line50 <- "names(myBiolData)     # Call all column headers with the 'name()' function.";
    line51 <- "summary(myBiolData)   # Run initial visual diagnostics on the dataset.";
    line52 <- "# plot(myBiolData)    # USE CAUTIOUSLY - DO NOT RUN if you have a HUGE DATASET !! [takes very long to plot BIG datasets - can cause your PC to freeze]";
    line53 <- "";
    line54 <- "####--- 2.2 - Data Wrangling [clean, transform & subset] ---####";
    line55 <- "# Execute whatever data cleaning, transformation & subsetting you require here...";
    line56 <- "";
    line57 <- "#### ~~~ ~~~ ~~~ ####";
    line58 <- "####--- SECTION 3 - Data Analyses ---####";
    line59 <- "####--- 3.1 - Means, Std. Error, Confidence Intervals etc. ---####";
    line60 <- "# Execute whatever data analyses you require here...";
    line61 <- "source('/home/erich/Tresors/Work Related/R Stuff/R_Tools/rUtils/F.Utils.Source.All.Funcs.In.Dir.R')";
    line62 <- "";
    line63 <- "";
    line64 <- "#### ~~~ ~~~ ~~~ ####";
    line65 <- "####--- SECTION 4 - MAP Analyses ---####";
    line66 <- "####--- 4.1 Introductory Maps (etc. etc.) ---####";
    line67 <- "####--- 4.1 [A] - Input Data Composition of Total Data Input (POOLED) ---####";
    line68 <- "source('/home/erich/Tresors/Work Related/R Stuff/R_Tools/rUtils/F.Utils.Source.All.Funcs.In.Dir.R')";
    line69 <- "";
    line70 <- "";
    line71 <- "#### ~~~ END OF SCRIPT ~~~ ####";

    # > 5.1.3 - Compile the content define above (Line01 - Line71) into a vector ...
    rvsInputTextFileMAIN_ <- c(
      line01, line02, line03, line04, line05, line06, line07, line08, line09, line10,
      line11, line12, line13, line14, line15, line16, line17, line18, line19, line20,
      line21, line22, line23, line24, line25, line26, line27, line28, line29, line30,
      line31, line32, line61, line33, line34, line35, line36, line37, line38, line39,
      line40, line41, line42, line43, line44, line45, line46, line47, line48, line49,
      line50, line51, line52, line53, line54, line55, line56, line57, line58, line59,
      line60, line61, line62, line63, line64, line65, line66, line67, line68, line69,
      line70, line71
    );

    # > 5.1.4 - Insert the compiled content into the "Main" file ...
    base::writeLines(rvsInputTextFileMAIN_, rnsAbsFilePathMAIN_);

    # > 5.1.5 - Output some code-execution success info ...
    base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | Wrote ", base::length(rvsInputTextFileMAIN_), " lines of template code to newly created 'Main' script file.\n"));
    base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | A new 'Main' file was successfully created and populated with data !!! \n"));
    base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | Newly created file path -> ", rnsAbsFilePathMAIN_, "\n"));

    # > 5.1.6 - Open the newly created file in the R-Studio Scripts Window ...
    rstudioapi::navigateToFile(
      file = rnsAbsFilePathMAIN_,
      line = -1L, column = -1L
    );
    ###! utils::file.edit(rnsAbsFilePathMAIN_, editor = getOption());   # <- OLD Approach ... doesn't work so well anymore !!!

  } else {

    # Notify the user on the content size of the file ...
    base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | The 'Main' R Project file already contains ", rniLinesFileMAIN_," lines of content !!!\n"));
    base::cat(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | No modifications were made to existing 'Main' R Project file !!!\n"));

  }

  # End of Function ...
  base::message(base::paste0(" > " , TAG_SCRIPT_ID, " ", rcf_utils.get.egln(rniStartEGLN_, rniFuncSignLines_)," | Template Script generated successfully - PROCESS TERMINATED !!!"));
}
