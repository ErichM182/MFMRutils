#######|       ->   NEVER DELETE THE FIRST 3 LINES OF THIS < Code-Pad > R SCRIPT   <-       |#######
####`   -> Use this R Script <file> for Random Coding Tasks during Code Development Cycle !!!   ####
####### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~#######

MFMRutils::devs.check.code.specs(sbCheckCRAN = T)



results <- devs.find.code.instances(
  ssFindText = "code.get.celn",
  vsTargetLibs = c("MFMRutils"),
  sbSearchInternals = FALSE
)



"devs.find.code.instances" <- function(
  ssFindText, vsTargetLibs=NULL, coRENVs=base::list(base::globalenv()), sbSearchInternals = FALSE, 
  sbUseRegex = TRUE, sbIgnoreCase = TRUE, sbIncludeGlobal = TRUE, sbVerboseSearch = TRUE, 
  snRetSnipSize = 82
) {
  
  ####   STEP 01 - Prime "Function Self-ID" CONSTANTS   ####
  RCT_SYS_TIME_NOW_     <- base::Sys.time();
  RCT_TAG_FUNC_LIBR_ID_ <- "MFMRutils";
  RCT_TAG_FUNC_ID_SHRT_ <- "Find.Code";
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Find.Code.Instances";
  
  ####   STEP 02 - Define "Local Aliases"   ####
  rasBaseLS                  <- base::ls;
  rasBaseGET                 <- base::get;
  rasBaseREP                 <- base::rep;
  rasBaseSUM                 <- base::sum;
  rasBaseATTR                <- base::attr;
  rasBaseGREP                <- base::grep;
  rasBaseNROW                <- base::nrow;
  rasBaseLIST                <- base::list;
  rasBaseSTOP                <- base::stop;
  rasBaseORDER               <- base::order;
  rasBaseRBIND               <- base::rbind;
  rasBaseIsNA                <- base::is.na;
  rasBaseNCHAR               <- base::nchar;
  rasBaseLENGTH              <- base::length;
  rasBaseRETURN              <- base::return;
  rasBasePASTE0              <- base::paste0;
  rasBaseSAPPLY              <- base::sapply;
  rasBaseSubSTR              <- base::substr;
  rasBaseSPRINTF             <- base::sprintf;
  rasBaseDEPARSE             <- base::deparse;
  rasBaseIsNULL              <- base::is.null;
  rasBaseMISSING             <- base::missing;
  rasBaseINTEGER             <- base::integer;
  rasBaseLOGICAL             <- base::logical;
  rasBaseMESSAGE             <- base::message;
  rasBaseSEQLEN              <- base::seq_len;
  rasBaseWARNING             <- base::warning;
  rasBaseAsFACTOR            <- base::as.factor;
  rasBaseSysTIME             <- base::Sys.time;
  rasBaseTryCATCH            <- base::tryCatch;
  rasBaseCHARACTER           <- base::character;
  rasBaseGlobalENV           <- base::globalenv;
  rasBaseSeqALONG            <- base::seq_along;
  rasBaseDataFRAME           <- base::data.frame;
  rasBaseIsFUNCTION          <- base::is.function;
  rasBaseENVIRONMENT         <- base::environment;
  rasBaseIsCHARACTER         <- base::is.character;
  rasBaseAsNAMESPACE         <- base::asNamespace;
  rasBaseGetNameSpaceEXPORTS <- base::getNamespaceExports;
  rasUtilsPackageNAME        <- utils::packageName;
  
  if (rasBaseMISSING(ssFindText) || !rasBaseIsCHARACTER(ssFindText) || rasBaseLENGTH(ssFindText) != 1) {
    rasBaseSTOP("ssFindText must be a single character string")
  }
  
  results <- rasBaseDataFRAME(
    "LIBRARY_ID"  = rasBaseCHARACTER(0),
    "FUNC_NAME"   = rasBaseCHARACTER(0),
    "IS_EXPORTED" = rasBaseLOGICAL(0),
    "LINE_NUMBER" = rasBaseINTEGER(0),
    "CODE_SNIP"   = rasBaseCHARACTER(0),
    "FILE_NAME"   = rasBaseCHARACTER(0),
    "SEARCH_TERM" = rasBaseCHARACTER(0),
    stringsAsFactors = FALSE
  )
  
  rcf_get.source.file <- function(func) {
    src_file <- NA_character_;
    srcref <- rasBaseATTR(func, "srcref", exact = TRUE)
    if (!rasBaseIsNULL(srcref)) {
      src_file <- rasBaseTryCATCH({
        rasBaseATTR(srcref, "srcfile", exact = TRUE)$filename
      }, error = function(e) NA_character_)
    }
    if (rasBaseIsNA(src_file)) {
      env <- rasBaseENVIRONMENT(func)
      if (!rasBaseIsNULL(env) && !rasBaseIsNULL(rasBaseATTR(env, "name"))) {
        src_file <- rasBaseATTR(env, "name")
      }
    }
    rasBaseRETURN(src_file)
  }
  
  # --- CRITICAL FIX START: Improved Search with srcref support ---
  rcf_search.single.func <- function(func, func_name, lib_id, is_exported = TRUE) {
    if (!rasBaseIsFUNCTION(func)) rasBaseRETURN(NULL)
    
    srcref <- rasBaseATTR(func, "srcref", exact = TRUE)
    start_line_offset <- 0
    
    if (!rasBaseIsNULL(srcref)) {
      # Use the literal source code preserved in the srcref
      func_code <- as.character(srcref)
      # Extract the absolute starting line number from the file
      start_line_offset <- as.vector(srcref)[1] - 1
    } else {
      # Fallback to deparse ONLY if source is not available
      # 'useSource = TRUE' attempts to find the original formatting
      func_code <- rasBaseTryCATCH({
        rasBaseDEPARSE(func, control = c("keepInteger", "keepNA", "useSource"))
      }, error = function(e) rasBaseCHARACTER(0))
    }
    
    if (rasBaseLENGTH(func_code) == 0) rasBaseRETURN(NULL)
    
    matches <- rasBaseGREP(
      ssFindText, func_code, 
      ignore.case = sbIgnoreCase, 
      perl = sbUseRegex, 
      value = FALSE
    );
    
    if (rasBaseLENGTH(matches) == 0) rasBaseRETURN(NULL)
    
    src_file <- rcf_get.source.file(func)
    
    func_results <- rasBaseDataFRAME(
      "LIBRARY_ID"  = rasBaseREP(lib_id, rasBaseLENGTH(matches)),
      "FUNC_NAME"   = rasBaseREP(func_name, rasBaseLENGTH(matches)),
      "IS_EXPORTED" = rasBaseREP(is_exported, rasBaseLENGTH(matches)),
      # Return the absolute line number in the source file
      "LINE_NUMBER" = matches + start_line_offset,
      "CODE_SNIP"   = rasBaseCHARACTER(rasBaseLENGTH(matches)),
      "FILE_NAME"   = rasBaseREP(src_file, rasBaseLENGTH(matches)),
      "SEARCH_TERM" = rasBaseREP(ssFindText, rasBaseLENGTH(matches)),
      stringsAsFactors = FALSE
    )
    
    for (i in rasBaseSeqALONG(matches)) {
      line_num <- matches[i]
      line_text <- func_code[line_num]
      # Clean up leading/trailing whitespace for the snippet
      line_text <- base::trimws(line_text)
      
      if (rasBaseNCHAR(line_text) > snRetSnipSize) {
        snippet <- rasBasePASTE0(rasBaseSubSTR(line_text, 1, snRetSnipSize - 3), "...")
      } else {
        snippet <- line_text
      }
      func_results$CODE_SNIP[i] <- snippet
    }
    rasBaseRETURN(func_results)
  }
  # --- CRITICAL FIX END ---
  
  # [Rest of logic for vsTargetLibs, coRENVs, and sorting remains same as provided]
  # ... (The loop logic below remains unchanged from your snippet) ...
  
  if (!rasBaseIsNULL(vsTargetLibs)) {
    for (pkg in vsTargetLibs) {
      if (sbVerboseSearch) rasBaseMESSAGE(rasBaseSPRINTF(" -> Searching package: %s", pkg))
      if (!requireNamespace(pkg, quietly = TRUE)) {
        rasBaseWARNING(rasBaseSPRINTF(" -> Package '%s' not available. Skipping.", pkg))
        next
      }
      ns <- rasBaseTryCATCH(rasBaseAsNAMESPACE(pkg), error = function(e) NULL)
      if (rasBaseIsNULL(ns)) next
      func_names <- if (sbSearchInternals) rasBaseLS(ns, all.names = TRUE) else rasBaseGetNameSpaceEXPORTS(pkg)
      func_names <- func_names[rasBaseSAPPLY(func_names, function(x) {
        obj <- rasBaseTryCATCH(rasBaseGET(x, envir = ns), error = function(e) NULL)
        !rasBaseIsNULL(obj) && rasBaseIsFUNCTION(obj)
      })]
      for (func_name in func_names) {
        func <- rasBaseGET(func_name, envir = ns)
        is_exported <- func_name %in% rasBaseGetNameSpaceEXPORTS(pkg)
        func_results <- rcf_search.single.func(func, func_name, pkg, is_exported)
        if (!rasBaseIsNULL(func_results) && rasBaseNROW(func_results) > 0) results <- rasBaseRBIND(results, func_results)
      }
    }
  }
  
  # Environment search and Global search logic follows the same pattern calling rcf_search.single.func
  for (env_idx in rasBaseSeqALONG(coRENVs)) {
    env <- coRENVs[[env_idx]]
    env_name <- rasBaseTryCATCH({
      if (identical(env, rasBaseGlobalENV())) ".GlobalEnv" else if (!rasBaseIsNULL(rasBaseATTR(env, "name"))) rasBaseATTR(env, "name") else rasBasePASTE0("Environment_", env_idx)
    }, error = function(e) rasBasePASTE0("Environment_", env_idx))
    obj_names <- rasBaseTryCATCH(rasBaseLS(env, all.names = TRUE), error = function(e) rasBaseCHARACTER(0));
    for (obj_name in obj_names) {
      obj <- rasBaseTryCATCH(rasBaseGET(obj_name, envir = env), error = function(e) NULL)
      if (!rasBaseIsNULL(obj) && rasBaseIsFUNCTION(obj)) {
        func_results <- rcf_search.single.func(obj, obj_name, env_name, TRUE)
        if (!rasBaseIsNULL(func_results) && rasBaseNROW(func_results) > 0) results <- rasBaseRBIND(results, func_results)
      }
    }
  }
  
  if (sbIncludeGlobal) {
    global_objs <- rasBaseLS(rasBaseGlobalENV(), all.names = TRUE)
    for (obj_name in global_objs) {
      obj <- rasBaseGET(obj_name, envir = rasBaseGlobalENV())
      if (rasBaseIsFUNCTION(obj)) {
        func_results <- rcf_search.single.func(obj, obj_name, ".GlobalEnv", TRUE)
        if (!rasBaseIsNULL(func_results) && rasBaseNROW(func_results) > 0) results <- rasBaseRBIND(results, func_results)
      }
    }
  }
  
  if (rasBaseNROW(results) > 0) {
    results <- results[rasBaseORDER(results$LIBRARY_ID, results$FUNC_NAME, results$LINE_NUMBER), ]
    base::rownames(results) <- NULL
    results$MATCH_ID <- rasBaseSEQLEN(rasBaseNROW(results));
    results$LIBRARY_ID  <- rasBaseAsFACTOR(results$LIBRARY_ID);
    results$FUNC_NAME   <- rasBaseAsFACTOR(results$FUNC_NAME);
    results$FILE_NAME   <- rasBaseAsFACTOR(results$FILE_NAME);
    results$SEARCH_TERM <- rasBaseAsFACTOR(results$SEARCH_TERM);
  }
  
  base::attr(results, "search_info") <- rasBaseLIST(
    "SEARCH_TERM"    = ssFindText,
    "TIME_STAMP"     = rasBaseSysTIME(),
    "TOTAL_MATCHES"  = rasBaseNROW(results)
  );
  class(results) <- c("Search_Results", "data.frame");
  rasBaseRETURN(results);
}


