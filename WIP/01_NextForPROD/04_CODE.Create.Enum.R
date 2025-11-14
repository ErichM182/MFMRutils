#? ### ### ### ### ### ### ###
#' @title Create Environment-safe and Immutable Enumerated R Data Objects
#' 
#' @description
#' A Helper Function that compiles an Environment-safe and immutable (locked)) R
#' Enumerated Data Object from an input list or vector of character objects.
#'
#' @param vsEnumVals the list or vector of character (string) values to be
#' compiled into an R enumerated data object.
#' @param sbKeepCase a logical (boolean) value that specifies whether the final
#' enumerated values should be in variable case (e.g. "enTest$VariCaseVal") or
#' should all be standardized to UPPER CASE (e.g. "enTest$VARICASEVAL") only.
#'
#' @returns
#' * This function outputs a single R object (i.e. the enumerated data object or
#'  "enum") that contains the input arguments as enumerated data values.
#'
#' @examples
#' ### Easily create Enumerated Data Objects (enums) as follows ...
#' require(MFMRutils)   # -> Installs and loads the "MFMRutils" package ...
#'
#'
#' ## DEMO 1a - Create a 'uniform case' "PlotsThemes" enum ...
#' enPlotsThemesD01 <- code.create.enum(
#'   vsEnumVals = c("black_WHite", "ClasSIC",   # -> Observe how the input character values do
#'   "dark", "gRAY", "lighT", "LINE_draw",      #    not have a standard (i.e. uniform) case !!!
#'   "MiNiMAL", "voiD")
#' );
#'
#' ## DEMO 1b - Results from DEMO 1a above ...
#' enPlotsThemesD01$DARK          # Output => 3 !!!
#' enPlotsThemesD01$MINIMAL       # Output => 7 !!!
#' enPlotsThemesD01$BLACK_WHITE   # Output => 1 !!!
#' enPlotsThemesD01$LIGHT         # Output => 5 !!!
#' enPlotsThemesD01$CLASSIC       # Output => 2 !!!
#' # Have you noticed👆 how the enum values have all been standardized to UPPER CASE? 👀
#'
#'
#'
#' ## DEMO 2a - Create a 'variable case' "PlotsThemes" enum ...
#' enPlotsThemesD02 <- code.create.enum(
#'   vsEnumVals = c("black_WHite", "ClasSIC",
#'   "dark", "gRAY", "lighT", "LINE_draw",
#'   "MiNiMAL", "voiD"), sbKeepCase = TRUE      # -> Set to TRUE !!!
#' );
#'
#' ## DEMO 2b - Results from DEMO 2a above ...
#' enPlotsThemesD02$dark          # Output -> 3 !!!
#' enPlotsThemesD02$MiNiMAL       # Output -> 7 !!!
#' enPlotsThemesD02$black_WHite   # Output -> 1 !!!
#' enPlotsThemesD02$lighT         # Output -> 5 !!!
#' # Please notice👆 that the enum values have the exact character cases as provided to
#' # the function via the `vsEnumVals` function argument !!!
#'
#'
#'
#' ## DEMO 3a - The function is duplication-safe (when standardizing case) !!!
#' enPlotsThemesD03 <- code.create.enum(         # -> This function was provided with duplicate
#'   vsEnumVals = c("black_WHite", "ClasSIC",    #    values for the levels "Classic", "Dark"
#'   "dark", "gRAY", "lighT", "LINE_draw",       #    and "Minimal" as input values !!!
#'   "MiNiMAL", "voiD", "darK", "Dark", "DaRk",
#'   "DarK", "claSSic", "CLASSic", "minIMAL",
#'   "MINImal"), sbKeepCase = FALSE            # -> Set to FALSE - the duplication check only
#' );                                          #    works if values are set to a uniform case !!!
#'
#' ## DEMO 3b - Results from DEMO 3a above ...
#' enPlotsThemesD03$DARK      # Output -> 3 !!!
#' enPlotsThemesD03$MINIMAL   # Output -> 7 !!!
#' enPlotsThemesD03$CLASSIC   # Output -> 2 !!!
#' # Repeated values👆 are not allowed for Enumerated Data Objects. As such the function
#' # only compiles (records) the first occurrence (index) of any duplicated input values !!!
#'
#' @export
#? ### ### ###
"code.create.enum" <- function(vsEnumVals=NULL, sbKeepCase=FALSE) {

  ssTagFuncID <- "EnumCreate";

  if (base::is.null(vsEnumVals)) {
    ssPostNote = MFMRutils::info.post.note(
      ssFuncCallerID = base::paste0(ssTagFuncID, " ", 79),
      csColorPostNote = EnvCOLORS$RedFORE, csColorCFID = EnvCOLORS$RedFORE,
      ssPostNote = "The `vsEnumVals` function argument should not be NULL !!!\n"
    )
  } else {

    if (sbKeepCase) {   # -> Maintain the character cases as supplied to function !!!
      values <- base::unique(vsEnumVals);
    } else {   # -> Standardize all character cases to UPPER CASE !!!
      values <- base::unique(base::toupper(vsEnumVals));
    }

    res <- stats::setNames(base::seq_along(values), values);
    res <- base::as.environment(base::as.list(res));
    base::lockEnvironment(res, bindings = TRUE);
    base::return(res);
  }

}


