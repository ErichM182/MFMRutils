

# Define custom null-coalescing operator ...
`%?!%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || any(is.na(x))) {
    y
  } else {
    x
  }
}

# Define the ?? operator as a primitive-like function
`???` <- function(e1, e2) {
  # Use .Primitive-like approach for efficiency
  # Check if e1 is NULL, length 0, or contains only NAs
  if (is.null(e1)) {
    return(e2)
  }
  
  if (length(e1) == 0) {
    return(e2)
  }
  
  # For vectors, if all elements are NA, use default
  if (all(is.na(e1))) {
    return(e2)
  }
  
  # Otherwise return e1
  return(e1)
}

library("MFMRutils")


# Make it a primitive-like function by setting its class
class(`???`) <- c("function", "primitive")
attr(`???`, "srcref") <- NULL

coaTest01_ <- NULL %?!% "default"           # Returns "default"
coaTest02_ <- "actual" %?!% "default"       # Returns "actual"
coaTest03_ <- NULL %?!% NULL %?!% "final"   # Returns "final"

vsDotsArgs_ <- list(
  ssProjID = "rProjTESTr", 
  ssFuncCallerID = "rcf_TEST_FUNC", 
  siFuncMode01 = 0L, 
  ssFuncType = "SML"
);

vsDotsArgs_['ssProjID']


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


rlsLibINFO_ <- pkgs.get.lib.info()
rlsLibINFO_$NAME
rlsLibINFO_$VERSION

FALSE | c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
c(FALSE, TRUE) | c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE) | FALSE
