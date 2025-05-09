% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkgs.check.code.R
\name{pkgs.check.code}
\alias{pkgs.check.code}
\title{CRAN Code Check with real-time File Version Tracking}
\usage{
pkgs.check.code(
  sbRunDocs = TRUE,
  sbRunCheck = FALSE,
  ssTimeZone = "Africa/Windhoek"
)
}
\arguments{
\item{sbRunDocs}{a logical (boolean) value that specifies whether to run the
standard package documentation process.}

\item{sbRunCheck}{a logical (boolean) value that specifies whether to run the
standard package documentation process.}

\item{ssTimeZone}{a simple character vector (string) that defines the Time
Zone to used for the package documentation.}
}
\value{
\itemize{
\item This function returns the programmatically amended or updated (active or
real-time) version number for the active R-Libs Project as a list of character
objects.
\item This function creates a Work-In-Progress (WIP) directory at the root of the
active R-Libs Project (if not already exists).
\item This function also creates a "DevsVersTimeStamp.txt" file in the "./WIP"
project path for secondary development version tracking.
}
}
\description{
A Helper Function that executes the CRAN pre-requisite Code Checking Procedure
during active R Package Development. This function programmatically updates the
package version number in the R Project DESCRIPTION file before running the
required documentation and/or CRAN Package Pre-Submission Requirements Checks
during iterative development cycles.
}
\examples{
### Print a dummy notification ...
# require(MFMRutils)             # -> Installs and loads the "MFMRutils" package ...
# pkgs.check.code()              # -> when "MFMRutils" library is loaded !!!
# MFMRutils::pkgs.check.code()   # -> when "MFMRutils" library is NOT loaded !!!

### Run 2 different types of code checks ...
# pkgs.check.code(sbRunDocs = TRUE)    # -> Executes only the DevTools Documentation Process.
# pkgs.check.code(sbRunCheck = TRUE)   # -> Executes the more complete CRAN Code Checks.

### Check (i.e. "devtools::check()") overrides the documentation process ...
# # The Documentation Process will only be executed once if both are == TRUE !!!
# pkgs.check.code(sbRunDocs = TRUE, sbRunCheck = TRUE)

}
