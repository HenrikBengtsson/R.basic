setMethodS3("assertVersion", "default", function(package, wantedVersion, errorMessage=NULL, url=NULL, ...) {
  if (package == "R")
    package <- "[R]";

  notAvailable <- FALSE;
  if (package == "R" || package == "[R]") {
    current <- paste(R.Version()[c("major", "minor")], collapse=".");
    package <- "[R]";
    url <- "http://cran.r-project.org/";
  } else {
    pkgs <- library()$results[,1];
    if (is.element(package, pkgs))
      current <- packageDescription(package, fields="Version")
    else
      notAvailable <- TRUE;
  }

  if (notAvailable == FALSE)
    tooOld <- (compareVersion(current, wantedVersion) < 0)
  else
    tooOld <- FALSE;
  if (notAvailable == TRUE || tooOld == TRUE) {
    if (is.null(errorMessage)) {
      if (tooOld == TRUE) {
        errorMessage <- paste(package, " v", current,
          " is too old. Please upgrade to version ", wantedVersion,
          " or later.", sep="");
      } else {
        errorMessage <- paste("Package ", package, " is not installed.",
          " Please install version ", wantedVersion, " or later.",
          " See library() for installed packages on your system.", sep="");
      }
      if (!is.null(url)) {
        errorMessage <- paste(errorMessage,
          " The wanted version is available at ", url, ".", sep="");
      }
      if (package == "[R]") {
        errorMessage <- paste(errorMessage, " It is always recommended to run the latest version of [R]. See \"What's new\" at http://www.r-project.org/ to see what the latest version is about.", sep="");
      }
    }
    stop(errorMessage);
  }
  invisible(tooOld);
})

############################################################################
# HISTORY:
# 2013-05-30
# o CLEANUP: Dropped R (< 1.9.0) workaround for assertVersion(),
#   because package requires R (>= 2.10.0).
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2004-04-21
# o Fixed deprecated warning about package.description() for R v1.9.0.
# 2002-01-25
# * Created. For background information see:
#     http://www.r-project.org/nocvs/mail/r-help/2002/subject.html#698
############################################################################
