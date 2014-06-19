#########################################################################/**
# @RdocDefault about
#
# @title "Gets the version information for some or all installed packages"
#
# @synopsis
#
# \arguments{
#  \item{package}{The name of the package. If @NULL, all installed
#        packages are listed.}
#  \item{lib.loc}{@character @vector describing the location of \R library
#        trees to search through, or @NULL.  The default value of @NULL
#        corresponds to all libraries currently known.}
#  \item{...}{Not used.}
# }
#
# \description{
#   Prints the version information for some or all installed packages.
#   Note that argument \code{package} should be a @character string.
# }
#
# \value{Returns a @character string with package information.}
#
# \examples{\dontrun{
#   about()
# }}
#
# \seealso{
#   @see "base::library" and
#   @see "utils::packageDescription".
# }
#
# @author
#
# @keyword "utilities"
# @keyword "print"
#*/#########################################################################
setMethodS3("about", "default", function(package=NULL, lib.loc=NULL, ...) {
  throw("about() of R.basic is deprecated. Instead use packageDescription() of the 'utils' package.");

  # AD HOC: To please R CMD check /HB 2010-11-28
  read.00Index <- NULL; rm(read.00Index);

  if (is.null(lib.loc))
    lib.loc <- .libPaths()

  db <- matrix(character(0), nrow=0, ncol=4);
  nopkgs <- character(0);
  for (lib in lib.loc) {
    pkgs <- .packages(all.available=TRUE, lib.loc=lib);
    if (!is.null(package))
      pkgs <- intersect(pkgs, package);
    pkgs <- sort(pkgs)
    for (pkg in pkgs) {
      INDEX <- file.path(lib, pkg, "TITLE");
      title <- if (file.exists(INDEX))
        read.00Index(INDEX)[, 2]
      else ""
      version <- packageDescription(pkg, fields="Version");
      db <- rbind(db, cbind(pkg, lib, title, version))
    }
    if (length(pkgs) == 0)
      nopkgs <- c(nopkgs, lib)
  } # for (lib in lib.loc)

  colnames(db) <- c("Package", "LibPath", "Title", "Version");
  y <- list(header=NULL, results=db, footer=NULL);
  class(y) <- "AboutMatrix";
  y;
}, deprecated=TRUE)


setMethodS3("print", "AboutMatrix", function(x, ...) {
  # To please R CMD check...
  object <- x;

  sQuote <- function(s) paste("`", s, "'", sep = "");

  db <- object$results;
  out <- if (nrow(db) == 0)
    NULL
  else
    lapply(split(1:nrow(db), db[,"LibPath"]), function(ind)
               db[ind,c("Package", "Title", "Version"), drop = FALSE])

  outFile <- tempfile("RAboutMatrix");
  outConn <- file(outFile, open="w");
  first <- TRUE;
  for (lib in names(out)) {
    writeLines(paste(ifelse(first, "", "\n"), "Packages in library ",
                 sQuote(lib), ":\n", sep = ""), outConn);
    version <- paste("[v", out[[lib]][,"Version"], "]", sep="");
    desc <- paste(out[[lib]][,"Title"], version, sep=" ");
    writeLines(formatDL(out[[lib]][,"Package"], desc), outConn);
    first <- FALSE;
  }

  if (first) {
    close(outConn);
    unlink(outFile);
    writeLines("no packages found");
  } else {
    close(outConn);
    file.show(outFile, delete.file=TRUE, title="R packages available");
  }
})



############################################################################
# HISTORY:
# 2013-05-30
# o CLEANUP: Dropped R (< 1.9.0) workaround for packageDescription(),
#   because package requires R (>= 2.10.0).
# 2010-11-28
# o DEPRECATED: about() is deprecated and will thrown an exception.
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2004-04-21
# o Fixed deprecated warning for R v1.9.0.
# 2002-07-07
# o Added Rdoc comments for argument lib.loc.
# 2002-04-02
# * Designed similar to library().
# * Revived from the death ;) and placed in R.base.
#   Totally different from old version() actually.
# 2001-08-06
# * Moved the function version() into R.oo even if it relies on R.lang too.
#   However, it will check if R.lang is installed and it only on request.
#   The rational for having version() in R.oo is that it will always be
#   loaded without the user having to type library(R.lang) first.
# 2001-07-16
# * Created. Wanted to have a very version() function so "beginners" easily
#   could report the what version they are using.
############################################################################
