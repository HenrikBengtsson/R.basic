#########################################################################/**
# @RdocDefault baseAndExtension
#
# @title "Gets the base and the extension of a filename"
#
# \description{
#  Gets the base and the extension of a filename, i.e. the character strings
#  before and after the last period ('.') in the filename. If no base
#  (extension) exists, it take the value \code{""}.
#
#  Note, here base means a different thing from what is meant in the
#  function @see "base::basename".
# }
#
# @synopsis
#
# \arguments{
#   \item{filename}{A filename.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns a @vector of two @character strings; the base and the extension.
# }
#
# @author
#
# \examples{
#   baseAndExtension("/usr/local/bin/foo.sh")  # "/usr/local/bin/foo" "sh"
#   baseAndExtension("/usr/local/bin/foo...")  # "/usr/local/bin/foo.." ""
#   baseAndExtension("/usr/local/bin/")        # "/usr/local/bin/" ""
# }
#
# \seealso{
#   @see "base::basename" (includes \code{dirname()}).
# }
#
# @keyword "file"
# @keyword "IO"
#*/#########################################################################t
setMethodS3("baseAndExtension", "default", function(filename, ...) {
  endsWith <- function(str, suffix) {
    str <- as.character(str);
    suffix <- as.character(suffix);
    (regexpr(paste(suffix, "$", sep = ""), str)[[1]] != -1);
  }

  if (length(filename) > 1)
    stop("Can not operator on vectors.")
  
  # Case that must be treated specially! A bug or just a anoying design?
  if (endsWith(filename, "\\."))
    filename <- paste(filename, ".", sep="")
  else if (regexpr("\\.", filename)[[1]] == -1)
    filename <- paste(filename, "..", sep="")
  
  fields <- strsplit(filename, split = "\\.")[[1]]
  nfields <- length(fields)
  ext <- fields[nfields]
  if (endsWith(ext, "/") || endsWith(ext, "\\")) {
    nfields <- nfields + 1;
    ext <- "";
  }
  base <- paste(fields[1:(nfields-1)], sep = "", collapse = ".");
  c(base, ext);  
})



############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2001-03-30
# * Made into one function instead of two.
# * Created from class File in R.io since it is so commonly used and I think
#   it is unecessary to load package R.io just for this function.
############################################################################
