#########################################################################/**
# @RdocDefault tail2
#
# @title "Gets the tail (first lines) of objects/files"
#
# @synopsis
#
# \description{
#  Gets the tail (last lines) of one or several objects or a file. Depending
#  on the argument \code{cat}, the result is either printed to the standard output
#  (\code{cat=TRUE}) or returned as a character string. Default is to print to
#  standard output. The number of lines printed is specified by the argument
#  \code{lines}.
# }
#
# \arguments{
#   \item{...}{Comma-separated @list of things to be displayed.}
#   \item{file}{File to be displayed.}
#   \item{lines}{Number of lines to be printed/returned}
#   \item{cat}{If @TRUE, the lines are printed to the standard output,
#      otherwise they are returned as a character string.}
#
#   Either \code{...} or \code{file} must be specified.
# }
#
# @author
#
# \examples{
#   tail2(tail2.default)
#   tail2(file=system.file("DESCRIPTION", package="base"))
#   tail2(list.files())
# }
#
# \seealso{
#   @see "more", @see "head2", @see "base::sink".
# }
#
# @keyword "print"
# @keyword "utilities"
#*/#########################################################################
setMethodS3("tail2", "default", function(..., file, lines=10, cat=TRUE) {
  if (lines < 0)
    stop("Argument 'lines' must be zero or larger. Default is ten.");

  len <- length(list(...));
  if (len > 0 && !missing(file))
    stop("Only one of the arguments '...' and 'file' can be given.");

  if (len > 0) {
    expr <- substitute(list(...));
  
    # Write the output to a temporary file using sink().
    file <- tempfile("tail");
    sink(file);
    for (k in 1:length(expr))
      print(eval(expr[[k]]));
    sink();
    on.exit(unlink(file));
  } else if (missing(file)) {
    stop("Either the argument '...' or 'file' must be given.");
  }

  theLines <- readLines(con=file);
  nbrOfLines <- length(theLines);
  theLines <- theLines[(nbrOfLines-min(nbrOfLines, lines)+1):nbrOfLines];
            
  output <- paste(paste(theLines, collapse="\n", sep=""), "\n", sep="");
  if (cat)
    cat(output)
  else
    output;
})



############################################################################
# HISTORY:
# 2011-06-06
# o CONFLICT FIX: Renamed tail() to tail2(), because in more recent
#   versions of R there is another tail() defined in the 'utils' package.
#   For such version of R, Rterm on Windows would hang on TAB completion.
#   Thanks "Neal" for report on this problem.
# 2005-02-20
# o Now using setMethodS3().
# 2002-01-10
# * Created.
############################################################################
