#########################################################################/**
# @RdocDefault getTerminalInfo
#
# @title "Gets information about the terminal in which [R] is running"
#
# @synopsis
#
# \description{
#   Gets information about the terminal in which [R] is running.
#   Currently only the number of rows and columns of the terminal is
#   returned.
#
#   This function is used by for instance \code{more()}.
# }
#
# \arguments{
#   \item{rows}{Default value of number of rows in it failed to 
#   retrieve the information from the system.}
#   \item{columns}{Default value of number of columns in it failed to
#   retrieve the information from the system.}
#   \item{...}{Not used.}
# }
#
# \value{Returns a named @list of terminal properties.}
#
# \details{
#   Internally, the system command 'stty' is tried by \code{system()}.
#   If it fails, the default values are returned.
# }
#
# \examples{
#   rows <- getTerminalInfo()$rows
#   cols <- getTerminalInfo()$columns
# }
#
# @author
#
# @keyword "programming"
#*/#########################################################################
setMethodS3("getTerminalInfo", "default", function(rows=NULL, columns=NULL, ...) {
  default <- list(rows=rows, columns=columns);

  # Extract information from stty output
  getFromStty <- function(stty) {
  	res <- unlist(strsplit(paste(stty, sep=""), ";"))
  	
  	rows <- res[grep("rows", res)]
    if (length(rows) == 0)
      return(list(rows=NULL,columns=NULL));
  	re <- regexpr("[0-9]+", rows)
  	rows <- substring(rows, re, re+attr(re, "match.length"))
  	rows <- as.integer(rows)
  	
  	cols <- res[grep("columns", res)]
  	re <- regexpr("[0-9]+", cols)
  	cols <- substring(cols, re, re+attr(re, "match.length")) 
  	cols <- as.integer(cols)
  
  	list(rows=rows, columns=cols)
  }

  # Try to call stty
  cmd <- "stty -a";
  # Note that system() has different argument on different OS's.
  res <- NULL;
  try(res <- system(cmd, intern=TRUE));
  if (length(res) > 0) {
    info <- getFromStty(res);
  } else {
    info <- default;
    if (missing(rows) && missing(columns))
      warning(paste("Could not find system command '", cmd, "'.", sep=""));
  }

  # Set the default values.
  if (length(info$rows) == 0)
    info$rows <- default$rows
  if (length(info$columns) == 0)
    info$columns <- default$columns

  info;
})



############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2004-03-03
# o BUG FIX: If getTerminalInfo() was called and 'stty' did not exists an
#   error was thrown. Not it is caught by try().
# 2002-05-25
# o Updated getFromStty() so it deals can handle stty's like:
#    "/usr/bin/STTY: standard input: Not a character device"
# 2002-04-04
# o Created.
############################################################################
