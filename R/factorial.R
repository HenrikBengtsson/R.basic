########################################################################/**
# @RdocFunction factorial
#
# @title "Calculates the n:th factorial"
#
# @synopsis
#
# \description{
#   Calculates \code{n}!, i.e. the \code{n}:th factorial by calling
#   \code{gamma(n+1)} and first asserting that \code{n} is an integer.
#   The largest factorial that can be calculated is 170!.
# }
#
# \arguments{
#   \item{x}{An @integer}.
# }
#
# \value{Returns a number (not an integer since integers can not store large enough values).}
#
# \examples{
#   factorial(4) == 24
#   # [1] TRUE
# }
#
# @author
#
# @keyword "algebra"
# @keyword "math"
#*/######################################################################### 
factorial <- function(x) {
  if (x %% 1 != 0)
    stop(paste("Argument 'x' must be an integer: ", x, sep=""));
  gamma(x+1);
}

############################################################################
# HISTORY:
# 2010-11-28
# o CONFORMATION: Now there exist a factorial() in the 'base' package.
#   It is defined identically except the assertion of 'x' being an integer.
#   Before removing this one, we here make sure it has the same argument
#   name as base::factorial().
# 2003-02-07
# o Created.
############################################################################
