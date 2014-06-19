#########################################################################/**
# @RdocFunction nChooseK
#
# @title "Computes 'n choose k'"
#
# @synopsis
#
# \description{
#  Computes "n choose k", where \emph{n} is any real number and \emph{k} is
#  any integer. "n choose k" is equal to \eqn{n!/((n-k)!k!)}.
#
#  If argument \code{n} and/or argument \code{k} is a vector, they will be
#  replicated such that they have the same length.
#
#  If "n choose k" is a too large number to be represented, the logarithm
#  of "n choose k" can be calculated by setting \code{log} to \code{TRUE}
#  or to any integer, which is then specifying the base of the logarithm.
# }
#
# \arguments{
#   \item{n}{The number of items t"n choose k"o choose from.}
#   \item{k}{The number of items to choose.}
#   \item{log}{The base used for the logarithm of the values returned. If
#     \code{log=TRUE} the natural base (\code{exp(1)}) will be used.
#     If \code{log=FALSE} or \code{log=NULL} the non-logged value is 
#     returned.}
# }
#
# \value{Returns a positive @integer greater or equal to one that is the
#    greatest common divider between the two given values.}
#
# @author
#
# \seealso{
#   \code{choose()} and \code{lchoose()} (@see "base::Special"). 
#   @see "factorial". See @see "base::Special" for information
#   about \code{gamma()} and \code{lgamma()}, which are used for 
#   calculating the factorials.
# }
#
# @examples "../incl/nChooseK.Rex"
#
# \references{
#  [1] Alexander Bogomolny, Euclid's Algorithm, Feb 2003, 
#      \url{http://www.cut-the-knot.com/blue/Euclid.shtml}
#  [2] R-help thread, About 'choose' function, November 7, 2004.
# }
#
# @keyword "math"
# @keyword "algebra"
#*/#########################################################################
nChooseK <- function(n, k, log=FALSE) {
  # Local function to calculate n-choose-k for one pair.
  # TODO: Not used yet. /HB 2004-11-08
  nChooseK0 <- function(n, k) {
    if((n == k) || (k==0)) 
      return(1);

    # "Use term-by-term ratios to keep numbers within bounds all the way" [2]
    m <- min(k, n-k);
    # prod((n:(n-m+1))/(m:1)); # May give: 'argument too large in magnitude'
    prod(seq(from=n, to=(n-m+1), by=-1)/(seq(from=m, to=1, by=-1)));
  } # nChooseK0()

  # Process the arguments
  if (is.logical(log)) {
    if (log == TRUE)
      log <- exp(1)
    else
      log <- NULL;
  }
    
  # Repeat n or k to make the of equal length.
  nn <- length(n);
  nk <- length(k);
  if (nn > nk) {
   k <- rep(k, length.out=nn);
   nk <- nn;
  } else if (nn < nk) {
   n <- rep(n, length.out=nk);
   nn <- nk;
  }
  
  if (is.null(log)) {
    gamma(n+1) / (gamma(n-k+1) * gamma(k+1));
  } else {
    (lgamma(n+1) - (lgamma(n-k+1) + lgamma(k+1))) / log(log);
  }
}

############################################################################
# HISTORY:
# 2003-11-08
# o Added reference to choose() and lchoose() in base.
# o Added internal nChooseK0() to implement suggestions in thread [2].
#   Will be used in the future.
# 2003-02-20
# o Created!
############################################################################

