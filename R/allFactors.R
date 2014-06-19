#########################################################################/**
# @RdocFunction allFactors
#
# @title "Factorizes one or several integers into all possible factors"
#
# @synopsis
#
# \description{
#   Factorizes one or several @integers into all possible @integer factors,
#   including, but not only, prime factors.
#
#   The obvious factors 1 and \code{x} are always excluded, which means
#   that \code{allFactors(1) == NULL} and \code{allFactors(p) == NULL}
#   where $p$ is a prime.
#
#   \emph{This implementation relies on the \code{primeFactors()} function,
#    which currently is not very efficient in certain cases, making this
#    function suffer its slowness.}
# }
#
# \arguments{
#   \item{x}{A @vector of @integers to be factorized.}
#   \item{unique}{If @TRUE the unique set of integer factors are 
#      returned otherwise duplicated integer factors might be returned.}
# }
#
# \value{If a single value is given a @vector of all factors are returned.
#    If a @vector is given a @list of vectors containing all factors are
#    returned.}
#
# @author
#
# @examples "../incl/allFactors.Rex"
#
# \seealso{
#    @see "primeFactors"
# }
#
# @keyword "datagen"
# @keyword "iteration"
# @keyword "manip"
#*/#########################################################################
allFactors <- function(x, unique=TRUE) {
  if (any(x <= 0))
    stop("Can only factorize positive integers.");

  allFactorsOne <- function(primeFactors, unique=TRUE) {
    getBits <- function(i) {
      ready <- FALSE;
      bits <- c();
      while (!ready) {
    	bit <- i %% 2;
    	bits <- c(bits, bit);
    	i <- i %/% 2;
    	ready <- (i==0);
      }
      bits;
    } # getBits()

    n <- length(primeFactors);

    if (n <= 1)
      return(c());

    # Note: from=1 will exclude '1' and to=...-1 will exclude 'x'.
    bits <- lapply(seq(from=1, to=2^n-1-1) + 2^n, FUN=function(i) {
      getBits(i)[-(n+1)];
    })
    bits <- as.matrix(as.data.frame(bits));
    rownames(bits) <- NULL;
    colnames(bits) <- NULL;

    factors <- apply(primeFactors * bits, MARGIN=2, FUN=function(x) {
      prod(x[x > 0])
    });
    if (unique)
      factors <- unique(factors);
    factors <- sort(factors);
    factors;
  }

  primeFactors <- primeFactors(x, unique=FALSE);
  if (is.list(primeFactors)) {
    lapply(primeFactors, FUN=allFactorsOne, unique=unique);
  } else {
    allFactorsOne(primeFactors, unique=unique);
  }
}


############################################################################
# HISTORY:
# 2003-02-21
# o Now the function accepts a vector too similar to primeFactors().
# o Added the argument 'unique'.
# 2003-02-20
# o Created!
############################################################################
