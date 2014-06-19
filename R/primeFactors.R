#########################################################################/**
# @RdocFunction primeFactors
#
# @title "Factorizes one or several integers into prime factors"
#
# @synopsis
#
# \description{
#   Factorizes one or several @integers into prime factors. 
#   The obvious prime factor 1 is always excluded, which means that
#   \code{primeFactors(1) == NULL}.
#
#   \emph{Note that the current implementation is not very efficient and
#    could most likely be improved.}
# }
#
# \arguments{
#   \item{x}{A @vector of @integers to be primeFactorsd.}
#   \item{unique}{If @TRUE the unique set of prime factors are 
#      returned otherwise duplicated prime factors might be returned.}
# }
#
# \value{If a single value is given a @vector of prime factors are returned.
#    If a @vector is given a @list of vectors containing prime factors are
#    returned.}
#
# @author
#
# @examples "../incl/allFactors.Rex"
#
# \seealso{
#   @see "allFactors"
# }
#
# @keyword "datagen"
# @keyword "iteration"
# @keyword "manip"
#*/#########################################################################
primeFactors <- function(x, unique=FALSE) {
  if (any(x <= 0))
    stop("Can only prime factorize positive integers.");

  primeFactorsOne <- function(x, unique=FALSE) {
    if (x == 1)
      return(c());

    factors <- c();
  
    p <- 2;
    while (x > 1) {
      r <- x %% p;
      if (r == 0) {
  #      cat("prod=", prod, ", x=", x, ", p=", p, "\n", sep="");
  	factors <- c(factors, p);
  	x <- x %/% p;
      } else {
  	p <- p + 1;
      }
    }
  
    if (unique == TRUE)
      factors <- unique(factors);

    factors;
  }

  if (length(x) == 1) {
    primeFactorsOne(x, unique=unique);
  } else {
    l <- lapply(x, FUN=primeFactorsOne, unique=unique);
    names(l) <- x;
    l;
  }
}


############################################################################
# HISTORY:
# 2003-02-21
# o Added the unique=FALSE argument to be consistent with allFactors().
# o Renamed from factorize() to primeFactors() to make it consistent with
#   allFactors(). Thanks Gordon Smyth for the suggestion.
# 2003-02-20
# o Created!
############################################################################
