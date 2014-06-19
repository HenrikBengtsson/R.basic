%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  allFactors.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{allFactors}
\alias{allFactors}


\title{Factorizes one or several integers into all possible factors}

\usage{
allFactors(x, unique=TRUE)
}

\description{
  Factorizes one or several \code{\link[base]{integer}}s into all possible \code{\link[base]{integer}} factors,
  including, but not only, prime factors.

  The obvious factors 1 and \code{x} are always excluded, which means
  that \code{allFactors(1) == NULL} and \code{allFactors(p) == NULL}
  where $p$ is a prime.

  \emph{This implementation relies on the \code{primeFactors()} function,
   which currently is not very efficient in certain cases, making this
   function suffer its slowness.}
}

\arguments{
  \item{x}{A \code{\link[base]{vector}} of \code{\link[base]{integer}}s to be factorized.}
  \item{unique}{If \code{\link[base:logical]{TRUE}} the unique set of integer factors are
     returned otherwise duplicated integer factors might be returned.}
}

\value{If a single value is given a \code{\link[base]{vector}} of all factors are returned.
   If a \code{\link[base]{vector}} is given a \code{\link[base]{list}} of vectors containing all factors are
   returned.}

\author{Henrik Bengtsson}

\examples{
x <- 2*2*2*3
print( primeFactors(x) )
print( allFactors(x) )
# 2 2 2 3
# 2 3 4 6 8 12 24

x <- 1
print( primeFactors(x) )
print( allFactors(x) )
# NULL
# NULL

x <- 132319
print( primeFactors(x) )
print( allFactors(x) )
# 11 23 523
# 11 23 253 523 5753 12029

x <- 3128392
print( primeFactors(x) )
print( allFactors(x) )
# 2 2 2 391049
# 2 4 8 391049 782098 1564196

x <- 1:10
str( primeFactors(x) )
# Gives a list of 10 vectors of (non-unique) prime factors
#  List of 10
#   $ 1 : NULL
#   $ 2 : num 2
#   $ 3 : num 3
#   $ 4 : num [1:2] 2 2
#   $ 5 : num 5
#   $ 6 : num [1:2] 2 3
#   $ 7 : num 7
#   $ 8 : num [1:3] 2 2 2
#   $ 9 : num [1:2] 3 3
#   $ 10: num [1:2] 2 5

str( allFactors(x) )
# Gives a list of 10 vectors of (unique) integer factors.
# Note that obvious factors are excluded, meaning that factorizing
# primes will result in an empty vector.
#  List of 10
#   $ 1 : NULL
#   $ 2 : NULL
#   $ 3 : NULL
#   $ 4 : num 2
#   $ 5 : NULL
#   $ 6 : num [1:2] 2 3
#   $ 7 : NULL
#   $ 8 : num [1:2] 2 4
#   $ 9 : num 3
#   $ 10: num [1:2] 2 5


}

\seealso{
   \code{\link{primeFactors}}()
}




\keyword{datagen}
\keyword{iteration}
\keyword{manip}
