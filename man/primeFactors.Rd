%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  primeFactors.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{primeFactors}
\alias{primeFactors}


\title{Factorizes one or several integers into prime factors}

\usage{
primeFactors(x, unique=FALSE)
}

\description{
  Factorizes one or several \code{\link[base]{integer}}s into prime factors.
  The obvious prime factor 1 is always excluded, which means that
  \code{primeFactors(1) == NULL}.

  \emph{Note that the current implementation is not very efficient and
   could most likely be improved.}
}

\arguments{
  \item{x}{A \code{\link[base]{vector}} of \code{\link[base]{integer}}s to be primeFactorsd.}
  \item{unique}{If \code{\link[base:logical]{TRUE}} the unique set of prime factors are
     returned otherwise duplicated prime factors might be returned.}
}

\value{If a single value is given a \code{\link[base]{vector}} of prime factors are returned.
   If a \code{\link[base]{vector}} is given a \code{\link[base]{list}} of vectors containing prime factors are
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
  \code{\link{allFactors}}()
}




\keyword{datagen}
\keyword{iteration}
\keyword{manip}
