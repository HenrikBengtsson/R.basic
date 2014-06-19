%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  permutations.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{permutations}
\alias{permutations}


\title{Generate all or some permutations of a vector}

\usage{
permutations(x, idx=NULL, method="lexicographic")
}

\arguments{
  \item{x}{The \code{\link[base]{vector}} from which the permutations should be generated.}
  \item{idx}{A \code{\link[base]{vector}} of integers specifying which permutations in order
   to be generated. If \code{\link[base]{NULL}}, all permutations are generated.}
  \item{method}{Specifies the permutation scheme to be used. Valid
   values are \code{"lexicographic"}.}
}

\description{
  Generate all or some permutations of a \code{\link[base]{vector}} according to a specified
  permutation scheme by calling an underlying permutation generator.
  Currently only lexicographical permutations can be generated.
}

\value{Returns a \code{\link[base]{matrix}} with \emph{N} columns and \code{length(idx)}
 (or \emph{N!}) rows, where \emph{N} is the length of the vector.}

\examples{
x <- c("a", "b", "c", "d")

p1 <- permutations(x)
print(p1)

p2 <- lexicographicPermutations(x)
print(p2)

if (!identical(p1, p2))
  stop("Permutations 'p1' and 'p2' should be identical!")


}

\references{
  [1] A.G. Thakurta, Lexicographic Permutation Analysis,
   \url{http://www.cs.wpi.edu/~dobrush/cs504/f02/projects/Anupama.htm} \cr
}

\author{Henrik Bengtsson}

\seealso{
  See \code{\link{lexicographicPermutations}}() to generate all
  lexicographic permutations of a vector and
  \code{\link{lexicographicPermutation}}() to generate (only) the n:th
  lexicographic permutation of a vector.
  See \code{x[\link[base]{sample}(n)]} to generate a random permutation.
}



\keyword{algebra}
\keyword{math}
