%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  lexicographicPermutations.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{lexicographicPermutations}
\alias{lexicographicPermutations.default}
\alias{lexicographicPermutations}

\title{Generate all lexicographic permutations of a vector}

\usage{
\method{lexicographicPermutations}{default}(x, ...)
}

\description{
  Generate all lexicographic permutations of a vector.
  If the vector is of length \emph{N}, there are \emph{N!} permutation.
}

\arguments{
  \item{x}{The \code{\link[base]{vector}} from which all permutations should be generated.}
  \item{...}{Not used.}
}

\value{Returns a \code{\link[base]{matrix}} with \emph{N} columns and \emph{N!} rows, where
 \emph{N} is the length of the vector.}

\examples{
x <- c("a", "b", "c", "d")

p1 <- NULL
for (k in 1:factorial(length(x))-1)
  p1 <- rbind(p1, lexicographicPermutation(x, k))
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

\seealso{
  See \code{\link{lexicographicPermutation}}() to generate (only) the n:th
  lexicographic permutation of a vector.
  See \code{\link{permutations}}() for a wrapper function to generate
  different types of permutations.
  See \code{x[\link[base]{sample}(n)]} to generate a random permutation.
}

\author{Henrik Bengtsson}


\keyword{algebra}
