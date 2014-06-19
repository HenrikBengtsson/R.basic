%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  lexicographicPermutation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{lexicographicPermutation}
\alias{lexicographicPermutation.default}
\alias{lexicographicPermutation}

\title{Generate the n:th lexicographic permutation of a vector}

\usage{
\method{lexicographicPermutation}{default}(x, n, ...)
}

\description{
  Generate the n:th lexicographic permutation of a vector. The 0:th
  permutation is the non-permuted vector. The last permutation is the
  (n-1):th.
  If the vector is of length \emph{N}, there are \emph{N!} permutation.
}

\arguments{
  \item{x}{The \code{\link[base]{vector}} from which the permutation should be generated.}
  \item{n}{An \code{\link[base]{integer}} between 0 and \emph{N!-1}, where \emph{N} is the
    length of the vector. If \code{n==0} the non-permutated vector is
    returned.}
  \item{...}{Not used.}
}

\value{Returns a permuted \code{\link[base]{vector}} of the same length as the input vector.}

\examples{\dontrun{See help(lexicographicPermutations) for an example.}}

\seealso{
  See \code{\link{lexicographicPermutations}}() to generate all
  lexicographic permutations of a vector.
  See \code{\link{permutations}}() for a wrapper function to generate
  different types of permutations.
  See \code{x[\link[base]{sample}(n)]} to generate a random permutation.
}

\author{Henrik Bengtsson}


\keyword{algebra}
