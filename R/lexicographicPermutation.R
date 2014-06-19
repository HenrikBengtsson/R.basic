########################################################################/**
# @RdocDefault lexicographicPermutation
#
# @title "Generate the n:th lexicographic permutation of a vector"
#
# @synopsis
#
# \description{
#   @get "title". The 0:th 
#   permutation is the non-permuted vector. The last permutation is the
#   (n-1):th.
#   If the vector is of length \emph{N}, there are \emph{N!} permutation. 
# }
#
# \arguments{
#   \item{x}{The @vector from which the permutation should be generated.}
#   \item{n}{An @integer between 0 and \emph{N!-1}, where \emph{N} is the 
#     length of the vector. If \code{n==0} the non-permutated vector is
#     returned.}
#   \item{...}{Not used.}
# }
#
# \value{Returns a permuted @vector of the same length as the input vector.}
#
# \examples{\dontrun{See help(lexicographicPermutations) for an example.}}
#
# \seealso{
#   See @see "lexicographicPermutations" to generate all
#   lexicographic permutations of a vector.
#   See @see "permutations" for a wrapper function to generate
#   different types of permutations.
#   See \code{x[\link[base]{sample}(n)]} to generate a random permutation.
# }
#
# @author
#
# @keyword "algebra"
#*/######################################################################### 
setMethodS3("lexicographicPermutation", "default", function(x, n, ...) {
  res <- c();
  for (i in (length(x)-1):0) {
    ifact <- factorial(i);
    n <- n %% ((i+1)*ifact);
    q <- n %/% ifact;
    k <- x[q+1];
    x <- setdiff(x, k);
    res <- c(res, k);
  }
  res;
})


############################################################################
# HISTORY:
# 2003-02-07
# o Created.
############################################################################
