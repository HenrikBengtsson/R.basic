########################################################################/**
# @RdocFunction permutations
#
# @title "Generate all or some permutations of a vector"
#
# @synopsis
#
# \arguments{
#   \item{x}{The @vector from which the permutations should be generated.}
#   \item{idx}{A @vector of integers specifying which permutations in order
#    to be generated. If @NULL, all permutations are generated.}
#   \item{method}{Specifies the permutation scheme to be used. Valid 
#    values are \code{"lexicographic"}.}
# }
#
# \description{
#   Generate all or some permutations of a @vector according to a specified
#   permutation scheme by calling an underlying permutation generator.
#   Currently only lexicographical permutations can be generated.
# }
#
# \value{Returns a @matrix with \emph{N} columns and \code{length(idx)}
#  (or \emph{N!}) rows, where \emph{N} is the length of the vector.}
#
# @examples "../incl/permutations.Rex"
#
# \references{
#   [1] A.G. Thakurta, Lexicographic Permutation Analysis,
#    \url{http://www.cs.wpi.edu/~dobrush/cs504/f02/projects/Anupama.htm} \cr
# }
#
# @author
#
# \seealso{
#   See @see "lexicographicPermutations" to generate all
#   lexicographic permutations of a vector and
#   @see "lexicographicPermutation" to generate (only) the n:th
#   lexicographic permutation of a vector.
#   See \code{x[\link[base]{sample}(n)]} to generate a random permutation.
# }
#
# @keyword "algebra"
# @keyword "math"
#*/######################################################################### 
permutations <- function(x, idx=NULL, method="lexicographic") {
  method <- match.arg(method);
  if (method == "lexicographic") {
    if (is.null(idx))
      return(lexicographicPermutations(x));
    res <- c();
    for (n in idx)
      res <- rbind(res, lexicographicPermutation(x, n));
    return(res);
  }
} # permutations()

############################################################################
# HISTORY:
# 2010-11-28
# o DOCUMENTATION: Fixed some Rd syntax errors.
# 2003-02-07
# o Created.
############################################################################
