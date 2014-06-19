########################################################################/**
# @RdocDefault lexicographicPermutations
#
# @title "Generate all lexicographic permutations of a vector"
#
# @synopsis
#
# \description{
#   @get "title".
#   If the vector is of length \emph{N}, there are \emph{N!} permutation. 
# }
#
# \arguments{
#   \item{x}{The @vector from which all permutations should be generated.}
#   \item{...}{Not used.}
# }
#
# \value{Returns a @matrix with \emph{N} columns and \emph{N!} rows, where
#  \emph{N} is the length of the vector.}
#
# @examples "../incl/lexicographicPermutations.Rex"
#
# \references{
#   [1] A.G. Thakurta, Lexicographic Permutation Analysis,
#     \url{http://www.cs.wpi.edu/~dobrush/cs504/f02/projects/Anupama.htm} \cr
# }
#
# \seealso{
#   See @see "lexicographicPermutation" to generate (only) the n:th
#   lexicographic permutation of a vector.
#   See @see "permutations" for a wrapper function to generate
#   different types of permutations.
#   See \code{x[\link[base]{sample}(n)]} to generate a random permutation.
# }
#
# @author
#
# @keyword "algebra"
#*/######################################################################### 
setMethodS3("lexicographicPermutations", "default", function(x, ...) {
  n <- length(x);
  if (n == 2)
    return(matrix(c(x,x[2],x[1]), nrow=2))
  else if (n == 1)
    return(matrix(x))
  else if (n == 0)
    return();

  res <- NULL;
  for (i in 1:n) {
    rest <- setdiff(x, x[i]);
    perms <- lexicographicPermutations(rest);
    # Append this permutation matrix with the current element
    if (length(perms) < 1)
      perms <- x[i]
    else
      perms <- cbind(rep(x[i], length.out=nrow(perms)), perms);
    res <- rbind(res, perms);
  }  
  as.matrix(res);  
})


############################################################################
# HISTORY:
# 2010-11-28
# o DOCUMENTATION: Fixed some Rd syntax errors.
# 2003-02-07
# o Created.
############################################################################


