#########################################################################/**
# @RdocDefault apply2d
#
# @title "Apply a function of a weighted subregion of a matrix"
#
# \description{
#   Applying a function of a weighted subregion of a @matrix by sweeping a
#   weight @matrix (mask) over the @matrix and for each subregion calling
#   the function.
# }
#
# @synopsis
#
# \arguments{
#   \item{X}{Data @matrix.}
#   \item{weights}{Weight @matrix (mask).}
#   \item{FUN}{A @function to be applied on the \code{w*x}, where
#     \code{w} (equal to \code{weights} except at the margins) is the
#     weights of the subregion and \code{x} is the values at the subregion.}
#   \item{...}{Other arguments accepted by the function specified by
#     \code{FUN}.}
# }
#
# \details{
#   When the weight @matrix, \code{weights}, is swept over the data @matrix,
#   \code{X}, its values will be multiplied (elementwise) with the values
#   of the data @matrix that are in the current region. At the margins the
#   calculations has the same effect as if the data matrix was padded with
#   zeros outside the margins.
# }
#
# @author
#
# @examples "../incl/apply2d.Rex"
#
# \seealso{
#   @see "stats::fft" and @see "stats::convolve".
#   Other (one-dimensional) apply functions are
#   @see "base::apply", \code{\link[base:lapply]{sapply}},
#   @see "base::tapply", @see "base::lapply".
#   Useful functions are also
#   @see "base::sweep" and @see "stats::aggregate".
# }
#
# @keyword "manip"
#*/#########################################################################
setMethodS3("apply2d", "default", function(X, weights=1, FUN, ...) {
  nrow <- nrow(X)
  ncol <- ncol(X)
  Z <- matrix(NA, nrow=nrow, ncol=ncol)
  weights <- as.matrix(weights)
  weights.nrow <- nrow(weights)
  weights.ncol <- ncol(weights)
  weights.x <- (weights.nrow %/% 2 + 1)
  weights.y <- (weights.ncol %/% 2 + 1)
  drow <- 1:weights.nrow - weights.y
  dcol <- 1:weights.ncol - weights.x
  for (row in seq(nrow)) {
    rs <- row + drow;
    rs.ok <- (0 < rs & rs <= nrow);
    rs <- rs[rs.ok];
    mrs <- rs - row + weights.y;
    for (col in seq(ncol)) {
      cs <- col+dcol;
      cs.ok <- (0 < cs & cs <= ncol);
      cs <- cs[cs.ok];
      mcs <- cs - col + weights.x;
      x <- X[rs, cs];
      w <- weights[mrs, mcs];
      Z[row,col] <- FUN(w*x, ...)
    }
  }
  Z;
})


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3().
# 2003-07-07
# o Updated the Rdoc comments.
# o Rdoc BUG FIX: The argument list has mismatching curly brackets.
# 2002-06-28
# * Created from former applyMask.R and applyRectangle.R.
############################################################################
