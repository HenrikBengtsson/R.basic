#########################################################################/**
# @RdocDefault "unwhich"
#
# @title "Gets a logical vector/matrix from indices"
#
# @synopsis
#
# \description{
#  Gets a logical vector/matrix from indices, indices which indicates
#  elements that are @TRUE. All other elements are @FALSE.
#
#  If the given set of indices is a integer vector, the result will be
#  a logical vector. The length of the vector is as long as the length
#  of the input vector, but it can also be specified using the argument
#  \code{dim}.
#
#  If the given set of indices is a matrix with two columns (as returned
#  by \code{which(x, arr.ind=TRUE)} the result is a logical matrix. The
#  dimension of the minimum size such that all indices in the input matrix
#  exists, but it can also be specified using the argument \code{dim},
#  where \code{dim[1]} is the number of rows and \code{dim[2]} is the
#  number of columns.
# }
#
# \arguments{
#   \item{x}{The index @vector/@matrix.}
#   \item{dim}{For a index vector this is a singular value and for a index
#     matrix it is a integer vector of length two. If @NULL the
#     minimum dimension will used.}
#   \item{...}{Not used.}
# }
#
# @author
#
# \examples{
#   # Examples with vectors
#   vec <- rep(c(TRUE, TRUE, FALSE), 3)
#
#   idx <- which(vec)
#   print(idx)    # 1 2 4 5 7 8
#
#   log <- unwhich(idx)
#   print(log)    # TRUE TRUE FALSE TRUE TRUE FALSE TRUE TRUE
#
#   log <- unwhich(idx, length(vec))
#   print(log)    # TRUE TRUE FALSE TRUE TRUE FALSE TRUE TRUE FALSE
#
#   # Examples with matrices
#   mat <- matrix(c(TRUE, TRUE, FALSE), nrow=3, ncol=3)
#
#   idx <- which(mat, arr.ind=TRUE)
#   print(idx)
#   # 	   row col
#   # [1,]   1   1
#   # [2,]   2   1
#   # [3,]   1   2
#   # [4,]   2   2
#   # [5,]   1   3
#   # [6,]   2   3
#
#   log <- unwhich(idx)
#   print(log)
#   #      [,1] [,2] [,3]
#   # [1,] TRUE TRUE TRUE
#   # [2,] TRUE TRUE TRUE
#
#   log <- unwhich(idx, dim=dim(mat))
#   print(log)
#   #       [,1]  [,2]  [,3]
#   # [1,]  TRUE  TRUE  TRUE
#   # [2,]  TRUE  TRUE  TRUE
#   # [3,] FALSE FALSE FALSE
# }
#
# \seealso{
#   @see "base::which".
# }
#
# @keyword "utilities"
# @keyword "datagen"
#*/#########################################################################
setMethodS3("unwhich", "default", function(x, dim=NULL, ...) {
  if (is.matrix(x)) {
    nrow <- max(x[,1], na.rm=TRUE);
    ncol <- max(x[,2], na.rm=TRUE);
    if (is.null(dim)) dim <- c(nrow, ncol);
    dim <- rep(dim, length.out=2);
    l <- matrix(FALSE, nrow=dim[1], ncol=dim[2]);
    l[x] <- TRUE;
  } else if (length(dim) > 1) {
    l <- rep(FALSE, length.out=dim[[1]]);
    l[x] <- TRUE;
    l <- matrix(l, nrow=dim[1], ncol=dim[2]);
  } else {
    if (is.null(dim)) dim <- max(x, na.rm=TRUE);
    l <- rep(FALSE, length.out=dim);
    l[x] <- TRUE;
  }
  l;
})


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2002-01-10
# * Corrected a type in the Rd documentation.
# 2001-07-06
# * Wrote the Rdoc comments.
# 2001-07-05
# * Created.
############################################################################
