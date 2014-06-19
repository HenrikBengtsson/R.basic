#########################################################################/**
# @RdocDefault size
#
# @title "Gets the size of an object"
#
# \description{
#  Gets the size of an object. If the object is a vector the length of the
#  vector is returned. If the object is a matrix the dimension of the matrix
#  is returned.
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{The object.}
#   \item{...}{Not used.}
# }
#
# @author
#
# \examples{
#   print(size(1:10))  # 10
#   print(size(matrix(1:10, nrow=5)))  # 5 2
# }
#
# \seealso{
#   See also @see "base::length" and @see "base::dim".
# }
#
# @keyword "attribute"
#*/#########################################################################t
setMethodS3("size", "default", function(x, ...) {
  if (is.vector(x))
    length(x)
  else if (is.matrix(x))
    dim(x)
  else if (is.data.frame(x))
    dim(x)
  else
    stop("The given object don't have a 'size'.");
})

############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2002-07-12
# * Added support for data frames.
# 2001-07-06
# * Created.
############################################################################
