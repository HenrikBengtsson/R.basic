#########################################################################/**
# @set "class=matrix"
# @RdocMethod flip
# @aliasmethod mirror
# @aliasmethod rotate90
# @aliasmethod rotate180
# @aliasmethod rotate270
#
# @title "Matrix flip, mirror, and rotate methods"
#
# \usage{
#  \method{flip}{matrix}(x, ...)
#  \method{mirror}{matrix}(x, ...)
#  \method{rotate90}{matrix}(x, ...)
#  \method{rotate180}{matrix}(x, ...)
#  \method{rotate270}{matrix}(x, ...)
# }
#
# \arguments{
#   \item{x}{A @matrix to be manipulated.}
#   \item{...}{Not used.}
# }
#
# \description{
#   @get "title".
# }
#
# @author
#
# \seealso{
#   @see "base::t".
#   @see "base::aperm".
# }
#
# @keyword "manip"
#*/######################################################################### 
# Flip matrix (upside-down)
setMethodS3("flip", "matrix",  function(x, ...) {
  mirror(rotate180(x))
})


# Mirror matrix (left-right)
setMethodS3("mirror", "matrix",  function(x, ...) {
  xx <- as.data.frame(x);
  xx <- rev(xx);
  xx <- as.matrix(xx);
  xx;
})


# Rotate matrix 90 clockworks
setMethodS3("rotate90", "matrix",  function(x, ...) {
  t(mirror(x))
})


# Rotate matrix 180 clockworks
setMethodS3("rotate180", "matrix",  function(x, ...) { 
  xx <- rev(x);
  dim(xx) <- dim(x);
  xx;
})


# Rotate matrix 270 clockworks
setMethodS3("rotate270", "matrix",  function(x, ...) {
  mirror(t(x))
})

############################################################################
# HISTORY:
# 2005-02-20
# o Removed explicit *.matrix() calls to rely on the generic functions.
# o Added '...' to please R CMD check.
# 2004-10-18
# o Added Rdoc comments.
# o Making use of setMethodS3().
# 2002-05-31
# o Created. Still have to write the Rdoc's.
############################################################################
