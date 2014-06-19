#########################################################################/**
# @RdocDefault drawGrid
#
# @title "Draws an n-times-k grid"
#
# @synopsis
#
# \description{
#  Draws an n-times-k grid in the current plot with grid points (corners
#  and intersections) (x,y) as given by the \code{x} and \code{y} arguments.
# }
#
# \arguments{
#  \item{x}{Either an n-times-k @matrix of all x coordinates in the 
#    grid, or a @vector of length k, which then corresponds to a
#    n-times-k matrix with rows all equal to \code{x}.
#    If @NULL, argument \code{x} is assumed to be a @list containing 
#    the two elements \code{x} and \code{y}.}
#  \item{y}{Either an n-times-k @matrix of all y coordinates in the 
#    grid, or a @vector of length n, which then corresponds to a
#    n-times-k matrix with columns all equal to \code{y}.}
#  \item{fcn}{Draw @function to be applied to each row and column of
#    grid coordinates.}
#  \item{...}{Arguments passed to \code{fcn}.}
# }
#
# \value{
#   Returns a @list with elements \code{x} and \code{y}, which both are
#   n-times-k matrices of the x and the y coordinates of the grid points.
# }
#
# @examples "../incl/drawGrid.Rex"
#
# @author
#
# \seealso{
#   @see "graphics::grid".
# }
#
# @keyword "aplot"
#*/######################################################################### 
setMethodS3("drawGrid", "default", function(x, y=NULL, fcn=lines, ...) { 
  if (is.null(y) && is.list(x)) {
    y <- x$y;
    x <- x$x;
  }

  if (is.vector(x)) {
    xn <- 0;
    xk <- length(x);
  } else if (is.matrix(x)) {
    xn <- nrow(x);
    xk <- ncol(x);
  } else {
    throw("Argument 'x' must be either a vector or a matrix.");
  }

  if (is.vector(y)) {
    yn <- length(y);
    yk <- xk;
    y <- matrix(y, nrow=yn, ncol=yk, byrow=FALSE);
  } else if (is.matrix(y)) {
    yn <- nrow(y);
    yk <- ncol(y);
  } else {
    throw("Argument 'y' must be either a vector or a matrix.");
  }

  if (xn == 0) {
    xn <- yn;
    x <- matrix(x, nrow=xn, ncol=xk, byrow=TRUE);
  } else if (xn != yn) {
    throw("Argument 'x' and 'y' does not agree on the number of grid rows: ", xn, " vs ", yn, ".");
  }

  if (xk != yk) {
    throw("Argument 'x' and 'y' does not agree on the number of grid columns: ", xk, " vs ", yk, ".");
  }

  for (k in seq(length=nrow(x)))
    fcn(x[k,],y[k,], ...);
  for (j in seq(length=ncol(x)))
    fcn(x[,j],y[,j], ...);

  invisible(list(x=x,y=y));
})


############################################################################ 
# HISTORY:
# 2005-02-20
# o Now using setMethodS3().
# 2004-02-12
# o Made into a default function
# 2003-12-20
# o Created from previously internal function.
############################################################################ 
