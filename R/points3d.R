#########################################################################/**
# @RdocDefault points3d
#
# @title "Adding data points to a three dimensional plot"
#
# \description{
#   Adding data points to a three dimensional plot previously created by
#   \code{plot3d()} or \code{persp()}.
#   Note that adding data points to a three dimensional plot will violate
#   the depth order of the data points; data points that should be in the
#   back, maybe hidden by other data points, might be plotted on top of
#   the latter instead. Normally this behavior is not wanted and it is
#   preferred to plot all data points at once using \code{plot3d()},
#   which preserved the depth order.
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{the coordinates of points in the plot. Alternatively, a single
#    plotting structure, function or any R object with a plot method can be
#    provided.}
#   \item{y}{the y coordinates of points in the plot, \emph{optional} if
#    \code{x} is an appropriate structure.}
#   \item{z}{the z coordinates of points in the plot, \emph{optional} if
#    \code{x} is an appropriate structure.}
#   \item{persp.matrix}{an 4-by-4 transformation matrix describing how
#     to project the (x,y,z) points to the drawing canvas as the one
#     returned by \code{persp}(). Default value is
#     \code{getOption("persp.matrix")}, which is set by \code{plot3d()}.}
#   \item{cex}{the character expansion of the data points. Default is
#     \code{par("cex")}.}
#   \item{col}{the color of the data points.
#    Default value is \code{par("col")}.}
#   \item{pch}{plotting "character", i.e. symbol to use. For more
#    information see @see "graphics::points".
#    Default value is \code{par("pch")}.}
#   \item{depthOrder}{If @TRUE, the data points are plotted 
#     back-to-from relative to the view plane, otherwise they are plotted
#     in the order they occur in the data.}
#   \item{...}{further arguments accepted by @see "graphics::points".}
# }
#
# @author
#
# @examples "../incl/points3d.Rex"
#
# \seealso{
#   For creating a 3D plot see @see "plot3d".
#   For adding lines to a 3D plot see @see "lines3d".
#   For adding text to a 3D plot see @see "text3d".
#   For adding polygons to a 3D plot see @see "polygon3d".
#   For adding stems to a 3D plot see @see "stem3d".
#   See also @see "graphics::persp".
#   Package \code{scatterplot3d} by Uwe Ligges.
#   For detail about the graphical parameter arguments, see 
#   @see "graphics::par".
# }
#
# @keyword "aplot"
#*/#########################################################################
setMethodS3("points3d", "default", function(x=seq(0, 1, len=nrow(z)), y=seq(0, 1, len=ncol(z)), z, persp.matrix=getOption("persp.matrix"), cex=par("cex"), col=par("col"), pch=par("pch"), depthOrder=TRUE, ...) {
  # Assert that the 4-by-4 perspective transform matrix is available
  if (is.null(persp.matrix))
    stop("Argument persp.matrix must be specified");

  if (!is.matrix(persp.matrix) ||
      nrow(persp.matrix) != 4 || ncol(persp.matrix) != 4) {
    stop("Argument persp.matrix must be a 4x4 matrix as returned by for instance persp() or plot3d().");
  }
  
  # Extract the (x,y,z) coordinates
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z;
        y <- x$y;
        x <- x$x;
      } else if (is.matrix(x)) {
        z <- x[,3];
        y <- x[,2];
        x <- x[,1];
      } else {
        z <- x;
        x <- seq(0, 1, len=nrow(z));
      }
    } else
      stop("no `z' matrix specified");
  } else if (is.list(x)) {
    y <- x$y;
    x <- x$x;
  }

  # Ben Bolker's function for projecting (x,y,z) to (x',y')
  trans3d <- function(x,y,z,pmat) {
    tmat <- t((cbind(x,y,z,1)%*% pmat))
    list(x=tmat[1,]/tmat[4,], y=tmat[2,]/tmat[4,], z=tmat[3,]/tmat[4,])
  }

  # Transform the (x,y,z) in R^3 to (x',y') in R^2
  xyz3d <- trans3d(x,y,z, persp.matrix);

  # Should the points in the back be plotted first?
  if (depthOrder) {
    # Extract the depth order to make sure points at the back are plotted
    # first
    o <- order(xyz3d$z);
    x <- xyz3d$x[o];
    y <- xyz3d$y[o];
    # Note that these parameters has to be included so they are also
    # reordered by the depth.
    n <- length(x);
    if (!is.null(cex)) cex <- rep(cex, length.out=n)[o];
    if (!is.null(col)) col <- rep(col, length.out=n)[o];
    if (!is.null(pch)) pch <- rep(pch, length.out=n)[o];
  } else {
    x <- xyz3d$x;
    y <- xyz3d$y;
    n <- length(x);
    if (!is.null(cex)) cex <- rep(cex, length.out=n);
    if (!is.null(col)) col <- rep(col, length.out=n);
    if (!is.null(pch)) pch <- rep(pch, length.out=n);
  }

  points(x,y, cex=cex, col=col, pch=pch, ...);
})


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3().
# 2003-07-07
# o Updated the Rdoc comments.
# 2003-03-08 - LAX airport
# o Made function also accept a matrix containing the xyz columns.
# o Made points3d() a generic function. 
# 2003-03-03
# o BUG FIX: depthOrder=TRUE for points3d() messed up the order of the
#   arguments cex, col and pch. This bug was introduced in previous update.
# 2003-01-05
# o Added argument depthOrder=TRUE to specify if the data points should be
#   plotted back-to-from relative to the view plane or not. If FALSE, the
#   plotting will be faster.
# 2002-03-15
# * Changed the default value of cex, col, pch and type.
# * Added the Rdocs.
# 2002-03-14
# * Added plot3d() and points3d().
# * Created from Ben Bolker's interesting email about persp() today.
############################################################################
