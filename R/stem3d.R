#########################################################################/**
# @RdocDefault stem3d
#
# @title "Adds stems to a 3d plot"
#
# \description{
#   Adds stems to a three dimensional plot previously created by
#   \code{plot3d()} or \code{persp()}. Three-dimensional stem plots 
#   display lines extending from the xy-plane (by default). A plot symbol
#   represents the z value at the end of each stem.
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
#     returned by \code{persp}()}. Default value is
#     \code{getOption("persp.matrix")}, which is set by \code{plot3d()}.
#   \item{plane}{Plane from which the stems are originating. Possible
#     planes are xy, xz and yz. If the letters are reversed, e.g. yx, the
#     stem will originate from the oposite side of the 3d cube.}
#   \item{cex}{the character expansion of the data points. Default is
#     \code{par("cex")}.}
#   \item{col}{the color of the data points.
#    Default value is \code{par("col")}.}
#   \item{pch}{plotting "character", i.e. symbol to use. For more
#    information see @see "graphics::points".
#    Default value is \code{par("pch")}.}
#   \item{depthOrder}{If \code{TRUE}, the data points are plotted 
#     back-to-from relative to the view plane, otherwise they are plotted
#     in the order they occur in the data.}
#   \item{...}{further arguments accepted by @see "graphics::points".}
# }
#
# @author
#
# @examples "../incl/stem3d.Rex"
#
#
# \seealso{
#   For creating a 3D plot see @see "plot3d".
#   For adding lines to a 3D plot see @see "lines3d".
#   For adding points to a 3D plot see @see "points3d".
#   For adding text to a 3D plot see @see "text3d".
#   For adding polygons to a 3D plot see @see "polygon3d".
#   For adding stems to a 3D plot see @see "stem3d".
#   See also @see "graphics::persp".
#   Package \code{scatterplot3d} by Uwe Ligges.
#   For detail about the graphical parameter arguments, 
#   see @see "graphics::par".
# }
#
# @keyword "aplot"
#*/#########################################################################
setMethodS3("stem3d", "default", function(x=seq(0, 1, len=nrow(z)), y=seq(0, 1, len=ncol(z)), z, persp.matrix=getOption("persp.matrix"), plane=c("xy", "xz", "yz", "yx", "zx", "zy"), cex=par("cex"), col=par("col"), pch=par("pch"), depthOrder=TRUE, ...) {

  plane <- match.arg(plane);

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

  xs <- ys <- zs <- matrix(NA, nrow=3, ncol=length(z));
  if (is.element(plane, c("yx", "zx", "zy"))) {
    plane <- paste(rev(strsplit(plane,"")[[1]]), collapse="");
    dim <- 2;
  } else {
    dim <- 1;
  }
  if (plane == "xy") {
    stemBase <- attr(persp.matrix, "zlim")[dim];
    xs[1,] <- x;         xs[2,] <- x;
    ys[1,] <- y;         ys[2,] <- y;
    zs[1,] <- stemBase;  zs[2,] <- z;
  } else if (plane == "xz") {
    stemBase <- attr(persp.matrix, "ylim")[dim];
    xs[1,] <- x;         xs[2,] <- x;
    ys[1,] <- stemBase;  ys[2,] <- y;
    zs[1,] <- z;         zs[2,] <- z;
  } else if (plane == "yz") {
    stemBase <- attr(persp.matrix, "xlim")[dim];
    xs[1,] <- stemBase;  xs[2,] <- x;
    ys[1,] <- y;         ys[2,] <- y;
    zs[1,] <- z;         zs[2,] <- z;
  }

  xs <- as.vector(xs);
  ys <- as.vector(ys);
  zs <- as.vector(zs);

  lines3d(xs, ys, zs, persp.matrix=persp.matrix, cex=cex, col=col, ...);
  points3d(x, y, z, persp.matrix=persp.matrix, cex=cex, col=col, pch=pch, depthOrder=depthOrder, ...);
})


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3().
# 2003-07-07
# o BUG FIX: The Rdoc arguments did not show up.
# 2003-03-08 - LAX airport
# o Made function also accept a matrix containing the xyz columns.
# o Made stem3d() a generic function. 
# 2003-02-06
# * Created after seeing a request on the r-help list.
############################################################################

