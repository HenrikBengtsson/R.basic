#########################################################################/**
# @RdocDefault lines3d
#
# @title "Adding text to a three dimensional plot"
#
# \description{
#   Adding text to a three dimensional plot previously created by
#   \code{plot3d()} or \code{persp()}.
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{the coordinates of lines in the plot. Alternatively, a single
#    plotting structure, function or any R object with a plot method can be
#    provided.}
#   \item{y}{the y coordinates of lines in the plot, \emph{optional} if
#    \code{x} is an appropriate structure.}
#   \item{z}{the z coordinates of lines in the plot, \emph{optional} if
#    \code{x} is an appropriate structure.}
#   \item{persp.matrix}{an 4-by-4 transformation @matrix describing how
#     to project the (x,y,z) lines to the drawing canvas as the one
#     returned by \code{persp}()}. Default value is
#     \code{getOption("persp.matrix")}, which is set by \code{plot3d()}.
#   \item{cex}{the character expansion of the data lines.}
#   \item{col,lty,lwd}{the colors, line types and the line widths of the lines.}
#   \item{...}{further arguments accepted by @see "graphics::lines".}
# }
#
# @author
#
# @examples "../incl/lines3d.Rex"
#
# \seealso{
#   For creating a 3D plot see "plot3d".
#   For adding points to a 3D plot see @see "points3d".
#   For adding text labels to a 3D plot see @see "text3d".
#   For adding polygons to a 3D plot see @see "polygon3d".
#   For adding stems to a 3D plot see @see "stem3d".
#   See also @see "graphics::persp".
#   Package \pkg{scatterplot3d} by Uwe Ligges.
#   For detail about the graphical parameter arguments, see
#   @see "graphics::par".
# }
#
# @keyword "aplot"
#*/#########################################################################
setMethodS3("lines3d", "default", function(x=seq(0, 1, len=nrow(z)), 
    y=seq(0, 1, len=ncol(z)), z, persp.matrix=getOption("persp.matrix"),
    cex=NULL, col=par("col"), lty=NULL, lwd=NULL, ...) {
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
  trans3d <- function(x,y,z, pmat) {
    tmat <- t((cbind(x,y,z,1)%*% pmat))
    list(x=tmat[1,]/tmat[4,], y=tmat[2,]/tmat[4,], z=tmat[3,]/tmat[4,])
  }

  # Transform the (x,y,z) in R^3 to (x',y') in R^2
  xyz3d <- trans3d(x,y,z, persp.matrix);

  x <- xyz3d$x;
  y <- xyz3d$y;

  split <- FALSE;
  split <- split || (!is.null(cex) && length(cex) > 1);
  split <- split || (!is.null(col) && length(col) > 1);
  split <- split || (!is.null(lty) && length(lty) > 1);
  split <- split || (!is.null(lwd) && length(lwd) > 1);
  
  # Note that these parameters has to be included so they are also
  # reordered by the depth.
  # Vectors of 'cex' and 'col' is supported when type="p".
  n <- length(x);
  if (!is.null(cex)) cex <- rep(cex, length.out=n);
  if (!is.null(col)) col <- rep(col, length.out=n);
  if (!is.null(lty)) lty <- rep(lty, length.out=n-1);
  if (!is.null(lwd)) lwd <- rep(lwd, length.out=n-1);

  if (split == TRUE) {
    for (k in seq(length(x)-1)) {
      idx <- 0:1 + k;
      lines(x[idx],y[idx], cex=cex[idx], col=col[idx], lty=lty[idx[1]], lwd=lwd[idx[1]], ...);
    }
  } else {
    lines(x,y, cex=cex, col=col, lty=lty, lwd=lwd, ...);
  }
})


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3().
# 2005-02-07
# o Changed default argument 'col' from NULL to par("col").
# 2003-12-28
# o BUG FIX: lty and lwd should have number of points minus one elements.
# 2003-03-08 - LAX airport
# o Made function also accept a matrix containing the xyz columns.
# o Made lines3d() a generic function. 
# 2002-04-06
# o Created from text3d.R.
############################################################################
