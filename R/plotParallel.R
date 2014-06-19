#########################################################################/**
# @RdocDefault plotParallel
#
# @title "Plots data in parallel coordinates"
#
# \description{
#   Plots data in parallel coordinates for displaying multidimensional
#   data.
# }
#
# @synopsis
#
# \arguments{
#   \item{object}{Object containing the data. Accepted data types are
#     data frames, matrices and list, which are all converted internally
#     by \code{as.data.frame(object)}.}
#   \item{order}{A @vector specifying the order in which the dimensions should
#     be plotted. Some dimensions can be excluded. If @NULL, all
#    dimensions are plotting in the order they appear in \code{object}.}
#   \item{flip}{Vector specifying if a dimension should be flipped
#     (inversed) or not. If @NULL, no dimensions are flipped.}
#   \item{horizontal}{If @TRUE the dimensions are plotted along
#     the x-axis, otherwise they are plotted along the y-axis.}
#   \item{col}{Vector specifying the line color for each data point.}
#   \item{lty}{Vector specifying the line types for each data point.}
#   \item{lwd}{Vector specifying the line width for each data point.}
#   \item{xlab, ylab}{The labels on the x and the y axis.}
#   \item{las}{The rotation of the dimension labels. Default value is
#     \code{2}, which means "always perpendicular to the axis". For
#     more information see @see "graphics::par".}
#   \item{dim.col}{The colors of the dimension lines.}
#   \item{ylim}{A vector of two numbers or a matrix with two rows. The
#     first and the second row corresponds to the lower and the upper 
#     cutoff levels, respectively. If a value is @NA, it means
#     that the data decides the cutoff level (in that direction).}
#   \item{...}{Other parameters accepted by @see "graphics::plot".}
# }
#
# @examples "../incl/plotParallel.Rex"
#
# @author
#
# \references{
#   Wegman, E., (1990). Hyperdimensional Data Analysis Using Parallel
#   Coordinates. Journal of American Statistics Association, 85, 664-675.
# }
#
# \seealso{
#   To plot data in three dimensions see @see "plot3d".
# }
#
# @keyword "hplot"
#*/#########################################################################
setMethodS3("plotParallel", "default", function(object, order=NULL, flip=NULL, horizontal=TRUE, col=par("col"), lty=NULL, lwd=NULL, ylab="", xlab="", las=2, dim.col="gray", ylim=NULL, ...) {
  if (is.matrix(object)) {
    object <- as.data.frame(object);
  } else if (is.data.frame(object)) {
    object <- as.data.frame(object);
  } else if (is.list(object)) {
  } else {
    stop("Unsupported data type.");
  }

  n <- nrow(object);
  ndim <- ncol(object);
  if (ndim == 0)
    stop("Nothing to plot.");
  
  y <- object; rm(object);
  if (is.null(order)) {
    order <- 1:ndim;
  } else {
    # Reorder the dimensions
    y <- y[, order];
    ndim <- ncol(y);
  }
  
  # Normalize the scale to [maxValue,minValue] for each dimension
  tmp <- list();
  if (!is.null(ylim)) {
    ylim <- as.matrix(ylim);
    if (nrow(ylim) != 2)
      throw("Argument 'ylim' must be a vector of length two or a matrix with two rows.");
    ylim <- matrix(rep(ylim, length.out=2*ndim), nrow=2);
  }
  for (k in 1:ndim) {
    y[,k] <- as.numeric(y[,k]);

    if (!is.null(ylim)) {
      if (!is.na(ylim[1,k])) {
  	tooSmall <- (y[,k] < ylim[1,k]);
  	tooSmallValues <- y[tooSmall,k];
  	y[tooSmall,k] <- NA;
      }
  
      if (!is.na(ylim[2,k])) {
  	tooLarge <- (y[,k] > ylim[2,k]);
  	tooLargeValues <- y[tooLarge,k];
  	y[tooLarge,k] <- NA;
      }
    }

    ok <- !is.na(y[,k]);
    r <- range(y[ok,k]);

    y[,k] <- (y[,k] - r[1]) / (r[2]-r[1]);

    if (!is.null(ylim)) {
      if (!is.na(ylim[1,k]) && length(tooSmallValues) > 0)
        y[tooSmall,k] <- tooSmallValues;
      if (!is.na(ylim[2,k]) && length(tooLargeValues) > 0)
        y[tooLarge,k] <- tooLargeValues;
    }
  }
  
  if (!is.null(flip)) {
    flip <- as.logical(flip);
    flip <- rep(flip, length.out=ndim);
    for (k in 1:ndim) {
      if (flip[k] == TRUE) {
        y[,k] <- 1 - y[,k];
      }
    }
  }
  
  dimnames <- colnames(y);

  # Create empty data matrices.
  y <- t(cbind(y, dummy=rep(NA, length(n))));
  x <- matrix(c(1:ndim, NA), nrow=ndim+1, ncol=n);

  
  dummy <- as.data.frame(matrix(c(0,1), ncol=4));
  if (horizontal != TRUE) {
    tmp <- x;
    x <- y;
    y <- tmp;
  }

  split <- FALSE;
  split <- split || (!is.null(col) && length(col) > 1);
  split <- split || (!is.null(lty) && length(lty) > 1);
  split <- split || (!is.null(lwd) && length(lwd) > 1);

  if (!is.null(col)) col <- rep(col, length.out=n);
  if (!is.null(lty)) lty <- rep(lty, length.out=n);
  if (!is.null(lwd)) lwd <- rep(lwd, length.out=n);

  x <- as.numeric(x);
  y <- as.numeric(y);
  if (split == TRUE) {
    plot(x,y, type="n", xlab=xlab, ylab=ylab, axes=FALSE);
    for (k in 1:n) {
      idx <- 1:ndim + (k-1)*(ndim+1);
      points(x[idx],y[idx], type="l", col=col[k], lty=lty[k], lwd=lwd[k], ...);
    }
  } else {
    plot(x,y, type="l", xlab=xlab, ylab=ylab, axes=FALSE, col=col, lty=lty, lwd=lwd, ...);
  }
  
  # Dimension lines
  if (!is.null(dim.col)) dim.col <- rep(dim.col, length.out=ndim);
  if (horizontal == TRUE) {
    axis(side=1, at=1:ndim, labels=dimnames, las=las);
    ydim <- par("usr")[3:4];
    for (k in 1:ndim)
      lines(c(k,k), ydim, col=dim.col[k])
  } else {
    axis(side=2, at=1:ndim, labels=dimnames, las=las);
    xdim <- par("usr")[1:2];
    for (k in 1:ndim)
      lines(xdim, c(k,k), col=dim.col[k])
  }
  
})


############################################################################
# HISTORY:
# 2012-03-27
# o ROBUSTNESS: Now plotParallel() calls axis() with full argument names.
# 2005-02-20
# o Now using setMethodS3().
# 2003-03-11
# o BUG FIX: The internal plot() function had ylim=c(0,1) hard coded making
#   horizontal=FALSE to fail working. I can not remember when I introduced
#   this bug, but it used to work at some time.
# 2002-11-28
# o Changed the generic function definition.
# o Added argument 'ylim=NULL'.
# 2002-04-06
# o Created.
############################################################################

