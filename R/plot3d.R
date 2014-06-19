#########################################################################/**
# @RdocDefault plot3d
#
# @title "Plotting data in three dimensions"
#
# \description{
#   Method for plotting data in three dimensions.
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
#   \item{xlim, ylim, zlim}{the ranges to be encompassed by the
#     x, y and z axes. The plot is produced so that the rectangular
#     volume defined by these limits is visible.}
#   \item{xlab, ylab, zlab}{the title for the x, y and z axis.
#     These must the character strings; expressions are not accepted.}
#   \item{main}{an overall title for the plot.}
#   \item{sub}{the subtitle for the plot.}
#   \item{col}{the color of the data points.}
#   \item{theta, phi}{angles defining the viewing direction. theta gives
#    the azimuthal direction and phi the colatitude. Default values are
#     \code{theta=0} and \code{phi=15}.}
#   \item{r}{the distance of the eyepoint from the centre of the plotting
#     box. Default value is \code{sqrt(3)}.}
#   \item{d}{a value which can be used to vary the strength of the
#     perspective transformation. Values of d greater than 1 will
#     lessen the perspective effect and values less and 1 will exaggerate
#     it. Default value is \code{1}.}
#   \item{scale}{before viewing the x, y and z coordinates of the points
#     defining the surface are transformed to the interval [0,1]. If scale
#     is \code{TRUE} the x, y and z coordinates are transformed separately.
#     If scale is \code{FALSE} the coordinates are scaled so that aspect
#     ratios are retained. This is useful for rendering things like DEM
#     information. Default value is \code{TRUE}.}
#   \item{expand}{a expansion factor applied to the z coordinates. Often
#     used with 0 < \code{expand} < 1 to shrink the plotting box in the
#     z direction. Default value is \code{1}.}
#   \item{border}{the color of the line drawn around the surface facets.
#     A value of \code{NA} will disable the drawing of borders. This is
#     sometimes useful when the surface is shaded.
#     If \code{NULL}, will use the default foreground color as defined by
#     the graphical parameters \code{par}.
#     Default value is \code{NULL}.}
#   \item{box}{should the bounding box for the surface be displayed. 
#     Default value is \code{TRUE}.}
#   \item{axes}{should ticks and labels be added to the box.
#     If \code{FALSE} no ticks or labels are drawn.
#     Default value is \code{TRUE}.}
#   \item{ticktype}{If \code{"simple"} the method draws just an arrow
#     parallel to the axis to indicate direction of increase.
#     If \code{"detailed"} it draws normal ticks as per 2D plots.
#     Default value is \code{"simple"}.}
#   \item{nticks}{the (approximate) number of tick marks to draw on the
#     axes. Has no effect if ticktype is \code{"simple"}.
#     Default value is \code{5}.}
#   \item{depthOrder}{If \code{TRUE}, the data points are plotted 
#     back-to-from relative to the view plane, otherwise they are plotted
#     in the order they occur in the data.}
#   \item{...}{Comma-separated list of things to be displayed.}
# }
#
# \details{
#   Internally \code{plot3d}() is based on the @see "graphics::persp"
#   function, which in fact returns a transformation @matrix.
# }
#
# @examples "../incl/plot3d.Rex"
#
# @author
#
# \seealso{
#   For adding lines to a 3D plot see @see "lines3d".
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
# @keyword "hplot"
#*/#########################################################################
setMethodS3("plot3d", "default", function(x=seq(0, 1, len=length(z)), y=seq(0, 1, len=length(z)), z, xlim=range(x, na.rm=TRUE), ylim=range(y, na.rm=TRUE), zlim=range(z, na.rm=TRUE), xlab=NULL, ylab=NULL, zlab=NULL, main=NULL, sub=NULL, col=par("col"), theta=0, phi=15, r=sqrt(3), d=1, scale=TRUE, expand=1, border=NULL, box=TRUE, axes=TRUE, nticks=5, ticktype="simple", depthOrder=TRUE, ...) {
  
  # Sets the labels on the axis
  if (is.null(xlab)) 
    xlab <- if (!missing(x)) deparse(substitute(x)) else "X"
  if (is.null(ylab)) 
    ylab <- if (!missing(y)) deparse(substitute(y)) else "Y"
  if (is.null(zlab)) 
    zlab <- if (!missing(z)) deparse(substitute(z)) else "Z";

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

  # Decide on the axis ranges
  if (is.null(xlim))
    xlim <- range(x, na.rm=TRUE);
  if (is.null(ylim))
    ylim <- range(y, na.rm=TRUE);
  if (is.null(zlim))
    zlim <- range(z, na.rm=TRUE);

  # Some default values for persp(), which are not needed here.
  ltheta <- -135;
  lphi <- 0;
  shade <- NA;
  
  # Get the ticktype ID
  ticktypeId <- pmatch(ticktype, c("simple", "detailed"));

  # Create a minimum empty persp plot.
  xdummy <- 0:1;
  ydummy <- 0:1;
  zdummy <- matrix(rep(NA,4), nrow=2, ncol=2);
  col2 <- NULL;
  
  # Note that argument col is included above to catch it from being passed
  # in the '...'.
  r <- persp(x=xdummy, y=ydummy, z=zdummy,
             xlim=xlim, ylim=ylim, zlim=zlim, 
             theta=theta, phi=phi, r=r, d=d, scale=scale, expand=expand,
             col=col2, border=border, ltheta=ltheta, lphi=lphi, 
             shade=shade, box=box, axes=axes, nticks=nticks,
             ticktype=ticktype, xlab=xlab, ylab=ylab, zlab=zlab, ...);

  # Add some extra information to the perspective matrix as it might be
  # used by other plot functions, e.g. stem3d() need the zlim.
  attr(r, "theta")  <- theta;
  attr(r, "phi")    <- phi;
  attr(r, "expand") <- expand;
  attr(r, "scale")  <- scale;
  attr(r, "r")      <- r;
  attr(r, "d")      <- d;
  attr(r, "xlim")   <- xlim;
  attr(r, "ylim")   <- ylim;
  attr(r, "zlim")   <- zlim;

  # Set any main or subtitle of the plot
  if (!is.null(main) || !is.null(sub)) 
    title(main = main, sub = sub, ...);

  # Plot the (x,y,z) points. 
  points3d(x,y,z, persp.matrix=r, col=col, depthOrder=depthOrder, ...);

  # Remember the 4-by-4 perspective transform matrix
  options(persp.matrix=r);
  
  # ...and return it.
  invisible(r)
})

############################################################################
# HISTORY:
# 2012-03-27
# o CLEAN UP: Dropped .Internal() call.
# 2005-02-20
# o Now using setMethodS3().
# 2005-02-07 
# o Changed default of argument 'col' from NULL to par("col"). In R v1.9.1
#   plot(1:9, col = NULL) would plot in black, but it does not in R v2.0.x.
# 2003-03-08 - LAX airport
# o Made function also accept a matrix containing the xyz columns.
# o Made plot3d() a generic function. 
# 2003-02-06
# o Add some extra information to the perspective matrix returned as it
#   might be needed by other plot functions, e.g. stem3d() need the zlim.
# 2003-01-05
# o Added argument depthOrder=TRUE.
# 2002-03-15
# * Added the Rdocs.
# 2002-03-14
# * Added plot3d() and points3d().
# * Created from Ben Bolker's interesting email about persp() today.
############################################################################
