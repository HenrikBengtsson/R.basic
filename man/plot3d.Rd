%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  plot3d.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{plot3d}
\alias{plot3d.default}
\alias{plot3d}

\title{Plotting data in three dimensions}

\description{
  Method for plotting data in three dimensions.
}

\usage{
\method{plot3d}{default}(x=seq(0, 1, len = length(z)), y=seq(0, 1, len = length(z)), z,
  xlim=range(x, na.rm = TRUE), ylim=range(y, na.rm = TRUE), zlim=range(z, na.rm = TRUE),
  xlab=NULL, ylab=NULL, zlab=NULL, main=NULL, sub=NULL, col=par("col"), theta=0, phi=15,
  r=sqrt(3), d=1, scale=TRUE, expand=1, border=NULL, box=TRUE, axes=TRUE, nticks=5,
  ticktype="simple", depthOrder=TRUE, ...)
}

\arguments{
  \item{x}{the coordinates of points in the plot. Alternatively, a single
   plotting structure, function or any R object with a plot method can be
   provided.}
  \item{y}{the y coordinates of points in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{z}{the z coordinates of points in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{xlim, ylim, zlim}{the ranges to be encompassed by the
    x, y and z axes. The plot is produced so that the rectangular
    volume defined by these limits is visible.}
  \item{xlab, ylab, zlab}{the title for the x, y and z axis.
    These must the character strings; expressions are not accepted.}
  \item{main}{an overall title for the plot.}
  \item{sub}{the subtitle for the plot.}
  \item{col}{the color of the data points.}
  \item{theta, phi}{angles defining the viewing direction. theta gives
   the azimuthal direction and phi the colatitude. Default values are
    \code{theta=0} and \code{phi=15}.}
  \item{r}{the distance of the eyepoint from the centre of the plotting
    box. Default value is \code{sqrt(3)}.}
  \item{d}{a value which can be used to vary the strength of the
    perspective transformation. Values of d greater than 1 will
    lessen the perspective effect and values less and 1 will exaggerate
    it. Default value is \code{1}.}
  \item{scale}{before viewing the x, y and z coordinates of the points
    defining the surface are transformed to the interval [0,1]. If scale
    is \code{TRUE} the x, y and z coordinates are transformed separately.
    If scale is \code{FALSE} the coordinates are scaled so that aspect
    ratios are retained. This is useful for rendering things like DEM
    information. Default value is \code{TRUE}.}
  \item{expand}{a expansion factor applied to the z coordinates. Often
    used with 0 < \code{expand} < 1 to shrink the plotting box in the
    z direction. Default value is \code{1}.}
  \item{border}{the color of the line drawn around the surface facets.
    A value of \code{NA} will disable the drawing of borders. This is
    sometimes useful when the surface is shaded.
    If \code{NULL}, will use the default foreground color as defined by
    the graphical parameters \code{par}.
    Default value is \code{NULL}.}
  \item{box}{should the bounding box for the surface be displayed.
    Default value is \code{TRUE}.}
  \item{axes}{should ticks and labels be added to the box.
    If \code{FALSE} no ticks or labels are drawn.
    Default value is \code{TRUE}.}
  \item{ticktype}{If \code{"simple"} the method draws just an arrow
    parallel to the axis to indicate direction of increase.
    If \code{"detailed"} it draws normal ticks as per 2D plots.
    Default value is \code{"simple"}.}
  \item{nticks}{the (approximate) number of tick marks to draw on the
    axes. Has no effect if ticktype is \code{"simple"}.
    Default value is \code{5}.}
  \item{depthOrder}{If \code{TRUE}, the data points are plotted
    back-to-from relative to the view plane, otherwise they are plotted
    in the order they occur in the data.}
  \item{...}{Comma-separated list of things to be displayed.}
}

\details{
  Internally \code{plot3d}() is based on the \code{\link[graphics]{persp}}
  function, which in fact returns a transformation \code{\link[base]{matrix}}.
}

\examples{
# Simulate some (x,y,z) data
n <- 20000
x <- runif(n, 0, 2*pi)
y <- runif(n, 0, 2*pi)
z <- sin(x) * cos(y)

zeroone <- function(x) {
  x.r <- range(x, na.rm=TRUE)
  (x - x.r[1])/(x.r[2]-x.r[1])
}

h <- zeroone(z)
s <- zeroone(x)
v <- zeroone(y)
col <- hsv(h=h, s=1, v=1)
plot3d(x,y,z, phi=30, theta=30, pch=".", col=col)


omega <- seq(0, 6*pi, length.out=500)
x <- sin(omega); y <- cos(omega)
plot3d(x,y,omega, pch=176, col=hsv(omega/max(omega), 1,1))
}

\author{Henrik Bengtsson}

\seealso{
  For adding lines to a 3D plot see \code{\link{lines3d}}().
  For adding points to a 3D plot see \code{\link{points3d}}().
  For adding text labels to a 3D plot see \code{\link{text3d}}().
  For adding polygons to a 3D plot see \code{\link{polygon3d}}().
  For adding stems to a 3D plot see \code{\link{stem3d}}().
  See also \code{\link[graphics]{persp}}.
  Package \pkg{scatterplot3d} by Uwe Ligges.
  For detail about the graphical parameter arguments, see
  \code{\link[graphics]{par}}.
}


\keyword{hplot}
