%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  points3d.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{points3d}
\alias{points3d.default}
\alias{points3d}

\title{Adding data points to a three dimensional plot}

\description{
  Adding data points to a three dimensional plot previously created by
  \code{plot3d()} or \code{persp()}.
  Note that adding data points to a three dimensional plot will violate
  the depth order of the data points; data points that should be in the
  back, maybe hidden by other data points, might be plotted on top of
  the latter instead. Normally this behavior is not wanted and it is
  preferred to plot all data points at once using \code{plot3d()},
  which preserved the depth order.
}

\usage{
\method{points3d}{default}(x=seq(0, 1, len = nrow(z)), y=seq(0, 1, len = ncol(z)), z,
  persp.matrix=getOption("persp.matrix"), cex=par("cex"), col=par("col"), pch=par("pch"),
  depthOrder=TRUE, ...)
}

\arguments{
  \item{x}{the coordinates of points in the plot. Alternatively, a single
   plotting structure, function or any R object with a plot method can be
   provided.}
  \item{y}{the y coordinates of points in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{z}{the z coordinates of points in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{persp.matrix}{an 4-by-4 transformation matrix describing how
    to project the (x,y,z) points to the drawing canvas as the one
    returned by \code{persp}(). Default value is
    \code{getOption("persp.matrix")}, which is set by \code{plot3d()}.}
  \item{cex}{the character expansion of the data points. Default is
    \code{par("cex")}.}
  \item{col}{the color of the data points.
   Default value is \code{par("col")}.}
  \item{pch}{plotting "character", i.e. symbol to use. For more
   information see \code{\link[graphics]{points}}.
   Default value is \code{par("pch")}.}
  \item{depthOrder}{If \code{\link[base:logical]{TRUE}}, the data points are plotted
    back-to-from relative to the view plane, otherwise they are plotted
    in the order they occur in the data.}
  \item{...}{further arguments accepted by \code{\link[graphics]{points}}.}
}

\author{Henrik Bengtsson}

\examples{
# Simulate three groups of (x,y,z) data
n <- 3*1000
gr <- list()
gr[[1]] <- matrix(rnorm(n, mean=c(0,0,1), sd=c(1,1,1)), ncol=3, byrow=TRUE)
gr[[2]] <- matrix(rnorm(n, mean=c(5,4,2), sd=c(1,2,1)), ncol=3, byrow=TRUE)
gr[[3]] <- matrix(rnorm(n, mean=c(2,2,4), sd=c(0.2,0.5,0.8)), ncol=3, byrow=TRUE)

# Calculate the overall limits of the axis
xlim <- ylim <- zlim <- NA
for (k in 1:3) {
  gr[[k]] <- as.data.frame(gr[[k]])
  colnames(gr[[k]]) <- c("x", "y", "z")
  xlim <- range(xlim, gr[[k]][,1], na.rm=TRUE)
  ylim <- range(ylim, gr[[k]][,2], na.rm=TRUE)
  zlim <- range(zlim, gr[[k]][,3], na.rm=TRUE)
}

# First plot one group using plot3d()...
plot3d(gr[[1]], phi=50, pch=176, col="red", xlim=xlim, ylim=ylim, zlim=zlim,
       xlab="x", ylab="y", zlab="z")

# ...and then use points3d() to plot the other groups
for (k in 2:3)
  points3d(gr[[k]], pch=176, col=c("blue", "green")[k-1])
}

\seealso{
  For creating a 3D plot see \code{\link{plot3d}}().
  For adding lines to a 3D plot see \code{\link{lines3d}}().
  For adding text to a 3D plot see \code{\link{text3d}}().
  For adding polygons to a 3D plot see \code{\link{polygon3d}}().
  For adding stems to a 3D plot see \code{\link{stem3d}}().
  See also \code{\link[graphics]{persp}}.
  Package \code{scatterplot3d} by Uwe Ligges.
  For detail about the graphical parameter arguments, see
  \code{\link[graphics]{par}}.
}


\keyword{aplot}
