%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  stem3d.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{stem3d}
\alias{stem3d.default}
\alias{stem3d}

\title{Adds stems to a 3d plot}

\description{
  Adds stems to a three dimensional plot previously created by
  \code{plot3d()} or \code{persp()}. Three-dimensional stem plots
  display lines extending from the xy-plane (by default). A plot symbol
  represents the z value at the end of each stem.
}

\usage{
\method{stem3d}{default}(x=seq(0, 1, len = nrow(z)), y=seq(0, 1, len = ncol(z)), z,
  persp.matrix=getOption("persp.matrix"), plane=c("xy", "xz", "yz", "yx", "zx", "zy"),
  cex=par("cex"), col=par("col"), pch=par("pch"), depthOrder=TRUE, ...)
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
    returned by \code{persp}()}. Default value is
    \code{getOption("persp.matrix")}, which is set by \code{plot3d()}.
  \item{plane}{Plane from which the stems are originating. Possible
    planes are xy, xz and yz. If the letters are reversed, e.g. yx, the
    stem will originate from the oposite side of the 3d cube.}
  \item{cex}{the character expansion of the data points. Default is
    \code{par("cex")}.}
  \item{col}{the color of the data points.
   Default value is \code{par("col")}.}
  \item{pch}{plotting "character", i.e. symbol to use. For more
   information see \code{\link[graphics]{points}}.
   Default value is \code{par("pch")}.}
  \item{depthOrder}{If \code{TRUE}, the data points are plotted
    back-to-from relative to the view plane, otherwise they are plotted
    in the order they occur in the data.}
  \item{...}{further arguments accepted by \code{\link[graphics]{points}}.}
}

\author{Henrik Bengtsson}

\examples{
x <- seq(0,2, length.out=20)
y <- x / 2
z <- sin(x) + cos(y)

layout(matrix(1:4, nrow=2, byrow=TRUE))

plot3d(x,y,z, type="n", theta=-25, phi=30) # empty 3d plot
stem3d(x,y,z, col="blue", pch=1, cex=1.5)

plot3d(x,y,z, type="n", theta=-25, phi=30) # empty 3d plot
stem3d(x,y,z, col="red", pch=1, cex=1.5, plane="xz")

plot3d(x,y,z, type="n", theta=-25, phi=30) # empty 3d plot
stem3d(x,y,z, col="green", pch=1, cex=1.5, plane="yz")

plot3d(x,y,z, type="n", theta=-25, phi=30) # empty 3d plot
stem3d(x,y,z, col="cyan", pch=".", cex=1.5, plane="xy")
stem3d(x,y,z, col="orange", pch=".", cex=1.5, plane="zx")


}


\seealso{
  For creating a 3D plot see \code{\link{plot3d}}().
  For adding lines to a 3D plot see \code{\link{lines3d}}().
  For adding points to a 3D plot see \code{\link{points3d}}().
  For adding text to a 3D plot see \code{\link{text3d}}().
  For adding polygons to a 3D plot see \code{\link{polygon3d}}().
  For adding stems to a 3D plot see \code{\link{stem3d}}().
  See also \code{\link[graphics]{persp}}.
  Package \code{scatterplot3d} by Uwe Ligges.
  For detail about the graphical parameter arguments,
  see \code{\link[graphics]{par}}.
}


\keyword{aplot}
