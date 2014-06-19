%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  polygon3d.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{polygon3d}
\alias{polygon3d.default}
\alias{polygon3d}

\title{Adds (planar) polygon to a three dimensional plot}

\description{
  Adds a (planar) polygon to a three dimensional plot previously created
  by \code{plot3d()} or \code{persp()}.
}

\usage{
\method{polygon3d}{default}(x, y=NULL, z=NULL, persp.matrix=getOption("persp.matrix"), ...)
}

\arguments{
  \item{x}{the coordinates of polygon in the plot. Alternatively, a single
   plotting structure, function or any R object with a plot method can be
   provided.}
  \item{y}{the y coordinates of polyogon in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{z}{the z coordinates of polyogon in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{persp.matrix}{an 4-by-4 transformation matrix describing how
    to project the (x,y,z) text to the drawing canvas as the one
    returned by \code{persp}()}. Default value is
    \code{getOption("persp.matrix")}, which is set by \code{plot3d()}.
  \item{...}{further arguments accepted by \code{\link[graphics]{polygon}}.}
}

\author{Henrik Bengtsson}

\examples{
n <- 32 + 1
theta <- seq(0, 2*pi, length.out=n)
#theta <- theta[-(n+1)]

x1 <- sin(theta)
y1 <- cos(theta)
z1 <- rep(1, n)

x0 <- rep(0, n)
y0 <- rep(0, n)
z0 <- rep(0, n)

plot3d(x1,y1,z1, type="n", zlim=c(0,1.3), xlab="x", ylab="y", zlab="z", phi=30)

#x1 <- c(x1, x1[1])
#y1 <- c(y1, y1[1])
#z1 <- c(z1, z1[1])

n <- n - 1;
for (k in c(1:n,1:n)[22+1:n-1]) {
  xs <- c(x0[k], x1[k], x1[k+1])
  ys <- c(y0[k], y1[k], y1[k+1])
  zs <- c(z0[k], z1[k], z1[k+1])
  col <- hsv(h=(k-1)/n, s=1, v=1)
  polygon3d(xs,ys,zs, col=col)
}

text3d(x1,y1,z1+0.1, 1:n)
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
  For detail about the graphical parameter arguments, see
  \code{\link[graphics]{par}}.
}


\keyword{aplot}
