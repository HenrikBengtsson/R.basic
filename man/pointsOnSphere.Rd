%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  pointsOnSphere.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{pointsOnSphere}
\alias{pointsOnSphere.default}
\alias{pointsOnSphere}

\title{Generates points on a sphere}

\description{
  Generates points on a sphere. Attempts to regularly distribute
  approximately \code{n} points on the surface of a unit sphere
  non-iteratively by laying them out along a spiral with a fixed
  (angular) pitch.
}

\usage{
\method{pointsOnSphere}{default}(n=1000, radius=1, ...)
}

\arguments{
  \item{n}{The number of points generated.}
  \item{radius}{The radius of the sphere.}
  \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{list}} of
  \code{x}, \code{y}, \code{z} - cartesian coordinates of the points,
  \code{theta} - their longitude,
  \code{phi} - their lattitude (in radians),
  \code{c} - the fixed (angular) pitch used, and
  \code{radius} - the radius of the sphere.
}

\examples{
nearest <- function(xyz) {
  # Takes a list with columns x, y, z and returns the (straightline)
  # nearest neighbour distances between the points in its rows.
  # Inefficient, but adequate for checking pointsOnSphere().

  res <- NA
  for (i in 1:length(xyz$x)) {
    res[i] <- sqrt(min((xyz$x[-i]-xyz$x[i])^2 + (xyz$y[-i]-xyz$y[i])^2 +
          (xyz$z[-i]-xyz$z[i])^2))
  }
  res
}

xyz <- pointsOnSphere(1000)
nxyz <- nearest(xyz)

layout(matrix(1:4, nrow=2, byrow=TRUE))

plot3d(xyz$x, xyz$y, xyz$z, main="3D view")
plot(nxyz, ylab="nearest neighbour distances",xlab="theta")
plot(xyz$x+sign(xyz$z),xyz$y, main="plan view")
plot(xyz$x+sign(xyz$y),xyz$z, main="side.view")
#hist(nxyz, main="nearest neighbour distances")
print(length(which(nxyz<0.06)))
}

\author{
   Mike Lonergan, \email{mel@mcs.st-and.ac.uk}.
   Adapted by Henrik Bengtsson (\url{http://www.braju.com/R/}).
}


\keyword{datagen}
