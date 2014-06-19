#########################################################################/**
# @RdocDefault pointsOnSphere
#
# @title "Generates points on a sphere"
#
# \description{
#   Generates points on a sphere. Attempts to regularly distribute
#   approximately \code{n} points on the surface of a unit sphere
#   non-iteratively by laying them out along a spiral with a fixed
#   (angular) pitch.
# }
#
# @synopsis
#
# \arguments{
#   \item{n}{The number of points generated.}
#   \item{radius}{The radius of the sphere.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns a @list of 
#   \code{x}, \code{y}, \code{z} - cartesian coordinates of the points, 
#   \code{theta} - their longitude, 
#   \code{phi} - their lattitude (in radians), 
#   \code{c} - the fixed (angular) pitch used, and
#   \code{radius} - the radius of the sphere.
# }
#
# @examples "../incl/pointsOnSphere.Rex"
#
# \author{
#    Mike Lonergan, \email{mel@mcs.st-and.ac.uk}.
#    Adapted by @get "author".
# }
#
# @keyword "datagen"
#*/#########################################################################
setMethodS3("pointsOnSphere", "default", function(n=1000, radius=1, ...) {
  # The fixed (angular) pitch:
  c <- sqrt(n*pi)/2;

  # The longitude
  theta <- c(0,2*pi)
  for(i in 3:floor(n/2)) {
      theta[i] <- theta[i-1]+pi/(c*cos(theta[i-1]/(2*c)-pi/2))
  #   theta[i] <- sqrt(2*c+theta[i-1]^2)
  }
  if (2*floor(n/2)==n)
     theta <- c(theta,2*pi*c-rev(theta))
  else
     theta <- c(theta, pi*c, 2*pi*c-rev(theta))

  # The lattitude
  phi <- theta/(2*c) - pi/2;

  list(
    x      = radius*cos(theta)*cos(phi),
    y      = radius*sin(theta)*cos(phi),
    z      = radius*sin(phi),
    theta  = theta,
    phi    = phi,
    c      = c,
    radius = radius
  )
})


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2002-10-29
# o Created.
############################################################################
