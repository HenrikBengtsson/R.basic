#########################################################################/**
# @RdocDefault image270
#
# @title "Display a Color Image"
#
# \description{
#    Creates a grid of colored or gray-scale rectangles with colors
#    corresponding to the values in \code{x}.  This can be used to display
#    three-dimensional or spatial data aka \emph{images}.
#    This method is identical to \code{image()} except that it first
#    rotates the image 270 degrees (equals 90 degrees clockwise) so 
#    that \code{x[1,1]} is in the upper left corner.
# }
#
# @synopsis
#
# \arguments{
#   \item{z}{a @matrix containing the values to be plotted (@NA's
#      are allowed).}
#   \item{...}{Any arguments that \code{image()} accepts.}
# }
#
# @author
#
# \examples{
#   x <- y <- seq(-4 * pi, 4 * pi, len = 27)
#   r <- sqrt(outer(x^2, y^2, "+"))
#   z <- cos(r^2) * exp(-r/6)
#   z[2,] <- min(z) # To show that z[1,1] is in the
#   z[,2] <- max(z) # upper left corner.
#   
#   colorMap <- gray((0:32)/32)
#   image270(z, col=colorMap)
#   image270(t(z), col=colorMap)
#   image270(mirror(z), col=colorMap)
#   image270(flip(z), col=colorMap)
#   
#   img <- matrix("white", nrow=12, ncol=9)
#   img[2:3,c(2:3,7:8)] <- "blue"
#   img[5:6,5] <- "green"
#   img[9,c(3,7)] <- "red"
#   img[10,4:6] <- "red"
#   
#   z <- rgb2col(col2rgb(img))   # assert format "#rrggbb"
#   colorMap <- sort(unique(z))
#   z <- match(z, colorMap)      # color indices
#   dim(z) <- dim(img)
#   
#   image270(z, col=colorMap)
#   image270(rotate90(z), col=colorMap)
# }
#
# \seealso{
#   @see "graphics::image".
# }
#
# @keyword "hplot"
#*/#########################################################################
setMethodS3("image270", "default", function(z, ...) {
  graphics::image.default(z=rotate270(z), ...)
})


############################################################################
# HISTORY:
# 2006-02-10
# o Removed deprecated image.matrix().
# 2005-05-03
# o Added a deprecated warning when image.matrix() is called.
# 2005-02-20
# o Added image270() to avoid overwriting function of image.default().
#   However, keeping image.matrix() for a while. Will phase it out.
# o Renamed first argument from 'z' to 'x' to please R CMD check.
# o Now using setMethodS3().
# 2003-07-07
# o BUG FIX: From R v1.7.x image.matrix() would generate the error
#   "evaluation is nested too deeply: infinite recursion?", which happened
#   because image() was called internally and not image.default().
# 2002-05-31
# o Created.
############################################################################
