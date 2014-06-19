%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  image270.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{image270}
\alias{image270.default}
\alias{image270}

 \title{Display a Color Image}

 \description{
    Creates a grid of colored or gray-scale rectangles with colors
    corresponding to the values in \code{x}.  This can be used to display
    three-dimensional or spatial data aka \emph{images}.
    This method is identical to \code{image()} except that it first
    rotates the image 270 degrees (equals 90 degrees clockwise) so
    that \code{x[1,1]} is in the upper left corner.
 }

 \usage{
\method{image270}{default}(z, ...)
}

 \arguments{
   \item{z}{a \code{\link[base]{matrix}} containing the values to be plotted (\code{\link[base]{NA}}'s
      are allowed).}
   \item{...}{Any arguments that \code{image()} accepts.}
 }

 \author{Henrik Bengtsson}

 \examples{
   x <- y <- seq(-4 * pi, 4 * pi, len = 27)
   r <- sqrt(outer(x^2, y^2, "+"))
   z <- cos(r^2) * exp(-r/6)
   z[2,] <- min(z) # To show that z[1,1] is in the
   z[,2] <- max(z) # upper left corner.
 
   colorMap <- gray((0:32)/32)
   image270(z, col=colorMap)
   image270(t(z), col=colorMap)
   image270(mirror(z), col=colorMap)
   image270(flip(z), col=colorMap)
 
   img <- matrix("white", nrow=12, ncol=9)
   img[2:3,c(2:3,7:8)] <- "blue"
   img[5:6,5] <- "green"
   img[9,c(3,7)] <- "red"
   img[10,4:6] <- "red"
 
   z <- rgb2col(col2rgb(img))   # assert format "#rrggbb"
   colorMap <- sort(unique(z))
   z <- match(z, colorMap)      # color indices
   dim(z) <- dim(img)
 
   image270(z, col=colorMap)
   image270(rotate90(z), col=colorMap)
 }

 \seealso{
   \code{\link[graphics]{image}}.
 }


\keyword{hplot}
