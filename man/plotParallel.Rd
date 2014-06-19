%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  plotParallel.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{plotParallel}
\alias{plotParallel.default}
\alias{plotParallel}

\title{Plots data in parallel coordinates}

\description{
  Plots data in parallel coordinates for displaying multidimensional
  data.
}

\usage{
\method{plotParallel}{default}(object, order=NULL, flip=NULL, horizontal=TRUE, col=par("col"), lty=NULL,
  lwd=NULL, ylab="", xlab="", las=2, dim.col="gray", ylim=NULL, ...)
}

\arguments{
  \item{object}{Object containing the data. Accepted data types are
    data frames, matrices and list, which are all converted internally
    by \code{as.data.frame(object)}.}
  \item{order}{A \code{\link[base]{vector}} specifying the order in which the dimensions should
    be plotted. Some dimensions can be excluded. If \code{\link[base]{NULL}}, all
   dimensions are plotting in the order they appear in \code{object}.}
  \item{flip}{Vector specifying if a dimension should be flipped
    (inversed) or not. If \code{\link[base]{NULL}}, no dimensions are flipped.}
  \item{horizontal}{If \code{\link[base:logical]{TRUE}} the dimensions are plotted along
    the x-axis, otherwise they are plotted along the y-axis.}
  \item{col}{Vector specifying the line color for each data point.}
  \item{lty}{Vector specifying the line types for each data point.}
  \item{lwd}{Vector specifying the line width for each data point.}
  \item{xlab, ylab}{The labels on the x and the y axis.}
  \item{las}{The rotation of the dimension labels. Default value is
    \code{2}, which means "always perpendicular to the axis". For
    more information see \code{\link[graphics]{par}}.}
  \item{dim.col}{The colors of the dimension lines.}
  \item{ylim}{A vector of two numbers or a matrix with two rows. The
    first and the second row corresponds to the lower and the upper
    cutoff levels, respectively. If a value is \code{\link[base]{NA}}, it means
    that the data decides the cutoff level (in that direction).}
  \item{...}{Other parameters accepted by \code{\link[graphics]{plot}}.}
}

\examples{
data(freeny)
plotParallel(freeny, order=2:5, flip=c(FALSE,TRUE,FALSE,FALSE))

data(airquality)
r <- range(airquality$Temp, na.rm=TRUE)
tempidx <- floor(99*(airquality$Temp-r[1])/(r[2]-r[1]))+1
col <- heat.colors(100)[tempidx]
r <- range(airquality$Wind, na.rm=TRUE)
windidx <- (airquality$Wind-r[1])/(r[2]-r[1])
lwd <- 2*windidx+1
plotParallel(airquality, order=1:4, col=col, lwd=lwd, horizontal=FALSE)
}

\author{Henrik Bengtsson}

\references{
  Wegman, E., (1990). Hyperdimensional Data Analysis Using Parallel
  Coordinates. Journal of American Statistics Association, 85, 664-675.
}

\seealso{
  To plot data in three dimensions see \code{\link{plot3d}}().
}


\keyword{hplot}
