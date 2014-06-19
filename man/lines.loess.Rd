%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  lines.loess.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{lines.loess}
\alias{lines.loess}

\title{Adds connected line segments to a plot using a fitted loess}

\usage{
\method{lines}{loess}(x, subset=NULL, ...)
}

\description{
  Adds connected line segments of a loess fitted structure to a plot
  similar to how lines() works for \code{lowess()}.
}

\arguments{
  \item{x}{An object of class \code{loess}.}
  \item{subset}{The subset of the data points in the fit to be used for
    drawing the line.
    If a single \code{\link[base]{integer}} or a proportion between 0 and 1 is given, a sample
    of that number of data points or proportion, respectively, will be
    drawn (guaranteed to include the left and right most data points) and
    used as the subset. If \code{\link[base]{NULL}}, all data points are included.}
  \item{...}{Arguments that are accepted by \code{lines}.}
}

\value{Returns nothing.}

\details{
  Specifying the subset of data points to be plotted as an integer or as
  as proportion of the number of data points, e.g.
  \code{subset=0.1}, is useful when the fitted data contains a lot of
  data points. This is especially a concern when vector graphics such as
  postscript images are generated where each single subline drawn takes
  time to display and takes up memory.
}

\examples{
   library(stats)   # loess()
   data(cars)
   # draw a smooth line through a scatter plot
   plot(cars, main="Stopping Distance versus Speed")
   lines(lowess(cars), col="blue")
   fit <- loess(dist~speed, data=cars, family="symmetric")
   lines(fit, col="red")
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[stats]{loess}}, \code{\link[stats]{lowess}}, \code{\link[graphics]{lines}}.
}



\keyword{methods}
\keyword{aplot}
\keyword{loess}
