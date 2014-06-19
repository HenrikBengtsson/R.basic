%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  drawGrid.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{drawGrid}
\alias{drawGrid.default}
\alias{drawGrid}

\title{Draws an n-times-k grid}

\usage{
\method{drawGrid}{default}(x, y=NULL, fcn=lines, ...)
}

\description{
 Draws an n-times-k grid in the current plot with grid points (corners
 and intersections) (x,y) as given by the \code{x} and \code{y} arguments.
}

\arguments{
 \item{x}{Either an n-times-k \code{\link[base]{matrix}} of all x coordinates in the
   grid, or a \code{\link[base]{vector}} of length k, which then corresponds to a
   n-times-k matrix with rows all equal to \code{x}.
   If \code{\link[base]{NULL}}, argument \code{x} is assumed to be a \code{\link[base]{list}} containing
   the two elements \code{x} and \code{y}.}
 \item{y}{Either an n-times-k \code{\link[base]{matrix}} of all y coordinates in the
   grid, or a \code{\link[base]{vector}} of length n, which then corresponds to a
   n-times-k matrix with columns all equal to \code{y}.}
 \item{fcn}{Draw \code{\link[base]{function}} to be applied to each row and column of
   grid coordinates.}
 \item{...}{Arguments passed to \code{fcn}.}
}

\value{
  Returns a \code{\link[base]{list}} with elements \code{x} and \code{y}, which both are
  n-times-k matrices of the x and the y coordinates of the grid points.
}

\examples{
plot(0:20,0:20)

# Simple grid
drawGrid(2:8,2:8)

# Advanced grid
x <- seq(1,19,by=1/2)
y <- matrix(10:15, nrow=6, ncol=length(x))
y <- t(t(y) + sin(0.3*(1:ncol(y))))
drawGrid(x,y, fcn=function(...) { points(...); lines(...); }, col="red")

}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[graphics]{grid}}.
}


\keyword{aplot}
