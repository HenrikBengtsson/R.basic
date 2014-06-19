%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  lines3d.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{lines3d}
\alias{lines3d.default}
\alias{lines3d}

\title{Adding text to a three dimensional plot}

\description{
  Adding text to a three dimensional plot previously created by
  \code{plot3d()} or \code{persp()}.
}

\usage{
\method{lines3d}{default}(x=seq(0, 1, len = nrow(z)), y=seq(0, 1, len = ncol(z)), z,
  persp.matrix=getOption("persp.matrix"), cex=NULL, col=par("col"), lty=NULL, lwd=NULL,
  ...)
}

\arguments{
  \item{x}{the coordinates of lines in the plot. Alternatively, a single
   plotting structure, function or any R object with a plot method can be
   provided.}
  \item{y}{the y coordinates of lines in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{z}{the z coordinates of lines in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{persp.matrix}{an 4-by-4 transformation \code{\link[base]{matrix}} describing how
    to project the (x,y,z) lines to the drawing canvas as the one
    returned by \code{persp}()}. Default value is
    \code{getOption("persp.matrix")}, which is set by \code{plot3d()}.
  \item{cex}{the character expansion of the data lines.}
  \item{col,lty,lwd}{the colors, line types and the line widths of the lines.}
  \item{...}{further arguments accepted by \code{\link[graphics]{lines}}.}
}

\author{Henrik Bengtsson}

\examples{
omega <- seq(0, 6*pi, length.out=1000)
x <- sin(omega); y <- cos(omega)
col <- hsv(omega/max(omega), 1,1)
plot3d(x,y,omega, pch=176, col=col)
o1 <- seq(1, length(x)-10, by=10);
o2 <- o1 + 160;
o3 <- o1 + NA;
o <- as.vector(matrix(c(o1,o2,o3), nrow=3, byrow=TRUE))
lines3d(x[o],y[o],omega[o], col=col[o], lty=1:2)

}

\seealso{
  For creating a 3D plot see "plot3d".
  For adding points to a 3D plot see \code{\link{points3d}}().
  For adding text labels to a 3D plot see \code{\link{text3d}}().
  For adding polygons to a 3D plot see \code{\link{polygon3d}}().
  For adding stems to a 3D plot see \code{\link{stem3d}}().
  See also \code{\link[graphics]{persp}}.
  Package \pkg{scatterplot3d} by Uwe Ligges.
  For detail about the graphical parameter arguments, see
  \code{\link[graphics]{par}}.
}


\keyword{aplot}
