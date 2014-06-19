%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  text3d.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{text3d}
\alias{text3d.default}
\alias{text3d}

\title{Adding text to a three dimensional plot}

\description{
  Adding text to a three dimensional plot previously created by
  \code{plot3d()} or \code{persp()}.
}

\usage{
\method{text3d}{default}(x=seq(0, 1, len = nrow(z)), y=seq(0, 1, len = ncol(z)), z, labels=seq(along = x),
  persp.matrix=getOption("persp.matrix"), font=NULL, cex=1, col=par("col"), ...)
}

\arguments{
  \item{x}{the coordinates of text in the plot. Alternatively, a single
   plotting structure, function or any R object with a plot method can be
   provided.}
  \item{y}{the y coordinates of text in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{z}{the z coordinates of text in the plot, \emph{optional} if
   \code{x} is an appropriate structure.}
  \item{labels}{the labels to be printed in the plot.}
  \item{persp.matrix}{an 4-by-4 transformation matrix describing how
    to project the (x,y,z) text to the drawing canvas as the one
    returned by \code{persp}()}. Default value is
    \code{getOption("persp.matrix")}, which is set by \code{plot3d()}.
  \item{cex}{the character expansion of the data text.
   Default value is \code{par(NULL)}.}
  \item{col}{the color of the data text.
   Default value is \code{par(NULL)}.}
  \item{font}{the font to be used. For more information see
   \code{\link[graphics]{text}}.
   Default value is \code{par(NULL)}.}
  \item{...}{further arguments accepted by \code{\link[graphics]{text}}.}
}

\author{Henrik Bengtsson}

\examples{
omega <- seq(0, 6*pi, length.out=1000)
x <- sin(omega); y <- cos(omega)
col <- hsv(omega/max(omega), 1,1)
plot3d(x,y,omega, pch=176, col=col)
o <- seq(1, length(x), by=20)
text3d(x[o],y[o],omega[o], col=rev(col[o]))
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
