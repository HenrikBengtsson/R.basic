%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  zscore.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{zscore}
\alias{zscore.default}
\alias{zscore}

\title{Gets the Z scores (standardized residuals)}

\description{
  Z-score is a popular term of what is more formally known as
  \emph{standardized residuals}. To calculate the standardized
  residuals of a data set, the average value and the standard
  deviation of the data value have to be estimated. This can be
  done in either a robust way or a non-robust way.
  Normally the average and the standard deviation are estimated
  using the \code{mean} and \code{sd} functions. These functions
  are sensitive to outliers and a more robust estimates can be
  obtained by the \code{median} and \code{mad} (square root)
  functions.
}

\usage{
\method{zscore}{default}(x, robust=FALSE, ...)
}

\arguments{
  \item{x}{The \code{\link[base]{vector}} of data points.}
  \item{robust}{If \code{\link[base:logical]{TRUE}}, the estimation of the average and
    standard deviation are robust, otherwise not.}
  \item{...}{Any other arguments \code{mean}, \code{median},
    \code{sd} and \code{mad} takes, e.g. \code{na.rm=TRUE}.}
}

\author{Henrik Bengtsson}

\examples{
  x <- rnorm(10000, 0.4, 0.8)
  z <- zscore(x, na.rm=TRUE)
  print(z)
  z <- zscore(x, robust=TRUE, na.rm=TRUE)
  print(z)
}

\seealso{
  See also \code{\link[base]{mean}}(), \code{\link[stats]{median}}, \code{\link[stats]{sd}},
  and \code{\link[stats]{mad}}.
  A similar function to \code{zscore} is \code{rstandard()}
  (see \code{\link[stats]{influence.measures}})
}


\keyword{univar}
