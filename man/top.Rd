%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  top.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{top}
\alias{top.default}
\alias{top}

\title{Gets the largest data points}

\description{
 Gets the \code{n} largest data points in a vector, where \code{n} can
 either be a fraction or a number. If \code{n} is fraction, at least
 \code{round(n*length(x)+0.5)} values will be found.
}

\usage{
\method{top}{default}(x, n=0.05, na.rm=TRUE, inf.rm=FALSE, ...)
}

\arguments{
  \item{x}{The \code{\link[base]{vector}} of data points}
  \item{n}{The number or the fraction of data points to be selected.
    A value \code{0<n<1} indicates a fraction. Default value is 0.05, i.e.
    the top 5\% percent.}
  \item{na.rm}{If \code{\link[base:logical]{TRUE}} all \code{\link[base]{NA}} and \code{\link[base:is.finite]{NaN}} values are omitted.}
  \item{inf.rm}{If \code{\link[base:logical]{TRUE}} all \code{\link[base:is.finite]{Inf}} values are omitted.}
  \item{...}{Not used.}
}

\author{Henrik Bengtsson}

\examples{
  idx <- which(top(1:100, n=0.021))
  print(idx)    # 98 99 100
}

\seealso{
  \code{\link[base]{which}}()
}


\keyword{datagen}
