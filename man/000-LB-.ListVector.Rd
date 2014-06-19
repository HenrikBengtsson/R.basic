%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ListVector.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{[.ListVector}
\alias{[.ListVector}
\alias{ListVector.[}
\alias{[,ListVector-method}

\title{Vector-style subseting of object}

\usage{
\method{[}{ListVector}(object, name, .partial=TRUE)
}

\description{
 Vector-style subseting of object similar to subsetting of a \code{\link[base]{vector}}.
}

\arguments{
 \item{name}{A \code{\link[base]{vector}} of partial element names to be retrieved.}
 \item{.partial}{If \code{\link[base:logical]{TRUE}}, partial name matching is applied, otherwise
    exact names are required.}
}

\value{
  Returns a named \code{\link[base]{vector}} of length zero or more consisting of
  matched elements.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{ListVector}}.
}

\keyword{programming}
\keyword{methods}
\keyword{internal}
\keyword{methods}
