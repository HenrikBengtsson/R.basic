%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ListVector.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{[[.ListVector}
\alias{[[.ListVector}
\alias{ListVector.[[}
\alias{[[,ListVector-method}

\title{List-style subseting of object}

\usage{
\method{[[}{ListVector}(object, name, .partial=TRUE, .dropUnique=TRUE)
}

\description{
 List-style subseting of object similar to subsetting of a \code{\link[base]{list}}.
}

\arguments{
 \item{name}{A \code{\link[base]{vector}} of (partial) element names to be retrieved.}
 \item{.partial}{If \code{\link[base:logical]{TRUE}}, partial name matching is applied, otherwise
    exact names are required.}
 \item{.dropUnique}{If \code{\link[base:logical]{TRUE}} and only one matching element exists, the element
    is returned by itself without being wrapped up in a \code{\link[base]{list}}.}
}

\value{
  Returns either a \code{\link[base]{list}}, a \code{\link[base]{vector}} of length one or more, or as \code{\link[base]{NULL}}.
  If more than one element were matched, the elements are returned
  as a \code{\link[base]{list}}.
  If \code{.dropUnique==TRUE} and only one element was matched, the element
  is returned "as is".
  If no matching element exists, \code{\link[base]{NULL}} is returned.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{ListVector}}.
}

\keyword{programming}
\keyword{methods}
\keyword{internal}
\keyword{methods}
