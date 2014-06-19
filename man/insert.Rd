%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  insert.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{insert}
\alias{insert.default}
\alias{insert}

\title{Insert values to a vector at certain positions}

\description{
 Insert values to a vector at certain positions.
}

\usage{
\method{insert}{default}(x, index, value=NA, ...)
}

\arguments{
  \item{x}{The \code{\link[base]{vector}} of data values.}
  \item{index}{The indices where the values should be inserted.}
  \item{value}{The values to be inserted. The values will be looped over
    if this vector is shorter than the index vector.}
  \item{...}{Not used.}
}

\author{Henrik Bengtsson}

\examples{
  vec     <- c(1:10,13:15,19:20)
  missing <- setdiff(1:20, vec)
  vec2 <- insert(vec, missing)
  print(vec2)
  # [1]  1  2  3  4  5  6  7  8  9 10 NA NA 13 14 15 NA NA NA 19 20
}


\keyword{manip}
