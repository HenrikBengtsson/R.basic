%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  factorial.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{factorial}
\alias{factorial}


\title{Calculates the n:th factorial}

\usage{
factorial(x)
}

\description{
  Calculates \code{n}!, i.e. the \code{n}:th factorial by calling
  \code{gamma(n+1)} and first asserting that \code{n} is an integer.
  The largest factorial that can be calculated is 170!.
}

\arguments{
  \item{x}{An \code{\link[base]{integer}}}.
}

\value{Returns a number (not an integer since integers can not store large enough values).}

\examples{
  factorial(4) == 24
  # [1] TRUE
}

\author{Henrik Bengtsson}



\keyword{algebra}
\keyword{math}
