%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  factorToNumeric.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{factorToNumeric}
\alias{factorToNumeric.default}
\alias{factorToNumeric}

\title{Converts factors to numeric}

\description{
  Converts factors to numeric.
}

\usage{
\method{factorToNumeric}{default}(f, ...)
}

\arguments{
  \item{f}{A \code{\link[base]{factor}} object.}
  \item{...}{Not used.}
}

\author{Henrik Bengtsson}

\examples{
  x <- factor(20:30)
  print(x)
  print(factorToNumeric(x))
}

\seealso{
  \code{\link[base]{factor}}().
}


\keyword{manip}
