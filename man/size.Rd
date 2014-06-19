%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  size.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{size}
\alias{size.default}
\alias{size}

\title{Gets the size of an object}

\description{
 Gets the size of an object. If the object is a vector the length of the
 vector is returned. If the object is a matrix the dimension of the matrix
 is returned.
}

\usage{
\method{size}{default}(x, ...)
}

\arguments{
  \item{x}{The object.}
  \item{...}{Not used.}
}

\author{Henrik Bengtsson}

\examples{
  print(size(1:10))  # 10
  print(size(matrix(1:10, nrow=5)))  # 5 2
}

\seealso{
  See also \code{\link[base]{length}}() and \code{\link[base]{dim}}().
}


\keyword{attribute}
