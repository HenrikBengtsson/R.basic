%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  tail2.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{tail2}
\alias{tail2.default}
\alias{tail2}

\title{Gets the tail (first lines) of objects/files}

\usage{
\method{tail2}{default}(..., file, lines=10, cat=TRUE)
}

\description{
 Gets the tail (last lines) of one or several objects or a file. Depending
 on the argument \code{cat}, the result is either printed to the standard output
 (\code{cat=TRUE}) or returned as a character string. Default is to print to
 standard output. The number of lines printed is specified by the argument
 \code{lines}.
}

\arguments{
  \item{...}{Comma-separated \code{\link[base]{list}} of things to be displayed.}
  \item{file}{File to be displayed.}
  \item{lines}{Number of lines to be printed/returned}
  \item{cat}{If \code{\link[base:logical]{TRUE}}, the lines are printed to the standard output,
     otherwise they are returned as a character string.}

  Either \code{...} or \code{file} must be specified.
}

\author{Henrik Bengtsson}

\examples{
  tail2(tail2.default)
  tail2(file=system.file("DESCRIPTION", package="base"))
  tail2(list.files())
}

\seealso{
  \code{\link{more}}(), \code{\link{head2}}(), \code{\link[base]{sink}}().
}



\keyword{print}
\keyword{utilities}
