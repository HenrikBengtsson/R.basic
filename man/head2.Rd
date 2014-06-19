%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  head2.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{head2}
\alias{head2.default}
\alias{head2}

\title{Gets the head (first lines) of objects/files}

\usage{
\method{head2}{default}(..., file, lines=10, cat=TRUE)
}

\description{
 Gets the head (first lines) of one or several objects or a file. Depending
 on the argument \code{cat}, the result is either printed to the standard output
 (\code{cat=TRUE}) or returned as a character string. Default is to print to
 standard output. The number of lines printed is specified by the argument
 \code{lines}.
}

\arguments{
  \item{...}{Comma-separated \code{\link[base]{list}} of things to be displayed.}
  \item{file}{File to be displayed.}
  \item{lines}{Number of lines printed/returned.}
  \item{cat}{If \code{\link[base:logical]{TRUE}}, the lines are printed to the standard output,
     otherwise they are returned as a character string.}

  Either \code{...} or \code{file} must be specified.
}

\author{Henrik Bengtsson}

\examples{
  head2(head2.default)
  head2(file=system.file("DESCRIPTION", package="base"))
  head2(list.files())
}

\seealso{
  \code{\link{more}}(), \code{\link{tail2}}(), \code{\link[base]{sink}}().
}



\keyword{print}
\keyword{utilities}
