%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  nChooseK.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{nChooseK}
\alias{nChooseK}


\title{Computes 'n choose k'}

\usage{
nChooseK(n, k, log=FALSE)
}

\description{
 Computes "n choose k", where \emph{n} is any real number and \emph{k} is
 any integer. "n choose k" is equal to \eqn{n!/((n-k)!k!)}.

 If argument \code{n} and/or argument \code{k} is a vector, they will be
 replicated such that they have the same length.

 If "n choose k" is a too large number to be represented, the logarithm
 of "n choose k" can be calculated by setting \code{log} to \code{TRUE}
 or to any integer, which is then specifying the base of the logarithm.
}

\arguments{
  \item{n}{The number of items t"n choose k"o choose from.}
  \item{k}{The number of items to choose.}
  \item{log}{The base used for the logarithm of the values returned. If
    \code{log=TRUE} the natural base (\code{exp(1)}) will be used.
    If \code{log=FALSE} or \code{log=NULL} the non-logged value is
    returned.}
}

\value{Returns a positive \code{\link[base]{integer}} greater or equal to one that is the
   greatest common divider between the two given values.}

\author{Henrik Bengtsson}

\seealso{
  \code{choose()} and \code{lchoose()} (\code{\link[base]{Special}}).
  \code{\link{factorial}}(). See \code{\link[base]{Special}} for information
  about \code{gamma()} and \code{lgamma()}, which are used for
  calculating the factorials.
}

\examples{
n <- 4; k <- 2;
print( nChooseK(n,k) )   # 6

n <- 6; k <- 2;
print( nChooseK(n,k) )   # 15

n <- 6; k <- 3;
print( nChooseK(n,k) )   # 20

# (a+b)^2 = 1*a^2 + 2*a*b + 1*b^2
n <- 2; k <- 0:2;
print( nChooseK(n,k) )   # 1 2 1

# (a+b)^3 = 1*a^3 + 3*a^2*b + 3*a*b^2 + 1*b^3
n <- 3; k <- 0:3;
print( nChooseK(n,k) )   # 1 3 3 1


}

\references{
 [1] Alexander Bogomolny, Euclid's Algorithm, Feb 2003,
     \url{http://www.cut-the-knot.com/blue/Euclid.shtml}
 [2] R-help thread, About 'choose' function, November 7, 2004.
}



\keyword{math}
\keyword{algebra}
