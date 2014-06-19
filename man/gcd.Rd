%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  gcd.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{gcd}
\alias{gcd}


\title{Finds the greatest common divider of two integers}

\usage{
gcd(a, b)
}

\description{
  Finds the \emph{greatest common divider} (gcd) of two integers using
  Euclid's algorithm.
}

\arguments{
  \item{a}{First value.}
  \item{b}{Second value.}
  \emph{Note}: \code{a} can be larger than \code{b} or vice versa.
}

\value{Returns a positive \code{\link[base]{integer}} greater or equal to one that is the
   greatest common divider between the two given values.}

\author{Henrik Bengtsson}

\examples{
# Example of Euclide's algorithm:
#
# Let a = 2322 and b = 654. Calculate gcd(a,b)!
#
# 2322 = 654*3 + 360   gcd(2322, 654) = gcd(654, 360)
# 654  = 360*1 + 294   gcd(654, 360)  = gcd(360, 294)
# 360  = 294*1 + 66    gcd(360, 294)  = gcd(294, 66)
# 294  = 66*4 + 30     gcd(294, 66)   = gcd(66, 30)
# 66   = 30*2 + 6      gcd(66, 30)    = gcd(30, 6)
# 30   = 6*5           gcd(30, 6)     = 6
#
# In other words, gcd(2322,654) = 6.

print( gcd(2322, 654) )     # 6
}

\references{
 [1] Alexander Bogomolny, Euclid's Algorithm, Feb 2003,
     \url{http://www.cut-the-knot.com/blue/Euclid.shtml}
}



\keyword{algebra}
\keyword{math}
