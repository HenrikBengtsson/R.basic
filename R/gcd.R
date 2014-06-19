#########################################################################/**
# @RdocFunction gcd
#
# @title "Finds the greatest common divider of two integers"
#
# @synopsis
#
# \description{
#   Finds the \emph{greatest common divider} (gcd) of two integers using
#   Euclid's algorithm.
# }
#
# \arguments{
#   \item{a}{First value.}
#   \item{b}{Second value.}
#   \emph{Note}: \code{a} can be larger than \code{b} or vice versa.
# }
#
# \value{Returns a positive @integer greater or equal to one that is the
#    greatest common divider between the two given values.}
#
# @author
#
# @examples "../incl/gcd.Rex"
#
# \references{
#  [1] Alexander Bogomolny, Euclid's Algorithm, Feb 2003, 
#      \url{http://www.cut-the-knot.com/blue/Euclid.shtml}
# }
#
# @keyword "algebra"
# @keyword "math"
#*/#########################################################################
gcd <- function(a,b) {
  # Euclid's algorithm:

  # 1. Take the smallest of the two numbers and call it b, the bigger one a. 
  if (a < b) {
    t <- a;
    a <- b;
    b <- t;
  }

  while (TRUE) {
    # 2. Divide a into b.
#    d <- a %/% b;

    # 3. Take the remainder, and call it r. 
    r <- a %% b;

#    cat(a, "\t= ", b, "*", d, "\t+ ", r, "\t",
#        "gcd(", a, ",", b, ")\t= gcd(", b, ",", r, ")\n", sep="");

    # 4. f r = 0, then we're done, and b is the answer
    if (r == 0)
      return(b);

    # 5. Otherwise, if r > 0, then go B = A and A = R start over at step 2.
    a <- b;
    b <- r;
  } # while()
} # gcd()

############################################################################
# HISTORY:
# 2003-02-20
# o Created!
############################################################################
