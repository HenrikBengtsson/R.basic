%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ListVector.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{ListVector}
\alias{ListVector}


\title{Vector that can be subsetted as a list}

\description{
  Vector that can be subsetted as a list.
  This class has special subsetting methods, that is, \code{"["},
  \code{"[["}, and \code{"$"} that allows the object to be treated as
  as a \code{\link[base]{vector}} or as a \code{\link[base]{list}}.
}

\usage{
ListVector(..., .overrideNames=TRUE)
}

\arguments{
 \item{...}{Named arguments to be put in the vector. All elements from
   the same argument gets the same name in the vector. If one of the
   arguments is a \code{\link[base]{list}} it is recursively unwrapped into a named \code{\link[base]{vector}}.}
 \item{.overrideNames}{If \code{\link[base:logical]{TRUE}}, the names of named elements in a \code{\link[base]{list}}
   element are overridden by the name of the list element itself.}
}

\value{
 Return a named \code{\link[base]{vector}} of class \code{ListVector}.
 If no arguments were given, \code{\link[base]{NULL}} is returned.
}

\author{Henrik Bengtsson}

\examples{
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Creating a ListVector
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
theta <- ListVector(a=1:5, b=2, c=100:103, bb=22:24)
print(theta)

# If arguments contains lists these are unwrapped into
# named vectors.
theta2 <- ListVector(list(list(a=1:5), b=2), c=100:103, bb=22:24)
stopifnot(identical(theta, theta2))

# Unnamed elements in a list get the same name as the list
theta3 <- ListVector(list(a=list(1:5), b=2), c=100:103, bb=22:24)
stopifnot(identical(theta, theta3))

# Creating a ListVector out of another ListVector work too.
theta5 <- ListVector(theta)
stopifnot(identical(theta, theta5))

# Creating a ListVector out of another vector as well.
theta4 <- ListVector(as.vector(theta))
stopifnot(identical(theta, theta4))

# Overriding element names
xi <- ListVector(c=list(a=2, b=3))
print(xi)  # Note that 'a' and 'b' are ignored since 'c' is given.



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subsetting a ListVector with "$" and "[[" like a list, but with
# partial name matching.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

cat("--------------------------------------------------\n")
cat("List-style subsetting\n")
cat("--------------------------------------------------\n")
cat("Partial name matching:\n")
print(theta$a)
print(theta$b)
print(theta$c)

print(theta[["a"]])
print(theta[["b"]])
print(theta[["c"]])

cat("Recursive name matching:\n")
print(theta[[c("a", "c")]])

cat("Non-partial name matching:\n")
print(theta[["a", .partial=FALSE]])
print(theta[["b", .partial=FALSE]])
print(theta[["c", .partial=FALSE]])

cat("Non-existing elements:\n")
print(theta[["na"]])

cat("as.list():\n")
print(as.list(theta))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subsetting a ListVector with "[" like a vector, but with
# partial name matching.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("--------------------------------------------------\n")
cat("Vector-style subsetting\n")
cat("--------------------------------------------------\n")
cat("Partial name matching:\n")
print(theta["a"])
print(theta["b"])
print(theta["c"])

cat("Non-partial name matching:\n")
print(theta["a", .partial=FALSE])
print(theta["b", .partial=FALSE])
print(theta["c", .partial=FALSE])

cat("as.vector():\n")
print(as.vector(theta))


}

\seealso{
  \code{\link[base]{vector}}().
  \code{\link[base]{list}}().
}

