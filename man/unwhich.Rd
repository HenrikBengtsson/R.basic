%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  unwhich.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{unwhich}
\alias{unwhich.default}
\alias{unwhich}

\title{Gets a logical vector/matrix from indices}

\usage{
\method{unwhich}{default}(x, dim=NULL, ...)
}

\description{
 Gets a logical vector/matrix from indices, indices which indicates
 elements that are \code{\link[base:logical]{TRUE}}. All other elements are \code{\link[base:logical]{FALSE}}.

 If the given set of indices is a integer vector, the result will be
 a logical vector. The length of the vector is as long as the length
 of the input vector, but it can also be specified using the argument
 \code{dim}.

 If the given set of indices is a matrix with two columns (as returned
 by \code{which(x, arr.ind=TRUE)} the result is a logical matrix. The
 dimension of the minimum size such that all indices in the input matrix
 exists, but it can also be specified using the argument \code{dim},
 where \code{dim[1]} is the number of rows and \code{dim[2]} is the
 number of columns.
}

\arguments{
  \item{x}{The index \code{\link[base]{vector}}/\code{\link[base]{matrix}}.}
  \item{dim}{For a index vector this is a singular value and for a index
    matrix it is a integer vector of length two. If \code{\link[base]{NULL}} the
    minimum dimension will used.}
  \item{...}{Not used.}
}

\author{Henrik Bengtsson}

\examples{
  # Examples with vectors
  vec <- rep(c(TRUE, TRUE, FALSE), 3)

  idx <- which(vec)
  print(idx)    # 1 2 4 5 7 8

  log <- unwhich(idx)
  print(log)    # TRUE TRUE FALSE TRUE TRUE FALSE TRUE TRUE

  log <- unwhich(idx, length(vec))
  print(log)    # TRUE TRUE FALSE TRUE TRUE FALSE TRUE TRUE FALSE

  # Examples with matrices
  mat <- matrix(c(TRUE, TRUE, FALSE), nrow=3, ncol=3)

  idx <- which(mat, arr.ind=TRUE)
  print(idx)
  # 	   row col
  # [1,]   1   1
  # [2,]   2   1
  # [3,]   1   2
  # [4,]   2   2
  # [5,]   1   3
  # [6,]   2   3

  log <- unwhich(idx)
  print(log)
  #      [,1] [,2] [,3]
  # [1,] TRUE TRUE TRUE
  # [2,] TRUE TRUE TRUE

  log <- unwhich(idx, dim=dim(mat))
  print(log)
  #       [,1]  [,2]  [,3]
  # [1,]  TRUE  TRUE  TRUE
  # [2,]  TRUE  TRUE  TRUE
  # [3,] FALSE FALSE FALSE
}

\seealso{
  \code{\link[base]{which}}().
}



\keyword{utilities}
\keyword{datagen}
