%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  gridOptim.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{gridOptim}
\alias{gridOptim.default}
\alias{gridOptim}

\title{Global optimization by partial gridding}

\description{
  Global optimization by partial gridding where the actual optimization is done transparently
  by calls to \code{\link[stats]{optim}}.
}

\usage{
\method{gridOptim}{default}(theta, par, min=NA, max=NA, width=max - min, steps=1, depth=1, shrink=1/2,
  optimizer=stats::optim, ..., maximize=TRUE, .checkArgs=TRUE, progressHandler=NULL,
  verbose=FALSE)
}

\arguments{
 \item{theta}{A \code{\link[base]{vector}} of parameters for which the grid search should
   be performed.
   Thes values are also used as the initial center of the grid.}
 \item{par}{A \code{\link[base]{vector}} of "free" parameters for which the objective
   function is optimized over, given fix \code{theta} parameters.
   Passed to the optimizer function.}
 \item{min,max}{Two \code{\link[base]{vector}}s of bounds for the grid search. No
   \code{theta} parameters will be search for outside these bounds.
   If \code{\link[base]{NA}}, there are no bounds.}
 \item{width}{A \code{\link[base]{vector}} of initial widths for each of the \code{theta}
   parameters.}
 \item{steps}{A \code{\link[base]{vector}} of the number of steps each \code{theta}
   parameter should search over. The total number of \code{theta}
   parameter sets to be optimized over is \code{prod(steps)}.}
 \item{depth}{The depth, or the number of "generations", to grid search
   over. When all \code{theta} parameters sets have been searched, the
   grid can be narrowed around the optimal \code{theta} and recursively
   search around that point. If \code{depth==1}, only the current
   generation is searched.}
 \item{shrink}{A \code{\link[base]{vector}} of shrink factors in (0,1) specifying how much
   the grid widths should be shrunk at each generation.}
 \item{...}{Other arguments passed to the optimizer function.}

 \item{optimizer}{An optimizer \code{\link[base]{function}} called at each grid point.
   For details see below.
   By default the optimizer is \code{\link[stats]{optim}}.}

 \item{maximize}{If \code{\link[base:logical]{TRUE}}, maximization is performed,
   otherwise minimization. Note that for instance the \code{control}
   argument used by \code{\link[stats]{optim}} is \emph{not} interpreted here.}

 \item{.checkArgs}{If \code{\link[base:logical]{TRUE}}, all arguments are checked and
   modified/extended if necessary. If \code{\link[base:logical]{FALSE}}, all arguments are assumed
   to be of correct length, type etc. This argument is used by internal
   recursive calls.}

 \item{progressHandler}{A \code{\link[base]{function}} that is called at certain steps of
   optimization.}

 \item{verbose}{If \code{\link[base:logical]{TRUE}}, detailed information while grid searching is
   printed.}
}

\value{
 Return a \code{\link[base]{list}} with information about the optimal settings:
  \item{theta}{The grid point \code{theta} for which the objective
   function was optimized.}
  \item{optim}{The result returned by \code{\link[stats]{optim}} at the optimum.}
  \item{gridCount}{Total number of grid points tested.}
  \item{callCount}{Total number of calls to the objective (and the
   gradient) function.}
}

\section{Requirements for the optimizer function}{
  The optimizer function specified by the \code{optimizer} argument must
  accept the argument \code{par} as the first argument, cf.
  \code{\link[stats]{optim}}, followed by \code{...}, followed by the named
  argument \code{theta} at any position.

  The optimizer function must return a \code{\link[base]{list}} with the element
  \code{value} containing the optimal numerical value given the
  current gridpoint (passed to the function by an argument named
  \code{theta}).
  Optionally, it may return the number of internal optimization steps
  using the \code{count} element (a \code{\link[base]{numeric}} scalar or \code{\link[base]{vector}}), which
  is then summed up together over the whole grid (including recursive
  grid) by this function.
  For instance, the \code{\link[stats]{optim}} function returns the number of
  calls to the objective and the gradient functions.
}

\section{Details for progress handler function}{
 The \code{progressHandler} function must accept the following arguments:
 \itemize{
  \item{event}{A \code{\link[base]{character}} string specifying what type of event has occured.}
  \item{args}{A \code{\link[base]{list}} of arguments used to call this function.}
  \item{optimum}{A \code{\link[base]{list}} specifying the current optimum. Due to the
    recursive nature of this function, this does not have to be the
    optimum found by all optimizers, but only in the child optimizers.}
 }
}

\author{Henrik Bengtsson}

\examples{
objective <- function(par, ..., theta=theta, verbose=FALSE) {
  if (verbose) {
    args <- list(
      theta=theta,
      par=par,
      ...
    );
    str(args);
  }
 
  sum(theta);
}

theta <- c(1,1)
par <- 0
min <- theta - 2
max <- theta + 2
depth <- 2
steps <- 3

opt <- gridOptim(theta=theta, par=par, min=min, max=max, steps=steps,
                 depth=depth, fn=objective, maximize=FALSE)
print(opt)

}

\seealso{
  \code{\link[stats]{optim}}.
}


\keyword{optimize}
