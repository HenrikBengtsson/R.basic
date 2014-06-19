%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  about.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{about}
\alias{about.default}
\alias{about}

\title{Gets the version information for some or all installed packages}

\usage{
\method{about}{default}(package=NULL, lib.loc=NULL, ...)
}

\arguments{
 \item{package}{The name of the package. If \code{\link[base]{NULL}}, all installed
       packages are listed.}
 \item{lib.loc}{\code{\link[base]{character}} \code{\link[base]{vector}} describing the location of \R library
       trees to search through, or \code{\link[base]{NULL}}.  The default value of \code{\link[base]{NULL}}
       corresponds to all libraries currently known.}
 \item{...}{Not used.}
}

\description{
  Prints the version information for some or all installed packages.
  Note that argument \code{package} should be a \code{\link[base]{character}} string.
}

\value{Returns a \code{\link[base]{character}} string with package information.}

\examples{\dontrun{
  about()
}}

\seealso{
  \code{\link[base]{library}}() and
  \code{\link[utils]{packageDescription}}.
}

\author{Henrik Bengtsson}



\keyword{utilities}
\keyword{print}
