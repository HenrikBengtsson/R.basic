%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  baseAndExtension.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{baseAndExtension}
\alias{baseAndExtension.default}
\alias{baseAndExtension}

\title{Gets the base and the extension of a filename}

\description{
 Gets the base and the extension of a filename, i.e. the character strings
 before and after the last period ('.') in the filename. If no base
 (extension) exists, it take the value \code{""}.

 Note, here base means a different thing from what is meant in the
 function \code{\link[base]{basename}}().
}

\usage{
\method{baseAndExtension}{default}(filename, ...)
}

\arguments{
  \item{filename}{A filename.}
  \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{vector}} of two \code{\link[base]{character}} strings; the base and the extension.
}

\author{Henrik Bengtsson}

\examples{
  baseAndExtension("/usr/local/bin/foo.sh")  # "/usr/local/bin/foo" "sh"
  baseAndExtension("/usr/local/bin/foo...")  # "/usr/local/bin/foo.." ""
  baseAndExtension("/usr/local/bin/")        # "/usr/local/bin/" ""
}

\seealso{
  \code{\link[base]{basename}}() (includes \code{dirname()}).
}



\keyword{file}
\keyword{IO}
