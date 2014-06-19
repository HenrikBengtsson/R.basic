%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  getTerminalInfo.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getTerminalInfo}
\alias{getTerminalInfo.default}
\alias{getTerminalInfo}

\title{Gets information about the terminal in which [R] is running}

\usage{
\method{getTerminalInfo}{default}(rows=NULL, columns=NULL, ...)
}

\description{
  Gets information about the terminal in which [R] is running.
  Currently only the number of rows and columns of the terminal is
  returned.

  This function is used by for instance \code{more()}.
}

\arguments{
  \item{rows}{Default value of number of rows in it failed to
  retrieve the information from the system.}
  \item{columns}{Default value of number of columns in it failed to
  retrieve the information from the system.}
  \item{...}{Not used.}
}

\value{Returns a named \code{\link[base]{list}} of terminal properties.}

\details{
  Internally, the system command 'stty' is tried by \code{system()}.
  If it fails, the default values are returned.
}

\examples{
  rows <- getTerminalInfo()$rows
  cols <- getTerminalInfo()$columns
}

\author{Henrik Bengtsson}


\keyword{programming}
