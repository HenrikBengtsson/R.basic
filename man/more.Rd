%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  more.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{more}
\alias{more.default}
\alias{more}

\title{Prints one or more objects to the standard output}

\description{
 Prints one or more objects or the contents of a file to the standard
 output. If not all of the output fits the console window, a prompt will
 be displayed. Valid entries in this prompt are:
  (1) \code{B} and \code{[ENTER]} for previous and next page, respectively;
  (2) \code{P} and \code{N} for previous and next line, respectively;
  (3) \code{Q} for quit; and (4) \code{H} or \code{?} for help.
 In addition to these for options entering an integer value will print
 the contents starting from the line with the entered number.
}

\usage{
\method{more}{default}(..., file, height=getTerminalInfo(rows = getOption("height"))$rows,
  width=getTerminalInfo(columns = getOption("width"))$columns, line.numbers=FALSE,
  auto.refresh=TRUE)
}

\arguments{
  \item{...}{Comma-separated \code{\link[base]{list}} of things to be displayed.}
  \item{file}{File to be displayed.}
  \item{height}{The height of the console window.}
  \item{width}{The width of the console window. Default value is
     \code{options("width")}. Currently this argument has no effect on
     the result.}
  \item{line.numbers}{If \code{\link[base:logical]{TRUE}}, each line is preceeded with a line
     number, otherwise just the contents is printed.}
  \item{auto.refresh}{If \code{\link[base:logical]{TRUE}}, the display is refreshed after prompting
     the user, otherwise not.}

  Either \code{...} or \code{file} must be specified.
}

\author{Henrik Bengtsson}

\examples{
 \dontrun{
  more(more.default)
  more(system("ls -l", intern=TRUE))
  more(file=paste(.path.package("base"), "/CONTENTS", sep=""))
 }
}

\seealso{
  \code{\link{head}}(), \code{\link{tail}}(), \code{\link[base]{sink}}().
}


\keyword{utilities}
