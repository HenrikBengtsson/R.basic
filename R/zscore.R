#########################################################################/**
# @RdocDefault zscore
#
# @title "Gets the Z scores (standardized residuals)"
#
# \description{
#   Z-score is a popular term of what is more formally known as
#   \emph{standardized residuals}. To calculate the standardized
#   residuals of a data set, the average value and the standard
#   deviation of the data value have to be estimated. This can be
#   done in either a robust way or a non-robust way.
#   Normally the average and the standard deviation are estimated
#   using the \code{mean} and \code{sd} functions. These functions
#   are sensitive to outliers and a more robust estimates can be
#   obtained by the \code{median} and \code{mad} (square root)
#   functions.
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{The @vector of data points.}
#   \item{robust}{If @TRUE, the estimation of the average and
#     standard deviation are robust, otherwise not.}
#   \item{...}{Any other arguments \code{mean}, \code{median},
#     \code{sd} and \code{mad} takes, e.g. \code{na.rm=TRUE}.}
# }
#
# @author
#
# \examples{
#   x <- rnorm(10000, 0.4, 0.8)
#   z <- zscore(x, na.rm=TRUE)
#   print(z)
#   z <- zscore(x, robust=TRUE, na.rm=TRUE)
#   print(z)
# }
#
# \seealso{
#   See also @see "base::mean", @see "stats::median", @see "stats::sd",
#   and @see "stats::mad".
#   A similar function to \code{zscore} is \code{rstandard()}
#   (see @see "stats::influence.measures")
# }
#
# @keyword "univar"
#*/#########################################################################t
setMethodS3("zscore", "default", function(x, robust=FALSE, ...) {
  if (length(x) < 2)
    return(x);
  
  if (robust == TRUE) {
    xavg <- median(x, ...);
    xdev <- mad(x, center=xavg, ...);
  } else {
    xavg <- mean(x, ...);
    xdev <- sd(x, ...);
  }
  (x-xavg)/xdev;
})


############################################################################
# HISTORY:
# 2012-05-04
# o BUG FIX: zscore(..., robust=TRUE) estimated the standard deviation
#   incorrectly as the square root of MAD, when it should have been MAD.
#   This error has probably been there since the very early days.
#   Thanks Taku Tokuyasu at UCSF for spotting this.
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2005-02-20
# o Now using setMethodS3().
# 2002-01-17
# * Created.
############################################################################



