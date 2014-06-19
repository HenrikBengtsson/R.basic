#########################################################################/**
# @RdocDefault top
#
# @title "Gets the largest data points"
#
# \description{
#  Gets the \code{n} largest data points in a vector, where \code{n} can
#  either be a fraction or a number. If \code{n} is fraction, at least
#  \code{round(n*length(x)+0.5)} values will be found.
# }
#
# @synopsis
#
# \arguments{
#   \item{x}{The @vector of data points}
#   \item{n}{The number or the fraction of data points to be selected. 
#     A value \code{0<n<1} indicates a fraction. Default value is 0.05, i.e.
#     the top 5\% percent.}
#   \item{na.rm}{If @TRUE all @NA and @NaN values are omitted.}
#   \item{inf.rm}{If @TRUE all @Inf values are omitted.}
#   \item{...}{Not used.}
# }
#
# @author
#
# \examples{
#   idx <- which(top(1:100, n=0.021))
#   print(idx)    # 98 99 100
# }
#
# \seealso{
#   @see "base::which"
# }
#
# @keyword "datagen"
#*/#########################################################################t
setMethodS3("top", "default", function(x, n=0.05, na.rm=TRUE, inf.rm=FALSE, ...) {
  if (n < 0) stop("Argument 'n' must be zero or greater.");
  len <- length(x);
  if (n > len) stop("Argument 'n' is larger than the number of data points.");
  if (n > 0 & n < 1) n <- n*len+0.5;
  n <- round(n);
  if (n == 0) return(unwhich(FALSE, dim=len));

  if (na.rm)  x[is.na(x)] <- -Inf;
  if (inf.rm) x[is.infinite(x)] <- -Inf;

  order <- order(x);
  idx <- (len-n+1):len;
  unwhich(order[idx], dim=len);
})



############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3() and added '...' to please R CMD check.
# 2001-07-06
# * Wrote the Rdoc comments.
# 2001-07-05
# * Created.
############################################################################
