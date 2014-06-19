#########################################################################/**
# @class loess
# @RdocMethod lines
#
# @title "Adds connected line segments to a plot using a fitted loess"
#
# @synopsis
#
# \description{
#   Adds connected line segments of a loess fitted structure to a plot
#   similar to how lines() works for \code{lowess()}.
# }
#
# \arguments{
#   \item{x}{An object of class \code{loess}.}
#   \item{subset}{The subset of the data points in the fit to be used for
#     drawing the line. 
#     If a single @integer or a proportion between 0 and 1 is given, a sample
#     of that number of data points or proportion, respectively, will be
#     drawn (guaranteed to include the left and right most data points) and
#     used as the subset. If @NULL, all data points are included.}
#   \item{...}{Arguments that are accepted by \code{lines}.}
# }
#
# \value{Returns nothing.}
#
# \details{
#   Specifying the subset of data points to be plotted as an integer or as
#   as proportion of the number of data points, e.g. 
#   \code{subset=0.1}, is useful when the fitted data contains a lot of
#   data points. This is especially a concern when vector graphics such as
#   postscript images are generated where each single subline drawn takes
#   time to display and takes up memory.
# }
#
# \examples{
#    library(stats)   # loess()
#    data(cars)
#    # draw a smooth line through a scatter plot
#    plot(cars, main="Stopping Distance versus Speed")
#    lines(lowess(cars), col="blue")
#    fit <- loess(dist~speed, data=cars, family="symmetric")
#    lines(fit, col="red")
# }
#
# @author
#
# \seealso{
#   @see "stats::loess", @see "stats::lowess", @see "graphics::lines".
# }
#
# @keyword "aplot"
# @keyword "loess"
#*/#########################################################################
setMethodS3("lines", "loess", function(x, subset=NULL, ...) {
  # To please R CMD check...
  object <- x;

  if (!inherits(object, "loess"))
    stop(paste("Argument is not of class loess:",  data.class(object)));
  if (!is.null(subset)) {
    if (length(subset) == 1) {
      if (subset < 0)
        stop(paste("Argument 'subset' is out of range:", subset));

      n <- length(object$x);
      if (subset >= 0 && subset <= 1)
        subset <- n * subset;
      
      o.min  <- which.min(object$x);
      o.max  <- which.max(object$x);
      subset <- sample(1:n, subset);
      subset <- unique(c(o.min, subset, o.max));
    }
    x <- object$x[subset];
    y <- object$fitted[subset];
  } else {
    x <- object$x;
    y <- object$fitted;
  } 
  o <- order(x);
  x <- x[o];
  y <- y[o];
  lines(x, y, ...);
  invisible(list(x=x, y=y, subset=subset));
})




############################################################################
# HISTORY:
# 2005-02-20
# o Renamed first argument from 'object' to 'x' to please R CMD check.
# o Now using setMethodS3().
# 2003-02-11
# o Updated error message to contain data.class(x) instead of class(x).
# 2002-11-04
# o Added support for argument subset.
# o Added to R.base and updated some Rdoc comments.
# 2002-04-25
# * Added the Rdoc comments.
# 2002-04-24
# * Created.
############################################################################
