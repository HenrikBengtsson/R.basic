# \item{a,b}{the intercept and slope, or alternatively location and direction.}
# \details{
#    The first form specifies the line in intercept/slope form,
#    or location and direction.
#     (alternatively 'a' can be specified on its own and is taken to
#     contain the slope and intercept in vector form).
# }
abline <- function(a=NULL, b=NULL, h=NULL, v=NULL, reg=NULL, coef=NULL, untf=FALSE, col=par("col"), lty=par("lty"), lwd=NULL, ...) {
  if (!is.null(reg))
    a <- reg

  if (!is.null(a) && is.list(a)) {
    temp <- as.vector(coefficients(a))
    if (length(temp) == 1) {
      a <- 0
      b <- temp
    } else {
      a <- temp[1]
      b <- temp[2]
    }
  }

  if (!is.null(a) && !is.null(b) && is.vector(a) && is.vector(b) &&
                                    length(a) == 2 && length(b) == 2) {
    # Slope
    b <- b[2]/b[1];
    # Intercept
    a <- a[2] - b*a[1];
  }

  if (!is.null(coef)) {
    a <- coef[1];
    b <- coef[2];
  }

  graphics::abline(a=a, b=b, h=h, v=v, untf=untf, col=col, lty=lty, lwd=lwd, ...)
} # abline()


############################################################################
# HISTORY:
# 2013-05-30
# o CLEANUP: Now R.basic::abline() calls graphics::abline() internally.
############################################################################
