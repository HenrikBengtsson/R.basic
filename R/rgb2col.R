rgb2col <- function(rgb) {
  hexDigit <- c(0:9, "A", "B", "C", "D", "E", "F")
  rgb <- rgb %% 256
  hi <- rgb %/% 16
#  lo <- rgb %% 16
  lo <- rgb - 16*hi  # Faster?
  x <- t(matrix(c(hi,lo), ncol=2)) + 1
  s <- matrix(hexDigit[x], nrow=6)
  s <- apply(s, MARGIN=2, FUN=paste, collapse="")
  paste("#", s, sep="")
}

############################################################################
# HISTORY:
# 2002-05-31
# * Optimized it for speed.
# * Created. Still have to write the Rdoc's.
############################################################################
