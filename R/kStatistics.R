#
# \url{http://mathworld.wolfram.com/k-Statistic.html}
#
setMethodS3("kStatistics", "default", function(x, k, na.rm=TRUE, ...) {
  if (na.rm == TRUE)
    x <- na.omit(x);
  n <- length(x);

  # sums of the r:th powers of the data points 
  S <- sapply(1:k, FUN=function(r) sum(x^r));
 
  t <- sapply(1:k, FUN=function(r) (n+1-r));

  if (k == 1) {
    k <- S[1];
  } else if (k == 2) {
    k <- n*S[2] - S[1]^2;
  } else if (k == 3) {
    k <- 2*S[1]^3 - 3*n*S[1]*S[2] + n^2*S[3];
  } else if (k == 4) {
    k <- -6*S[1]^4 + 12*S[1]^2*S[2] - 3*n*(n-1)*S[2]^2 
         - 4*n*(n+1)*S[1]*S[3] + n^2*(n+1)*S[4];
  } 

  k/prod(t);
})

setMethodS3("skewness", "default", function(x, na.rm=TRUE, ...) {
  kStatistics(x, k=3, na.rm=na.rm);
})

setMethodS3("kurtosis", "default", function(x, na.rm=TRUE, ...) {
  kStatistics(x, k=4, na.rm=na.rm, ...);
})
