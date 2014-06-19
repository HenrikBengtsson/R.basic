# Define f(x)
f <- expression(0.1*x^4 + 1*x^3 + 2*x^2 + x + 10*sin(2*x))

# Simulate data from this function in the range [a,b]
a <- -2; b <- 5
x <- seq(a,b, length=2000)
y <- eval(f)

# Add some noise to the data
y <- y + rnorm(length(y), 0, 10)

# Plot the function and its second derivative
plot(x,y, type="l", lwd=4)

# Fit a cubic smoothing spline and plot it
g <- smooth.spline(x,y, df=16)
lines(g, col="yellow", lwd=2, lty=2)

# Calculating the log likelihood of the fitted spline
l <- logLikelihood(g)

cat("log likelihood           =", l$l, "\n")
cat("weighted res. sum of sq. =", l$wrss, "\n")
cat("smoothing parameter      =", l$lambda, "\n")
cat("roughness score          =", l$roughness, "\n")
cat("penalty term             =", l$lambda*l$roughness, "\n")
cat("n                        =", length(g$x), "\n")


# Simulate non-unique x's.
idx <- sample(2:length(x), size=length(x)/8, replace=TRUE)
#x[idx] <- x[idx-1];
x[2] <- x[1]

# Fit a cubic smoothing spline and plot it
g <- smooth.spline(x,y, df=16)
lines(g, col="yellow", lwd=2, lty=2)

# Calculating the log likelihood of the fitted spline
l <- logLikelihood(g)

cat("log likelihood           =", l$l, "\n")
cat("weighted res. sum of sq. =", l$wrss, "\n")
cat("smoothing parameter      =", l$lambda, "\n")
cat("roughness score          =", l$roughness, "\n")
cat("penalty term             =", l$lambda*l$roughness, "\n")
cat("n                        =", length(g$x), "\n")

