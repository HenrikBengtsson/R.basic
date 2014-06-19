# Simulate some (x,y,z) data
n <- 20000
x <- runif(n, 0, 2*pi)
y <- runif(n, 0, 2*pi)
z <- sin(x) * cos(y)

zeroone <- function(x) {
  x.r <- range(x, na.rm=TRUE)
  (x - x.r[1])/(x.r[2]-x.r[1])
}

h <- zeroone(z)
s <- zeroone(x)
v <- zeroone(y)
col <- hsv(h=h, s=1, v=1)
plot3d(x,y,z, phi=30, theta=30, pch=".", col=col)


omega <- seq(0, 6*pi, length.out=500)
x <- sin(omega); y <- cos(omega)
plot3d(x,y,omega, pch=176, col=hsv(omega/max(omega), 1,1))
