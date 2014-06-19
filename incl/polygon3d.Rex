n <- 32 + 1
theta <- seq(0, 2*pi, length.out=n)
#theta <- theta[-(n+1)]

x1 <- sin(theta)
y1 <- cos(theta)
z1 <- rep(1, n)

x0 <- rep(0, n)
y0 <- rep(0, n)
z0 <- rep(0, n)

plot3d(x1,y1,z1, type="n", zlim=c(0,1.3), xlab="x", ylab="y", zlab="z", phi=30)

#x1 <- c(x1, x1[1])
#y1 <- c(y1, y1[1])
#z1 <- c(z1, z1[1])

n <- n - 1;
for (k in c(1:n,1:n)[22+1:n-1]) {
  xs <- c(x0[k], x1[k], x1[k+1])
  ys <- c(y0[k], y1[k], y1[k+1])
  zs <- c(z0[k], z1[k], z1[k+1])
  col <- hsv(h=(k-1)/n, s=1, v=1)
  polygon3d(xs,ys,zs, col=col)
}

text3d(x1,y1,z1+0.1, 1:n)
