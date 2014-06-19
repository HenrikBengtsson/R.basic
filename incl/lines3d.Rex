omega <- seq(0, 6*pi, length.out=1000)
x <- sin(omega); y <- cos(omega)
col <- hsv(omega/max(omega), 1,1)
plot3d(x,y,omega, pch=176, col=col)
o1 <- seq(1, length(x)-10, by=10);
o2 <- o1 + 160;
o3 <- o1 + NA;
o <- as.vector(matrix(c(o1,o2,o3), nrow=3, byrow=TRUE))
lines3d(x[o],y[o],omega[o], col=col[o], lty=1:2)

