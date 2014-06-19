x <- seq(0,2, length.out=20)
y <- x / 2
z <- sin(x) + cos(y)

layout(matrix(1:4, nrow=2, byrow=TRUE))

plot3d(x,y,z, type="n", theta=-25, phi=30) # empty 3d plot
stem3d(x,y,z, col="blue", pch=1, cex=1.5)

plot3d(x,y,z, type="n", theta=-25, phi=30) # empty 3d plot
stem3d(x,y,z, col="red", pch=1, cex=1.5, plane="xz")

plot3d(x,y,z, type="n", theta=-25, phi=30) # empty 3d plot
stem3d(x,y,z, col="green", pch=1, cex=1.5, plane="yz")

plot3d(x,y,z, type="n", theta=-25, phi=30) # empty 3d plot
stem3d(x,y,z, col="cyan", pch=".", cex=1.5, plane="xy")
stem3d(x,y,z, col="orange", pch=".", cex=1.5, plane="zx")


