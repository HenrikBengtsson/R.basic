# Simulate three groups of (x,y,z) data
n <- 3*1000
gr <- list()
gr[[1]] <- matrix(rnorm(n, mean=c(0,0,1), sd=c(1,1,1)), ncol=3, byrow=TRUE)
gr[[2]] <- matrix(rnorm(n, mean=c(5,4,2), sd=c(1,2,1)), ncol=3, byrow=TRUE)
gr[[3]] <- matrix(rnorm(n, mean=c(2,2,4), sd=c(0.2,0.5,0.8)), ncol=3, byrow=TRUE)

# Calculate the overall limits of the axis
xlim <- ylim <- zlim <- NA
for (k in 1:3) {
  gr[[k]] <- as.data.frame(gr[[k]])
  colnames(gr[[k]]) <- c("x", "y", "z")
  xlim <- range(xlim, gr[[k]][,1], na.rm=TRUE)
  ylim <- range(ylim, gr[[k]][,2], na.rm=TRUE)
  zlim <- range(zlim, gr[[k]][,3], na.rm=TRUE)
}

# First plot one group using plot3d()...
plot3d(gr[[1]], phi=50, pch=176, col="red", xlim=xlim, ylim=ylim, zlim=zlim, 
       xlab="x", ylab="y", zlab="z")

# ...and then use points3d() to plot the other groups
for (k in 2:3)
  points3d(gr[[k]], pch=176, col=c("blue", "green")[k-1])
