nearest <- function(xyz) { 
  # Takes a list with columns x, y, z and returns the (straightline) 
  # nearest neighbour distances between the points in its rows.
  # Inefficient, but adequate for checking pointsOnSphere().

  res <- NA
  for (i in 1:length(xyz$x)) {
    res[i] <- sqrt(min((xyz$x[-i]-xyz$x[i])^2 + (xyz$y[-i]-xyz$y[i])^2 +
          (xyz$z[-i]-xyz$z[i])^2))
  }
  res
}

xyz <- pointsOnSphere(1000)
nxyz <- nearest(xyz)

layout(matrix(1:4, nrow=2, byrow=TRUE))

plot3d(xyz$x, xyz$y, xyz$z, main="3D view")
plot(nxyz, ylab="nearest neighbour distances",xlab="theta")
plot(xyz$x+sign(xyz$z),xyz$y, main="plan view")
plot(xyz$x+sign(xyz$y),xyz$z, main="side.view")
#hist(nxyz, main="nearest neighbour distances")
print(length(which(nxyz<0.06)))
