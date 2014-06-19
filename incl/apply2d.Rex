squareMask <- function(side=ceiling(sqrt(length(x))), ...) {
  x <- matrix(1, nrow=side, ncol=side, ...)
  x / sum(x)
} # squareMask()

translateMask <- function(dx=0, dy=0) {
  x <- matrix(0, nrow=2*abs(dy)+1, ncol=2*abs(dx)+1)
  x[abs(dy)+1-dy, abs(dx)+1-dx] <- 1
  x
} # translateMask()

neighborMask <- function(neighbors=4) {
  if (neighbors == 4) {
    x <- matrix(0, ncol=3, nrow=3)
    x[2,c(1,3)] <- x[c(1,3),2] <- 1
  } else if (neighbors == 8) {
    x <- matrix(1, ncol=3, nrow=3)
    x[2,2] <- 0
  } else {
    throw("Unknown neighbor configuration.")
  }
  x / sum(x)
} # neighborMask()

coneMask <- function(radius=1) {
  x <- matrix(radius+1, nrow=2*radius+1, ncol=2*radius+1)
  for (r in 0:radius) {
    rs <- c(-r,r)
    for (c in 0:radius) {
      d <- radius**2 - sqrt((r**2 + c**2))
      cs <- c(-c,c)
      x[radius+1+rs,radius+1+cs] <- d
    }
  }
  x / sum(x)
} # coneMask()

x <- y <- seq(-2*pi, 2*pi, len=100)
r <- sqrt(outer(x**2, y**2, "+"))
z <- cos(r**2) * exp(-r/6)

#load("RadialPattern.matrix")
zlim <- range(z)

layout(matrix(1:9, ncol=3, byrow=TRUE))
opar <- par(mar=c(1,2,2,2)+0.1)

# Create the color map
colorMap <- gray((0:32)/32)

image270(z, col=colorMap, zlim=zlim, axes=FALSE, main="original"); box()

n4 <- neighborMask(4)
z2 <- apply2d(z, n4, FUN=sum)
image270(z2, col=colorMap, zlim=zlim, axes=FALSE, main="4 neighbors"); box()

n8 <- neighborMask(8)
z2 <- apply2d(z, n8, FUN=sum)
image270(z2, col=colorMap, zlim=zlim, axes=FALSE, main="8 neighbors"); box()

sq5 <- squareMask(side=5)
z2 <- apply2d(z, sq5, FUN=sum)
image270(z2, col=colorMap, zlim=zlim, axes=FALSE, main="5x5"); box()

t33 <- translateMask(dx=3, dy=3)
z2 <- apply2d(z, t33, FUN=sum)
image270(z2, col=colorMap, zlim=zlim, axes=FALSE, main="translate (+3,+3)"); box()

c2 <- coneMask(radius=2)
z2 <- apply2d(z, c2, FUN=sum)
image270(z2, col=colorMap, zlim=zlim, axes=FALSE, main="cone r=2"); box()

c5 <- coneMask(radius=5)
z2 <- apply2d(z, c5, FUN=sum)
image270(z2, col=colorMap, zlim=zlim, axes=FALSE, main="cone r=5"); box()


# To handle NA's run apply2d() twice, once on the data and once on 
# an indicator matrix with the same mask and function. Example:
n <- length(z)
zn <- z
zn[sample(1:n, 0.10*n)] <- NA   # Add 10 percent NA's

image270(zn, col=colorMap, zlim=zlim, axes=FALSE, main="with NA's"); box()

# Create an indicator matrix (for NA's)
i <- 1 * !is.na(zn)

zi <- apply2d(i, sq5, FUN=sum)
zd <- apply2d(zn, sq5, FUN=sum, na.rm=TRUE)
# An alternatively for the mean function is that one could
# set all NA's to 0's in zn, e.g. 
#  zn[is.na(zn)] <- 0
#  zd <- apply2d(zn, sq5, FUN=sum)
z2 <- zd/zi

image270(z2, col=colorMap, zlim=zlim, axes=FALSE, main="5x5 w/ NA's"); box()


par(opar)

