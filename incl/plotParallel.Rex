data(freeny)
plotParallel(freeny, order=2:5, flip=c(FALSE,TRUE,FALSE,FALSE))

data(airquality)
r <- range(airquality$Temp, na.rm=TRUE)
tempidx <- floor(99*(airquality$Temp-r[1])/(r[2]-r[1]))+1
col <- heat.colors(100)[tempidx]
r <- range(airquality$Wind, na.rm=TRUE)
windidx <- (airquality$Wind-r[1])/(r[2]-r[1])
lwd <- 2*windidx+1
plotParallel(airquality, order=1:4, col=col, lwd=lwd, horizontal=FALSE)
