plot(0:20,0:20)

# Simple grid
drawGrid(2:8,2:8)

# Advanced grid
x <- seq(1,19,by=1/2)
y <- matrix(10:15, nrow=6, ncol=length(x))
y <- t(t(y) + sin(0.3*(1:ncol(y))))
drawGrid(x,y, fcn=function(...) { points(...); lines(...); }, col="red")

