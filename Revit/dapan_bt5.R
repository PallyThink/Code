# X1, X2 ~ N(0,1)
# Y = (X1)^2 + (X2)^2

m = 0
s = 1
MauY <- function(){
  X1 <- rnorm(1,m,s)
  X2 <- rnorm(1,m,s)
  Y = (X1)^2 + (X2)^2
  return(Y)
}

Y <- function(n){
  replicate(n,MauY())
}

draw <- function(m){
  hist(Y(m),freq=0,breaks=40)
  curve(dchisq(x,2),col="blue",lty=1,lwd=2,add=TRUE)
}

par(mfrow = c(1,3))
draw(100)
draw(1000)
draw(10000)
