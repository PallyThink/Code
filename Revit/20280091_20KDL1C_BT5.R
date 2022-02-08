#bai1
mu = 0
sigma=1
rnorm(1,mu,sigma)
P=function()
{
  a1=rnorm(1,mu,sigma)
  a2=rnorm(1,mu,sigma)
  b=a1^2 + a2^2
}
VecP=function(n) replicate(n,P())
VecP(10)
draw=function(n)
{
  hist(VecP(10000),freq = 0,breaks = 40)
  curve(dchisq(x,2),col="red",lty=1,lwd=3,add = TRUE)
}
par(mfrow=c(1,3))
draw(100)
draw(1000)
draw(10000)

x=rnorm(10,2,2)
(10-1)*var(x)/2^2




