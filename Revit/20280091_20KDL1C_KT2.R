#cau1
mu=0
sigma=1
X=function() rnorm(1,mu,sigma)
vecX=function(n) replicate(n,X())
n=10000
vecX(n)
hist(vecX(n)^2,freq=0,breaks=40)
curve(dchisq(x,1),col="blue",lty=1,lwd=2,add = TRUE)

#cau2
X=function(m) rchisq(1,m)
vecX=function(n,m) replicate(n,X(m))
Y=function(m) rchisq(1,m)
vecY=function(n,m) replicate(n,Y(m))
Z=function(m,n)
{
  (X(m)/m)/(Y(n)/n)
}
vecZ=function(n,a,b) replicate(n,Z(a,b))
m=10
n=30
X(m)
Y(n)
Z(m,n)
hist(vecZ(1000,10,30),freq = 0,breaks = 40 , main="Bieu do mau Z")
curve(df(x,m,n),col="blue",add=TRUE)