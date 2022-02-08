mu=2
sigma=2
Y=function() rnorm(1,mu,sigma)
Y()
vec1Y=function(n) replicate(n,Y())
n=4
vec1Y(n)
#phan phoi trung binh mau ngau nhien kich thuoc n
MeanY=function() mean(vec1Y(n))
MeanY()
SampleMeanY=function(n) replicate(m,MeanY())
m=10000
hist(SampleMeanY(m),freq = 0,breaks = 40)
curve(dnorm(x,mu,sigma/sqrt(n)),col="blue",lty=1,lwd=2,add = TRUE)
curve(dnorm(x,2,2),from = -7,to=12)
n=10
z=function()
{
  x=rnorm(n,mu,sigma)
  (n-1)*var(x)/sigma^2
}
z()
vecZ=function(m) replicate(m,z())
hist(vecZ(1000),freq = 0,breaks = 40)
curve(dchisq(x,df=n-1),col="blue",lty=1,lwd=2,add = T)

size =10
prob=0.3
mauX=function(n)
{
 x=rbinom(n,size,prob)
 sqrt(n)*(mean(x)-3)/sqrt(2.1)
}
y=function(m) replicate(m,mauX(n))
n=100
m=1000
hist(y(m),freq = 0,breaks = 40)
curve(dnorm(x),col="blue",lty=1,lwd=2,add=TRUE)
n=10000
hist(y(m),freq=0,breaks = 40)
curve(dnorm(x),col="blue",lty=1,lwd=2,add=TRUE)
n=100000
hist(y(m),freq=0,breaks = 40)
curve(dnorm(x),col="blue",lty=1,lwd=2,add=TRUE)

prob=0.3
Y<-function(n) {
  x<-rbinom(n,1,prob)
  (mean(x)-prob)*sqrt(n)/sqrt(prob*(1-prob))
}
vecY<-function(m) replicate(m,Y(n))
n=100
m=1000
hist(vecY(m),freq=0,breaks=40)
curve(dnorm(x),col="blue",lty=1,lwd=2,add=TRUE)

Z=function(n)
{
  x=rbinom(n,1,prob)
  (mean(x)-prob)*sqrt(n)/sqrt(mean(x)*(1-mean(x)))
}
vecz=function(m) replicate(m,Z(n))
hist(vecz(m),freq=0,breaks = 40)
curve(dnorm(x),col="blue",lty=1,lwd=2,add=TRUE)