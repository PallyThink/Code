#bai1
# N=100 M=25 n=15
x=0:15
m=25
n=100-m
k=15
dhyper(x,m,n,k)
barplot(dhyper(x,m,n,k))
#bai2
x=5:12
sum(dhyper(x,m,n,k))
phyper(12,m,n,k)-phyper(4,m,n,k)
#bai3
#a
curve(dexp(x,0.6),0,10)
#b
curve(dexp(x,0.3),0,10,add = T)
#c
integrate(function(x)0.6*exp(-0.6*x),lower = 0,upper = Inf)
integrate(function(x)0.3*exp(-0.3*x),lower = 0,upper = Inf)
#bai4
dpois(0:8,1)
plot(stepfun(0:8,c(0,dpois(0:8,1))))
#bai5
x=0:10
curve(dchisq(x,3))
#bai6
par(mfrow=c(1,2))
x=0:50
plot(x,dbinom(x,50,0.08),type='h')
plot(x,dpois(x,4),type='h',ylim=c(0,0.25),col='blue')
#bai7
x=0:50
plot(x,dbinom(x,50,0.4),type='h')
curve(dnorm(x,20,sqrt(12)),from=0,to=50,col='blue',add=TRUE)


