#bai1
X=function() rnorm(1,10,5)
vecX=function(n) replicate(n,X())
alpha=0.05
x.bar=mean(vecX(35))
x.bar
x.sd=sd(vecX(35))
x.sd
epsilon=qnorm(1-alpha/2)*x.sd/sqrt(35)
mu.lower=x.bar - epsilon
mu.upper=x.bar+epsilon
cat('khoang tin cay = [',mu.lower,';',mu.upper,']')
#bai2
df=read.csv(file.choose())
ci.mean=function(x,alpha)
{
  x.bar=mean(x)
  s.sd=sd(x)
  n=length(x)
  z=ifelse(n>30,qnorm(1-alpha/2),qt(1-alpha/2,n-1))
  epsl=z*s.sd/sqrt(n)
  cat('[',x.bar-epsl,';',x.bar+epsl,']')
}
ci.mean(df$profit,0.05)
ci.mean(df$profit,0.01)
#bai3
# data32.csv
df2=read.csv(file.choose())
#a
df2
a=ci.mean(df2$KHTN,0.05)
#b
names(df2);attach(df2)

ci.prop=function(f,n,alpha)
{
  p=f/n
  q=1-p
  z=qnorm(1-alpha/2)
  ep=z*sqrt(p*q/n)
  cat('[',p-ep,';',p+ep,']')
}
n=length(df2$KHTN)
f=length(df2$KHTN[df2$KHTN>5])
ci.prop(f,n,0.05)  
ci.prop(f,n,0.1)
ci.prop(f,n,0.01)
#bai 4
#a
a=seq(1.2,2.0,by=0.2)
b=seq(1.4,2.2,by=0.2)
x=(a+b)/2
x
n=c(6,34,31,42,12)
x.vec=rep(x,n)
ci.mean(x.vec,0.05)
#b
f=length(x.vec[x.vec>=1.7])
ci.prop(f,sum(n),0.05)
#bai 5
ktc.tb=function(x.bar,s=-1,n,alpha,sigma=-1)
{
  if(sigma>-1)
  {
    eps=qnorm(1-alpha/2)
    cd=x.bar-eps*sigma/sqrt(n)
    ct=x.bar + eps*sigma/sqrt(n)
  }
  else
  {
    if(n>30)
    {
      eps=qnorm(1-alpha/2)
      cd=x.bar-eps*s/sqrt(n)
      ct=x.bar + eps*s/sqrt(n)
    }
    else
    {
      eps=s/sqrt(n)*qt(1-alpha/2,n-1)
      cd=x.bar-eps
      ct=x.bar + eps
    }
  }
  cat('[',cd,';',ct,']')
}
n=35
x=rnorm(n,10,5)
alpha=0.05
x.bar=mean(x)
s=sd(x)
ktc.tb(x.bar,s,n=n,alpha=alpha)
#bai6
ktc.tb.mau=function(x,alpha,sigma=-1)
{
  n=length(x)
  x.bar=mean(x)
  s=sd(x)
  ktc.tb(x.bar=x.bar,s=s,n=n,alpha = alpha,sigma=sigma)
}
x=rnorm(35)
ktc.tb.mau(x,0.05)
#bai7
x=seq(12,12.4,by=0.05)
n=c(2,3,7,9,10,8,6,5,3)
ktc.tb.mau(x=rep(x,n),0.05)
