a=c(36,42,48,54,60,66,72)
b=c(15,12,25,18,10,10,10)
vec.X=rep(a,b)
vec.X
x.bar=mean(vec.X)
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
n=length(vec.X)
ktc.tb(x.bar,n=n,alpha = 0.04)
ktc.tb(x.bar = 256.67,alpha=0.04,n=150,sigma = 23.93)
#b
datchuan=vec.X[vec.X>=60]
n=length(datchuan)
x.bar=mean(datchuan)
ktc.tb(x.bar = x.bar,n=n,alpha=0.05)
#bai2
a=seq(95,155,by=10)
b=seq(105,165,by=10)
x=(a+b)/2
n=c(10,10,15,30,10,10,15)
x.vec=rep(x,n)

