#bai1
df=read.csv(file.choose())
attach(df)
names(df)
#a
hist(df$profit)
#b
b=df$profit[df$profit>65]
b
b.mean=mean(b)
b.sd=sd(b)
n=length(b)
alpha=0.01
epsilon=qnorm(1-alpha/2)*b.sd/sqrt(n)
cat(b.mean-epsilon,';',b.mean+epsilon)
#c
t.test(df$profit,alternative = "less",mu=60,conf.level = 0.99)
#Tinh trung binh mau, do lech chuan mau 
x.bar <- mean(b)
s <- sd(b)
n <- length(b)
alpha <- 0.01
epsilon = qnorm(1-alpha/2)*s/sqrt(n)
mu.lower = x.bar - epsilon
mu.upper = x.bar + epsilon
cat('Khoang tin cay',100*(1-alpha),'% cho ky vong mu la:\n')
cat('[',mu.lower,';',mu.upper,']\n')

# Cau c:
# Dat gia thuyet:
# H0: "mu <= 60" Không mang lai hieu qua
# H1: "mu > 60" Mang lai hieu qua
# t.test(x, alternative = "greater", mu = 60, conf.level = 0.99)
t.test(b, alternative = "less", mu = 60, conf.level = 0.99)
# Nhan xet: Ket qua cho ta p-value = 0.9699 < 1% = alpha
# do do chua du co so de bac bo gia thuyet H0: "mu <= 60" voi muc y nghia 1%.

# Ket luan: Voi muc y nghia 1%, phuong thuc ban hang moi 
# khong mang lai hieu qua bang phuong thuc ban hang truoc do.

#bai2
#a
xi=c(5,6,7,8,9,10)
ni=c(5,10,15,20,12,8)
x=rep(xi,ni)
stem(x)
#b
test.geq.oneside= function(x,mu0,alpha)
{
  n=length(x)
  x.bar=mean(x)
  x.sd=sd(x)
  z_0=(x.bar-mu0)*sqrt(n)/x.sd
  p.val=1-pnorm(z_0)
  kl=ifelse(p.val<alpha,"bac bo H0",'chua du co so de bac bo H0')
  result=list(kl,p.val)  
}
b=test.geq.oneside(x,8,0.05)
b
#c
test.leq.oneside=function(x,mu0,alpha)
{
  n=length(x)
  x.bar=mean(x)
  x.sd=sd(x)
  t=(x.bar-mu0)*sqrt(n)/x.sd
  p.val=pt(t,n)
  kl=ifelse(p.val<alpha,"bac bo H0","chu du co so de bac bo H0")
  result=list(kl,p.val)
}
c=test.leq.oneside(x,8,0.05)
t.test(df$profit,alternative = "less",mu=60,conf.level = 0.99)
#bai3
setwd("C:/Users/Administrator/Desktop/Math/Xac suat thong ke/TH XSTK/data/Data cho cac bai thuc hanh")
read.table('teen-birth-rate-2002.txt')
#bai4
df=load('data04.rda')
n=80
f=sum(survey)
p.hat=f/n
p0=0.6
alpha=0.05
z=(p.hat-p0)*sqrt(n)/sqrt(p0*(1-p0))
p.val=1-pnorm(z)
kl=ifelse(p.val<alpha,'bac bo H0','chua du co so de bac bo H0')
p.val
cat(kl)
binom.test(f,n,p=0.6,alternative = 'greater')
#bai5

#bai6
#a
df=read.csv('times.csv')
f=length(df$KHTN[df$KHTN>5])
n=length(df$KHTN[is.na(df$KHTN)!= TRUE])
a=prop.test(f,n,p=0.5,conf.level = 0.95) #kiem dinh bang ham prop.test
#b
proptest.geq=function(f,n,p0,alpha)
{
  p.hat=f/n #ti le mau
  se=sqrt(p0*(1-p0)/n) #tinh sai so chuan
  z0=(p.hat-p0)/se #tinh gtri cua tieu chuan kiem dinh
  p.val=1-pnorm(z0) # tinh p.value
  
  kl=ifelse(p.val<alpha,'bac bo H0','chua du co so de bac bo H0')
  result=list(kl,p.val)
}
result=proptest.geq(f,n,0.5,0.05)
result
#c
proptes.leq=function(f,n,p0,alpha)
{
  p_hat=f/n
  z0=((f-p0)*sqrt(n)/sqrt(p0*(1-p0)))
  p.val=pnorm(z0)
  
}
a=c(5,6,7,8,9,10)
b=c(7,10,17,20,12,9)
vec.X=rep(a,b)
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
x.bar=mean(vec.X)
ktc.tb(x.bar = x.bar,n=n,alpha=0.01)
#c
ktc.tb(x.bar=8,n=n,alpha=0.05)
c=vec.X[vec.X]