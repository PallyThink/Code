k=0:8
p=function(k) choose(8,k)*0.3^k*0.7^(8-k);
p(k)
sum(p(k))
#b
f = function(x, mu=0, sigma=1){
  1/sqrt(2*pi*sigma^2) * exp(-(x-mu)^2/(2*sigma^2))
}
integrate(function(x) f(x,0,1),lower=-Inf,upper=Inf)
f(0)
#vd2
#a
plot(k, p(k), type = "h", ylab = "P(X = x)")
#b
curve(f(x,0,1),from = -3,to=3,ylab="fX(x)")
#vd3 tinh ham phan phoi xac suat
#a
F = function(k) sum(p(0:k))
F = Vectorize(F)
F(4)
#b
F2=function(a,mu=0,sigma=1){
  integrate(function(x) f(x,mu,sigma), lower = -Inf, upper=a)$value
}
F2=Vectorize(F2)
F2(1.96)
#vd4 ve ham phan phoi xac suat
#a
plot(stepfun(k,c(0,F(k))), ylab="FX(x)",main="")
#b
curve(F2(x),from=-3,to=3,ylab="FX(x)")
#phan vi
K=k[F(k)>=0.25]
K[1]
F(0)
F(1)
#b tinh gia tri x de xac suat >= p
uniroot(function(x) F2(x)-0.975, c(-3,3))$root
F2(1.959992)
#5 mo phong
x=1:7
sample(x)
sample(x)
y=sample(c("red","green","blue","black"))
y
sample(1:10,3)
sample(1:5, size = 9,replace = TRUE)
sample(1:3, size = 2, replace = TRUE, prob = c(25 / 100, 20 / 100,55 / 100))


#1
a=0:10
b=function(a) choose(10,a)*0.5^a*0.5^(10-a)
b(a)
#2
sum(b(a))
#3
plot(a, b(a), type = "h", ylab = "P(X = x)")
#4
sum(b(0:4))
sum(b(0:6))
#5
F = function(k) sum(b(0:k))
F = Vectorize(F)
#6
plot(stepfun(a,c(0,F(a))), ylab="FX(x)",main="")
#7
F(4)
F(6)

setwd("C:/Users/Administrator/Desktop/Math/Xac suat thong ke/TH XSTK/data/Data cho cac bai thuc hanh")
data1=read.csv("giamcan.csv",header = TRUE)
a=data1$Nguoi
b=data1$Truoc
c=data1$Sau
giamcan=data.frame(a,b,c)
giamcan
sn=length(giamcan$a)
sn
q=giamcan$
  #sn_saugiamcan=function(giamcan)
  #{
  #  s=0
  #  for (i in giamcan)
  #  {
  #    if((<65) & (<65))
  #    {
  #      s=s+1
  #    }
  #  }
  #  return(s)
#}
#tyle=sn_saugiamcan(giamcan)/sn
#tyle


giamcan<- read.csv ("giamcan.csv", header=TRUE,)
giamcan
x<-c(dim(giamcan))
x[1]
#so nguoi khao sat x[1]=50
conut=0
for(i in 1:nrow(giamcan))
{
  if((giamcan$Truoc[i]<65)&(giamcan$Sau[i]<65)) count=count+1;
}
count
#tinh ty le:
tyle=count/50














