#bai1
#a sigma1=0.002 sigma2=0.0025 alpha=0.05
#
data=read.csv(file.choose())
x=data$machine1
y=data$machine2
mu0=0
sigma1=0.002
sigma2=0.0025
n1=length(x)
n2=length(y)

x.mu=mean(x)
y.mu=mean(y)
z=((x.mu-y.mu)-mu0)/sqrt(sigma1^2/n1+sigma2^2/n2)
p.value=2*(1-pnorm(abs(z)))
p.value
kl=ifelse(p.value<alpha,"bac bo H0","chua du co so de bac bo H0")
result=list(kl,p.value)
# chuan doan la dung va the tich sua dua vao cac hop la nhu nhau
a=t.test(x,y,alternative = "two.sided",val.equal=FALSE,conf.level = 0.95)
a
#b
result
#c

#d
test.leq.oneside=function(x,y,mu0,sigma1,sigma2,alpha)
{
  n1=length(x) #tinh co mau cua hai mau
  n2=length(y)
  #tinh trung binh mau cua hai mau
  x.mu=mean(x)
  y.mu=mean(y)
  #tinh gia tri cua tieu chuan kiem dinh
  z=((x.mu-y.mu)-mu0)/sqrt(sigma1^2/n1+sigma2^2/n2)
  #tinh p.value
  p.value=pnorm(z)
  #ket luan
  kl=ifelse(p.value<alpha,"bac bo H0","chua du co so de bac bo H0")
  result=list(kl,p.value)
}
result=test.leq.oneside(x,y,0,0.002,0.0025,0.05)
result
#e
test.geq.oneside=function(x,y,mu9,sigma1,sigma2,alpha)
{
  n1=length(x) #tinh co mau cua hai mau
  n2=length(y)
  #tinh trung binh mau cua hai mau
  x.mu=mean(x)
  y.mu=mean(y)
  #tinh gia tri cua tieu chuan kiem dinh
  z=((x.mu-y.mu)-mu0)/sqrt(sigma1^2/n1+sigma2^2/n2)
  p.val=1-pnorm(z)
  kl=ifelse(p.val<alpha,'bac bo H0','chua du co so de bac bo H0')
  result=list(kl,p.val)
}
#bai2
#a
data2=read.csv(file.choose())
x=data2$extru.ma.1
y=data2$extru.ma.2
y=y[is.na(y)!=TRUE]
a=t.test(x,y,alternative = "two.sided",val.equal=TRUE,conf.level = 0.95)
a
ifelse(a$p.value<0.05,'Bac bo H0','chua du co so de bac bo H0')
#cac thanh thep co duong kinh nhu nhau
#b
a$p.value
#c

#d
test.geq.oneside=function(x,y,mu0,alpha)
{
  n1=length(x) #tinh co mau cua hai mau
  n2=length(y)
  x.mu=mean(x)
  x.sd=sd(x)
  y.mu=mean(y)
  y.sd=sd(y)
  sp2=((n-1)*x.sd^2+(m-1)*(y.sd^2))/(n+m-2)
  sp=sqrt(sp2)
  t=((x.mu-y.mu)-mu0)/(sp*sqrt(1/n+1/m))
  p.value=1-pt(t,n+m-2)
  kq=ifelse(p.value<alpha,'bac bo h0','chua du co so de bac bo H0')
  result=list(kq,p.value)  
}
d=test.geq.oneside(x,y,0,0.05)
d
#e
test.leq.oneside=function(x,y,mu0,alpha)
{
  n1=length(x) #tinh co mau cua hai mau
  n2=length(y)
  x.mu=mean(x)
  x.sd=sd(x)
  y.mu=mean(y)
  y.sd=sd(y)
  sp2=((n-1)*x.sd^2+(m-1)*(y.sd^2))/(n+m-2)
  sp=sqrt(sp2)
  t=((x.mu-y.mu)-mu0)/(sp*sqrt(1/n+1/m))
  p.value=pt(t,n+m-2)
  kq=ifelse(p.value<alpha,'bac bo h0','chua du co so de bac bo H0')
  result=list(kq,p.value)
}
e=test.leq.oneside(x,y,0,0.05)
e
#bai3
data3=read.csv(file.choose())

x=data3$Thoigian[data3$Hang=="G"]
y=data3$Thoigian[data3$Hang=="M"]
mu0=0
sigma1=1.5
sigma2=1
n1=length(x)
n2=length(y)
n1;n2
x.mu=mean(x)
y.mu=mean(y)
z=((x.mu-y.mu)-mu0)/sqrt(sigma1^2/n1+sigma2^2/n2)
p.value=2*(1-pnorm(abs(z)))
p.value
kl=ifelse(p.value<alpha,"bac bo H0","chua du co so de bac bo H0")
result=list(kl,p.value)
result
#thoi gian khoi dong cua hang G khong phu hop
#bai4
#H0:muX>=muy <=> muX-muY>=0
#H1:muX-muY<0
data4<-read.table(file.choose())
data4$V2
X=data4$V2[2:length(data4$V2)]
X
Y=X=data4$V3[2:length(data4$V3)]
Y
D=X-Y
D.mean=mean(D)
D.sd=sd(D)
n=length(D)
t0=D.mean*sqrt(n)/D.sd
t0
p.value=pt(t0,n-1)
p.value
ifelse(p.value<0.05,"Bac bo H0","Chap nhan H0")
#Chap nhan H0: Che do an kieng co hieu qua
#bai5
data5=read.csv(file.choose()) #giamcan.csv
s1=sd(data5$Truoc)
s2=sd(data5$Sau)

t.test(data5$Truoc-1.5,data5$Sau,conf.level = 0.95,alt="greater",)

#b
p.val
