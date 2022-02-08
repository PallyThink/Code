rm(list=ls(all=T))
x <- c( rep (1 , 3) , rep (2 , 4) , rep (3 ,5) , rep (4 ,4) ,rep (5 ,3))
a = mean ( x )
b = length (x )
c = median (x )
# --------------------Bai 1----------------------
n <- 35
x <- rnorm(n,10,5)

x.bar = mean(x)
x.sd = sd(x)
epsilon = qnorm(0.975)*x.sd/sqrt(n)
mu.lower = x.bar - epsilon;mu.upper=x.bar + epsilon
cat('Khoang tin cay',100*(1-alpha),'% cho ky vong mu la:\n')
cat('[',mu.lower,';',mu.upper,']\n')

# --------------------Bai 2----------------------
path <- '/Users/mac/Google Drive/KHTN/Thuc Hanh Xac Suat Thong Ke/Class_09_2021/XSTK_Dataset/'
data31_path <- paste(path, 'data31.csv', sep = '/')
data31 <- read.csv(file.choose())
names(data31)
attach(data31)
profit
#Ham ci.mean
ci.mean<- function(x,alpha){
  x.bar= mean(x)
  x.sd= sd(x)
  n= length(x)
  if(n>=30)
    epsilon=qnorm(1-alpha/2)* x.sd/sqrt(n)
  else
    epsilon=qt(1-alpha/2,n-1)* x.sd/sqrt(n)
  mu.lower= x.bar-epsilon
  mu.upper= x.bar+epsilon
  
  cat("KTC", 100*(1-alpha),"% cua cho ky vong mu la : ")
  cat("[",mu.lower,";",mu.upper,"]\n")
}
x=c(1,2,3,4,5,6,7,8)
y=c(13,18,14,23,15,16,17,4)
bb=rep(x,y)
bbb=bb[bb>5];bbb
sd=sd(bb)
zz=qnorm(0.995);zz
ci.mean(bbb,0.05)
# Xuat ra KTC
ktc1 <- ci.mean(profit,0.05)
ktc2 <- ci.mean(profit,0.01)
detach(data31)

# --------------------Bai 3----------------------
path <- '/Users/mac/Google Drive/KHTN/Thuc Hanh Xac Suat Thong Ke/Class_09_2021/XSTK_Dataset/'
data32_path <- paste(path, 'data32.csv', sep = '/')
data32 <- read.csv(file.choose())
names(data32)
attach(data32)
data32

#Tim KTC cho thoi gian hoc nhom trung binh cua sinh vien truong KHTN
ktc.khtn <- ci.mean(KHTN, 0.05)

#Ham ci.prop
ci.prop <- function(f, n, alpha){
  p.hat = f/n 			#Ty le mau
  z = qnorm(1-alpha/2) 		#Phan vi muc 1 - alpha/2 cua Z~N(0,1)
  ep = z*sqrt(p.hat*(1-p.hat)/n)	#Sai so
  cat('Khoang tin cay',100*(1-alpha),'% cho ty le p la:\n')
  cat('[',p.hat - ep,',',p.hat + ep,']\n')
  rval <- list(p.lower = p.hat - ep, p.upper = p.hat + ep)
  return(rval)
}

#Tim KTC cho ty le sinh vien co thoi gian tu hoc tren 5 gio moi ngay
#Co mau
n <- length(KHTN)
n
ci.mean(n,0.02)
#So sv co thoi gian tu hoc tren 5 gio moi ngay
f <- length(KHTN[KHTN > 5])
f
#Khoang tin cay
ktc.prop = ci.prop(f,n,0.1)
ktc.prop = ci.prop(f,n,0.05)
ktc.prop = ci.prop(f,n,0.01)
detach(data32)

# --------------------Bai 4----------------------
#Chuyen bang tan so ve dang vecto
a = seq(1.2,2.0,by=0.2)
b = seq(1.4,2.2,by=0.2)
m = (a+b)/2
n = c(6,34,31,42,12)
x = rep(m,n)
#KTC cho chieu cao trung binh
ktc.height = ci.mean(x,0.05)
ktc.height

#KTC cho ty le thanh nien dat suc khoe loai A
f = length(x[x >=1.7])
ktc.typeA = ci.prop(f,sum(n),0.05)

# --------------------Bai 5----------------------
# Luu y: Gia tri do lech chuan sigma, s >= 0
ktc.tb<-function(x.bar, s=-1, sigma=-1, n, alpha){
  if(sigma > -1){
    cd<-qnorm(alpha/2,x.bar,sigma/sqrt(n));
    ct<-qnorm(1-alpha/2,x.bar,sigma/sqrt(n));
  }else{
    if(n>30){
      cd<-qnorm(alpha/2,x.bar,s/sqrt(n));
      ct<-qnorm(1-alpha/2,x.bar,s/sqrt(n));
    }else{
      eps<-s/sqrt(n)*qt(1-alpha/2,n-1)
      cd<-x.bar-eps;
      ct<-x.bar+eps;
    }
  }
  cat('Khoang tin cay',100*(1-alpha),'% cho ky vong mu la:\n')
  round(c(lower=cd, upper=ct),2);
}
#Kiem tra
n <- 35
x <- rnorm(n,10,5)
alpha<-0.05;
x.bar <- mean(x); s <- sd(x);
ktc.tb(x.bar, s, n=n, alpha=alpha)

# --------------------Bai 6----------------------
ktc.tb.mau<-function(x,alpha,sigma=-1){
  n<-length(x);
  x.bar<-mean(x); s<-sd(x);
  ktc.tb(x.bar=x.bar, s=s, sigma=sigma, n=n, alpha=alpha);
}

#Kiem tra
x<-rnorm(35); alpha<-0.05;
ktc.tb.mau(x=x, alpha=alpha)

# --------------------Bai 7----------------------
x<-c(12.00, 12.05, 12.10, 12.15, 12.20, 12.25, 12.30, 12.35, 12.40);
n<-c(2,3,7,9,10,8,6,5,3);
ktc.tb.mau(x=rep(x,n), alpha=0.05)


