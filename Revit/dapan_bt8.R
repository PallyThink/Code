rm(list=ls(all=T))

# --------------------Bai 1----------------------
path <- '/Users/mac/Google Drive/KHTN/Thuc Hanh Xac Suat Thong Ke/Class_09_2021/XSTK_Dataset/'
profit_path <- paste(path, 'profit.csv', sep = '/')
data <- read.csv(profit_path)
x <- data$profit
# Cau a
hist(x)

# Cau b: 
hi.pro <- x[x > 65]
#Tinh trung binh mau, do lech chuan mau
x.bar <- mean(hi.pro)
s <- sd(hi.pro)
n <- length(hi.pro)
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
t.test(x, alternative = "less", mu = 60, conf.level = 0.99)
# Nhan xet: Ket qua cho ta p-value = 0.9699 < 1% = alpha
# do do chua du co so de bac bo gia thuyet H0: "mu <= 60" voi muc y nghia 1%.

# Ket luan: Voi muc y nghia 1%, phuong thuc ban hang moi 
# khong mang lai hieu qua bang phuong thuc ban hang truoc do.


# --------------------Bai 2----------------------
# Cau a: Bien doi du lieu ve dang vecto
xi <- c(5,6,7,8,9,10)
ni <- c(5,10,15,20,12,8)
x <- rep(xi,ni)
x

# Ve bieu do stem & leaf cho du lieu x:
stem(x)

# Cau b:
# Viet ham test.geq.oneside:

test.geq.oneside <- function(x, mu_0, alpha){
  X.bar = mean(x)
  s = sd(x)
  n = length(x)
  # tinh gia tri thong ke kiem dinh t0:
  t_0 = (X.bar - mu_0)*sqrt(n)/s
  # tinh p - value
  p.value = 1- pt(t_0,n-1)
  cat('Voi muc y nghia alpha =',alpha, ':\n')
  if(p.value < alpha)
    cat('Bac bo H0 voi p-value =',p.value)
  else
    cat('Chua du co so de bac bo H0 voi p-value =',p.value)
}

# Ap dung:
test.geq.oneside(x,8,0.05)

# Kiem tra lai bang ham t.test:
t.test(x, alternative = "greater", mu = 8, conf.level = 0.95)

# Nhan xet: Ket qua cho ta p-value = 0.9668 > 5% = alpha
# do do chua du co so de bac bo gia thuyet H0:"<=8" voi muc y nghia 5%.

# Ket luan: Voi muc y nghia 5%, diem trung binh cua hoc vien khong lon hon 8.


# Cau c:
# Viet ham:

test.leq.oneside <- function(x, mu_0,alpha){
  X.bar = mean(x)
  s = sd(x)
  n = length(x)
  t_0 = (X.bar - mu_0)*sqrt(n)/s
  p.value = pt(t_0,n-1)
  cat('Voi muc y nghia alpha =',alpha, ':\n')
  if(p.value < alpha)
    cat('Bac bo H0 voi p-value =',p.value)
  else
    cat('Chua du co so de bac bo H0 voi p-value =',p.value)}

# Ap dung

test.leq.oneside(x,8,0.05)

# Kiem tra lai bang ham t.test:
t.test(x, alternative = "less", mu = 8, conf.level = 0.95)

# Nhan xet: Ket qua cho ta p-value = 0.0332 < 5% = alpha
# do do bac bo gia thuyet H0:"mu > 8" voi muc y nghia 5%.

# Ket luan: Voi muc y nghia 5%, diem trung binh cua hoc vien khong lon hon 8.

# --------------------Bai 3----------------------
teen_birth_rate_path <- paste(path, 'teen-birth-rate-2002.txt', sep = '/')
data <- read.table(teen_birth_rate_path,header=T,sep='\t')
# Xac dinh cac bien co trong file teen-birth-rate-2002.txt:
names(data)
# Tach va gan 1 data frame cac bien du lieu Black, Hispanic, White:
newdat <- data.frame(Black=data$Black,Hispanic=data$Hispanic,White=data$White)
newdat

#Tinh cac gia tri trong bang
# Do trong du lieu co gia tri "NA", nen khi tinh mean ta dung them thong so "na.rm = T"
Xbar <- apply(newdat,2,function(x) mean(x,na.rm=T))
s <- apply(newdat,2,function(x) sd(x,na.rm=T))
n <- apply(newdat,2,function(x) length(x[is.na(x)==F]))
mu0 <- mean(c(newdat$Black,newdat$Hispanic,newdat$White),na.rm=T)

z.val <- function(x,mu0){
  mean.x <- mean(x,na.rm=T)
  sd.x <- sd(x,na.rm=T)
  n.x <- length(x[is.na(x)==F])
  z <- (mean.x - mu0)*sqrt(n.x)/sd.x
  return(z)
}

Z <- apply(newdat,2,function(x) z.val(x,mu0))
p <- apply(newdat,2,function(x) 2*(1-pnorm(abs(z.val(x,mu0)))))
table <- round(data.frame(Xbar,s,n,Z,p),2)
table

# ---------------------------Bai 4-----------------------------
path <- '/Users/mac/Google Drive/KHTN/Thuc Hanh Xac Suat Thong Ke/Class_09_2021/XSTK_Dataset/'
data04_path <- paste(path, 'data04.rda', sep = '/')
load(data04_path)

n <- length(survey)
y <- length(survey[survey == 1])

p0 <- 0.6
p_hat <- y/n
z0 <- (p_hat - p0)/sqrt(p0*(1-p0)/n)
z0


# (1)
# H0: p = 0.6
# H1: p < 0.6
pnorm(z0)
prop.test(y, n, p = p0, alternative = "less", conf.level = 0.95)

# (2)
# H0: p = 0.6
# H1: p > 0.6
1 - pnorm(z0)
prop.test(y, n, p = p0, alternative = "greater", conf.level = 0.95)
 
# ---------------------------Bai 5-----------------------------
prop.test(20, 100, p = 0.15, alternative = "two.sided", conf.level = 0.95)

prop = function(f, n, p, alpha){
  Z = (n*f - n*p)/sqrt(n*p*(1 - p))
  z = qnorm(1 - alpha/2)
  if(abs(Z) > z){
    print("Bac ho H0")
  }else{
    print("Chua du co so bac bo Ho")
  }
  p.value = 2*(1 - pnorm(abs(Z)))
  return (p.value)
}
prop(0.2, 100, 0.15, 0.05)
binom.test(20, 100, p = 0.15, alternative="two.sided")

# ---------------------------Bai 6-----------------------------
data = read.csv("times.csv", header = TRUE)
data[is.na(data)] = 0
sv_KHTN = length(data$KHTN)
sv_KHTN_greater_than_5 = length(data$KHTN[data$KHTN > 5])
prop.test(sv_KHTN_greater_than_5, sv_KHTN, p = 0.5, alternative = "two.sided", conf.level = 0.95)
#H0: p = p0
#H1: p > p0
proptest.geq <- function(f, n, p0, alpha){
  p_hat = f / n;
  Z0 = (p_hat - p0) * sqrt(n) / sqrt(p0 * (1 - p0))
  z = qnorm(1 - alpha)
  p_value = 1 - pnorm(Z0)
  if (Z0 > z){
    print("Bac bo gia thuyet H0")
    sprintf("Gia tri p-value la: %f < %f = alpha", p_value, alpha)
  }else{
    print("Chua du co so bac bo gia thuyet H0")
    sprintf("Gia tri p-value la: %f >= %f = alpha", p_value, alpha)
  }
}
proptest.geq(sv_KHTN_greater_than_5, sv_KHTN, 0.5, 0.05)

#H0: p = p0
#H1: p < p0
proptest.leq <- function(f, n, p0, alpha){
  p_hat = f / n;
  Z0 = (p_hat - p0) * sqrt(n) / sqrt(p0 * (1 - p0))
  z = qnorm(1 - alpha)
  p_value = pnorm(Z0)
  if (Z0 < -z){
    print("Bac bo gia thuyet H0")
    sprintf("Gia tri p-value la: %f < %f = alpha", p_value, alpha)
  }else{
    print("Chua du co so bac bo gia thuyet H0")
    sprintf("Gia tri p-value la: %f >= %f = alpha", p_value, alpha)
  }
}
proptest.leq(sv_KHTN_greater_than_5, sv_KHTN, 0.5, 0.05)

# ---------------------------Bai 7-----------------------------
test.mean <- function(X, alternative = c("2 phia", "ben trai", "ben phai"), mu_0 = 0, alpha = 0.05){
  n = length(X)
  X_mean = mean(X)
  s = sd(X)
  T_0 = (X_mean - mu_0) * sqrt(n) / s
  conclusion = ""
  p_value = 0
  alternative = match.arg(alternative)
  if (alternative == "2 phia"){
    t = qt(1 - alpha, n - 1)
    conclusion = ifelse(abs(T_0) > t, "Bac bo H_0", "Chua du co so bac bo H_0")
    p_value = 2 * pt(abs(T_0), n - 1, lower.tail = FALSE)
  }
  if (alternative == "ben trai"){
    t = qt(1 - alpha, n - 1)
    conclusion = ifelse(T_0 < -t, "Bac bo H_0", "Chua du co so bac bo H_0")
    p_value = pt(T_0, n - 1, lower.tail = FALSE)
  }
  if (alternative == "ben phai"){
    t = qt(1 - alpha, n - 1)
    conclusion = ifelse(T_0 > t, "Bac bo H_0", "Chua du co so bac bo H_0") 
    p_value = pt(T_0, n - 1)
  }
  results = list(conclusion, X_mean, alpha, p_value)
  names(results) = c("Ket luan","Trung binh mau", "Alpha", "P_value")
  class(results) <- 'table'
  print(results)
}
# ---------------------------Bai 8-----------------------------
#X=1.96, X~ N(0,1), kiem dinh hai phia
p_value=2*min(pnorm(1.96),1-pnorm(1.96));
p_value
#X=1.96, X~ N(0,1), kiem dinh ben trai
p_value=1-pnorm(1.96);
p_value
#X=1.96, X~ N(0,1), kiem dinh ben phai
p_value=pnorm(1.96);
p_value
#X=1.7, X~ N(0,1), kiem dinh hai phia
p_value=2*min(pnorm(1.7),1-pnorm(1.7));
p_value
#X=1.7, X~ N(0,1), kiem dinh ben trai
p_value=1-pnorm(1.7);
p_value
#X=1.7, X~ N(0,1), kiem dinh ben phai
p_value=pnorm(1.7);
p_value
#Y=18, Y~ B(50,0.5), kiem dinh hai phia

#Y=18, Y~ B(50,0.5), kiem dinh ben trai

#Y=18, Y~ B(50,0.5), kiem dinh ben phai


#bai2
data=read.csv(file.choose())
x=data$HCM
y=data$HaNoi
# H0 px=py ti le thu nhap cao o TPHCM va HN bang nhau
# H1 px>py ti le thu nhap cao o TPHCM lon hon HN
prop.test.leq <- function(x, y, alpha)
{
  # co mau 
  n1 = length(x)
  n2 = length(y)
  # so phan tu thoa tinh chat
  y1 = length(x[x > 11.5])
  y2 = length(y[y > 11.5])
  # Cac ty le mau
  p1.hat = y1/n1
  p2.hat = y2/n2
  p.hat = (y1 + y2)/(n1 + n2)
  # Thong ke kiem dinh
  z = (p1.hat - p2.hat)/sqrt(p.hat*(1-p.hat)*((1/n1)+(1/n2)))
  # p gia tri   
  p.value <- 1- pnorm(z)
  if(p.value < alpha)
    cat('Bac bo H0 voi p-value =',p.value)
  else
    cat('Chua du co so de bac bo H0 voi p-value =',p.value)
}
prop.test.leq(x,y,0.05)
#bai1
xi <- c(12,13,15,16,17,18,19)
ni <- c(3,2,7,7,3,2,1)
x <- rep(xi,ni)
x
#H0 : mu >=15 tiep tuc ban
#H1 : mu <15 nghi ban
test.geq.oneside <- function(x, mu_0, alpha){
  X.bar = mean(x)
  s = sd(x)
  n = length(x)
  t_0 = (X.bar - mu_0)*sqrt(n)/s
  # tinh p - value
  p.value = 1- pnorm(t_0)
  cat('Voi muc y nghia alpha =',alpha, ':\n')
  if(p.value < alpha)
    cat('Bac bo H0 voi p-value =',p.value)
  else
    cat('Chua du co so de bac bo H0 voi p-value =',p.value)
}

test.geq.oneside(x,15,0.05)

# Kiem tra lai bang ham t.test:
t.test(x, alternative = "greater", mu = 15, conf.level = 0.95)





