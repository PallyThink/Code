#============================ BAI 1 ============================
setwd('D:/XAC_SUAT_THONG_KE/TH XSTK/Data cho cac bai thuc hanh')
volume <-read.csv('volume.csv')
sigma1 = 0.002
sigma2 = 0.0025
#--------CAU A------
# H0: The tich sua trung binh duoc hai may dau vao cac hop la nhu nhau
# H1: The tich sua trung binh duoc hai may dau vao cac hop khac nhau
test <- var.test(volume$machine1,volume$machine2)
test$p.value
# Do p gia tri = 0.01128317 < muc y nghia = 0.05 suy ra phuong sai khac nhau
result <- t.test(volume$machine1,volume$machine2, conf.level = 1 - 0.05)
ifelse(result$p.value < 0.05, 'Bac bo H0','Khong co so bac bo H0' )

#--------CAU B------
cat('P gia tri cua dinh tren la: ', result$p.value)

#--------CAU C------
cat('Khoang tin cay 95%: ', result$conf.int)

#--------CAU D------
#Biet truoc phuong sai nen thong ke Z ~ N(0,1) => p.value = dnorm(z)
test.leg.oneside <- function(x, y, mu, sigma1, sigma2, alpha)
{
  # Trung binh mau cua 2 mau
    mean.X <- mean(x)
    mean.Y <- mean(y) 
  # Co mau
    n <- length(x)
    m <- length(y)
  # Thong ke kiem dinh
    Z <- (mean.X - mean.Y - mu)/sqrt((sigma1^2/n) + (sigma2^2/m))
  # p gia tri
    p_value <- dnorm(Z)
  
  result <- ifelse(p_value < alpha, "Bac bo H0", "Khong co so bac bo H0")
  cat('Ket luan: ',result)
  cat('\n P gia tri cua dinh tren la: ',p_value)
}
test.leg.oneside(volume$machine1, volume$machine2, 0, sigma1, sigma2, 0.05  )

#--------CAU E------
test.geg.oneside <- function(x, y, mu, sigma1, sigma2, alpha)
{
  # Trung binh mau cua 2 mau
    mean.X <- mean(x)
    mean.Y <- mean(y) 
  # Co mau
     n <- length(x)
     m <- length(y)
  # Thong ke kiem dinh
    Z <- (mean.X - mean.Y - mu)/sqrt((sigma1^2/n) + (sigma2^2/m))
  # p gia tri
    p_value <- 1 - dnorm(Z)
    
  result <- ifelse(p_value < alpha, "Bac bo H0", "Khong co so bac bo H0")
  cat('Ket luan: ',result)
  cat('\n P gia tri cua dinh tren la: ',p_value)
}
test.geg.oneside(volume$machine1, volume$machine2,0, sigma1, sigma2, 0.05  )


#============================ BAI 2 ============================
diameter <- read.csv('diameter.csv')
# Gia su rang duong kinh cua cac thanh thep duoc can boi hai may
# nay co phan phoi chuan voi phuong sai bang nhau
# H0: Cac thanh thep voi duong kinh bang nhau
# H1: Cac thanh thep voi duong kinh khac nhau
extru.ma.1 <- diameter$extru.ma.1[!is.na(diameter$extru.ma.1)]
extru.ma.2 <- diameter$extru.ma.2[!is.na(diameter$extru.ma.2)]
#--------CAU A------
alpha <- 0.05
re <- t.test(extru.ma.1, extru.ma.2)
re
ifelse(re$p.value < alpha, 'Bac bo H0', 'Khong co so bac bo H0')
#--------CAU B------
cat('\n P gia tri cho thong ke o cau tren la: ', re$p.value, '\n')
#--------CAU C------
cat('Khoang tin cay 95%: ', re$conf.int)

#--------CAU D------
# Chua biet phuong sai cua 2 mau nen ta dung T => p.value = qt(t, n + m - 2)
test.leg.oneside <- function(x, y, mu, alpha)
{
  # Trung binh mau cua 2 mau
    mean.X <- mean(x)
    mean.Y <- mean(y) 
  # Co mau
    n <- length(x)
    m <- length(y)
  # Do lech chuan mau
    sd.X <- sd(x) 
    sd.Y <- sd(y)
  #Thong ke kiem dinh
    t <- (mean.X - mean.Y - mu)/sqrt((sd.X^2/n) + (sd.Y^2/m))
  #p gia tri
    p_value <- pt(t, n + m - 2)
    
  result <- ifelse(p_value < alpha, "Bac bo H0", "Khong co so bac bo H0")
  cat('Ket luan:',result)
  cat('\n P gia tri cua dinh tren la: ',p_value)
}
test.leg.oneside(extru.ma.1, extru.ma.2, 0, 0.05)

#--------CAU E------
test.geg.oneside <- function(x, y, mu, alpha)
{
  # Trung binh mau cua 2 mau
    mean.X <- mean(x)
    mean.Y <- mean(y) 
  # Co mau
    n <- length(x)
    m <- length(y)
  # Do lech chuan mau
    sd.X <- sd(x) 
    sd.Y <- sd(y)
  #Thong ke kiem dinh
    t <- (mean.X - mean.Y - mu)/sqrt((sd.X^2/n) + (sd.Y^2/m))
  # p gia tri
    p_value <- 1 - pt(t, n + m - 2)
    
  result <- ifelse(p_value < alpha, "Bac bo H0", "Khong co so bac bo H0")
  cat('Ket luan: ',result)
  cat('\n P gia tri cua dinh tren la: ',p_value)
}
test.geg.oneside(extru.ma.1, extru.ma.2, 0, 0.05)


#============================ BAI 3 ============================
time <- read.csv('tg.khoidong.csv')
sigma1 = 1.5; sigma2 = 1
n = 100; m = 150
alpha = 0.05

time.G <- time$Thoigian[time$Hang == 'G']
time.M <- time$Thoigian[time$Hang == 'M']

#H0: Cong nghe G co toc do khoi dong cua HDH nhanh gap doi cong nghe M
# mu.X >= mu.Y
#H1 : Cong nghe G co toc do khoi dong cua HDH thap hon hoac bang cong nghe M
# mu.X <= mu.Y
test.leg.oneside(time.G, time.M, 0, sigma1, sigma2, alpha)



#============================ BAI 4 ============================
chol <- read.table(file = 'cholesterol.txt', header = TRUE, sep = ' ')

#--------CAU A------
#BAI TOAN SO SANH THEO CAP: mu.D = mu.X - mu.Y
#H0: Che do an kieng va luyen tap the duc da co tac dung dung trong 
#    lam khong lam giam luong cholesterol trong mau: mu.D < 0
#H1: Che do an kieng va luyen tap the duc da co tac dung dung trong 
#    lam giam luong cholesterol trong mau: mu.D > 0
re <- t.test(chol$Before, chol$After, alternative = 'less', paired= TRUE)
re
ifelse(re$p.value < 0.05,'Bac bo H0','Khong co so bac bo H0')

#--------CAU B------
#H0: mu = mu0
#H1: mu < mu0 (mu =  mu.X - mu.Y)
test.leg.oneside <- function(x, y, mu, alpha)
{
  # Trung binh mau cua 2 mau
    mean.X <- mean(x)
    mean.Y <- mean(y) 
  # Co mau
    n <- length(x)
    m <- length(y)
  # Do lech chuan mau
    sd.X <- sd(x) 
    sd.Y <- sd(y)
  #Thong ke kiem dinh
    t <- (mean.X - mean.Y - mu)/sqrt((sd.X^2/n) + (sd.Y^2/m))
  #p gia tri
    p_value <- pt(t, n + m - 2)
  
  result <- ifelse(p_value < alpha, "Bac bo H0", "Khong co so bac bo H0")
  cat('Ket luan:',result)
  cat('\n P gia tri cua dinh tren la: ',p_value)
}
test.leg.oneside(chol$Before, chol$After,0, 0.05)

#--------CAU C------
#H0: mu = mu0
#H1: mu > mu0 (mu =  mu.X - mu.Y)
test.geg.oneside <- function(x, y, mu, alpha)
{
  # Trung binh mau cua 2 mau
    mean.X <- mean(x)
    mean.Y <- mean(y) 
  # Co mau
    n <- length(x)
    m <- length(y)
  # Do lech chuan mau
    sd.X <- sd(x) 
    sd.Y <- sd(y)
  # Thong ke kiem dinh
    t <- (mean.X - mean.Y - mu)/sqrt((sd.X^2/n) + (sd.Y^2/m))
  # p gia tri
    p_value <- 1 - pt(t, n + m - 2)
  
  result <- ifelse(p_value < alpha, "Bac bo H0", "Khong co so bac bo H0")
  cat('Ket luan: ',result)
  cat('\n P gia tri cua dinh tren la: ',p_value)
}
test.geg.oneside(chol$Before, chol$After,0, 0.05)













#============================ BAI 5 ============================
giamcan <- read.csv('giamcan.csv')
#H0: San pham an rieng co tac dung giam can it nhat 1.5kg khi sd 1 thang. mu.D >= 1.5 
#H1: San pham an rieng co khong tac dung gian can it nhat 1.5kg khi sd 1 thang mu.D < 1.5
#BAI TOAN SO SANH THEO CAP: mu.D = mu.X - mu.Y
#--------CAU A------
# muc y nghia 0.05 va mu = 1.5kg
re.A <- t.test(giamcan$Truoc, giamcan$Sau,  alternative = 'less', mu = 1.5 , paired = TRUE)
re.A
ifelse(re$p.value < 0.05,'Bac bo H0','Khong co so bac bo H0')

#--------CAU B------
# muc y nghia 0.01 va mu = 1.5kg
re.B <- t.test(giamcan$Truoc, giamcan$Sau, alternative = 'less', mu = 1.5, paired = TRUE, conf.level = 0.99)
re.B
ifelse(re$p.value < 0.01,'Bac bo H0','Khong co so bac bo H0')

#--------CAU C------
#H0: mu.D >= 2.5
#H1: mu.D < 2.5
# muc y nghia 0.05 va mu = 2.5kg
re.C1 <- t.test(giamcan$Truoc, giamcan$Sau,  alternative = 'less', mu= 2.5, paired = TRUE)
re.C1
ifelse(re$p.value < 0.05,'Bac bo H0','Khong co so bac bo H0')

# muc y nghia 0.01 va mu = 2.5kg
re.C2 <- t.test(giamcan$Truoc, giamcan$Sau, alternative = 'less', mu = 2.5, paired = TRUE, conf.level = 0.99)
re.C2
ifelse(re$p.value < 0.01,'Bac bo H0','Khong co so bac bo H0')



























#============================ BAI 6 ============================
prop.time <- read.csv('prop.time.csv')
#H0: Thoi gian lap trinh trung binh cua hai ngon ngu la nhu nhau
#H1: Thoi gian lap trinh trung binh cua hai ngon ngu la khac nhau
language1 <- as.numeric(prop.time[-c(1),]$X.1)
language2 <- as.numeric(prop.time[-c(1),]$X.2)

#--------CAU A------
re <- t.test(language1, language2)
re
ifelse(re$p.value < 0.05,'Bac bo H0','Khong co so bac bo H0')
#--------CAU B------
# Chua biet phuong sai cua 2 mau nen ta dung T => p.value = qt(t, n + m - 2)
test.leg.oneside <- function(x, y, mu, alpha)
{
  # Trung binh mau cua 2 mau
  mean.X <- mean(x)
  mean.Y <- mean(y) 
  # Co mau
  n <- length(x)
  m <- length(y)
  # Do lech chuan mau
  sd.X <- sd(x) 
  sd.Y <- sd(y)
  #Thong ke kiem dinh
  t <- (mean.X - mean.Y - mu)/sqrt((sd.X^2/n) + (sd.Y^2/m))
  #p gia tri
  p_value <- pt(t, n + m - 2)
  
  result <- ifelse(p_value < alpha, "Bac bo H0", "Khong co so bac bo H0")
  cat('Ket luan:',result)
  cat('\n P gia tri cua dinh tren la: ',p_value)
}
test.leg.oneside(language1, language2, 0, 0.05)

#--------CAU C------
test.geg.oneside <- function(x, y, mu, alpha)
{
  # Trung binh mau cua 2 mau
  mean.X <- mean(x)
  mean.Y <- mean(y) 
  # Co mau
  n <- length(x)
  m <- length(y)
  # Do lech chuan mau
  sd.X <- sd(x) 
  sd.Y <- sd(y)
  #Thong ke kiem dinh
  t <- (mean.X - mean.Y - mu)/sqrt((sd.X^2/n) + (sd.Y^2/m))
  # p gia tri
  p_value <- 1 - pt(t, n + m - 2)
  
  result <- ifelse(p_value < alpha, "Bac bo H0", "Khong co so bac bo H0")
  cat('Ket luan: ',result)
  cat('\n P gia tri cua dinh tren la: ',p_value)
}
test.geg.oneside(language1, language2, 0, 0.05)






















#============================ BAI 7 ============================
#H0: Hai loai may co ti le phe pham nhu nhau
#H1: Hai loai may co ti le phe pham khac nhau
# Muc y nghia
  alpha = 0.025
# Co mau
  n = c(300, 300)
# So phan tu thoa tinh chat
  y = c(15, 8)
re <- prop.test(y, n, conf.level = 1 - alpha)
re
ifelse(re$p.value < 0.025,'Bac bo H0','Khong co so bac bo H0')

#============================ BAI 8 ============================
#H0: Ti le nguoi ung ho viec tang toc do tai hai thanh pho nhu nhau
#H1: Ti le nguoi ung ho viec tang toc do tai hai thanh pho khac nhau
# Muc y nghia
alpha = 0.05
# Co mau
n = c(500, 400)
# So phan tu thoa tinh chat
y = c(385, 267)
re <- prop.test(y, n, conf.level = 1 - alpha)
re
ifelse(re$p.value < 0.025,'Bac bo H0','Khong co so bac bo H0')

#============================ BAI 9 ============================
#H0: Ti le su dung dai an toan o TPHCM va Ha Noi nhu nhau
#H1: Ti le su dung dai an toan o TPHCM va Ha Noi khac nhau
#--------CAU A------
# Muc y nghia
alpha = 0.05
# Co mau
n = c(200, 250)
# So phan tu thoa tinh chat
y = c(165, 198)
re <- prop.test(y, n, conf.level = 1 - alpha)
re
ifelse(re$p.value < 0.05,'Bac bo H0','Khong co so bac bo H0')

#--------CAU B------
# Muc y nghia
alpha = 0.1
# Co mau
n = c(200, 250)
# So phan tu thoa tinh chat
y = c(165, 198)
re <- prop.test(y, n, conf.level = 1 - alpha)
re
ifelse(re$p.value < 0.1,'Bac bo H0','Khong co so bac bo H0')
#--------CAU C------
# Co mau
n = c(400, 500)
# So phan tu thoa tinh chat
y = c(330, 396)

# Muc y nghia 
  alpha = 0.05
re.C1 <- prop.test(y, n, conf.level = 1 - alpha)
re.C1
ifelse(re.C1$p.value < 0.05,'Bac bo H0','Khong co so bac bo H0')

# Muc y nghia 
  alpha1 = 0.1
re.C2 <- prop.test(y, n, conf.level = 1 - alpha1)
re.C2
ifelse(re.C2$p.value < 0.1,'Bac bo H0','Khong co so bac bo H0')
#NHAN XET







#============================ BAI 10 ============================
profit <- read.csv('Profit-th05.csv')

#--------CAU A------
re <- t.test(profit$Dist.1, profit$Dist.3)
cat('Khoang tin cay 95% cho su sai khac ve doang so ban hang trung binh cua hai chi nhanh nay: ',re$conf.int)

#--------CAU B ------
# H0: p1 <= p2
# H1: p1 > p2
dist1 <- profit$Dist.1[profit$Dist.1 > 600]
dist3 <- profit$Dist.3[profit$Dist.3 > 600]
# Co mau
  n = c(200, 200)
# so phan tu thoa tinh chat
 y = c(length(dist1),length(dist3))
 
re <- prop.test(y, n, alternative = 'greater', conf.level = 1 - 0.05)
re
ifelse(re.C2$p.value < 0.05,'Bac bo H0','Khong co so bac bo H0')

#--------CAU C------
prop.test.leq <- function(x, y, alpha)
{
  # co mau 
    n1 = length(x)
    n2 = length(y)
  # so phan tu thoa tinh chat
    y1 = length(x[x > 600])
    y2 = length(y[y > 600])
  # Cac ty le mau
    p1.hat = y1/n1
    p2.hat = y2/n2
    p.hat = (y1 + y2)/(n1 + n2)
  # Thong ke kiem dinh
    Z = (p1.hat - p2.hat)/sqrt(p.hat*(1-p.hat)*((1/n1)+(1/n2)))
  # p gia tri   
    p.value <- pnorm(z)
    ifelse(p.value < alpha,'Bac bo H0','Khong co so bac bo H0')
}

#============================ BAI 11 ============================
inf.sal <- read.csv('Inf.Sal.csv')
#--------CAU A------
re <- t.test(inf.sal$HCM, inf.sal$HaNoi)
cat('Khoang tin cay 95% cho su sai khac ve doang so ban hang trung binh cua hai chi nhanh nay: ',re$conf.int)
#--------CAU B------
alpha = 0.025
HCM <- inf.sal$HCM[inf.sal$HCM > 11.5 ]
HaNoi <- inf.sal$HaNoi[inf.sal$HaNoi > 11.5]
HaNoi <- HaNoi[!is.na(HaNoi)]
# Co mau
 n1 = length(inf.sal$HCM)
 n2 = length(inf.sal$HCM[!is.na(inf.sal$HCM)])
 n = c(n1, n2)
# so phan tu thoa yeu cau
 y = c(length(HCM), length(HaNoi))
re <- prop.test(y, n,alternative = 'greater', conf.level = 1 - alpha)
re
ifelse(re$p.value < alpha,'Bac bo H0','Khong co so bac bo H0')

#--------CAU C------
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
  Z = (p1.hat - p2.hat)/sqrt(p.hat*(1-p.hat)*((1/n1)+(1/n2)))
  # p gia tri   
  p.value <- 1- pnorm(z)
  ifelse(p.value < alpha,'Bac bo H0','Khong co so bac bo H0')
}












