rm(list=ls())
# --------------------Bai 1----------------------
x <- c(1,2,5,7,-3,0,5,1,5,6)
y <- c(2,2,0,-5,7,8,11,9,3,2)

# cau a
x + y
x - y
x*y

# cau b
x[x%%2==0]
y[y%%2==1]

z<-c()
a <- 1
for (i in 1:length(x)){
  if(x[i] %% 2 == 0){
    z[a]<-x[i]
    a <- a + 1
  }
  
}
z

# cau c
x[x > 0]
y[y > 0]

# cau d
mean(x)
var(x)
sd(x)
sd(x)/sqrt(length(x))

# cau e
min(x)
max(x)

# cau f
sort(x)
sort(x,decreasing=T)

# cau g
save(x,file='varx.rda')
save(y,file='vary.rda')

# --------------------Bai 2----------------------
path <- '/Users/mac/Google Drive/KHTN/Thuc Hanh Xac Suat Thong Ke/Class_09_2021/XSTK_Dataset/'
data1_path <- paste(path, 'data01.csv', sep = '/')
data1 <- read.csv(data1_path,header=T)
data1
names(data1)

# cau a
mean(data1$FPSA)
var(data1$FPSA)
median(data1$FPSA)

# cau b
plot(data1$FPSA,type='l')
boxplot(data1$FPS)

# cau c
fpsa0 <- subset(data1$FPS, K==0)
fpsa0
fpsa1 <- subset(data1$FPS, K==1)
fpsa1

# cau d
data2_path <- paste(path, 'data02.csv', sep = '/')
data2 <- read.csv(data2_path,header=T)
data2
data <- data.frame(data1[,1:3],data2)
# merge(x = data1, y = data2, by = "K", all.x = TRUE)

# cau e
data$tPSA <- 0
data$tPSA[data$Age<=30] <- 0
data$tPSA[data$Age>30 && data$Age<=50] <- 1
data$tPSA[data$Age>50] <- 2
table(data$tPSA)


cal_tPSA = function(Age){
  X = numeric(length(Age))
  for(i in 1:length(Age)){
    if(Age[i] <= 30)
      X[i] = 0
    else if(Age[i] <= 50)
      X[i] = 1
    else
      X[i] = 2
  }
  return (X)
}

cal_tPSA(data1$Age)


# --------------------Bai 3----------------------
# cau a
sv <-1:10
ques1 <- c(3,3,3,4,3,4,3,4,3,4)
ques2 <- c(5,3,5,5,2,2,5,5,4,2)
ques3 <- c(1,3,1,1,1,3,1,1,1,1)

# cau b
tab1 <- table(ques1)
tab1
tab2 <- table(ques2)
tab2
tab3 <- table(ques3)
tab3

# cau c
barplot(tab1)
barplot(tab2)
barplot(tab3)

# cau 
barplot(tab2, horiz=T)
barplot(tab3, horiz=T)

# --------------------Bai 4----------------------
# cau a
x <- rbinom(100,60,0.4)
hist(x, main='Binomial Distribution')

# cau b
y <- rpois(100,4)
hist(y)

# cau c
z <- rnorm(100,50,4)
plot(density(z),main='Bieu do ham mat do')
plot(z,dnorm(z,50,4),type='h',col="blue")
curve(dnorm(x,50,4),from=30,to=70,col="blue")

# cau d
t <- rexp(100,1/2500)
plot(density(t),main='Bieu do ham mat do') #Mat do tu so lieu tao
plot(t,dexp(t,1/2500),type='h',main='Bieu do ham mat do cua pp mu')
curve(dexp(x,1/2500),from=0,to=50000,col="blue")

# --------------------Bai 5----------------------
path <- '/Users/mac/Google Drive/KHTN/Thuc Hanh Xac Suat Thong Ke/Class_09_2021/XSTK_Dataset/'

# cau a
diesel_engine_path <- paste(path, 'diesel_engine.dat', sep = '/')
diesel_time_path <- paste(path, 'diesel_time.csv', sep = '/')

diesel_engine = read.table(diesel_engine_path,header=T)
diesel_time = read.csv(diesel_time_path,header=T)

# cau b
names(diesel_engine)
names(diesel_time)

# cau c
length(diesel_engine$speed[is.na(diesel_engine$speed)])
diesel_engine$speed[is.na(diesel_engine$speed)] = 1500

length(diesel_engine$load[diesel_engine$load=='NA'])
diesel_engine$load[is.na(diesel_engine$load)]=20

diesel_engine$speed
diesel_engine$load

# cau d
diesel_engine$alcohol
mean(diesel_engine$alcohol)
var(diesel_engine$alcohol)
sd(diesel_engine$alcohol)

# cau e
diesel = data.frame(diesel_engine,diesel_time)
diesel

# cau f
diesel$run[diesel$delay<1.000]

# cau g
length(diesel$run[diesel$timing==30])

# cau h
boxplot(diesel$speed, diesel$timing, diesel$delay)

par(mfrow = c(2,2))
boxplot(diesel$speed)
boxplot(diesel$timing)
boxplot(diesel$delay)

# cau i
plot(diesel$timing,diesel$speed)
plot(diesel$temp,diesel$press)

# cau j
diesel$load = factor(diesel$load)
diesel$load

# cau k
diesel$delay
diesel$new_delay = cut(diesel$delay,breaks=4)
diesel$new_delay

tab = table(diesel$new_delay)
tab
barplot(tab)

# cau l
cut_points = c(0.283,0.7,0.95,1.2,1.56)
diesel$new_delay1 = cut(diesel$delay,breaks=cut_points)
diesel$new_delay1

tab1 = table(diesel$new_delay1)
tab1
barplot(tab1)


# --------------------Bai 6----------------------
# Cau a
year <- c(1970,1971,1972,1973,1974,1975,1976,1977,1978,1979)
snow_cover <- c(6.5,12.0,14.9,10.0,10.7,7.9,21.9,12.5,14.5,9.2)
data <- data.frame(year,snow_cover)
data

# Cau b
plot(data$snow_cover, data$year, xlab= 'snow.cover',ylab = 'year', main = 'Bieu do snow.cover theo year')

# Cau c
hist(data$snow_cover)

# Cau d:
data$log_snow_cover <- log(data$snow_cover)

plot(data$log_snow_cover, data$year, xlab= 'log.snow.cover',ylab = 'year', main = 'Bieu do log.snow.cover theo year')

hist(data$log_snow_cover)

# --------------------Bai 7----------------------
Temperature <- c(53,57,63,70,70,75)
Erosionincidents <- c(3,1,1,1,1,0)
Blowbyincidents <- c(2,0,0,0,0,2)
Totalincidents <- c(5,1,1,1,1,2)
data <- data.frame( Temperature, Erosionincidents, Blowbyincidents, Totalincidents)
plot(data$Temperature,data$Totalincidents,type = 'l')


# --------------------Bai 8----------------------
Nam = c(1960:1980)
us=c(1.5,1.1,1.1,1.2,1.4,1.6,2.8,2.8,4.2,5,5.9,4.3,3.6,6.2,10.9,9.2,5.8,6.4,7.6,11.4,13.6)
anh=c(1,3.4,4.5,2.5,3.9,4.6,3.7,2.4,4.8,5.2,6.5,9.5,6.8,8.4,16,24.2,16.5,15.9,8.3,13.4,18)
nhat=c(3.6,5.4,6.7,7.7,3.9,6.5,6,4,5.5,5.1,7.6,6.3,4.9,12,24.6,11.7,9.3,8.1,3.8,3.6,8)
duc=c(1.5,2.3,4.5,3,2.3,3.4,3.5,1.5,18,2.6,3.7,5.3,5.4,7,7,5.9,4.5,3.7,2.7,4.1,5.5)
lamphat1 = data.frame(Nam = Nam, US = us, Anh = anh)
lamphat2 = data.frame(Nam = Nam, Nhat = nhat, Duc = duc)

# b)
lamphat = merge(lamphat1, lamphat2)
colnames(lamphat) = c("Nam", "US", "Anh", "Nhat", "Duc")
lamphat = as.data.frame(sapply(lamphat, as.numeric))
# c)
sum(lamphat$US > 5  & lamphat$Anh > 5 & lamphat$Nhat > 5 & lamphat$Duc > 5)

# d)
par(mfrow = c(2,2))
plot(lamphat$Nam ,lamphat$US,  lwd = 5, col = 'green')
plot(lamphat$Nam ,lamphat$Anh,  lwd = 5, col = 'orange')
plot(lamphat$Nam ,lamphat$Duc,  lwd = 5, col = 'pink')
plot(lamphat$Nam ,lamphat$Nhat,  lwd = 5, col = 'blue')
# e)
std <- function(x) sd(x)/sqrt(length(x))
summary(lamphat[, 2:5])
rbind(sd = apply(lamphat[, 2:5], 2, sd), std = apply(lamphat[, 2:5], 2, std))

# f)
# standard deviation
# Nhu vay 
# Anh bien thien nhieu nhat
# Duc bien thien it nhat

# g)
lamphat1 = lamphat[lamphat$Nam != 1980, ]
# h)

X = lamphat1$Nam
Y = lamphat1$US
n = nrow(lamphat1)
B2 = (sum(X * Y) - n * mean(X) * mean(Y)) / (sum(X^2) - n * mean(X)^2)
B1 = mean(Y) - B2 * mean(X)
HQ = function(x) {
  B1 + B2 * x
}
par(mfrow = c(1,1))
plot(lamphat$Nam ,lamphat$US, fg = 10, lwd = 5, col.lab = 'red',
     col = 'blue', pch = 3, cex = 2, main = "Regression")
curve(HQ, X, add = T, col = 'red',
      lwd = 10, lty = 1)

# i)
predict = HQ(1980) 
real = lamphat$US[lamphat$Nam == 1980]
abs(real - predict)
