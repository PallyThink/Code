#bai 1
x<-c(4,2,6)
y=c(1,0,-1)
length(x)
sum(x)
sum(x^2)
x+y
x*y
x-2
x^2

#bai 2
a=7:11
b=seq(2,9)
c=seq(4,10,by=2)
d=seq(3,30,length=10)
e=seq(6,-4,by=-2)

#bai 3
a=rep(2,4)
b=rep(c(1,2),4)
c=rep(c(1,2),c(4,4))
d=rep(1:4,4)
e=rep(1:4,rep(3,4))

#bai 4
a=rep(6,6)
b=rep(c(5,8),4)
c=rep(c(5,8),c(4,4))

#bai 5
x=c(5,9,2,3,4,6,7,0,8,12,2,9)
# x la vecto kieu numberic
x[2]
x[2:4]
x[c(2,3,6)]
x[c(1:5,10:12)]
x[-(10:12)]

#bai 6
y=c(33,44,29,16,25,45,33,19,54,22,21,49,11,24,56)
summary(y[1:3])
summary(y[4:6])
summary(y[7:9])
summary(y[10:12])
summary(y[13:15])

y1=y[c(1,4,7,10,13)]
summary(y1)
y2=y[c(2,5,8,11,14)]
summary(y2)
y3=y[c(3,6,9,12,15)]
summary(y3)

#bai7
x=matrix(c(3,2,-1,1),nrow = 2,ncol = 2,byrow=TRUE)
y=matrix(c(1,4,0,0,1,-1),nrow=2,ncol=3,byrow=TRUE)
2*x
x*x
x%*%y
t(y)
solve(x)

# bai 8
a=x[1,]
b=x[2,]
c=x[,2]
d=y[1,2]
e=y[,2:3]

#bai 9
data(quakes)
head(quakes)
summary(quakes[,3])
summary(quakes[,4])
data(mtcars)
head(mtcars)
summary(mtcars[,6])
summary(mtcars[,1])

#tuan2
setwd("C:/Users/Administrator/Desktop/Toán/Xác su???t th???ng kê/TH XSTK/data/Data cho cac bai thuc hanh")
x<-c(1,2,5,4,7,8,9,3)
sum<-function(x,i)
{
  s<-0
  repeat{
    s=s+a[i]
    i=i-1
    if(i==-1)
    break
  }
  return(s)
}
sum(x,4)



read.csv('data01.csv',header=TRUE)
age=data01[,1]
index=rep(0,length(age))

for(i in 1:length(age))
{
  x=age[i]
  if(x<=60)
  {
    index[i]=0
  }else if(x>60&&x<=70)
  {
    index[i]=1
  }
  else if(x>70&&x<=80){
    index[i]=2
  }
  else{
    index[i]=3
  }
}
index
table(index)















