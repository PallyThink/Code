setwd("C:/Users/Administrator/Desktop/Math/Xac suat thong ke/TH XSTK/data/Data cho cac bai thuc hanh")
#Bai 1
x=c(1,2,3,4,5,6,7,8,9)
Sum<-function(x){
  s=0
  for(i in x){
    s=s+x[i]
  }
  return(s)
}
s=sum(x)
#Bai 2
radius<-c(seq(3,20))
volume<-c((4*pi*radius^3)/3)
Data=data.frame(radius,volume)
#Bai3
Data1<- read.csv ("data01.csv", header=TRUE)
age=Data1[,1]
index=rep(0,length(age))
i=1
repeat{
  if(i==length(index)+1){
    break
  }
  if(age[i]<=60){
    index[i]=0
  }
  else if(age[i]<=70){
    index[i]=1
  }
  else if (age[i]<=80){
    index[i]=2
  }
  else {
    index[i]=3
  }
  i=i+1
}
Data1$Index=index
Data1

#bai4
data2=read.csv("data11.csv",header = TRUE)
a=data2$a
b=data2$b
n=data2$n
data11=data.frame(a,b,n)


#bai 5
x=c(1,3,6,2,1,9,0)
phanvi<-function(x,p){
  y=sort(x)
  n=length(x)
  i=round(p*n/100,0)
  if(p*n%% 100==0){
    return(y[i])
  }
  else {
    return((y[i]+y[i+1])/2)
  }
}

phanvi(x,400/7)

