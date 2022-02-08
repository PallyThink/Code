getwd()
setwd('C:/Users/Administrator/Desktop/Math/Xac suat thong ke/TH XSTK/data/Data cho cac bai thuc hanh')
#bai1
x=c(1,2,5,7,-3,0,5,1,5,6)
y=c(2,2,0,-5,7,8,11,9,3,2)
#a
x+y
x*y
x-y
#b
#z=x[x%%2==0]
#t=y[y%%2==1]
Z=function(x)
{
  a=1
  z=c()
  for(i in 1:length(x))
  {
    if(x[i]%%2==0)
    {
      z[a]=x[i]
      a=a+1
    }
  }
  return(z)
}
T=function(x)
{
  a=1
  t=c()
  for(i in 1:length(x))
  {
    if(x[i]%%2==1)
    {
      t[a]=x[i]
      a=a+1
    }
  }
  return(t)
}
z=Z(x)
t=T(y)
#c
x[x>0]
y[y>0]
#d
mean(x)
var(x)
sd(x)
mean(y)
var(y)
sd(y)
#e
max=max(x)
min=min(y)
#f
sort(x)
sort(y,decreasing = TRUE)
#g
save(x,file = 'varx.rda')
save(y,file = 'vary.rda')

#bai2
# tao mot data frame truc tiep 
edit(data.frame())
# chon file nhanh ^^
df <- read.csv(file.choose())











