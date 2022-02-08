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
df= read_excel(file.choose())
summary(df)
#a
mean(df$FPSA)
var(df$FPSA)
median(df$FPSA)
#b
plot(df$FPSA,type = "h")
boxplot(df$FPSA)
#c
FPSA0=subset(df$FPSA,df$K==0)
FPSA1=subset(df$FPSA,df$K==1)
#d

#bai3
#a
sv=1:10
Q1=c(3,3,3,4,3,4,3,4,3,4)
Q2=c(5,3,5,5,2,2,5,5,4,2)
Q3=c(1,3,1,1,1,3,1,1,1,1)
#b
tab1=table(Q1)
tab2=table(Q2)
tab3=table(Q3)
#c
par(mfrow=c(1,3))
barplot(tab1)
barplot(tab2)
barplot(tab3)
#d
barplot(tab2,horiz = TRUE)
barplot(tab3,horiz = TRUE)
#bai4
#a
x=rbinom(100,60,0.4)
hist(x)
#b
y=rpois(100,4)
hist(y)
#c
z=rnorm(100,50,4)
plot(density(z),main="bieu do ham mat do")
plot(z,dnorm(z,50,4),type = "h",col="blue")
#d
t=rexp(100,1/2500)
curve(dexp(x,1/25),0,100)
curve(pexp(x,1/25),0,100)
