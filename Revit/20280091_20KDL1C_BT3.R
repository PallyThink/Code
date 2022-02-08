#bai1
#a
f=function(p)
{
  return(0.07*p**(-0.93))
}
integrate(f,lower = 0,upper = 0.2)
#b
integrate(f,lower = 0,upper = 1)
#bai2
x=sample(1:5,100,TRUE,c(0.1,0.2,0.4,0.2,0.1))
a=table(x)/100
a
par(mfrow=c(1,2))
hist(x, freq = FALSE)
hist(x)
#plot(a,xlab="X",ylab="P(X)", main="",lwd=10)
#barplot(a,main="",xlab="X",ylab="P(X)")