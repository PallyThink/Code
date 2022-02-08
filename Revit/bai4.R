#2.3 tinh toan
#vd1 tinh gia tri
#a
dbinom(4,8,0.3)
choose(8,4)*0.3^4*0.7^(8-4)
#b
dnorm(1.7,2,0.12)
1/sqrt(2*pi*0.12^2)*exp(-(1.7-2)^2/(2*0.12^2))
#vd2 tinh gia tri tai nhieu gia tri
dbinom(c(4,6),8,0.3)
dexp(2,c(1,2,3))
#vd3
vec=dexp(2,c(1,2,3))
vec
#vd3 ve ham mat do
plot(0:8,dbinom(0:8,8,0.3),type="h",ylab = "P(X=x)")
curve(dnorm(x,2,0.12),from = 1.5,to=2.5,ylab = "fX(x)")
#vd 4 ham phan phoi 
#a
pbinom(4,8,0.3)
sum(dbinom(0:4,8,0.3))
#b
pnorm(2.1,2,0.12)
#c
pexp(2,3,lower.tail = FALSE)
exp(-6)
#vd 5 bieu dien ham phan phoi cua BNN
#a
plot(stepfun(0:8,c(0,pbinom(0:8,8,0.3))),ylab="FX(x)",main="")
#b
curve(pnorm(x,2,0.12),from=1.5,to=2.5,ylab="FX(x)")

