# -------------------bai 1--------------------
M =25
N = 100
n = 15
x = 0:15
# Probability distribution function of hypergeometric distribution H(100,25,15)
dhyper(x, M,N-M,15 )
barplot(dhyper(x, M,N-M,n ))

# -------------------bai 2--------------------
# Calculate P(5<=X<=12) = P(X =5)+ ...+P(X=12)
sum(dhyper(5:12,M,N-M,n))

# Calculate P(X<=12) - P(X<=4)
phyper(12,M,N-M,n) - phyper(4,M,N-M,n)

# -------------------bai 3--------------------
# a)
x = 0:50
lambda = 0.6
curve(dexp(x,lambda),0,10)

# b)
lambda1 = 0.3
curve(dexp(x,lambda1),0,10,add=T,col='blue')

#c)
graph <- function(x){
  dexp(x,0.6)-dexp(x,0.3)
}
point=uniroot(graph, c(0,10))$root
# P(X <= point) + P(X > point)
pexp(point,0.3) + pexp(point,0.6,lower.tail = FALSE)

# -------------------bai 4--------------------
x = 0:8
lambda = 1
plot(x,dpois(x,lambda))

# -------------------bai 5--------------------
x = 0:10
dchisq(x,3)
curve(dchisq(x,3))

# -------------------bai 6--------------------
n =50
x = 0:50
p = 0.08
par(mfrow=c(2,1))
plot(x,dbinom(x,n,p),xlab = "x", type='h', ylim = c(0,0.25), ylab = "Binormial density")
plot(x,dpois(x,lambda=n*p),xlab = "x",type='h', ylim = c(0,0.25),ylab = "Poisson density" )

# -------------------bai 7--------------------
n = 50 
p =0.4

mu = n*p
sigma = n*p*(1-p)

plot(0:50,dbinom(0:50,n,p), xlim = c(0,40), type='h',xlab = "x", ylab = "", col = "blue")
curve(dnorm(x,mu,sqrt(sigma)),from=0,to=40, col = "red", add=TRUE)
legend("topright",c("Binormial Density","Normal Density"), fill=c("blue","red"))







