#bai1
price=c(300,250,400,550,317,289,425,289,389,559)
no.bedroom=c(3,3,4,5,4,3,6,3,4,5)
#a
plot(no.bedroom,price)
#b
#h0 b1.hat=60
#h1 b1.hat>60
#bai2
#a
std=c(1:10)
beers=c(5,2,9,8,3,7,5,3,5)
BAL=c(0.1,0.03,0.19,0.04,0.095,0.07,0.06,0.02,0.05)
plot(beers,BAL)
abline(lm(BAL~beers))
#b
gl(3,5,20)
#c

shtq <- function ( n) {
  if ( n < 2)
    n
  else
    shtq (n - 1) + 3* shtq ( n - 2)
}
## Day so
day <- function ( n ){
  list = numeric ( n )
  for ( k in 1 : n ){
    list [k ] = shtq ( k ) }
  return ( list )
}
z = day (8)
rep(z[5],z[3])
rep(c(z[2],z[5]),c(z[3],z[4]))
mean(z)
max(z)
sd(z)    
x <- seq (10 , 22 , 2)
y <- x - 3
z <- x + 3
df = data.frame (x , y , z)
u = ( length (df)*max(df )) %% abs( ncol (df) - nrow (df ))
curve ( dnorm (x , 0 , 1) , from = -2 , to = 2)
curve ( pnorm (x , 0 , 1) , from = -2 , to = 2)
line (seq ( -2 ,2 ,0.01) , dnorm ( seq ( -2 ,2 ,0.01)))
plot (seq ( -2 ,2 ,0.01) , pnorm ( seq ( -2 ,2 ,0.01)))
