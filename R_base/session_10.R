# --------------------------------------------------------------------------------------------------
# Likelihood

# --------------------------------------------------------------------------------------------------
# Ejemplo para la ley de Poisson
n<-1000
L<-1
X<-rpois( n, L )
# lambda estimado vía verosimilitud
l<-mean(X)

# función de verosimilitud
logl<-function( l, X ) {
  return( log(l) * sum(X) - length(X) * l -sum( log( factorial( X ) ) ) )
}

N<-500
ls<-sort( runif( N, 1e-20, 5 ) )
logls<-sapply( ls, FUN = logl, X )
logL<-logl( L, X )

plot( ls, logls, col = 'dodgerblue3', cex = 0.7, pch = 1, xlim = c(0,5), ylim = c(-5000,-1300 ),
      main = expression( l(X,lambda) == log(lambda)*sum(X[i], i==1, n) - n*lambda - 
                           sum( log(X[i]*"!"), i==1, n ) ),
      xlab = expression(lambda), ylab = expression( l(x,lambda) ), axes = FALSE )
points( L, logL, col = 'red3', cex = 2.0, pch = 19 )
axis( side = 1, at = seq(0,5,0.5) )
axis( side = 2, at = seq(-5000,-1300, 500 ) )
text( L, 1.3 * logL, expression( lambda == frac(1,n) * sum( X[i], i==1, n ) ) )
box()

rm( list = ls() )
gc()

# --------------------------------------------------------------------------------------------------
# Ejemplo para ley Normal
n<-1000
U<-1
S<-0.5
X<-rnorm( n, U, S )
# u estimado vía verosimilitud
u<-mean(X)
# s estimado vía verosimilitud
s<-sd(X)

# función de verosimilitud
logl<-function( u, s, X ) {
  n<-length(X)
  return( -n * log(s) - 0.5*n*log(2*pi) - 0.5 * (s^(-2)) * sum( (X-u)^2) )
}

Nu<-30
Ns<-30
us<-seq(0,2,length.out=Nu)
ss<-seq(0.1,1,length.out=Ns)
logls<-matrix(0,Nu,Ns)
for( i in 1:Nu ) {
  for( j in 1:Ns ) {
    logls[i,j]<-logl( us[i], ss[j], X )  
  }
}
logL<-logl( u, s, X )

library( rgl )
persp3d( us, ss, logls, col = 'dodgerblue3', theta = 30, phi = 30, alpha = 0.9,
         xlab = 'u', ylab = 's', zlab = 'l(u,s,X)' )
points3d( u, s, logL, col = 'red', size = 10 )
