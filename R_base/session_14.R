# --------------------------------------------------------------------------------------------------
M<-2000
n<-100
b<-c(1.5,2.7)
b0<-3
p<-length( b )
s<-0.2

k<-1
B<-data.frame()
E2<-data.frame()
C<-data.frame()
while( k <= M ) {
  X<-matrix( runif( 2 * n ), n, 2 )
  e<-rnorm( n, 0, s )
  D<-data.frame( Y = X %*% b + b0 + e, X1 = X[,1], X2 = X[,2] )
  MC<-lm( Y ~ X1 + X2, data = D )
  B<-rbind( B, coefficients( MC ) )
  E2<-rbind( E2, sum( residuals( MC )^2 ) )
  XX<-cbind( rep( 1, n ), X )
  C<-rbind( C, diag( solve( t(XX) %*% XX ) ) )
  k<-k+1
}
colnames( B )<-c( 'b0', 'b1', 'b2' )
colnames( E2 )<-c( 'e2' )
colnames( C )<-c( 'c0', 'c1', 'c2' )
rm( k, X, e, D, MC )

# --------------------------------------------------------------------------------------------------
# Estimación de s
sqrt( mean( E2$e2 / ( n - p - 1 ) ) )

# --------------------------------------------------------------------------------------------------
X11( width = 13, height = 5 )
layout( matrix( c(1,2,3), 1, 3, byrow = TRUE ) )
hist( B$b0, breaks = 30, freq = FALSE, col = 'orange1', xlab = 'b0', main = 'Hist b0' )
hist( B$b1, breaks = 30, probability = TRUE, col = 'orange1', xlab = 'b1', main = 'Hist b1' )
hist( B$b2, breaks = 30, probability = TRUE, col = 'orange1', xlab = 'b2', main = 'Hist b2' )

X11()
plot( B, col = 'lightblue4', cex = 0.5, pch = 1 )

X11()
hist( E2$e2 / s^2, breaks = 40, probability = TRUE, col = 'pink',
      xlab = 'e^2', main = 'Hist e^2', ylim = c( 0.0, 0.03 ) )
curve( dchisq( x, df = n - p - 1 ), col = 'blue3', add = TRUE )

X11( width = 13, height = 5 )
layout( matrix( c(1,2,3), 1, 3, byrow = TRUE ) )
hist( ( B$b0 - b0 ) / sqrt( C$c0 * E2$e2 / ( n - p - 1 ) ), 
      breaks = 40, probability = TRUE, col = 'lightgreen', xlab = 'b0', main = 'Hist' )
curve( dt( x, df = n - p - 1 ), col = 'purple4', add = TRUE, lwd = 2 )
hist( ( B$b1 - b[1] ) / sqrt( C$c1 * E2$e2 / ( n - p - 1 ) ), 
      breaks = 40, probability = TRUE, col = 'lightgreen', xlab = 'b1', main = 'Hist' )
curve( dt( x, df = n - p - 1 ), col = 'purple4', add = TRUE, lwd = 2  )
hist( ( B$b2 - b[2] ) / sqrt( C$c2 * E2$e2 / ( n - p - 1 ) ), 
      breaks = 40, probability = TRUE, col = 'lightgreen', xlab = 'b2', main = 'Hist' )
curve( dt( x, df = n - p - 1 ), col = 'purple4', add = TRUE, lwd = 2 )
