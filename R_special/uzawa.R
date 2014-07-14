#___________________________________________________________________________________________________
library( Matrix )

# Numerical gradient
# J objective function
# x point
# e epsilon
gradient<-function( J, u, e, m ) {
  n<-length( u )
  g<-Matrix(0,m,n)  
  for ( i in 1:n ) { # i<-1
    y<-u
    y[i]<-y[i] + e
    z<-u
    z[i]<-z[i] - e
    g[,i]<-0.5 * ( J( y ) - J( z ) ) / e
  }
  return( g )
}

#___________________________________________________________________________________________________
# Uzawa algorithm for constrained optimization
uzawa_method<-function( n, e1, e2, a, b, u0, p0, J, F, c ) {
  p<-p0
  u<-u0
  P<-as.matrix( p )
  U<-as.matrix( u )
  N<-length( p )

  for ( i in 1:n ) {
    gJ<-gradient( J, u, e2, 1 )
    gF<-gradient( F, u, e2, N )
    g<-t( gJ + t(p) %*% gF )
  	u<-u - a * g
  	f<-F( u )
  	p<-p + b * f
  	if ( c ) {
      p<-Matrix( apply( p, 1, FUN = max, 0 ), N, 1 )
  	}	
    U<-cbind( U, as.matrix( u ) )
  	P<-cbind( P, as.matrix( p ) )
  	if ( norm( g ) <= e1 ) {
  		break
  	}
  }
  return( list( u = u, p = p, J = J(u), F = F(u), U = U, P = P ) )
}

#___________________________________________________________________________________________________
# Example Portfolio Optimization, Merton model
n<-10
e<-matrix( 0, n, 1 )
for ( i in 1:n ) {
  e[i]<-i / n
}
A<-Matrix( 0, n, n )
A<-diag( e )
R<-Matrix( runif( n * n ), n, n )
A<-A + 0.001 * t(R) %*% R
r<-2.5

# Equality restriccions
F<-function(u) { 
	return( Matrix( c( as.numeric( t(e) %*% u - r ), 
                     as.numeric( Matrix(1,1,n) %*% u - 1) ) ) )
}

# Objective function
J<-function( u ) {
	return( t(u) %*% A %*% u )
}

nn<-500
e1<-0.00001
e2<-0.00001
a<-0.1
b<-0.1
u0<-Matrix( 0, n, 1 )
p0<-Matrix( 1, 2, 1 )
c<-FALSE

result<-uzawa_method( nn, e1, e2, a, b, u0, p0, J, F, c )

X11()
plot( colSums( result$U ), col = 'red4', cex = 0.7, main = 'Proportion constraint evolution',
      xlab = 'i', ylab = expression( sum( u[i], i == 1, n ) ) )
X11()
E<-as.vector( t(e) %*% result$U )
plot( E, col = 'red4', cex = 0.7, main = 'Expectation constraint evolution',
      xlab = 'i', ylab = expression( sum( u[i]*ER[i], i == 1, n ) ))

#___________________________________________________________________________________________________
# F<-function(u) { 
#   return( Matrix( c( as.numeric( t(e) %*% u - r ), 
#                      as.numeric( Matrix(1,1,n) %*% u - 1), 
#                      as.vector( -u ) ), length(u) + 2, 1 ) )
# }
