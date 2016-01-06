library( Matrix )

M<-100
N<-10
X<-matrix( rbinom( N * M, 1, prob = 0.6 ), M, N )
X<-Matrix( cbind( rep(1,M), X ) )
y<-Matrix( rbinom( M, 1, prob = 0.6 ), M, 1 )

g<-function(x) pnorm( x )
dg<-function(x) dnorm( x )
gi<-function(x) qnorm( x )
d2b<-function( x ) 1

b<-Matrix( colMeans( X ), N+1, 1 )
# w<-Matrix( runif( M, 0.5, 1 ), M, 1 )
w<-Matrix( 1, M, 1 )
  
I<-800
for ( i in 1:I ) {
  n<-X %*% b
  u<-apply( n, 2, FUN = g )
  v<-dg( u )
  z<-n + ( y - u ) * v
  W<-Matrix( 0, M, M )
  diag( W )<-w / v
  b0<-b
  b<-solve( a = t( X ) %*% W %*% X, b = t( X ) %*% W %*% z )
  
  cat( paste( '\rRelative error: ', formatC( norm( b - b0 ) / norm( b0 ), digits = 30, format = 'f' ), sep = '' ) )
}

