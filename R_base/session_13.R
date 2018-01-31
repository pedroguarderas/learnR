# --------------------------------------------------------------------------------------------------
library(Matrix)

# --------------------------------------------------------------------------------------------------
# Generating random normal variables
n<-10
m<-10000
E<-Matrix(0,n,n)
diag(E)<-rep(1,n)
R<-Matrix( 0, n, n )
for ( i in 1:n ) {
  if ( i > 1 ) {
    R[i-1,i]<-1
  }
  if ( i < n ) {
    R[i+1,i]<-1
  }
}
E<-E + t(R) %*% R
L<-chol(E)
X<-Matrix( rnorm( m * n ), m, n )
u<-Matrix( rep( 1, n ), n , 1 )
Y<-t( L %*% t( X ) + u )

# Determination of covariance matrix
Ea<-cov( as.matrix( Y ) )

x<-as.data.frame( as.matrix(X) )
y<-as.data.frame( as.matrix(Y) )
X11()
plot( x, cex = 0.5, col = 'purple3' )

X11()
plot( y, cex = 0.5, col = 'purple3' )
