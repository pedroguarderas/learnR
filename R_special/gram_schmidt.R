#___________________________________________________________________________________________________
# Gram-Schmidt orthogonalization
gramschmidt<-function( X ) {
  n<-nrow(X)
  Y<-matrix(0,n,n)
  Y[,1]<-X[,1]
  w<-t( Y[,1] ) %*% Y[,1]
  for ( i in 2:n ) {
    Y[,i]<-X[,i]
    for ( j in 1:(i-1) ) {
      Y[,i]<-Y[,i] - ( t( X[,i] ) %*% Y[,j] ) * Y[,j] / w[j]
    } 
    w<-c( w, t( Y[,i] ) %*% Y[,i] )
  }
  return( Y )
}

#___________________________________________________________________________________________________
# Gram-Schmidt orthonormalization
gramschmidt<-function( X ) {
  n<-nrow(X)
  Y<-matrix(0,n,n)
  Y[,1]<-X[,1] / sqrt( t( X[,1] ) %*% X[,1] )
  for ( i in 2:n ) {
    Y[,i]<-X[,i]
    for ( j in 1:(i-1) ) {
      Y[,i]<-Y[,i] - ( t( X[,i] ) %*% Y[,j] ) * Y[,j]
    }
    Y[,i]<-Y[,i] / sqrt( t( Y[,i] ) %*% Y[,i] )
  }
  return( Y )
}

#___________________________________________________________________________________________________
# Gram-Schmidt with a metric matrix
gramschmidt<-function( X, G ) {
  n<-nrow(X)
  Y<-matrix(0,n,n)
  Y[,1]<-X[,1]
  w<-t( Y[,1] ) %*% G %*% Y[,1]
  for ( i in 2:n ) {
    Y[,i]<-X[,i]
    for ( j in 1:(i-1) ) {
      Y[,i]<-Y[,i] - ( t( X[,i] ) %*% G %*% Y[,j] ) * Y[,j] / w[j]
    } 
    w<-c( w, t( Y[,i] ) %*% G %*% Y[,i] )
  }
  return( Y )
}

#___________________________________________________________________________________________________
# Example
n<-4
X<-matrix( rnorm(n*n), n, n )
E<-cor( x = X )
Y<-gramschmidt(X,E)

t(Y) %*% E %*% Y
t(Y) %*% Y
diag( t(Y) %*% E %*% Y )