# __________________________________________________________________________________________________
# 
# author: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: kmeans.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
library(reshape2)

distance<-function( X, d ) {
  n<-ncol( X )
  m<-nrow( X )
  
  D<-matrix( 0, m, m )
  for ( i in 1:(m-1) ) {
    for ( j in (i+1):m ) {
      D[i,j]<-d( X[i,], X[j,])
      if ( i != j ) {
        D[j,i]<-D[i,j]
      }
    }
  }
  diag(D)<-0
  return( D )
}

distance.matrix<-function( X, C, d ) {
  n<-nrow( C )
  m<-nrow( X )
  
  D<-matrix( 0, m, n )
  for ( i in 1:m ) {
    for ( j in 1:n ) {
      D[i,j]<-d( X[i,], C[j,] )
    }
  }
  return( D )
}

choose.min<-function( x ) {
  return( which.min( x )[1] )
}

Kmeans<-function( X, d, I = 10, N = 5 ) {
  K<-data.frame( X )
  n<-nrow( K )
  m<-ncol( K )
  C<-sample(1:n,N)
  C<-as.matrix( K[C,] )
  
  for ( i in 1:I ) {
    D<-distance.matrix( X, C, d )
    K$cluster<-apply( D, 1, FUN = choose.min )
    C<-melt( K, id.vars = 'cluster' )
    C<-aggregate( value ~ cluster + variable, C, mean )
    C<-dcast( data = C, cluster ~ variable, value.var = 'value' )
    C$cluster<-NULL
    C<-as.matrix( C )
  }
  return( list( cluster = K, centroid = C, distnace = D ) )
}

N<-2
I<-50
X<-matrix( runif( 100 * 2 ), 100, 2 )
w<-c( 0.2, 0.8 )
M<--w %o% w
diag( M )<-w - diag( M )
M<-0.5 * M
d<-function( x, y ) {
  return( sqrt( t( x - y ) %*% M %*% ( x -y ) ) )
}
KM<-Kmeans( X, d, I, N )
K<-KM$cluster

