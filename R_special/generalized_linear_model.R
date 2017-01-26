# __________________________________________________________________________________________________
# 
# author: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: generalizaed_linear_model.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
library( Matrix )
library( pracma )

# Observations
M<-100
N<-10
X<-matrix( rbinom( N * M, 1, prob = 0.6 ), M, N )
X<-Matrix( cbind( rep(1,M), X ) )
y<-Matrix( rbinom( M, 1, prob = 0.6 ), M, 1 )

# Link function
g<-function(x) pnorm( x )
dg<-function(x) dnorm( x )
gi<-function(x) qnorm( x )

# Cumulant function
a<-function( x ) x*exp( x ) 
da<-function( x ) (2+x)*exp( x ) 
d2a<-function( x ) (3+x)*exp( x )

# Algorithm
# Initial value for b
b<-Matrix( colMeans( X ), N+1, 1 )
# w<-Matrix( runif( M, 0.5, 1 ), M, 1 )
w<-Matrix( 1, M, 1 )

I<-500
for ( i in 1:I ) {
  n<-X %*% b
  u<-apply( n, 2, FUN = g )
  v<-dg( u )
  z<-n + ( y - u ) * v
  W<-Matrix( 0, M, M )
  f<-function( x, ru ) da(x) - ru
  # Employing Newton-Raphson to find theta
  a2<-sapply( u, FUN = function( z ) newtonRaphson( f, x0 = z, ru = z, maxiter = 50, tol = 1e-5 )$root )
  a2<-sapply( a2, FUN = d2a )
  diag( W )<-w / ( a2 * v )
  b0<-b
  b<-solve( a = t( X ) %*% W %*% X, b = t( X ) %*% W %*% z )
  
  cat( paste( '\rRelative error: ', formatC( norm( b - b0 ) / norm( b0 ), digits = 30, format = 'f' ), sep = '' ) )
}

