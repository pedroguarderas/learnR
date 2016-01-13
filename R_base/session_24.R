# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_24.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
# Asakura Oosawa depletion interaction

s<-1.45
N<-5
M<-50000
L<-20

A<-L - 2 * N * s
y<-matrix( runif( N * M, 0, A ), M, N )
y<-t( apply( y, 1, FUN = sort ) )
x<-t( apply( y, 1, FUN = function( x ) x + ( 2 * 0:(N-1) + 1 ) * s ) )
z<-rep( 0, N )

X<-as.vector( x )

Z<-function( N, L, s ) {
  z<-0
  if ( L > 2 * N * s ) {
    z<-( L - 2 * N * s )^N 
  }
  return( z )
}

rho<-function( k, x, N, L, s ) {
  return( choose( N - 1, k ) * Z( k, x - s, s ) * Z( N - 1 - k, L - x - s, s ) )
}

p<-function( x, N, L, s ) {
  return( sum( sapply( 0:(N-1), FUN = rho, x, N, L, s )  ) / Z( N, L, s ) ) 
}

x<-seq(0,L,length.out = 300 )
pr<-sapply( x, FUN = p, N, L, s )

xs<-seq( 0, L, length.out = 21 )
ys<-seq( 0, 0.2, length.out = 11 )
plot( x, pr, col = 'red3', type = 'n', lwd = 3, main = 'Density Asakura Oosawa depletion interaction',
      xlab = 'x', ylab = 'p(x)', xaxt = 'n', yaxt = 'n', xlim = c( 0, L ), ylim = c( 0, 0.2 ),
      panel.first = { 
        axis( 1, at=xs, labels = xs )
        axis( 2, at=ys, labels = ys )
        abline( v = xs, col = 'black', lwd = 1, lty = 3 )
        abline( h = ys, col = 'black', lwd = 1, lty = 3 )
      } )
hist( X, breaks = 250, freq = FALSE, col = 'steelblue', border = 'steelblue3', add = TRUE )
points( x, pr, col = 'darkgreen', type = 'l', lwd = 3 )
