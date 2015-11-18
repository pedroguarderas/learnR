# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_18.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
# Solving ordinary differential equations with stochastic methods

f<-function( t ) {
  return( cos( 2*pi*t ) * sin( t ) )
}


T0<-0
T1<-5

N<-5000
t<-c( T0, runif( N-2, T0, T1 ), T1 )
t<-sort( t )

u0<-0.5
u<-u0
F<-sapply( t, FUN = f )
U<-0
for( n in 2:N ) {
  u<-c( u, u[n-1] + ( t[n] - t[n-1] ) * F[n]  ) # Euler method
  I<-t[n]-t[1]
  U<-c( U, I * mean( F[1:n] ) ) # Random method
}
U<-U + u0

plot( t, u, type = 'l', lty = 1, lwd = 3, col = 'orange', ylim = c(0.2,0.8) )
points( t, U, type = 'l', lty = 1, lwd = 3, col = 'dodgerblue3' )

