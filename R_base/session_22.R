# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_22.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


#___________________________________________________________________________________________________
# Black - Scholes option valuation

S<-900 # current price of the underlying stock 
K<-950 # trike price
sigma<-0.22 # volatility
r<-0.02 # risk-free rate
u<-0.02 
T<-0.25 # maturity time
t<-0 # inicial time

d1<-( log( S / K ) + ( r + 0.5 * (sigma^2) ) * ( T - t ) ) / ( sigma * sqrt( T - t ) )
d2<-d1 - sigma * sqrt( T - t )

# Call option
C<-S * pnorm( d1 ) - K * exp( -r * ( T - t ) ) * pnorm( d2 )

# Put option
P<-K * exp( -r * ( T - t ) ) * pnorm( -d2 ) - S * pnorm( -d1 )

print( C )
print( P )

#___________________________________________________________________________________________________
# Monte-Carlo method for Black-Scholes
N<-1000
time<-seq(t,T,length.out=N)

M<-1000
D<-NULL
for ( j in 1:M ) {
  W<-cumsum( c( 0, sqrt( diff( time ) ) * rnorm( N - 1 ) ) )
  St<-S * exp( sigma * ( W - W[1] ) + ( r - 0.5 * (sigma^2) ) * ( time - time[1] ) )
  D<-c( D, St[N] )
}

C2<-as.vector( D - K )
C2<-sapply( C2, FUN = function( x ) max(x,0) )
C2<-exp( -r * ( T - t ) ) * mean( C2 )

P2<-as.vector( K - D, )
P2<-sapply( P2, FUN = function( x ) max(x,0) )
P2<-exp( -r * ( T - t ) ) * mean( P2 )

print( C2 )
print( P2 )

#___________________________________________________________________________________________________
# Finite Differences
library( Matrix )

n<-1000
m<-500
XM<-10
Xm<--10

th<-0.5
dt<-(T-t)/(m-1)
dx<-(XM-Xm)/(n-1)

# Discretization first derivative
D<-Diagonal( n, 0 )
for ( i in 1:(n-1) ) {
  D[i,i+1]<- 1
  D[i+1,i]<- -1
}
D<-( ( 0.5 * sigma^2 - r ) / ( 2 * dx ) ) * D

# Discretization second derivative
L<-Diagonal( n, 2 )
for ( i in 1:(n-1) ) {
  L[i,i+1]<- -1
  L[i+1,i]<- -1
}
L<-0.5 * ( ( sigma / dx )^2 ) * L

# Discretization 0 degree term
R<-Diagonal( n, r )

# Operator discretization
A<- L + D + R
I<-Diagonal( n, 1 )
B<-solve( ( I + th * dt * A ), I )
A<-B %*% ( I - ( 1 - th ) * dt * A )
u0<-sapply( exp( Xm + 1:n * dx ) - K, FUN = function( x ) max(x,0) )
u<-u0

for ( j in 1:m ) {
  u<-A %*% u
}

j<-floor((log(S)-Xm)/dx)
u[j]
u[j-1]
u[j+1]