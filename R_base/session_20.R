# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_20.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
# Solving Schrödinger radial equation for the hydrogen atom
# Discretization in Bohr radius
library( Matrix )

Xl<-c( -25, 25 )
Yl<-c( -25, 25 )
n<-c( 50, 50 )
N<-n[1]*n[2]
x<-seq( Xl[1],Xl[2],length.out = n[1])
y<-seq( Yl[1],Yl[2],length.out = n[2])
h<-c( (Xl[2]-Xl[1])/(n[1]-1), (Yl[2]-Yl[1])/(n[2]-1) )

Vf<-function( x ) {
  return( 1/sqrt( x[1]^2 + x[2]^2 ) )
}

G<-expand.grid( x, y )
G<-G[,2:1]
V<-apply( G, 1, FUN = Vf )
# V[1]<-V[2]
V<-Diagonal( N, V )

me<-9.10938356e-31
mp<-1.6726219e-27
mr<-mp*me / ( mp + me )
k<-me/mr


H<-Diagonal( N, 2 * ( 1/(h[1]^2) + 1/(h[2]^2) ) )
for ( i in 1:(N-1) ) {
  H[i,i+1]<--1/(h[1]^2)
  H[i+1,i]<--1/(h[1]^2)
}
for ( i in 1:(N-n[2]) ) {
  H[i,i+n[2]]<--1/(h[2]^2)
  H[i+n[2],i]<--1/(h[2]^2)
}

# Hamiltonian definition
H<-0.5 * k * H - V
# Solving problem
S<-eigen( H, symmetric = TRUE )

#___________________________________________________________________________________________________
# Plots
X11()
plot( S$values, col = 'red', pch = 16, cex = 0.7 )


library(rgl)
P<-S$vectors[,2470]^2
P<-matrix( P, n[1], n[2] )
persp3d( x = x, y = y, z = P, col = 'gold', alpha = 0.9 )

# Checking normalization
summary( colSums( S$vectors^2 ) )
O<-t(S$vectors) %*% S$vectors
