# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_19.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
# Solving Schrödinger radial equation for the hydrogen atom
# Discretization in Bohr radius
R<-25
r<-0
n<-1000
x<-seq(r,R,length.out = n)
h<-(R-r)/(n-1)

Vf<-function( x ) {
  return( 1/sqrt((x^2)) )
}

V<-sapply( x, FUN = Vf )
V[1]<-V[2]
V<-diag( V )

L<-diag( 2, n, n )
for ( i in 1:(n-1) ) {
  L[i,i+1]<--1
  L[i+1,i]<--1
}
L<-(1/(h^2))*L

me<-9.10938356e-31
mp<-1.6726219e-27
mr<-mp*me / ( mp + me )
k<-me/mr
# Hamiltonian definition
H<-0.5 * k * L - V
# Solving problem
S<-eigen( H, symmetric = TRUE )

#___________________________________________________________________________________________________
# Plots
X11()
plot( S$values, col = 'red', pch = 16, cex = 0.7 )


m<-6
cols<-sample( colors()[ grepl('magenta', colors() ) | 
                        grepl('grey', colors() ) ], m )
X11()
plot( x, S$vectors[,n]^2, type = 'l', col = cols[1] )
for ( i in (n-1):(n-m+1) ) {
  points( x, S$vectors[,i]^2, type = 'l', col = cols[n-i+1] )
}

# Checking normalization
summary( colSums( S$vectors^2 ) )
