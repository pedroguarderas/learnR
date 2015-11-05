# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_17.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


#___________________________________________________________________________________________________
# Simulation of mathematical reserves
# Only with jump process
# B(t) = \sum_{0<s<=t} \delta * B(s) 
# dB(t) = \sum_{0<s<=t} B(s) dirac(s)


# Time grid
n<-500
T<-1
t<-seq(0,T,length.out = n)

# Interest rate constant
r<-0.01

# Discrete jump process
# its differential is a sum of Dirac measures times the value
N<-100
l<-3

mlog<-0.05
sdlog<-0.5

# Number of simulations
S<-100

# Mathematical reserves Y[(t,T]] where t is every time in the grid
Y<-matrix( 0, n, S )

for ( s in 1:S ) {
  tau<-rexp( N, rate = l )
  tau<-sort( tau[ tau <= 1 ] )
  M<-length( tau )
  B<-rlnorm( M, meanlog = mlog, sdlog = sdlog )
  for ( i in 1:n ) {
    k<-which( tau >= t[i] )
    Y[i,s]<-sum( B[k] * exp( -r * ( tau[k] - t[i] ) ) )
  }
}

# Expected mathematical reserve
V<-rowMeans( Y )

# Plots
cols<-sample( colors()[ grepl( 'green', colors() ) ], S, replace = TRUE )
ymax<-(round( max( Y[1,] ), 0 ) %/% 10 + 1)*10
xs<-round( seq( 0, T, T/10 ), 2 )
ys<-round( seq( 0, ymax, ymax/10 ), 1 )
plot( t, Y[,1], ylim = c( 0, ymax ), type = 'l', lwd = 1, col = cols[1], 
      xlab = 'time t', ylab = 'reserve Y((t,T])', xaxt = 'n', yaxt = 'n', 
      panel.first = { 
        title( main = 'Simulations of Y((t,T]) and E[Y((t,T])]' )
        axis( 1, at=xs, labels = xs )
        axis( 2, at=ys, labels = ys )
        abline( v = xs, col = 'black', lwd = 1, lty = 3 )
        abline( h = ys, col = 'black', lwd = 1, lty = 3 )
      })
for ( s in 2:S ) {
  points( t, Y[,s], type = 'l', lwd = 1, col = cols[s] )
}
points( t, V, type = 'l', lwd = 2, col = 'red2' )
