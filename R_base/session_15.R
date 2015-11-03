# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_15.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


#___________________________________________________________________________________________________
# Variance reduction method applied to the standard log-normal distribution
mlog<-log(1)
sdlog<-log(exp(1))
lmean<-exp( mlog + 0.5 * sdlog^2 )

# Determination of weights
n<-25
q<-seq(0,200,length.out = n)
w<-plnorm( q[2:n] )-plnorm( q[1:(n-1)] )
w<-c( w, 1-plnorm( q[n] ) )
q<-c( q, Inf )

# Simulation
N<-500
M<-1000
X<-matrix( rlnorm( N * M, mlog, sdlog ), N, M )
intfind<-function(x, w, q ) {
  pos<-findInterval( x, q )
  tb<-table( pos )
  ptb<-sapply( pos, FUN = function( x ) which( as.numeric( attr(tb,'dimnames')$pos ) %in% x ) )
  return( w[ pos ] / tb[ptb] )
}
W<-apply( X, 2, FUN = intfind, w, q )
WX<-W*X

# Computation of the mean
wx<-colSums( WX )
x<-colMeans( X )

# Value of the variance
vared<-var( x ) / var( wx )
message( paste( 'Reduccion de varianza: ', vared, sep = '' ) )

#___________________________________________________________________________________________________
# Plots
layout( matrix( c( 1, 1, 2, 2 ), 2, 2 ) )
xs<-unique( round( seq( 0, M, 100 ), 0 ) )
xs<-c( 1, xs[ xs > 0 ] )
ys<-seq( 1.2, 2.2, 0.1 )
plot( 1:M, sort( x ), ylim = c( 1.2, 2.2 ), cex = 0.7, pch = 16, col = 'dodgerblue3', 
      xlab = 'simulaciones', ylab = 'media', xaxt = 'n', yaxt = 'n', 
      panel.first = { 
        axis( 1, at=xs, labels = xs )
        axis( 2, at=ys, labels = ys )
        abline( v = xs, col = 'black', lwd = 1, lty = 3 )
        abline( h = ys, col = 'black', lwd = 1, lty = 3 )
        abline( h = exp( 0.5 ), col = 'orange', lwd = 4 )
      })
points( 1:M, sort( wx ), cex = 0.7, pch = 16, col = 'darkgreen' )

ys<-seq( 0, 0.5, 0.05 )
plot( 1:M, sort(abs(x-lmean)), ylim = c( 0, 0.5 ), cex = 0.7, pch = 16, col = 'dodgerblue3', 
      xlab = 'simulaciones', ylab = 'error', xaxt = 'n', yaxt = 'n',
      panel.first = { 
        axis( 1, at=xs, labels = xs )
        axis( 2, at=ys, labels = ys )
        abline( v = xs, col = 'black', lwd = 1, lty = 3 )
        abline( h = ys, col = 'black', lwd = 1, lty = 3 )
      })
points( 1:M, sort(abs(wx-lmean)), cex = 0.7, pch = 16, col = 'darkgreen', 
        xlab = 'simulaciones', ylab = 'error' )
