# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_12.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________
# 

#___________________________________________________________________________________________________
# Order statistics
library(animation)

n<-30
N<-10000
X<-matrix( runif( n * N ), N, n )

saveGIF({
  for ( k in 1:n ) {
    a<-k
    b<-n+1-k
    Xord<-apply( X, c(1), FUN = function( x, k ) x[ order( x )[k] ], k )
  
    hist( Xord, probability = TRUE, breaks = 50, col = 'gray71', 
          xlab = parse( text = paste( 'X[', k, ']', sep = '' ) ), ylab = 'Density', 
          main = 'Histogram and Density for Ordered Statistic', 
          axes = FALSE )
    curve( dbeta(x,a,b), add = TRUE, col = 'brown2' )
  }
}, movie.name = "ordered.gif", ani.width = 600, ani.height = 600 )
