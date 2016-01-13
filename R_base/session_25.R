# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_25.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
# Brownian bridge

N<-1000
L<-1
t<-seq( 0, L, length.out = N )
B<-sqrt( diff(t,1) ) * rnorm(N-1)
B<-cumsum( c( 0, B ) )

X<-B-B[N] * ( t - t[1] ) / ( t[N] - t[1] )

plot( t, X, col = 'red', type = 'l', lwd = 2, 
      panel.first = { abline( h = 0, col = 'orange', lwd = 2 ) } )

