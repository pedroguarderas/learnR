# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: thiele.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
# Thiele equation solver

T<-40
N<-100*10
t<-seq( 0, T, length.out = N )

n<-4
V<-matrix( rep( 0, n ), 1, n, byrow = TRUE )
r<-0.04
b<-c( 10, -4, 3, 8 )

B<-list( list( e = c( 2, 3, 4 ), b = c( 100, -200, 0.04 ) ), 
				 list( e = NULL, b = 0 ),
         list( e = c( 4 ), b = c( 0.05 ) ),
         list( e = c( 1, 3 ), b = c( -0.01, 0.02 ) ) )

U<-list( list( e = c( 2, 3, 4 ), u = c( 0.01, 0.7, 0.2 ), p = c( 0.01 / 0.91, 0.7 / 0.91, 0.2 / 0.91 ) ), 
         list( e = NULL, u = 0, p = 1 ),
         list( e = c( 4 ), u = c( 0.8 ), p = c( 0.8 / 0.8 ) ),
         list( e = c( 1, 3 ), u = c( 0.6, 0.3 ), p = c( 0.6 / 0.9, 0.3 / 0.9 ) ) )

I<-1:4

# Backward solver
for ( k in (N-1):1 ) {
  dt<-t[k+1] - t[k]
  v<-V[1,]
  for ( i in 1:n ) {
    v[i]<-v[i] - 
      dt * ( r * v[i] - b[i] - 
               sum( B[[i]]$b * U[[i]]$u ) - 
               sum( v[B[[i]]$e] * U[[i]]$u ) + 
               v[i] * sum( U[[i]]$u ) )
  }
  V<-rbind( v, V )
}

plot( t, V[,1], type = 'o', cex = 0.7, col = 'orange', pch = 16 )
plot( t, V[,2], type = 'o', cex = 0.7, col = 'orange', pch = 16 )
plot( t, V[,3], type = 'o', cex = 0.7, col = 'orange', pch = 16 )
plot( t, V[,4], type = 'o', cex = 0.7, col = 'orange', pch = 16 )

