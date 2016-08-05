# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: curvature.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________

#___________________________________________________________________________________________________
library( ggplot2 )

g<-function( x ) {
  return( c( cos(3*x), sin(2*x) ) )  
}

dg<-function( x ) {
  return( c( -3*sin(3*x), 2*cos(2*x) ) )  
}

d2g<-function( x ) {
  return( c( -9*cos(3*x), 4*sin(2*x) ) )  
}

k<-function( x ) {
  d1<-dg( x )
  d2<-d2g( x )
  nd1<-sum( d1 * d1 )^(3.0/2.0)
  return( ( d1[1] * d2[2] - d1[2] * d2[1] ) / nd1 )
}

R<-function( x ) return( 1.0 / abs( k(x) ) )
C<-function( x) {
  d1<-dg(x)
  d2<-d2g(x)
  n<-sum( d1 * d1 ) / ( d1[1] * d2[2] - d1[2] * d2[1] )
  return( g(x) + n * c( -d1[2], d1[1] ) )
}

n<-300
x<-seq( 0,2*pi,length.out = n)
G<-as.data.frame( t( sapply( x, FUN = g ) ) )
names( G )<-c('x','y')
t<-0.24 * 2 * pi
X<-g( t )
c<-C( t )
r<-R( t )
t<-seq(0,2*pi,length.out = n)
crc<-data.frame( x = r * cos( t ) + c[1],
                 y = r * sin( t ) + c[2] )

p<-ggplot( ) + 
  geom_path( data = G, aes( x = x, y = y ), col = 'dodgerblue4' ) +
  geom_path( data = crc, aes( x = x, y = y ), col = 'orange' ) +
  geom_point( aes( x = c[1], y = c[2] ), col = 'darkgreen' ) +
  geom_point( aes( x = X[1], y = X[2] ), col = 'red3' ) +
  xlim( -2, 2 ) +
  ylim( -2, 2 ) +
  theme_minimal()

plot( p )



