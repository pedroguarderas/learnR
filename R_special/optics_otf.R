# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: optic_otf.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________

#___________________________________________________________________________________________________
library(RColorBrewer)
library(rgl)

n<-200
x<-seq( -1, 1, length.out = n )

#___________________________________________________________________________________________________
# Image function
If1<-function( x, y ) {
  r<-sqrt( x*x + y * y )
  f<-ifelse( r < 0.1, 1.0, 0.0 )
  return( f )
}

If2<-function( x, y, k ) {
  h<-1.0/(2*k)
  t<- h * (-k:k)
  r<-which( t <= x )
  r<-ifelse( length( r ) > 0, max(r), 1 )
  f<-ifelse(  r %% 2 == 0 & abs(y) <= 0.5, 1.0, 0.0 )
  return( f )
}

f<-function( x, y ) If2( x, y, 9.0 )

#___________________________________________________________________________________________________
# Obstructions
circ<-function( x, y, r ) ifelse( x^2 + y^2 <= r^2, 1.0, 0.0 )
f1<-function( x, y ) 1e-87 * exp( 100 * ( x^2 + y^2 ) ) * circ( x, y, 1.5 )
f2<-function( x, y ) 1e-86 * exp( 100 * ( x^2 + y^2 ) ) * circ( x, y, 1.4 )
LF<-list( f1, f2 )

O<-matrix( 0, n, n )
H<-matrix( 0, n, n )
for( i in 1:n ) {
  for ( j in 1:n ) {
    O[i,j]<-LF[[1]]( x[i], x[j] )
  }
}

for ( k in 2:length( LF ) ) {
  for( i in 1:n ) {
    for ( j in 1:n ) {
      H[i,j]<-LF[[k]]( x[i], x[j] )
    }
  }
  
  O<-convolve( O, H )  
}

OTF<-fft( O )
MTF<-apply( OTF, c(1,2), FUN = Mod )
PTF<-apply( OTF / MTF, c(1,2), FUN = Arg )

I<-matrix( 0, n, n )
for( i in 1:n ) {
  for ( j in 1:n ) {
    I[i,j]<-f( x[i], x[j] )
  }
}

IF<-convolve( I, O )

C<-matrix( 0, n, n )
M<-100
CL<-rainbow( M )
IVF<-as.vector( IF )
t<-seq( min(IVF), max(IVF), length.out = M )
for( i in 1:n ) {
  for ( j in 1:n ) {
    l<-max( which( t <= IF[i,j] ) )
    C[i,j]<-CL[l]
  }
}
rm(i,j,k,l,IVF)
gc()

#___________________________________________________________________________________________________
# Image
nc<-100
image( x, x, IF, col = grey.colors( nc, start = 0, end = 1 ) )
image( x, x, I, col = grey.colors( nc, start = 0, end = 1 ) )

persp3d( x, x, IF, col = C, theta = 30, phi = 30, border = NA, alpha = 0.9 )
persp3d( x, x, I, col = C, theta = 30, phi = 30, border = NA, alpha = 0.9 )

image( x, x, MTF, col = CL )
image( x, x, PTF, col = grey.colors( nc, start = 0, end = 1 ) )
# persp3d( x, x, MTF, col = C )
# persp3d( x, x, PTF, col = C )
