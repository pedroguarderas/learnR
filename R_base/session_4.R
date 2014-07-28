# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_4.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________
# 

#___________________________________________________________________________________________________
# Estudio de la desigualdad de Chebyshev P( | X - E[X] | > t ) <= Var[X] / t^2
m<-1000
n<-200
p<-0.4
x<-rbinom( m, n, p )
t<-5
Ex<-mean(x)
Vx<-var(x)
xmEx<-abs( x - Ex )
xmExgt<-xmEx[ xmEx > t ]
Pr<-length( xmExgt ) / m

# Verificación de la desigualdad a partir de la muestra
print( Pr < Vx / (t^2) )

# Verificación teórica
print( 1 - ( pbinom( Ex + t, n, p ) - pbinom( Ex - t, n, p ) ) < n * p * ( 1 - p ) / (t^2) )

rm( list = ls () )

#___________________________________________________________________________________________________
# Ley de los grandes números y muestreo
e<-0.1
EX<-0.5
VX<-1/12

# P( | mX - E[X] | > e ) <= V[mX]/ n * e^2
# Despejando el tamaño de la muestra

n<-VX / ( ( 1 - ( punif( EX + e ) - punif( EX - e ) ) ) * e^2 )
n<-ceiling(n)

m<-1000
x<-matrix( runif( n * m ), n, m )
Ex<-colMeans( x )
Vx<-apply( x, c(2), FUN = var )
difE<-abs( Ex - EX )
sum( difE > e ) / m

VX / ( n * e^2 )

#___________________________________________________________________________________________________
# Esperanza condicional condicional
m<-400
n<-100
s<-2.0
l<-0.4
x<-rexp( m, rate = l )
J<-NULL
for ( i in 1:m ) {
  J<-rbind( J, data.frame( x = x[i], y = rnorm( n,  2 * sin( pi * x[i] / 2.0 ), s ) ) )
}
rm(i)

# Media de E[y] y media de E[y|x]
Ey<-mean( J$y )
Ey_x<-aggregate( y ~ x, data = J, FUN = mean )
EEy_x<-mean( Ey_x$y )

# Verificando que E[y] y E[E[y|x]] son casi iguales
print( abs( EEy_x - Ey ) )

# Variaza de y
Vy<-var( J$y )
VEy_x<-var( Ey_x$y )
Vy_x<-aggregate( y ~ x, data = J, FUN = var )
EVy_x<-mean( Vy_x$y )

# Verificando que Var[y] y Var[E[y|x]] + E[Var(y|x)] son casi iguales
print( abs( Vy - ( VEy_x + EVy_x ) ) )

# Imágenes
X<-seq( 0, 10, length.out = 50 )
Y<-seq( -15, 15, length.out = 50 )
condist<-function( x, y ) {
  return( dnorm( y, 2 * sin( pi * x / 2.0 ), s ) * dexp( x, l ) )
}
Z<-outer( X, Y, condist )

# pdf('condicional.pdf', width = 20, height = 8 )
layout( matrix(c(1,2,1,2), 2, 2, byrow = TRUE) )
persp( X, Y, Z, theta = 55, phi = 20, col = 'dodgerblue3', border = 'dodgerblue1', 
       xlim = c(0, 10 ), ylim = c(-15,15), zlim = c(0,0.1), ticktype = 'detailed' )
plot( J$x, J$y, col = 'dodgerblue4', pch = 16, cex = 0.5, xlab = 'X', ylab = 'Y',
      xlim = c( 0, ceiling( max(x) ) ), ylim = c( -15, 15 ) )
axis(side = 1, at = seq(0,ceiling( max(x) ),1) )
axis(side = 2, at = seq(-15,15,5) )
# dev.off()
