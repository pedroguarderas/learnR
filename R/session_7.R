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
# Probabilidad condicional
m<-200
n<-200
s<-2.0
l<-0.8
x<-rexp( m, rate = l )
J<-NULL
for ( i in 1:m ) {
  J<-rbind( J, data.frame( x = x[i], y = rnorm( n, x[i], s ) ) )
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
plot( J$x, J$y, col = 'darkgreen', pch = 15, cex = 0.5, xlab = 'x', ylab = 'y',
      xlim = c( 0, 7 ), ylim = c( -15, 15 ) )
X<-seq( 0, 7, length.out = 30 )
Y<-seq( -15, 15, length.out = 30 )
condist<-function( x, y ) {
  return( dnorm( y, x, s ) * dexp( x, l ) )
}
Z<-outer( X, Y, condist )
persp( X, Y, Z, theta = 60, phi = 30, col = 'dodgerblue3', border = 'dodgerblue1', box = FALSE,
       xlim = c( 0, 7 ), ylim = c( -15, 15 ), zlim = c( 0, 0.2 )  )

#___________________________________________________________________________________________________
Chi<-function(z) return( exp( -0.5 * z ) / sqrt( 2 * pi * z ) )

