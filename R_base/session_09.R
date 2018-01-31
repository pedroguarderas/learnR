# --------------------------------------------------------------------------------------------------
# Generación de variables aleatorias chi cuadrado
n<-10000
X<-rnorm( n, 0, 1 )
Z1<-X^2

G<-function( y, l, a ) {
  return( ( l * exp( -l * y ) * ( l * y )^(a-1) ) / gamma( a ) )
}

l<-1/2
a<-1/2
y1<-seq(0,max(Z1),length.out=200)
g1<-sapply( y1, FUN = G, l, a )

# --------------------------------------------------------------------------------------------------
# Generación de variables aleatorias sumas de chi cuadrados
N<-10000
n<-10
X<-matrix( rnorm( N * n, 0, 1 ), n, N )
X<-X^2
Z2<-colSums( X )

l<-1/2
a<-n/2
y2<-seq(0,max(Z2),length.out=200)
g2<-sapply( y2, FUN = G, l, a )

# --------------------------------------------------------------------------------------------------
# Generación de variables aleatorias medias de chi cuadrados
N<-10000
n<-10
X<-matrix( rnorm( N * n, 0, 1 ), n, N )
X<-X^2
Z3<-colMeans( X )

l<-n/2
a<-n/2
y3<-seq(0,max(Z3),length.out=200)
g3<-sapply( y3, FUN = G, l, a )

# --------------------------------------------------------------------------------------------------
# Generación de gráficos
layout( matrix(c(1,1,2,2,3,3,0,0), 2, 4, byrow = TRUE) )

hist( Z1, col = 'steelblue', breaks = 80, probability = TRUE )
points( y1, g1, type = 'l', col = 'red2', lwd = 3 )

hist( Z2, col = 'steelblue', breaks = 80, probability = TRUE )
points( y2, g2, type = 'l', col = 'red2', lwd = 3 )

hist( Z3, col = 'steelblue', breaks = 80, probability = TRUE )
points( y3, g3, type = 'l', col = 'red2', lwd = 3 )

rm( list = ls () )
gc()

# --------------------------------------------------------------------------------------------------
# Estandarización de una variable aleatoria
n<-10000
u<-3
s<-2
X<-rnorm( n, u, s )
Y<-( X - u ) / s

mean( X )
sd( X )

mean( Y )
sd( Y )

# --------------------------------------------------------------------------------------------------
# Simulación distribución beta
n<-10000
a<-8
b<-2
X<-rbeta( n, a, b )

x<-seq(0,1,length.out=200)
y<-sapply( x, FUN = dbeta, shape1 = a, shape2 = b )
hist( X, col = 'steelblue', breaks = 100, probability = TRUE )
points( x, y, type = 'l', col = 'red2', lwd = 3 )

y<-rgamma( 10000, shape = 8, scale = 2 )
y<-rgamma( 10000, shape = 8, rate = 2 )
mean(y)
var(y)
