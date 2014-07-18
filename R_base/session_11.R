#___________________________________________________________________________________________________
# Forma genérica de verosimilitud
library( flexsurv )

shape<-2.322
rate<-0.018
n<-134128
X<-rgompertz( n , shape, rate )
logl<-log( dgompertz( X, shape = shape, rate = rate ) )
mlogl<-mean( logl )

# Función de entropia
entropia<-function( x ) {
  f<-dgompertz( x, shape, rate )
  return( log(f) * f )
}

# Entropía calculada a través de una integral numérica
integral<-integrate( entropia, 0, 4.91 )

# plot de la función
x<-seq(0,4.91,length.out=1000)
y<-entropia(x)
plot( x, y, col = 'skyblue4', typ = 'l', xlim = c(0,4.91),  ylim = c(-0.4,0) )
