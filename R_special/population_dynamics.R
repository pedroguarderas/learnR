m<-10000
n<-550
t<-seq( 0, 20, length.out = m )
x<-seq( 0, 110, length.out = n )
p0<-1000000*dnorm( x, mean = 20, sd = 22 )
p0<-p0-p0[n]
pw<-rep( 0, m )

d<-exp( c( seq( log( 0.001 ), log( 0.0005 ), length.out = 175 ),
           seq( log( 0.0005 ), log( 0.99 ), length.out = n-175 ) ) )
b<-dnorm( x[2:n], mean = 20, sd = 5 )
p0<-sum( b * p0[2:n] * diff( x ) ) * p0 / p0[1]
b<-p0[1] * b / sum( b * p0[2:n] * diff( x ) )
A<-function( p ) {
  a<-p
  return( a )
}

p<-matrix( 0, m, n )
p[1,]<-p0
p[,n]<-pw

for ( i in 1:(m-1) ) {
  for ( j in 2:(n-1) ) {
    dt<-t[i+1]-t[i]
    dx<-x[j+1]-x[j]
    l<- dt / ( 2 * dx )
    k<- dt^2 / ( 2 * dx^2 )
    # Lax - Wendroff scheme
    p[i+1,j]<-p[i,j] - l * ( A( p[i,j+1] ) - A( p[i,j-1] ) ) +
      k * ( A( p[i,j+1] ) - 2 * A( p[i,j] ) + A( p[i,j-1] ) ) - dt * d[j] * p[i,j]
    # p[i+1,j]<-p[i,j] - l * ( A( p[i,j+1] ) - A( p[i,j-1] ) ) - dt * d * p[i,j]
  }
  p[i+1,1]<-sum( b * p[i+1,2:n] * diff( x ) )
}

X11()
plot( x, p[1,], col = 'red', type = 'l', ylim = c( 0, 1.5 * max( p[1,] ) ), xlim = c( 0, 110 ) )
u<-500
I<-seq( u, m, u )
for ( i in I ) {
  points( x, p[i,], col = 'red', type = 'l' )
}

X11()
l<-apply( p, 1, FUN = function( r ) sum( r * c( diff( x ), x[n]-x[n-1] ) ) )
plot( t, l, col = 'purple', type = 'l', xlab = 't', ylab = 'l' )

library( rgl )
persp3d( t[I], x, p[I,1:n], col = 'gold', alpha = 0.7, xlab = 't', ylab = 'x', zlab = 'l(t,x)' )
