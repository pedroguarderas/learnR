# --------------------------------------------------------------------------------------------------
# Order statistics
library(animation)

n<-30
N<-10000
X<-matrix( runif( n * N ), N, n )

# --------------------------------------------------------------------------------------------------
# Creating animation
saveGIF({
  for ( k in 1:n ) {
    a<-k
    b<-n+1-k
    Xord<-apply( X, c(1), FUN = function( x, k ) x[ order( x )[k] ], k )
  
    hist( Xord, probability = TRUE, breaks = 50, col = 'gray71', 
          xlab = parse( text = paste( 'X[(', k, ')]', sep = '' ) ), ylab = 'Density', 
          main = 'Histogram and Density for Ordered Statistic', 
          axes = FALSE )
    curve( dbeta(x,a,b), add = TRUE, col = 'brown2' )
  }
}, movie.name = "ordered.gif", ani.width = 600, ani.height = 600 )

rm( list = ls() )
gc()

# --------------------------------------------------------------------------------------------------
# Probability plots
n<-400
X<-rnorm( n )
Y<-rbeta( n, 1, 2 )
Xord<-sort( X )
Yord<-sort( Y )
FXord<-pnorm( Xord )
FYord<-pnorm( Yord )

E<-1:n / ( n + 1 )
plot( E, FXord, cex = 0.5, col = 'purple4', pch = 1, xlab = 'E', ylab = '', 
      main = expression(X[(1)] <={ X[(2)] <= { cdots <= X[(n)] }}), axes = FALSE )
points( E, FYord, cex = 0.5, col = 'cyan3', pch = 1 )
abline( 0, 1, col = 'green3', lwd = 3 )
axis( side = 1, at = seq(0,1,0.1) )
axis( side = 2, at = seq(0,1,0.1) )
box()
