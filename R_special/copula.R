#---------------------------------------------------------------------------------------------------
library(MASS)
library(mvtnorm)
library(rgl)

N<-30
p<-0.8
x<-seq(0,1,length.out=N)
y<-seq(0,1,length.out=N)
E<-matrix( c(1,p,p,1), 2, 2 )
Ei<-solve(E)
I<-matrix( c(1,0,0,1), 2, 2 )

# Gaussian Copula
# Density
CopulaG<-function(x,y) {
  z<-pmvnorm( upper=c( qnorm(x), qnorm(y) ), sigma = E )[1]
  return( z )
}

# Distribution
copulaG<-function(x,y) {
  v<-c( qnorm(x), qnorm(y) )
  z<-exp( -0.5 * t(v) %*% (Ei-I) %*% v ) / sqrt( det(E) )
  return( z )
}

# Sample of the copula
M<-5000
X<-apply( rmvnorm( M, sigma = E, method = "chol" ), MARGIN = c(1,2), FUN = pnorm )

z<-matrix(0,N,N)
w<-matrix(0,N,N)
for ( i in 1:N ) {
  for ( j in 1:N ) {
    z[i,j]<-CopulaG( x[i], y[j] )
    w[i,j]<-copulaG( x[i], y[j] )
  }
}

persp3d( x, y, z, col="gold")
persp3d( x, y, w, col="gold")
points3d( X[,1], X[,2], z = 0, pch = 16, cex = 0.7, col = "darkgreen", add = TRUE )
plot( X[,1], X[,2], pch = 16, cex = 0.7, col = "darkgreen" )

