# Expectation maximization -------------------------------------------------------------------------
# EM

X<-c( rlnorm( 3000, 3, 1.5 ), rlnorm( 5000, 5, 1.6 ) )
X<-sample( X, size = length( X ) )

# 1
p1<-c( 4, 3 )
p2<-c( 7, 2 )
G<-c( rep( 'A', 4000 ), rep( 'B', 4000 ) )

k<-1
while( k <= 100 ) {
  # 2
  r1<-sum( G == 'A' ) / length( G )
  r2<-sum( G == 'B' ) / length( G )
  Pg1<-dlnorm( X, p1[1], p1[2] )
  Pg2<-dlnorm( X, p2[1], p2[2] )
  P1<-Pg1 * r1 / ( Pg1 * r1 + Pg2 * r2 )
  P2<-Pg2 * r2 / ( Pg1 * r1 + Pg2 * r2 )
  G<-ifelse( P1 > P2, 'B', 'A' )
  
  # 3
  p1<-c( mean( log( X[ G == 'A' ] ) ), sd( log( X[ G == 'A' ] ) ) )
  p2<-c( mean( log( X[ G == 'B' ] ) ), sd( log( X[ G == 'B' ] ) ) )
  # p1<-c( mean( X[ G == 'A' ] ), sd( X[ G == 'A' ] ) )
  # p2<-c( mean( X[ G == 'B' ] ), sd( X[ G == 'B' ] ) )
  
  # 4
  k<-k+1
}

p1
p2

hist( X, breaks = 50, probability = TRUE )
hist( X[ G == 'A' ], breaks = 50, probability = TRUE )
hist( X[ G == 'B' ], breaks = 50, probability = TRUE )

n1<-length( X[ G == 'A' ] )
n2<-length( X[ G == 'B' ] )
U1<-plnorm( sort( X[ G == 'A' ] ), p1[1], p1[2] )
U2<-plnorm( sort( X[ G == 'B' ] ), p2[1], p2[2] )
plot( seq( 0,1,length.out = n1 ), U1, col = 'midnightblue', 
      type = 'l', lwd = 3, ylim = c( 0, 1 ), xlab = 'X', ylab = 'Q' )
points( seq( 0,1,length.out = n2 ), U2, col = 'olivedrab3', type = 'l', lwd = 3 )
