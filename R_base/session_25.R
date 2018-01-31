# --------------------------------------------------------------------------------------------------
# Brownian bridge

N<-1000
L<-1
t<-seq( 0, L, length.out = N )
B<-sqrt( diff(t,1) ) * rnorm(N-1)
B<-cumsum( c( 0, B ) )

X<-B-B[N] * ( t - t[1] ) / ( t[N] - t[1] )

plot( t, X, col = 'red', type = 'l', lwd = 2, 
      panel.first = { abline( h = 0, col = 'orange', lwd = 2 ) } )

