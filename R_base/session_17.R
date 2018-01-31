# --------------------------------------------------------------------------------------------------
# Simulation of mathematical reserves
# Only with jump process
# B(t) = \sum_{0<s<=t} \delta * B(s) 
# dB(t) = \sum_{0<s<=t} B(s) dirac(s)


# Time grid
n<-500
T<-30
t<-seq(0,T,length.out = n)

# Interest rate constant
r<-0.01

# Discrete jump process
# its differential is a sum of Dirac measures times the value
N<-100
l<-0.1

mlog<-log(1000)
sdlog<-0.5

# Number of simulations
S<-100

# Operative cost
b<-15000

D<-148


# Mathematical reserves Y[(t,T]] where t is every time in the grid
Y<-matrix( 0, n, S )

for ( s in 1:S ) {
  tau<-rexp( N, rate = l )
  tau<-sort( tau[ tau <= T ] )
  M<-length( tau )
  B<-rlnorm( M, meanlog = mlog, sdlog = sdlog )
  for ( i in 1:n ) {
    k<-which( tau <= t[i] )
    Y[i,s]<-sum( B[k] * exp( -r * ( tau[k] - t[1] ) ) ) - 
      b * exp( -r * ( t[i] - t[1] ) ) / r +
      D * N * exp( -r * ( t[i] - t[1] ) ) / r 
  }
}

# Expected mathematical reserve
V<-rowMeans( Y )

# Plots
cols<-sample( colors()[ grepl( 'green', colors() ) ], S, replace = TRUE )
ymax<-(round( max( Y ), 0 ) %/% 10 + 1)*10
ymin<-(round( min( Y ), 0 ) %/% 10 - 1)*10
xs<-round( seq( 0, T, T/10 ), 2 )
ys<-round( seq( ymin, ymax, (ymax-ymin)/10 ), 1 )
plot( t, Y[,1], ylim = c( ymin, ymax ), type = 's', lwd = 1, col = cols[1], 
      xlab = 'time t', ylab = 'reserve Y((t,T])', xaxt = 'n', yaxt = 'n', 
      panel.first = { 
        title( main = 'Simulations of Y((t,T]) and E[Y((t,T])]' )
        axis( 1, at=xs, labels = xs )
        axis( 2, at=ys, labels = ys )
        abline( v = xs, col = 'black', lwd = 1, lty = 3 )
        abline( h = ys, col = 'black', lwd = 1, lty = 3 )
      })
for ( s in 2:S ) {
  points( t, Y[,s], type = 's', lwd = 1, col = cols[s] )
}
abline( h = 0, col = 'orange', lwd = 2, lty = 1 )
points( t, V, type = 'l', lwd = 2, col = 'red2' )
