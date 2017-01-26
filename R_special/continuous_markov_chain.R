# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: continuous_markov_chain.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
# Simulation of continuous Markov chain with finite states
# Instantaneous transition rate structure
U<-list( list( e = c( 2, 3, 4 ), u = c( 0.01, 0.7, 0.2 ), p = c( 0.01 / 0.91, 0.7 / 0.91, 0.2 / 0.91 ) ), 
         list( e = NULL, u = 0, p = 1 ),
         list( e = c( 4 ), u = c( 0.8 ), p = c( 0.8 / 0.8 ) ),
         list( e = c( 1, 3 ), u = c( 0.6, 0.3 ), p = c( 0.6 / 0.9, 0.3 / 0.9 ) ) )

u<-c( 0.91, 0.0, 0.8, 0.9 )

# Initial state
S<-list( list( e = 1, t = NULL ),
         list( e = 3, t = NULL ),
         list( e = 2, t = NULL ),
         list( e = 3, t = NULL ), 
         list( e = 1, t = NULL ), 
         list( e = 3, t = NULL ),
         list( e = 1, t = NULL ) )

N<-length( S )

# Max number of jumps by individual
M<-1e4

# Gillespie algorithm
for ( k in 1:M ) {
  for ( i in 1:N ) {
    l<-length( S[[i]]$e )
    j<-S[[i]]$e[l]
    E<-U[[j]]$e
    P<-U[[j]]$p
  
    if ( length( E ) > 0 ) {
      t<-rexp( n = 1, rate = u[ j ] )
      if ( length( E ) > 1 ) {
        e<-sample( size = 1, x = E, prob = P )
      } else {
        e<-E
      }
      S[[i]]$e<-c( S[[i]]$e, e )
      S[[i]]$t<-c( S[[i]]$t, t )
    }
  }
}
