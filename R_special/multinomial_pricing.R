# __________________________________________________________________________________________________
# 
# author: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: multinomial_pricing.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________
# 

# Multinomial lattice ------------------------------------------------------------------------------
MLattice<-function( N, U, S0 ) {
  S<-list( S0, U * S0 )
  n<-length( U )
  
  for ( t in 3:(N+1) ) {
    M<-n + (t-2)*(n-1)
    K<-M - n + 1
    S[[t]]<-c( U * S[[t-1]][1], U[n] * S[[t-1]][2:K] )
  }
  return( S )
}

# Princing function for the multinomial models -----------------------------------------------------
# Type nomenclature:
# E = european option
# A = american option
# F = futures option
# S = swaps option
# R is the interest rate, that could be a fixed value or a tree with the term structure information
MPrice<-function( S, option, EQ, R, Q, Type = 'E' ) {
  C<-S
  N<-length(S)
  n<-length(Q)
  
  check<-1
  if ( length( R ) > 1 ) {
    ns<-unlist( lapply( S, FUN = length ) ) 
    nr<-unlist( lapply( R, FUN = length ) )
    if ( length( ns ) < length(nr) ) {
      if ( all( ns == nr[1:length(ns)] ) ) check<-2
    } else if ( length( ns ) == length(nr) ) {
      if ( all( ns == nr ) ) check<-2
    } else if ( length( ns ) > ( length(nr) + 1 ) ) {
      if ( all( ns[1:length(nr)] == nr ) ) check<-2
    }
  }
  
  if ( Type == 'E' & check == 1 ) {
    C[[N]]<-sapply( C[[N]], FUN = option )
    for ( t in (N-1):1 ) {
      M<-n + (t-2)*(n-1)
      C[[t]]<-sapply( 1:M, FUN = function(k) EQ( 1/R, Q, C[[t+1]][k:(k+n-1)] )  )
    }
  } else if ( Type == 'E' & check == 2 ) {
    C[[N]]<-sapply( C[[N]], FUN = option )
    for ( t in (N-1):1 ) {
      M<-n + (t-2)*(n-1)
      C[[t]]<-sapply( 1:M, FUN = function(k) EQ( 1 / ( 1 + R[[t]][k] ), Q, C[[t+1]][k:(k+n-1)] )  )
    }
  } else if ( Type == 'A' & check == 1 ) {
    C[[N]]<-sapply( C[[N]], FUN = option )
    for ( t in (N-1):1 ) {
      M<-n + (t-2)*(n-1)
      C[[t]]<-sapply( 1:M, FUN = function(k) max( option( S[[t]][k] ), 
                                                  EQ( 1/R, Q, C[[t+1]][k:(k+n-1)] ) ) )
    }
  } else if ( Type == 'A' & check == 2 ) {
    C[[N]]<-sapply( C[[N]], FUN = option )
    for ( t in (N-1):1 ) {
      M<-n + (t-2)*(n-1)
      C[[t]]<-sapply( 1:M, FUN = function(k) max( option( S[[t]][k] ), 
                                                  EQ( 1 / ( 1 + R[[t]][k] ), Q, C[[t+1]][k:(k+n-1)] ) ) )
    }
  } else if ( Type == 'F' & check == 1 ) {
    C<-S
    for ( t in (N-1):1 ) {
      M<-n + (t-2)*(n-1)
      C[[t]]<-sapply( 1:M, FUN = function(k) EQ( 1 / R, Q, C[[t+1]][k:(k+n-1)] )  )
    }
  } else if ( Type == 'F' & check == 2 ) {
    C<-S
    for ( t in (N-1):1 ) {
      M<-n + (t-2)*(n-1)
      C[[t]]<-sapply( 1:M, FUN = function(k) EQ( 1 / ( 1 + R[[t]][k] ), Q, C[[t+1]][k:(k+n-1)] )  )
    }
  } else if ( Type == 'S' & check == 2 ) {
    C[[N]]<-sapply( C[[N]], FUN = option )
    C[[N]]<-C[[N]] / ( 1 + R[[N]] )
    for ( t in (N-1):1 ) {
      M<-n + (t-2)*(n-1)
      C[[t]]<-sapply( 1:M, FUN = function(k) {
        EQ( 1 / ( 1 + R[[t]][k] ), Q, C[[t+1]][k:(k+n-1)] ) + 
          option( S[[t]][k] ) / ( 1 + R[[t]][k] )
      } )
    }
  }
  return( C )
}


# Example ------------------------------------------------------------------------------------------
s<-0.3
T<-0.25
N<-15
c<-0.01
u<-exp( s * sqrt( T/N))
d<-1/u
r<-0.02
S0<-100
K<-110
R<-exp( r * T / N )
q<-( R - d ) / ( u - d )
Q<-c( 1-q, q )
U<-c( d, u )

# K is global
call<-function( S ) { 
  max( S - K, 0 )
}

put<-function( S ) { 
  max( K - S, 0 )
}

# Equivalent measure
EQ<-function(R,Q,C) {
  return( sum( R * Q * C ) )
}

S<-MLattice( N, U, S0 )
# Pricing american call
Ca<-MPrice( S, call, EQ, R, Q, Type = 'A' )
# Pricing american put
Pa<-MPrice( S, put, EQ, R, Q, Type = 'A' )
