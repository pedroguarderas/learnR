# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_3.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________
# 

#___________________________________________________________________________________________________
# Distribución normal multivariada
multnorm<-function( x, u, E ) {
  n<-length(x)
  return( (2*pi)^( -0.5 * n ) ) * sqrt( det(E) ) * exp( -0.5 * t( x - u ) %*% E %*% ( x - u ) ) )
}

#___________________________________________________________________________________________________
# Distribución de pareto
pareto<-function( x, a, xm ) {
  par<-NULL
  if ( x >= xm ) {
    par<-( a * xm^a ) / ( x^(a+1) )
  } else {
    par<-0
  }
  return( par )
}

#___________________________________________________________________________________________________
# Subsets en data frames
options( stringsAsFactors = FALSE )

fchini<-as.Date('2011-01-01')
fchfin<-as.Date('2014-01-01')
seqfch<-seq( fchini, fchfin, by = 'day' )
n<-20
D<-data.frame( nom = sample( c('Andrea','Juan','Luis','María','Daniel'), n, replace = TRUE ),
               val = sample( c(1:10,NA), n, replace = TRUE ),
               fch = sample( seqfch, n, replace = TRUE ) )

#___________________________________________________________________________________________________
# Expresiones regulares
grep( '(ab)', c( 'acb', 'abc', 'aabc', 'acabcb', 'aaac' ) )

grepl( '(ab)', c( 'acb', 'abc', 'aabc', 'acabcb', 'aaac' ) )

grepl( '(ab)|(ef)', c( 'acb', 'abc', 'aabc', 'acabcb', 'aaac', 'ef' ) )

grepl( '(ab)|((ef){2})', c( 'acb', 'abc', 'aabc', 'acabcb', 'aaac', 'ef', 'efefcd', 'efefef' ) )

grepl( '(María){~2}', c( 'María', 'MaríA', 'MArIA', 'PEDRO' ) )

grepl( '(María){~5}', c( 'María', 'MaríA', 'MArIA', 'PEDRO' ) )

# Maching no exacto
gsub( '(María){~3}', 'MARIA', c( 'María', 'MaríA', 'MArIA', 'PEDRO' ) )

#___________________________________________________________________________________________________
# Aplicación de funciones sobre vectores y data.frames
prctj<-function( x, fi, ff ) {
  return( as.numeric( x - fi ) / as.numeric( ff - fi ) )
}

#___________________________________________________________________________________________________
# Función sapply
D$prctj<-sapply( D$fch, FUN = prctj, fi = fchini, ff = fchfin )

#___________________________________________________________________________________________________
# Función apply
D<-matrix( rnorm( 40, 2, 1 ), 10, 4 )

# Desciación típica por filas
SD<-apply( D, c(1), FUN = sd )

# Media por columnas
MD<-apply( D, c(2), FUN = mean )
