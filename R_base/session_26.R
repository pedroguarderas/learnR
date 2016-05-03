# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_26.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________


# __________________________________________________________________________________________________
# Solving issues with rounding

# Round half away from zero
roundHAFZ<-function( x, n = 2 ) {
  return( sign(x)*round( abs(x) + 0.5*10^(-(n+1)), n ) )
}

formatHAFZ<-function( x, n = 2 ) {
  return( formatC( sign(x)*round( abs(x) + 0.5*10^(-(n+1)), n ), format = 'f', digits = n ) )
}

options(digits=15)

roundHAFZ( 90.625, 2 )
roundHAFZ( 1100.0005, 3 )
formatHAFZ( 1100.0005, 3 )

x<-seq( -10, 10, 0.5 )
y<-sapply( x,FUN = roundHAFZ, 0 )
z<-sapply( x,FUN = round, 0 )

cat(x)
cat(y)
cat(z)

