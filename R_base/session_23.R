# __________________________________________________________________________________________________
# 
# autor: Pedro Guarderas
# email: ajusworkopensource@gmail.com
# file: session_23.R
# 
# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License as published by the Free Software Foundation; 
# either version 2 of the License, or (at your option) any later version.
# __________________________________________________________________________________________________



# __________________________________________________________________________________________________
# Using Quandl library
library( Quandl )
library( zoo )

# Raytheon
rtn<-Quandl( "GOOG/NYSE_RTN", type = 'zoo' )
# Lockheed Martin
lmt<-Quandl( "GOOG/NYSE_LMT", type = 'zoo' )
# Bae systems
ba_<-Quandl( "GOOG/LON_BA_", type = 'zoo' )
# Boeing
ba<-Quandl( "GOOG/NYSE_BA", type = 'zoo' )
# Goldman Sachs
gs<-Quandl( "GOOG/NYSE_GS", type = 'zoo' )
# IBM
ibm<-Quandl( "YAHOO/IBM", type = 'zoo' )
# Nvidia
nvda<-Quandl( "GOOG/NASDAQ_NVDA", type = 'zoo' )
# ThyssenKrupp
tka<-Quandl( "GOOG/FRA_TKA", type = 'zoo' )
# Cray
cray<-Quandl( "YAHOO/CRAY", type = 'zoo' )
# AccelorMittal
mt<-Quandl( "GOOG/NYSE_MT", type = 'zoo' )

# Bananas price
bn<-Quandl( "ODA/PBANSOP_USD", type = 'zoo' )

iro<-Quandl( "ODA/PIORECR_USD", type = 'zoo' )

ura<-Quandl( "ODA/PURAN_USD", type = 'zoo' )

plot( rtn )
plot( lmt )
plot( ba_ )
plot( ba )
plot( gs )
plot( ibm )
plot( nvda )
plot( tka )
plot( cray )
plot( mt )
plot( bn )

# Ecuador
# Oil production
ecuoil<-Quandl("EIA/IES_5_53_1_ECU",type = 'zoo')
# Oil proved reserves
ecuoilres<-Quandl("BP/OIL_RESERVES_ECU",type = 'zoo')
# Hydroelectric consumption
ecuhyd<-Quandl("BP/HYDRO_CONSUM_ECU",type = 'zoo')
# Gas consumption
ecugas<-Quandl("BP/GAS_CONSUM_ECU",type = 'zoo')
# Sovereign Risk Data
# No good data, bad information
ecurisk<-Quandl("PSCS/SRD_ECU",type = 'zoo')

plot( ecuoil )
plot( ecuhyd )
plot( ecugas )
plot( ecurisk )


str( mt )
hist( diff( lmt[,1] ), breaks = 30, probability = TRUE )

hist( diff( gs[,1] ), breaks = 50, probability = TRUE )

hist( diff( cray[,1] ), breaks = 50, probability = TRUE )

mt_cq<-diff( mt[,1] )
mt_q<-quantile( mt_cq, probs = 0.99, na.rm = TRUE )[[1]]
mt_cq<-mt_cq[ mt_cq <= mt_q  & mt_cq >= -mt_q ]
hist( mt_cq, breaks = 50, probability = TRUE )

dif_iro<-diff( iro[,1] )
hist( dif_iro, breaks = 50, probability = TRUE )

dif_bn<-diff( bn[,1] )
hist( dif_bn, breaks = 50, probability = TRUE )

dif_ura<-diff( ura[,1] )
hist( dif_ura, breaks = 50, probability = TRUE )
