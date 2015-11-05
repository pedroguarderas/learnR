library(Rcpp)

Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
Sys.setenv("PKG_LIBS"="-fopenmp")
code<-sourceCpp( "/home/aju/Development/learnR/R_special/rccpopenmp.cpp",
                 rebuild = TRUE, verbose = TRUE )


x<-runif(200000000)
system.time( sumCPP(x) )
system.time( sum(x*x) )


FI<-ymd("2010-02-01", tz = "ECT")
FF<-ymd("2015-12-31", tz = "ECT")
TR<-ymd("2013-10-20", tz = "ECT")
D<-ymd("1984-07-01", tz = "ECT")
DI<-D
DF<-D
year(DI)<-year(FI)
year(DF)<-year(FF)
Ds<-data.frame( fcht = seq( DI, DF, by = 'year' ) )
Ds<-Ds[ Ds$fcht > FI & Ds$fcht < FF, ]
Ds<-rbind( FI, Ds )
Ds<-c.POSIXlt( DI, Ds, DF )
Ds<-Ds[ order( Ds ) ]

d<-DI
Ing<-ymd("2011-01-07", tz = "ECT")
D<-NULL
while ( d <= DF ) {
  if ( d < Ing ) {
    d<-Ing
  }
  D<-c( D, d )
}


