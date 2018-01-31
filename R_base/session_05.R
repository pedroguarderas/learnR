# --------------------------------------------------------------------------------------------------
options( stringsAsFactors = FALSE )

# Cargando archivo RData
load( 'RData/cruces.RData' )

# Instalación y carga de librería sqldf.
# SQL en R
# install.packages( 'sqldf' )
library( sqldf )

# --------------------------------------------------------------------------------------------------
# Algunos ejemplos de base de consultas SQL
# sqldf
sub_data1<-sqldf( "select * 
                   from data
                   where mhratio > 95" )
# just R
Rsub_data1<-subset( data, mhratio > 95 )

# --------------------------------------------------------------------------------------------------
# sqldf
sub_data2<-sqldf( "select * 
                   from data
                   where mhratio > 95 and pais like 'E%'" )

# just R
Rsub_data2<-subset( data, mhratio > 95 & grepl( "\\<E", pais  ) )

# --------------------------------------------------------------------------------------------------
# sqldf
sub_data3<-sqldf( "select year, sum( total ) as sum_total, 
                     avg( mujeres ) as m_mujeres,
                     avg( hombres ) as m_hombres
                   from data
                   group by year")

# just R
Rsub_data3<-aggregate( total ~ year, 
                       data = data, 
                       FUN = sum )
aux<-aggregate( cbind( mujeres, hombres ) ~ year, 
                data = data, 
                FUN = mean )
Rsub_data3<-merge( Rsub_data3, aux, by = c( 'year' ) )
   
# --------------------------------------------------------------------------------------------------
# sqldf
sub_data4<-sqldf( "select *
                   from gdp
                   where pais in ( select pais
                                   from data
                                   where total > 10000 )")

# just R
Rsub_data4<-subset( gdp, pais %in% subset( data, total > 10000, select = pais )[,1] )

n<-100
tabla<-data.frame( ing = runif( n, 1000, 2000 ),
                   an_edu = rexp( n ),
                   edad = sample( 20:70, n, replace = TRUE ) )
lm( ing ~ an_edu * edad, data = tabla, subset = an_edu > 3 )
