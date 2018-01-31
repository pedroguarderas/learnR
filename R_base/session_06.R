# --------------------------------------------------------------------------------------------------
# Obtención del lugar de trabajo
getwd()

# --------------------------------------------------------------------------------------------------
# Creando un directorio para guardar datos
dir.create( path = 'RData' )

# --------------------------------------------------------------------------------------------------
# Creando un data.frame para ser guardado
options( stringsAsFactors = FALSE )
fchini<-as.Date('2010-10-01')
fchfin<-as.Date('2012-10-31')
n<-100

fchs<-sample( seq( fchini, fchfin, by = 'day' ), n, replace = TRUE )
vals<-sample( c( 1:3, NA ), n, replace = TRUE )
cods<-paste( 'E', sample( 10000:10500, n, replace = TRUE ), sep = '' )

D<-data.frame( cod = cods, fch = fchs,  val = vals  )

save( D, file = 'RData/valores.RData' )

rm( list = ls() )

# --------------------------------------------------------------------------------------------------
# Cargando datos guardados
load( 'RData/valores.RData' )

# --------------------------------------------------------------------------------------------------
# Guardando en un archivo separado por comas 
# Comma separated value "csv"
dir.create( path = 'reportes' )
write.csv( D, file = 'reportes/valores.csv' )
write.csv( D, file = 'reportes/valores.csv', row.names = FALSE )
write.csv( D, file = 'reportes/valores.csv', row.names = FALSE, na = "" )

# --------------------------------------------------------------------------------------------------
# Cargando librería para lectura de archivos excel
library(xlsx)

options(stringsAsFactors=FALSE)

# Lectura del primer archivo
data<-read.xlsx(file = 'datos/1a.xls', 
                sheetIndex = 1, 
                startRow = 5, 
                endRow = 234, 
                colIndex = c(1,2,4,5,6,8))

colnames(data)<-c('pais', 'year', 'total', 'mujeres', 'hombres', 'mhratio')

# Algunas subconsultas básicas
sdata<-data[data$mhratio > 100,]
sdata<-sdata[order(sdata$mhratio),]
ndata<-subset( data, mhratio > 100)
rownames(sdata)<-NULL

# Lectura del segundo archivo
gdp<-read.xlsx(file = 'datos/5a.xls', 
                sheetIndex = 1, 
                startRow = 4, 
                endRow = 224, 
                colIndex = c(1,2,3,6,7,8))
colnames(gdp)<-c('pais', 'year', 'gdp', 'total', 'hombres', 'mujeres')

# Preparación y corrección de datos
gdp$total<-gsub( "…", NA, gdp$total )
gdp$total<-as.numeric( gdp$total )
gdp$hombres<-as.numeric( gdp$hombres )
gdp$mujeres<-as.numeric( gdp$mujeres )

# --------------------------------------------------------------------------------------------------
# "Merge" Cruce de datos

# Cruce por país
cruce1<-merge( data, gdp,
               by.x = c( 'pais' ),
               by.y = c( 'pais' ),
               all = FALSE, 
               sort = TRUE )

# Cruce por país y año
cruce2<-merge( data, gdp,
               by.x = c( 'pais', 'year' ),
               by.y = c( 'pais', 'year' ),
               all = FALSE, 
               sort = TRUE )

# Guardando cruces en un archivo tipo RData
save( data, gdp, cruce1, cruce2, file = 'RData/cruces.RData' )

# Generando un reporte en excel de los cruces
if ( !file.exists('reportes') ) {
  dir.create( path = 'reportes' )
}

write.xlsx( x = cruce1, file = 'reporte/cruce.xlsx', sheetName = 'cruce1', showNA = FALSE, 
            row.names = FALSE )
write.xlsx( x = cruce2, file = 'reporte/cruce.xlsx', sheetName = 'cruce2', showNA = FALSE, 
            row.names = FALSE, append = TRUE )
