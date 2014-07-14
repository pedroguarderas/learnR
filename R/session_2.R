#___________________________________________________________________________________________________
# Simulación de regresión
n<-100
a<-2
b<-3
e<-rnorm( n, 0, 400 )
X<-matrix( 0, n, 2 )
X[,1]<-runif( n, 1000, 2000 )
X[,2]<-runif( n, 2000, 3000 )
x1<-sort( X[,1] )
x2<-sort( X[,2] )
Y<-a * X[,1] + b * X[,2] + e
ChX<-chol( t(X) %*% X )
B<-chol2inv( ChX ) %*% t(X) %*% Y

#___________________________________________________________________________________________________
# Tipos de variables en R
# tipo entero
x<-seq(0,100,10) # generación de secuencia de 0 hasta el 100 de 10 en 10
typeof(x)

# conversión entero
x<-as.integer(x)
typeof(x)

#___________________________________________________________________________________________________
# Tipo double
x<-seq( -1, 1, length.out = 10 ) # escogiendo longuitud de la secuencia en R
length(x)
typeof(x)

#___________________________________________________________________________________________________
# tipo Date
x<-as.Date("2014-12-20")
typeof(x)
class(x) # clase del tipo

# Leer fechas con un formato
x<-as.Date( "01-01-2013", format = "%d-%m-%Y" )  
typeof(x)
class(x)

# Conversión a tipo numérico
as.numeric(x)

format( x, "%Y") # extrayendo solo el año, como character

#___________________________________________________________________________________________________
# Data frames
# Es la estructura más utilizada en el tratamiento de datos
D<-data.frame( x = 1:20, y = seq( -1, 1, length.out = 20 ) )

typeof(D)
class(D)

# acceder elementos por columas
D[,1]
D$x
D$y

# Ejemplo
D<-data.frame( nom = sample( c('Andrea','Juan','Luis','María'), 100, replace = TRUE ),
               val = sample( c(1:10,NA), 100, replace = TRUE ) )

# Número de filas
nrow(D)

# Número de columnas
ncol(D)

# Cabeza del data.frame
head(D)

# Cola del data.frame
tail(D)

# Vista del data.frame
View(D)

# Test lógico si es data frame
is.data.frame(D)

# Extracción de un subconjunto
d<-D[ D$nom == 'Andrea' | D$nom == 'Luis', ]

# Adición de una columna
D$otr<-seq( 1:100 )

# Eliminación de una columna
D$otr<-NULL

# Anclando columnas del data.frame al espacio de trabajo
attach(D)

# Quitando columnas del data.frame en el espacio de trabajo
detach(D)

# Extracción de vector lógico con filas duplicadas
duplicated(D)

# Estracción de registros únicos
d<-unique(D)

# Nombres de columnas
names(D)
colnames(D) 

# Nombres de filas
rownames(D)

# Eliminando nombre de filas
rownames(d)<-NULL

# Vector lógico de filas completas en donde no hay valores faltantes
complete.cases(D)

# Extracción filas completas
C<-D[ complete.cases(D), ]

#___________________________________________________________________________________________________
# Inicio programación estructurada
# Definición de funciones

cuad<-function( x ) {
  return( x * x )
}

# Distribución de probabilidad de Dirichlet, para n variables soble el simplex
pdfDirichlet<-function( x, a ) {
  return( (  gamma( sum(a) ) / prod( gamma(a) ) ) * prod( x^(a-1) ) )
}

