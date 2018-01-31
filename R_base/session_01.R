# --------------------------------------------------------------------------------------------------
# autor: Pedro Guarderas
# email: pedro.felipe.guarderas@gmail.com
# --------------------------------------------------------------------------------------------------

# Tutorial básico de R
# R is un lenguage de programación libre y un entorno para estadística computacional.
# R is una implementación del lenguage de programación S.
# R fue creado por Ross Ihaka y Robert Gentleman en la Universidad de Auckland en Nueva Zelanda y 
# actualmente es desarrollado por el "R Development Core Team"
# R está escrito principalmente en C, Fortran y R
# R está libremente disponible bajo la licencia Public General GNU
# R posee un interpretador de línea de comandos
# R posee más de 5300 paquetes disponibles
# El sitio ofical de R es http://www.r-project.org/

# R como lenguage es sensible a los casos, es decir A no# es lo mismo que a
# La palabra "gato" no es la misma que "Gato" o "GaTO", etc.

# Declarar variable con nombre x y valor 12
x<-12

# Borrar variable x
rm(x)

# Declarar un variable y como vector con los valore [1 2 1 2]
y<-c(1,2,1,3)

# Multiplicar cada valor de y por 3 y guardarlos en una variable
# con nombre x
x<-3*y

# Acceder al 2 "segundo" elemento en el vector y
y[2]

# Acceder al mismo tiempo a los elementos 1 y 4 de y
y[c(1,4)]

# Contar cuantos elementos hay en y. Longitud de y
length(y)

# Sumar los valores en y
sum(y)

# Producto de los elementos de y, lo almacenaos en una variable
# con nombre py
py<-prod(y)

# Generar una secuacia de números del 5 al 10 y almacenarlos
# la variable de nombre z
z<-5:10

# Concatenar vectores
# concatenamos y y z en un variable con nombre w
w<-c(y,z)

# Los caracteres en R se escriben entre comillas
# variable de nombre Ar con valor "casa"
Ar<-"casa"

# Vector con caracteres  "A","E","I","O","U"
v<-c("A","E","I","O","U")

# Concatenación de caracteres "A" y "B" para formar
# la palabra "AB"
# ojo sep es el caracter que separa a "A" de "B", en este caso el
# caracter vacío ""
w<-paste( "A", "B", sep="" )

# --------------------------------------------------------------------------------------------------
# Comparaciones lógicas
# Retornan verdadero "TRUE" o falso "FALSE"

# negación lógica
!TRUE
!FALSE

# igualdad
1 == 2

# igualdad para vectores, solo con la misma dimensión
# se compara elemento por elemento
a<-c(1,2,4)
b<-c(1,2,3)
a == b

# mayor que
1 > 2
2 > 1
a > b

# menor que
3 < 4
6 < 6 # false 6 es igual a 6, pero no más grande
a < b

# mayor o igual
3 >= 2
5 >= 5
a >= b

# --------------------------------------------------------------------------------------------------
# Estructuras de control
# if
# si no se satisface la expresión lógica no entra en el if
# ejemplos

x<-NULL # damos valor nulo a x para iniciar
if ( 1 > 2 ) {
  x<-2
}
# imprimir x
print(x)

if ( 1 < 2 ) {
  x<-3
}
print(x)

# for
# el bucle necesita un argumento indice

# llenamos a
a<-c(1,1,2,2,3,3)

# para cada i en a
for ( i in a ) {
  print( 2*i ) # imprime el valor que toma i multiplicado por 2
}

# --------------------------------------------------------------------------------------------------
# matrices
# matriz A con ceros de dimension 2 x 2
A<-matrix( 0, 2, 2 )

# matriz echa a partir de un vector
# se toman el vector y se llena columna por columna
# el vector debe terner la cantidad de elementos de la matriz esperada
v<-c(1,0,0,2,1,0) # longitud 6
B<-matrix( v, 3, 2 ) # matriz de 3x2, total de elementos es 6

# también se puede hacer una matriz llenando por filas
C<-matrix( v, 3, 2, byrow=TRUE )

