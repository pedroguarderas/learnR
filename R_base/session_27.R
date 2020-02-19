# Examples data.table
library( data.table )

# Generating data.table
n <- 100
m <- 10
N <- m * n
D <- data.table( id = rep( 1:n, each = m ),
                 x = rnorm( N ),
                 y = sample( c( 'a', 'b', 'c' ), size = N, replace = TRUE ),
                 z = rgamma( N, shape = 2 ) )

# Subsets ------------------------------------------------------------------------------------------
# Selection by rows
I <- sample( 1:N, size = 30, replace = FALSE )
D1 <- D[ I ]

# Selection colums
D2 <- D[ , list( x, z ) ]

# Selection x values
D3 <- D[ x >= 2 ] 

# Selection y values
D4 <- D[ y == 'a' ]

# Operations ---------------------------------------------------------------------------------------
# Adding colums
D[ , u := x * z ]
D[ , v := pmax( x, z ) ]
D[ , z_mean := mean( z ), by = list( y ) ]

# Reshaping
D5 <- dcast.data.table( data = D, formula = id ~ y, value.var = 'z', fun.aggregate = sum )

# Aggregation
D6 <- D[ , list( N = .N, X = sum( x ), Z = sum( z ), U = sum( v * log( z ) ) ), by = list( y ) ]

