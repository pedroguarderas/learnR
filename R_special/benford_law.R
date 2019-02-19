library( data.table )

N<-1e5
y<-data.table( x = rlnorm( N, 5, 2 ) )
y[ , n := floor( log10( abs( x ) ) ) ]
y[ , m := x / ( 10^n ) ]
y[ , d := as.integer( abs( m ) ) ]
y<-y[ , list( N = .N ), by = d ]
y[ , N := N / sum( N ) ]
y[ , p := log10( 1 + 1/d ) ]
y<-y[ order( d ) ]

xlabs<-1:9
ylabs<-seq( 0, 0.32, 0.01 )
plot( y$d, y$p, type = 'h', col = 'olivedrab3', xlab = 'd', ylab = 'p', 
      xlim = c( 1, 9 ), ylim = c( 0, 0.32 ), xaxt = 'n', yaxt = 'n', lwd = 2,
      panel.first = { abline( v = xlabs, col = 'grey55', lty = 2, lwd = 0.5 )
        abline( h = ylabs, col = 'grey55', lty = 2, lwd = 0.5 ) } )
points( y$d, y$N, col = 'midnightblue', pch = 16, cex = 1.0  )
axis( side = 1, at = xlabs, labels = xlabs )
axis( side = 2, 
      at = ylabs, 
      labels = formatC( ylabs, format = 'f', digits = 2 ), 
      las = 1 )
