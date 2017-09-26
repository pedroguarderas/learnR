# cron script
v<-data.frame( Saludo = 'Hola' )

write.table( v, 
             file = '/home/aju/Development/learnR/R_special/cron_exo.txt',
             row.names = FALSE )
