#___________________________________________________________________________________________________
# Lectura
library( xlsx )
options( stringsAsFactors = FALSE )

banano<-read.xlsx( 'datos/banano 2000-2013.xls',
                   sheetIndex = 1,
                   startRow = 5,
                   endRow = 33,
                   colIndex = 1:53 )

noms<-expand.grid( 2000:2012,
            c('ss', 'sc', 'pr', 'rd' ))
noms<-noms[ order( noms[,1] ), ]
rownames(noms)<-NULL
nombres<-paste( noms[,1], noms[,2], sep = '_' )
print(nombres)

colnames( banano )<-c( 'prov', nombres )
banano<-banano[4:nrow(banano),]
rownames( banano )<-NULL

banano[,2:ncol(banano)]<-apply(
  banano[,2:ncol(banano)],
  c(1,2),
  FUN = as.numeric )

agrega<-apply( banano[,2:ncol(banano)],
               c(2),
               FUN = sum,
               na.rm = TRUE )

mprod<-subset( banano,
prov %in% c('Los Ríos','Guayas','El Oro',
            'Cañar'),
select = c( 'prov', 
            paste(2000:2012,'pr',sep='_') ) )

mprod<-subset( banano,
  select = c( 'prov', 
              paste(2000:2012,'pr',sep='_') ) )
rownames(mprod)<-NULL

totprod<-data.frame( prov = mprod[,1],
  tot = apply( mprod[,2:ncol(mprod)], 
               c(1), FUN = sum,
               na.rm = TRUE ) )

totprod<-totprod[ order(-totprod$tot),]

m<-min(mprod[c(7,13),2:ncol(mprod)])
M<-max(mprod[c(7,13),2:ncol(mprod)])



x11()
pdf('mayor_productor.pdf',
    width = 12, height = 8)


png('mayor_produccion.png')
plot( 2000:2012, 
      mprod[13,2:ncol(mprod)], 
      typ ='l', col = 'red',
      ylim = c(m,M),
      xlab = 'Años',
      ylab = 'Tm')
points( 2000:2012, 
        mprod[7,2:ncol(mprod)], 
        typ ='l', col = 'blue' )
dev.off()
#Graficos








