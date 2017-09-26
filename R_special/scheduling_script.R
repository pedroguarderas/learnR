
# Empleando el reloj del sistema
RUN<-FALSE
while ( RUN == FALSE ) {
  CurrentTime<-format( Sys.time(), '%Y-%m-%d-%H-%M' )
  if ( CurrentTime == '2017-09-26-16-00' ) {
    RUN<-TRUE
    cat( '\tHola' )
  }
}

# Durmiendo la máquina por un tiempo dado en segundos
Sys.sleep( 10 ) # ejecución después de 10 segundos de ejecutado el script
cat( '\tHola' )

# Using the scheduling systen cron in linux
library( cronR )
Rscript<-paste( getwd(), '/R_special/cron_script.R', sep = '' )
sch_script<-cron_rscript( Rscript )

cron_ls()
cron_add( command = sch_script, 
          frequency = 'daily',
          id = 'CronExo',
          description = 'Cron example',
          at = '16:44' )

cron_ls()
cron_rm( id = 'CronExo' ) # Remove specific job
cron_ls()
cron_clear( ask = FALSE ) # Clear all jobs
