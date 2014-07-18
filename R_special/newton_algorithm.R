#___________________________________________________________________________________________________
# Simple newton algorithm
diff<-function( f, x, e ) {
  return( 0.5 * ( f(x+e) - f(x-e) ) / e )
}

newton<-function( f, n, x0, e1, e2 ) {
  x<-x0
  i<-0
  while( i < n ) {
    df<-diff( f, x, e2 )
    x<-x-f(x)/df
    if ( abs( df ) <= e1 ) {
      break;
    }
    i<-i+1
  }
  return( x )
}
