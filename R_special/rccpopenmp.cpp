#include <Rcpp.h>
#include <omp.h>

using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
double sumCPP( const NumericVector& x ) {
  size_t i;
  double s;
  
  s = 0.0;
  #pragma omp parallel for private( i ) reduction (+:s)
  for ( i = 0; i < x.size(); i++ ) {
    s = s + x[i] * x[i];
  }
  return s;
}