#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector c_survival(double alpha, NumericVector offset) {
  
  return exp(- exp(alpha + offset));
}

// [[Rcpp::export]]
NumericVector c_limitToRange(NumericVector x, NumericVector range) {
  
  for (int i = 0; i < x.length(); i++) {
    
    if (x[i] < range[0]) {
      x[i] = range[0];
    }
    
    if (x[i] > range[1]) {
      x[i] = range[1];
    }
  }
  
  return x;
}

// [[Rcpp::export]]
NumericVector c_survival_limit(
    double alpha, NumericVector offset, NumericVector range
) {
  
  return c_limitToRange(c_survival(alpha, offset), range);
}

// [[Rcpp::export]]
NumericVector c_marginal_survival(
  double alpha, NumericVector t, double bz1, double bz0, double s
)
{
  double xr = 2.5 * s;

  NumericVector x = NumericVector::create(
    .1488743389, .4333953941, .6794095682, .8650633666, .9739065285
  ); 
  
  NumericVector w = NumericVector::create(
    .2955242247, .2692667193, .2190863625, .1494513491, .0666713443
  );
  
  NumericVector total = NumericVector(t.length());

  x = x * xr;

  for (int i = 0; i < x.length(); i++) {
    
    double dx = x[i];
    
    // Density of Normal distribution (= dnorm(x, mean = 0, sd = sd))
    double phi = exp(- dx * dx / (2 * s * s)) / (sqrt(2 * M_PI) * s);

    total = total + w[i] * phi * (
      c_survival(alpha, t * exp(bz1 + dx) + bz0) +
      c_survival(alpha, t * exp(bz1 - dx) + bz0)
    );
  }
  
  return c_limitToRange(xr * total, NumericVector::create(1.0e-12, 1.0));
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
