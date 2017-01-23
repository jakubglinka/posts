/*
 * Simulate from Shifted Gompertz Distribution
 * using Accept-Reject Algorithm
 * -----------------------------------------------------
 * Copyright: Jakub Glinka
 * License: GPLv3
 * -----------------------------------------------------
 * 
 */

#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;
using namespace std;

double tmp_dsgomp(double x, double b, double eta) {
  
  if (b < .0 || eta < .0 || x < .0) throw std::range_error("Inadmissible value");
  
  double res = 0.0;
  res = b * exp(- b * x) * exp(- eta * exp(- b * x)) * (1 + eta * (1 - exp(- b * x)));
  
  return res;
}

// [[Rcpp::export]]
NumericVector dsgomp(NumericVector x, double b, double eta) {
  
  if (b < .0 || eta < .0 || min(x) < .0) throw std::range_error("Inadmissible value");
  
  NumericVector res(x.length());
  res = b * exp(- b * x) * exp(- eta * exp(- b * x)) * (1 + eta * (1 - exp(- b * x)));
  
  return res;
}

double tmp_psgomp(double x, double b, double eta) {
  
  if (b < .0 || eta < .0 || x < .0) throw std::range_error("Inadmissible value");
  
  double res = 0.0;
  res = (1 - exp(- b * x)) * exp(- eta * exp(- b * x));
  
  return res;
}

// [[Rcpp::export]]
NumericVector psgomp(NumericVector x, double b, double eta) {
  
  if (b < .0 || eta < .0 || min(x) < .0) throw std::range_error("Inadmissible value");
  
  NumericVector res(x.length());
  res = (1 - exp(- b * x)) * exp(- eta * exp(- b * x));
  
  return res;
}


// [[Rcpp::export]]
NumericVector rsgomp(int n, double b, double eta, bool verbose) {
  
  std::cout << "\n";
  
  if (b < .0 || eta < .0) throw std::range_error("Inadmissible value");
  NumericVector res(n);
  
  // SG density mode
  double t0 = 0.0;
  double z = 0.0;
  
  if (eta > .5) {
    
    z = (3 + eta - pow(eta * eta + 2 * eta + 5,.5)) / (2 * eta);
    t0 = - 1 / b * log(z);
  }
  
  if (verbose) std::cout << "Mode detected at: " << round(t0 * 1e3) / 1e3 << "\n";
  
  // SG max;
  double m = tmp_dsgomp(t0, b, eta);
  if (verbose) std::cout << "Density maximum: " << round(m * 1e3) / 1e3 << "\n";
  
  // exponential denisty cut-off point
  double t1 = - 1 / b * (log(m) - log(1 + eta) - log(b));
  if (verbose) std::cout << "Exponential density cut-off point: " << round(t1 * 1e3) / 1e3 << "\n";
  
  // calculate integral of the density
  double auc = m * t1 + (exp(- t1 * b)) * (1 + eta);
  if (verbose) std::cout << "Integral of un-normalised density: " << round(auc * 1e3) / 1e3 << "\n";
  
  // calculate probability of uniform part of the density
  double p1 = m * t1 / auc;
  if (verbose) std::cout << "Probability of uniform component: " << round(p1 * 1e3) / 1e3 << "\n";
  
  // sampling
  bool accept = false;
  double draw_x, draw_y;
  int part;
  
  for(int i = 0; i < n; i++) {
    
    accept = false;
    while (!accept) {
      
      part = ::Rf_runif(0.0, 1.0) < p1 ? 1 : 2;
      if (part == 1) {
        
        draw_x = ::Rf_runif(0.0, t1);
        draw_y = ::Rf_runif(0.0, m);
        
      } else {
        
        draw_x = t1 + ::Rf_rexp(1.0 / b);
        draw_y = ::Rf_runif(0.0, ::Rf_dexp(draw_x, 1.0 / b, 0) * (1.0 + eta));
        
      }
      
      if (draw_y < tmp_dsgomp(draw_x, b, eta)) {
        accept = true;
      }
      
    }  
    
    res[i] = draw_x;
    
  };
  
  
  return res;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

  b = .3
  eta = 10.0
  n = 1e4

  system.time(s <- rsgomp(n = n, b = b, eta = eta, verbose = FALSE))


  par(mfrow = c(1, 2))

  x <- seq(0.0, 40, .01)
  hist(s, breaks = 50, freq = FALSE)
  lines(x, dsgomp(x, b, eta))


  plot(ecdf(s), xlim = c(0, 40))
  lines(x, psgomp(x, b, eta), lty = "dashed")

  par(mfrow = c(1, 1))

*/