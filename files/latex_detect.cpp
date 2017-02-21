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
List detect_inline_latex(std::string line) {

  IntegerVector start(0);
  IntegerVector stop(0);
  List res(2);
  int n = line.size();
  
  // scan through string
  for (int i = 0; i < n; i++) {
    if (line[i] == '$') {
      if (i == 0 | line[i - 1] != '$') {
          if (i < n - 2 && line[i + 1] != '$') {
            for (int j = i + 2; j < n; j++) {
              if (line[j] == '$') {
                start.push_back(i + 1);
                stop.push_back(j + 1);
                i = j;
              break;
            }
          }
        }
      }
    }
  }

  res[0] = start;
  res[1] = stop;
  return res;
}

// [[Rcpp::export]]
List detect_equation_latex(std::string line) {
  
  IntegerVector start(0);
  IntegerVector stop(0);
  List res(2);
  int n = line.size();
  
  // scan through string
  for (int i = 0; i < n; i++) {
    if (line[i] == '$') {
      if (i > 0 | line[i - 1] == '$') {
        if (i < n - 3 && line[i + 1] != '$') {
          for (int j = i + 2; j < n - 1; j++) {
            if (line[j] == '$' & line[j + 1] == '$') {
              start.push_back(i);
              stop.push_back(j + 2);
              i = j + 2;
              break;
            }
          }
        }
      }
    }
  }
  
  res[0] = start;
  res[1] = stop;
  return res;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
detect_inline_latex("aaaa $a = b$ and $$a = e^{\\beta}$$ and $\\frac{1}{2}$")
detect_equation_latex("aaaa $a = b$ and $$a = e^{\\beta}$$ and $\\frac{1}{2}$")
*/
