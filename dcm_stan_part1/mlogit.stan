/*
* Multinomial Logit Model
* for Multiple Purchase Periods
* -----------------------------------------------------
* Copyright: Jakub Glinka <jakub.glinka@gfk.com>
* Date: 18 October 2016
* License: GPLv3
* -----------------------------------------------------
* 
*/

data {

  int<lower=0> n_periods;
  int<lower=0> period_start[n_periods];
  int<lower=0> period_stop[n_periods];
  
  int<lower=0> F;
  int<lower=0> nr;
    
  matrix[nr,F] X;
  
  row_vector<lower=0>[nr] y;
  
} 

parameters {

  vector[F] beta;

} 

transformed parameters {
  
  vector[nr] u;
  
  for (period in 1:n_periods) {
    
    u[(period_start[period]) : (period_stop[period])] = X[(period_start[period]) : (period_stop[period]),] * beta;
      

}
  
    
  
}


model {
  
  beta ~ cauchy(0, 2.5);
  
   for (period in 1:n_periods) {
    
    target += y[(period_start[period]) : (period_stop[period])] * log_softmax(u[(period_start[period]) : (period_stop[period])]);
      
  }
  
  
}






