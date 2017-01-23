/*
* Estimate diffusion curves using SG distribution
* on google trends data
* -----------------------------------------------------
* Copyright: Jakub Glinka <jgstatistics@gmail.com>
* License: GPLv3
* -----------------------------------------------------
* 
*/

functions {

  vector sg_weekly_share(int T, real b, real eta) {
    
    vector[T + 1] F;
    vector[T] f;
    
    for (t in 0:T) {
      F[t + 1] = (1 - exp(- b * t)) * exp(- eta * exp(- b * t));
    }
    
    for (t in 1:T) {
      
        f[t] = F[t + 1] - F[t];
        f[t] = f[t] > 0 ? f[t] : 0; 
        
    }
    return f;
  }
  
}

data {
  
  int<lower = 2> T;                                     // number of observed periods
  real<lower = 0> S[T];                                 // share of users adoptions
}

parameters {
  
  real<lower=0> b;                                      // appeal of innovation
  real<lower=0> eta;                                    // rate of adoption
  real<lower=0> sigma;
  real<lower=0> m;
  
}

transformed parameters {
  
  vector[T] share;
  share = sg_weekly_share(T, b, eta);
  
}

model {
    
      if (sigma == 0)
        reject("sampled sigma = 0")
      
      # likelihood
      S ~ normal(m * share, sigma);
}

generated quantities {
  
  vector[T] pred_s;

  real p;
  real q;
  
  pred_s = m * share;
  p = b * exp(- eta);
  q = b - p;

}


