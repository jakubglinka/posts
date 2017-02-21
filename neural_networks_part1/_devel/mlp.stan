/*
* neural network classifier with single hidden layer
* and tanh activation function
* -----------------------------------------------------
* Copyright: Jakub Glinka <jgstatistics@gmail.com>
* License: GPLv3
* -----------------------------------------------------
* 
*/

// principles:
// likelihood by hand
// weights centered to zero
// normal priors on weights
// with nnet like functions
// final files into gist
// with roxygen like docs
// then I can make an R package out of all gists :)


functions {
  
  real class_cross_entropy(matrix out, int[] y) {
    
    int N = cols(out);
    int K = rows(out);
    real res = 0.0;
    vector[K] log_p;
    
    for (n in 1:N) {
      res = res + categorical_logit_lpmf(y[n] | out[:, n]);
    }
    
    return(res);
  }
  
  matrix X_wtX_tanh(matrix w, matrix X) {
    return(tanh(w * X));
  }
  
  matrix X_wtX(matrix w, matrix X) {
    return(w * X);
  }
    
  matrix centered_weights(matrix w) {
    
    matrix[rows(w) + 1, cols(w)] cw;
    cw[1:rows(w), :] = w[:, :];
    
    for (d in 1:cols(w))
      cw[rows(w) + 1, d] = -sum(w[:, d]);
  
    return(cw);
  }
  
}



data {
  
  int<lower = 1> N;                           // number of observations
  int<lower = 1> D;                           // input dimension
  int<lower = 2> K;                           // number of classes
  int<lower = K> J;                           // size of hidden layer
  
  int y[N];                                   // label
  matrix[D, N] X;                             // input data
}

parameters {
  matrix[J - 1, D] w_raw;
  matrix[K - 1, J] v_raw;
}

transformed parameters {
  matrix[J, D] w;                             // centered weights
  matrix[K, J] v;                             
  matrix[K, N] h;                             
  
  w = centered_weights(w_raw);
  v = centered_weights(v_raw);
    
  h = X_wtX(v, X_wtX_tanh(w, X));

}


model {
  
  to_vector(w) ~ normal(0, 1);
  to_vector(v) ~ normal(0, 1);
  
  target += class_cross_entropy(h, y);
  
}

// ----------------------------------------------------//
