data {
    int n; 
    int<lower=1> n_response; 
    
    vector[n] contact; 
    vector[n] intention; 
    vector[n] action;
    array[n] int<lower=1, upper=n_response> response; 
    
}

parameters {
    real b_contact; 
    real b_intention; 
    real b_action;
    real k;
    ordered[n_response - 1] alpha;
}

transformed parameters {
    vector[n] phi; 
    phi = k + b_action * action + 
          b_intention * intention + 
          b_contact * contact; 
}

model { 
    for (i in 1:n_response - 1) {
        alpha[i] ~ normal(0, 1);
    }
    k ~ normal(0, .5);
    b_contact ~ normal(0, 0.5);
    b_intention ~ normal(0, 0.5);
    b_action ~ normal(0, 0.5);
    response ~ ordered_logistic(phi, alpha);
}
