data {
    int n; 
    int<lower=1> n_response; 
    int<lower=1> n_id;
    
    vector[n] contact; 
    vector[n] intention; 
    vector[n] action;
    array[n] int<lower=1, upper=n_response> response; 
    array[n] int<lower=1, upper=n_id> id;
    
}

parameters {
    real b_contact; 
    real b_intention; 
    real b_action;
    real k_bar;
    real<lower=0> tau;

    vector[n_id] z; 
    ordered[n_response - 1] alpha;
}

transformed parameters {
    vector[n] phi; 
    vector[n_id] k;
    k = k_bar + tau * z;
    phi = k[id] + 
          b_action * action + 
          b_intention * intention + 
          b_contact * contact; 
}

model { 
    for (i in 1:n_response - 1) {
        alpha[i] ~ normal(0, 1);
    }
    tau ~ exponential(1);
    k_bar ~ normal(0, 0.5);
    z ~ normal(0, 1);
    b_contact ~ normal(0, 0.5);
    b_intention ~ normal(0, 0.5);
    b_action ~ normal(0, 0.5);
    response ~ ordered_logistic(phi, alpha);
}

generated quantities {
    vector[n] log_lik;
    for (i in 1:n) {
        log_lik[i] = ordered_logistic_lpmf(response[i] | phi[i], alpha);
    }
}