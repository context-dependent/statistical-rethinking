data {
    int<lower=0> n; 
    int<lower=0> n_pred;
    
    array[n] int pred;
    array[n] int density; 
    array[n] int surv;
}

parameters {
    vector[n_pred] a;
    vector[n_pred] q;
    real<lower=0> sigma;
}

model {
    sigma ~ exponential(1);
    q ~ normal(a, sigma);
    surv ~ binomial_logit(density, q[pred]);
}

generated quantities {
    vector[n_pred] sim_surv;
    vector[n_pred] sim_propsurv;
    for (i in 1:n_pred) {
        sim_surv[i] = binomial_rng(100, inv_logit(q[i]));
        sim_propsurv[i] = sim_surv[i] / 100;
    }
}
