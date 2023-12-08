data { 
    int n; 
    int n_pred;
    int n_size; 

    array[n] int density; 
    array[n] int surv;
    array[n] int tank; 
    array[n] int pred;
    array[n] int size;

    vector[n] density_lz;
}

parameters {
    matrix[n_size, n_pred] b_sp; 
    vector[n_pred] b_density; 
    vector[n] a;
    real a_bar;
    real<lower=0> sigma; 
}

transformed parameters {
    vector[n] q;
    for (i in 1:n) {
        q[i] = a[tank[i]] + b_density[pred[i]] * density_lz[i] + b_sp[size[i], pred[i]];
    }
}

model { 
    sigma ~ exponential(1);
    
    to_vector(b_sp) ~ normal(0, 1);
    
    a_bar ~ normal(0, 1.5);
    a ~ normal(a_bar, sigma);
    b_density ~ normal(0, 1);

    surv ~ binomial_logit(density, q);
}

generated quantities {
    vector[n] log_lik;
    vector[n] sim__surv; 
    vector[n] sim__propsurv;
    
    for (i in 1:n) {
        log_lik[i] = binomial_logit_lpmf(surv[i] | density[i], q[tank[i]]);
    }

    for (i in 1:n) {
        sim__surv[i] = binomial_rng(density[i], inv_logit(q[tank[i]]));
        sim__propsurv[i] = sim__surv[i] / density[i];
    }
}
