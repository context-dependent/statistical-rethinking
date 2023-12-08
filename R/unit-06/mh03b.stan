data { 
    int n; 
    int n_density_f;

    array[n] int density; 
    array[n] int density_f;
    array[n] int surv;
    array[n] int tank; 
}

parameters {
    simplex[n_density_f - 1] delta; 
    real b_density; 
    real a_bar;
    real<lower=0> sigma; 
    vector[n] a; 
}

transformed parameters {
    vector[n] q;
    vector[n_density_f] density_effect; 
    vector[n_density_f] delta_j;
    delta_j = append_row(0, delta);
    for (k in 1:n_density_f) {
        density_effect[k] = b_density * sum(delta_j[1:k]);
    }
    for (i in 1:n) {
        q[i] = a[i] + density_effect[density_f[i]];
    }
}

model { 
    sigma ~ exponential(1);
    a ~ normal(a_bar, sigma);
    a_bar ~ normal(0, 1.5);
    b_density ~ normal(0, 1);
    delta ~ dirichlet(rep_vector(1, n_density_f - 1));
    surv ~ binomial_logit(density, q[tank]);
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
