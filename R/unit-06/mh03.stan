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
    real a; 
}

transformed parameters {
    vector[n_density_f] q; 
    vector[n_density_f] delta_j;
    delta_j = append_row(0, delta);
    for (k in 1:n_density_f) {
        q[k] = a + b_density * sum(delta_j[1:k]);
    }
}

model { 
    a ~ normal(0, 1.5);
    b_density ~ normal(0, 1);
    delta ~ dirichlet(rep_vector(1, n_density_f - 1));
    surv ~ binomial_logit(density, q[density_f[tank]]);
}
