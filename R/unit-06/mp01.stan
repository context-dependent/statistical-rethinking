data {
    int n;
    array[n] int surv;
    array[n] int density;
    array[n] int tank;
}

parameters {
    real a_bar;
    real<lower=0> sigma;
    array[n] real a;
}

transformed parameters {
    array[n] real p;
    for (i in 1:n) {
        p[i] = inv_logit(a[i]);
    }
}

model {
    a ~ normal(a_bar, sigma);
    sigma ~ exponential(1);
    for (i in 1:n) {
        surv[i] ~ binomial(density[i], p[i]);
    }
}

generated quantities {
    array[n] real log_lik;
    for (i in 1:n) {
        log_lik[i] = binomial_lpmf(surv[i] | density[i], p[i]);
    }
}
