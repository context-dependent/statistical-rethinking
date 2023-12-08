// A simple model for reedfrog survival with no pooling
data {
    int n;
    array[n] int surv;
    array[n] int density;
    array[n] int tank;
}

parameters {
    array[n] real a;
}

transformed parameters {
    vector[n] p;
    for (i in 1:n) {
        p[i] = inv_logit(a[i]);
    }
}

model {
    a ~ normal(0, 1.5);
    for (i in 1:n) {
        surv[i] ~ binomial(density[i], p[i]);
    }
}

generated quantities {
    vector[n] log_lik;
    for (i in 1:n) {
        log_lik[i] = binomial_lpmf(surv[i] | density[i], p[i]);
    }
}
