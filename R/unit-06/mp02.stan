data {
    int<lower=1> n;
    int<lower=1> n_pred;
    int<lower=1> n_size;
    array[n] int surv;
    array[n] int density;
    array[n] int tank;
    array[n] int pred; 
    array[n] int size; 
}

parameters {
    real a_bar;
    real<lower=0> sigma;
    matrix[n_size, n_pred] b;
    vector[n] a;
}

transformed parameters {
    vector[n] p;
    for (i in 1:n) {
        p[i] = inv_logit(a[tank[i]] + b[size[i], pred[i]]);
    }
}

model {
    a_bar ~ normal(0, 1.5);
    a ~ normal(a_bar, sigma);
    for (i in 1:n_size) {
        b[i] ~ normal(0, 1);
    }
    surv ~ binomial(density, p);
}

generated quantities {
    vector[n] log_lik;
    vector[n] pr__surv;
    vector[n] pr__propsurv;
    for (i in 1:n) {
        log_lik[i] = binomial_lpmf(surv[i] | density[i], p[i]);
    }
    for (i in 1:n) {
        pr__surv[i] = binomial_rng(density[i], p[i]);
        pr__propsurv[i] = pr__surv[i] / density[i];
    }
}

