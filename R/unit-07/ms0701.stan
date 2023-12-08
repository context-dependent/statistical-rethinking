data {
    int<lower=1> n;
    int<lower=1> n_district;

    array[n] int<lower=1,upper=n_district> district;
    array[n] int<lower=0, upper=1> use_contraception; 
}

parameters {
    vector[n_district] a;
    real a_bar; 
    real<lower=0> sigma;
}

model {
    a_bar ~ normal(0, 1);
    sigma ~ cauchy(0, 1);
    a ~ normal(a_bar, sigma); 
    use_contraception ~ bernoulli_logit(a[district]);
}

generated quantities {
    vector[n] log_lik;
    vector[n] sim__contraception; 
    for (i in 1:n) {
        log_lik[i] = bernoulli_logit_lpmf(use_contraception[i] | a[district[i]]);
    }
    for (i in 1:n) {
        sim__contraception[i] = bernoulli_logit_rng(a[district[i]]);
    }
}
