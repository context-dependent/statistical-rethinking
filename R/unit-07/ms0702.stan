data {
    int<lower=0> n;
    int<lower=0> n_district;

    vector[n] urbanity;
    array[n] int<lower=1, upper=n_district> district;
    array[n] int<lower=0, upper=1> use_contraception;
}

parameters {
    vector[n_district] z_a;
    vector[n_district] z_b;
    vector[n_district] a_bar;
    vector[n_district] b_bar;
    real<lower=0> sigma;
    real<lower=0> tau;
}

transformed parameters {
    vector[n_district] a; 
    vector[n_district] b;
    vector[n] q;
    a = a_bar + sigma * z_a;
    b = b_bar + tau * z_b;
    q = a[district] + b[district] .* urbanity;
}

model {
    use_contraception ~ bernoulli_logit(q);
    z_a ~ normal(0, 1);
    z_b ~ normal(0, 1);
    a_bar ~ normal(0, 1);
    b_bar ~ normal(0, 1); 
    sigma ~ cauchy(0, 1);
    tau ~ cauchy(0, 1);
}

generated quantities {
    vector[n] log_lik;
    vector[n] p;
    for (i in 1:n) {
        log_lik[i] = bernoulli_logit_lpmf(use_contraception[i] | q[i]);
    }
    p = inv_logit(q);
}
