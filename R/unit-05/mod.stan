data {
    int<lower=0> N;
    vector[N] S; 
    vector[N] I; 
    vector[N] M; 
}

parameters {
    real<lower=0> sigma;
    real<lower=0> alpha;
}

model {
    sigma ~ normal(0, 1);
    alpha[S, I] ~ normal(0, 1);
    M ~ normal(alpha[S, I], sigma);
}