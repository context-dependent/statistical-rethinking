data {
    int<lower=0> N; // number of observations
    int<lower=1> K; // number of individual predictors
    int<lower=1> J; // number of groups
    int<lower=1> L; // number of group predictors
    array[N] int<lower=1, upper=J> jj; // group for observation n in N
    matrix[N, K] x; // individual predictors (urban)
    array[J] row_vector[L] u; // group predictors (district)
    array[N] int<lower=0, upper=1> y; // outcome (used contraception)
}

parameters {
    corr_matrix[K] Omega; 
    vector<lower=0>[K] tau;
    matrix[L, K] gamma; 
    array[J] vector[K] beta; 
}

model {
    tau ~ cauchy(0, 2.5);
    Omega ~ lkj_corr(2);
    to_vector(gamma) ~ normal(0, 5);
    {
        array[J] row_vector[K] u_gamma;
        for (j in 1:J) {
            u_gamma[j] = u[j] * gamma; 
        }
        beta ~ multi_normal(u_gamma, quad_form_diag(Omega, tau)); 
    }
    for (n in 1:N) {
        y[n] ~ bernoulli_logit(x[n] * beta[jj[n]]);
    }
}
