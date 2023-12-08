data {
    int<lower=0> n; 
    int<lower=0> n_district; 
    int<lower=0> n_kids; 
    
    vector[n] urban; 
    vector[n] age_z; 
    array[n] int<lower=1, upper=n_kids> kids; 
    array[n] int<lower=1, upper=n_district> district;
    array[n] int<lower=0, upper=1> safe;

}

parameters {
    matrix[2, n_district] z;
    simplex[n_kids - 1] delta; // cutpoints for kids fx
    cholesky_factor_corr[2] l_rho;
    vector<lower=0>[2] sigma;
    vector[n_district] z_kids; 
    real<lower=0> tau;
    real b_age; 
    real a_bar;
    real b_bar; 
    real b_bar_kids;
}

transformed parameters {
    matrix[n_district, 2] v; 
    vector[n_district] a;
    vector[n_district] b;
    vector[n_district] b_kids;
    vector[n_kids] s_b_kids;
    v = (diag_pre_multiply(sigma, l_rho) * z)';
    a = a_bar + v[, 1];
    b = b_bar + v[, 2];
    b_kids = b_bar_kids + z_kids * tau;
    s_b_kids[1] = 0;
    for (i in 2:n_kids) {
        s_b_kids[i] = sum(delta[1:i - 1]);
    }
}

model {
    a_bar ~ normal(0, 1);
    b_bar ~ normal(0, 1);
    b_age ~ normal(0, 1);
    v[, 1] ~ normal(0, 1);
    v[, 2] ~ normal(0, 1);
    delta ~ dirichlet(rep_vector(1, n_kids - 1));
    b_kids ~ normal(0, 1);
    sigma ~ cauchy(0, 2.5);
    l_rho ~ lkj_corr_cholesky(2);
    to_vector(z) ~ normal(0, 1);
    z_kids ~ normal(0, 1);
    tau ~ cauchy(0, 2.5);

    {
        vector[n] theta;
        theta = a[district] + 
                b[district] .* 
                urban + 
                b_kids[district] .* s_b_kids[kids] + 
                b_age * age_z;
        safe ~ bernoulli_logit(theta);
    }
}

generated quantities {
    matrix[2, 2] rho; 
    rho = multiply_lower_tri_self_transpose(l_rho);
}
