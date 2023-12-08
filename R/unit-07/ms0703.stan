data {
    int<lower=0> n; 
    int<lower=0> n_district;
    vector[n] urban; 
    array[n] int<lower=1, upper=n_district> district;
    array[n] int<lower=0, upper=1> use_contraception;
}

parameters {
    matrix[n_district, 2] v; 
    vector[2] a_bar; 
    corr_matrix[2] Rho;
    vector<lower=0>[2] sigma;
}

transformed parameters {
    vector[n_district] a;
    vector[n_district] b;

    b = v[, 2];
    a = v[, 1];
}

model {
    sigma ~ cauchy(0, 2.5);
    Rho ~ lkj_corr(2);
    a_bar ~ normal(0, 1);
    for (i in 1:n_district) {
        v[i, :] ~ multi_normal(a_bar, quad_form_diag(Rho, sigma));
    } 
    {
        vector[n] q;
        for (i in 1:n) {
            q[i] = a[district[i]] + b[district[i]] * urban[i];
        }
        use_contraception ~ bernoulli_logit(q);
    }
}
