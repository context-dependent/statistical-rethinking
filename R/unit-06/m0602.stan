data {
    int<lower=1> n;
    array[n] int surv;
    array[n] int density;

    int<lower=1> n_tanks;
    array[n_tanks] int tank;
}

parameters {
    real a_bar; 
    real<lower=0> sigma;
    vector[n_tanks] tank_zoffset;  
}

transformed parameters {
    vector[n_tanks] tank_mean;
    tank_mean = a_bar + sigma * tank_zoffset;  
}

model {
    sigma ~ exponential(1); 
    a_bar ~ normal(0, 1.5);
    tank_zoffset ~ normal(0, 1); 
    for (i in 1:n) {
        surv[i] ~ binomial_logit(density[i], tank_mean[tank[i]]);
    }
}
