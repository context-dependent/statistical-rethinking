data {
    int<lower=1> n;
    array[n] int surv;
    array[n] int density; 
    array[n] int pred; 
    array[n] int size;
    array[n] int tank; 

    int<lower=1> n_tanks;
    int<lower=1> n_size;
    int<lower=1> n_pred; 

}

parameters {
    real a_bar;
    real<lower=0> sigma_tank_offset;
    vector[n_tanks] tank_offset; 
    array[n_size] vector[n_pred] b;
}

transformed parameters {
    vector[n_tanks] tank_mean;
    tank_mean = a_bar + tank_offset * sigma_tank_offset;  
}

model {
    real logit_p;
    a_bar ~ normal(0, 1.5);
    sigma_tank_offset ~ exponential(1);
    tank_offset ~ normal(0, 1);
    for (i in 1:n_size) {
        b[i] ~ normal(0, 1);
    }

    for (i in 1:n) {
        logit_p = tank_mean[tank[i]] + b[size[i], pred[i]];
        surv[i] ~ binomial_logit(
            density[i], 
            logit_p
        );
    }
}

generated quantities {
    vector[n] logit_p;
    vector[n] p;

    for (i in 1:n) {
        logit_p[i] = tank_mean[tank[i]] + b[size[i], pred[i]];
        p[i] = inv_logit(logit_p[i]);
    }
}
