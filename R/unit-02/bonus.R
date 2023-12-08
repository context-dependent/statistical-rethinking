library(tidyverse)
library(rethinking)
library(ggbeeswarm)

data(Oxboys)

d0 <- Oxboys

d1 <- d0 |>
    group_by(Subject) |>
    mutate(growth = height - lag(height)) |>
    filter(Occasion > 1)

d1 |>
    ggplot(aes(Occasion, growth)) +
    geom_beeswarm(size = 2, shape = 21, fill = "#0000FF")


mboys <- quap(
    alist(
        growth ~ dlnorm(alpha, sigma),
        alpha ~ dnorm(0, 0.1),
        sigma ~ dexp(3)
    ),
    data = list(growth = d1$growth)
)
alpha <- rnorm(1e3, 0, 0.1)
sigma <- rexp(1e3, 3)
growth_sim <- rlnorm(1e3, alpha, sigma)
dens(growth_sim)


post <- extract.samples(mboys)
dsim <- rlnorm(1e3, post$alpha, post$sigma)
dens(dsim)

inc_sum <- sapply(1:1000, function(s) sum(rlnorm(8, post$alpha[s], post$sigma[s])))

dens(inc_sum)
