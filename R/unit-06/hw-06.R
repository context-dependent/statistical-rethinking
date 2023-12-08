library(cmdstanr)
library(tidyverse)
library(tidybayes)
library(coda)


data(reedfrogs, package = "rethinking")
data(Trolley, package = "rethinking")

d2 <- Trolley |> as_tibble()
d2 |> count(edu)



d <- as_tibble(reedfrogs) |>
    mutate(
        tank = row_number(),
        density_f = factor(density),
        density_lz = as.double(scale(log(density)))
    )

d_stan <- compose_data(d)

mh03 <- cmdstan_model("R/unit-06/mh03.stan")

fh03 <- mh03$sample(d_stan)

plot(as_mcmc.list(fh03))


fh03 |>
    recover_types(d) |>
    spread_rvars(q[density_f]) |>
    mutate(p = exp(q) / (1 + exp(q))) |>
    ggplot(aes(xdist = p, y = density_f)) +
    stat_halfeye() +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    scale_x_continuous(limits = c(0, 1))


mh03b <- cmdstan_model("R/unit-06/mh03b.stan")
fh03b <- mh03b$sample(d_stan)

#' @param x a vector of rvars
#' @param ... arguments passed to \code{rvar(sample(1:length(x), size = ndraws(x), replace = TRUE, ...))}
mix_rvars <- function(x, ...) {
    n_rvar <- length(x)
    i <- posterior::rvar(
        sample(
            1:n_rvar,
            size = posterior::ndraws(x),
            replace = TRUE, ...
        )
    )
    x[[i]]
}

fh03b |>
    recover_types(d) |>
    spread_rvars(a_bar, density_effect[density_f]) |>
    mutate(
        logit_p = a_bar + density_effect,
        p = exp(logit_p) / (1 + exp(logit_p))
    ) |>
    ggplot(aes(xdist = logit_p, y = density_f)) +
    stat_slab(color = "white", alpha = .8)

fh03b$loo()


mh03c <- cmdstan_model("R/unit-06/mh06c.stan")
fh03c <- mh03c$sample(d_stan)

fh03c$summary(variables = c("sigma", "b_density", "b_sp"))

fh03c |>
    recover_types(d) |>
    spread_rvars(b_density[pred])


mh04rt <- rethinking::ulam(
    alist(
        response ~ dordlogit(phi, alpha),
        phi <- b_action * action +
            b_intention * intention +
            b_contact * contact,
        c(b_action, b_intention, b_contact) ~ normal(0, .5),
        alpha ~ normal(0, 1)
    ),
    data = d2, iter = 100, chains = 2
)

rethinking::stancode(mh04rt)

d2 <- d2 |>
    mutate(response = factor(response))

d2_stan <- compose_data(d2)

mh04a <- cmdstan_model("R/unit-06/mh04a.stan")
fh04a <- mh04a$sample(d2_stan, iter_warmup = 500, iter_sampling = 500)

fh04a$summary(variables = c("k", "b_action", "b_intention", "b_contact", "alpha"))

mh04b <- cmdstan_model("R/unit-06/mh04b.stan")
fh04b <- mh04b$sample(d2_stan, iter_warmup = 500, iter_sampling = 500)
fh04b$summary(variables = c("k_bar", "b_action", "b_intention", "b_contact", "alpha"))
