library(tidyverse)
library(tidybayes)
library(cmdstanr)


data(reedfrogs, package = "rethinking")
d <- as_tibble(reedfrogs) |>
    mutate(tank = row_number())


d_stan <- compose_data(d)

mp00 <- cmdstan_model("R/unit-06/mp00.stan")
fp00 <- mp00$sample(data = d_stan, seed = 123)

pp00 <- fp00 |>
    spread_draws(p[tank]) |>
    median_qi() |>
    mutate(
        mod = "No Pooling"
    )

mp01 <- cmdstan_model("R/unit-06/mp01.stan")
fp01 <- mp01$sample(data = d_stan, seed = 123)

loo::loo_compare(list(m00 = fp00$loo(), m01 = fp01$loo()))

pp01 <- fp01 |>
    spread_draws(p[tank]) |>
    median_qi() |>
    mutate(mod = "Partial Pooling")

obs <- d |>
    select(tank, p = propsurv) |>
    mutate(mod = "Observed")


library(posterior)
library(bayesplot)
library(loo)

fp01 |>
    as_draws() |>
    pareto_diags()

rvar_draws <- as_draws_rvars(fp01)
mcmc_trace(rvar_draws)

fp01$diagnostic_summary()
fp01$loo(variables = "p")

lp01 <- loo(fp01$draws(), reloo = TRUE, cores = 4)
plot(lp01)

mp02 <- cmdstan_model("R/unit-06/mp02.stan")
fp02 <- mp02$sample(data = d_stan, seed = 123)

loo::loo_compare(list(m00 = fp00$loo(), m01 = fp01$loo(), m02 = fp02$loo()))

pp02 <- fp02 |>
    spread_draws(p[tank]) |>
    median_qi() |>
    mutate(mod = "FP + Pred and Size")

bind_rows(obs, pp00, pp01, pp02) |>
    ggplot(aes(tank, p)) +
    geom_point(aes(fill = mod), shape = 21, size = 3, alpha = .8)

fp02 |>
    recover_types(d) |>
    spread_rvars(a_bar, b[size, pred]) |>
    mutate(
        logit_p = a_bar + b,
        p = exp(logit_p) / (1 + exp(logit_p))
    ) |>
    ggplot(aes(xdist = p, y = pred)) +
    stat_slab(aes(fill = size), alpha = .8, size = 1, color = "white")

cp02 <- fp02 |>
    recover_types(d) |>
    spread_rvars(a_bar, b[size, pred]) |>
    mutate(
        logit_p = a_bar + b,
        p = exp(logit_p) / (1 + exp(logit_p))
    ) |>
    select(-c(a_bar, b, logit_p)) |>
    pivot_wider(names_from = pred, values_from = p) |>
    mutate(
        contrast_pred = pred - no
    )

cp02

cp02 |>
    ggplot(aes(xdist = contrast_pred, y = size)) +
    stat_halfeye(alpha = .8)


# Total effect of Predation
sim_fx_02 <- fp02 |>
    recover_types(d) |>
    spread_rvars(b[size, pred], pr__propsurv[tank])

sim_fx_02

mp03 <- cmdstan_model("R/unit-06/mp03.stan")

fp03 <- mp03$sample(data = d_stan, seed = 123)

library(coda)
plot(as_mcmc.list(fp03))


component <- rvar_rng(rnorm, 2, mean = c(1, 5))
i <- rvar_rng(rbinom, 1, size = 1, p = .75) + 1L
mix <- component[[i]]

length(component)

#' @param x a vector of rvars
#' @param ... arguments passed to \code{rvar(sample(1:length(x), size = ndraws(x), replace = TRUE, ...))}
mix_rvars <- function(x, ...) {
    n_rvar <- length(x)
    i <- rvar(sample(1:n_rvar, size = ndraws(x), replace = TRUE, ...))
    x[[i]]
}

cp02 <- fp02 |>
    recover_types(d) |>
    spread_rvars(a_bar, b[size, pred]) |>
    mutate(
        logit_p = a_bar + b,
        p = exp(logit_p) / (1 + exp(logit_p))
    ) |>
    select(-c(a_bar, b, logit_p)) |>
    pivot_wider(names_from = pred, values_from = p) |>
    mutate(
        contrast_pred = pred - no
    )

cp02 |>
    ggplot(aes(xdist = contrast_pred, y = size)) +
    stat_halfeye(alpha = .8)

size_rep <- d |>
    group_by(size) |>
    summarize(n_size = n()) |>
    mutate(p_size = n_size / sum(n_size))


cp02 |>
    median_qi(contrast_pred)

cp02 |>
    left_join(size_rep) |>
    summarize(total_effect = mix_rvars(contrast_pred, prob = p_size)) |>
    median_qi()

pr_total <- fp02 |>
    spread_rvars(pr__propsurv[tank]) |>
    summarize(mix = mix_rvars(pr__propsurv))


pr_total |>
    ggplot(aes(xdist = mix, y = 1)) +
    stat_halfeye(alpha = .8) +
    geom_point(data = d, aes(x = propsurv, y = 1.1), shape = 21, size = 3, alpha = .8, fill = "white")

pr_total |> median_qi()


mh03 <- cmdstan_model("R/unit-06/mh03.stan")
