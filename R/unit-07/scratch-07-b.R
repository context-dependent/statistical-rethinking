library(cmdstanr)
library(tidyverse)
library(posterior)
library(tidybayes)

data(bangladesh, package = "rethinking")

d <- bangladesh |>
    rename(
        use_contraception = use.contraception,
        age_centered = age.centered,
        living_children = living.children
    ) |>
    mutate(
        district = factor(district)
    ) |>
    as_tibble()

d_stan <- compose_data(d)

ms03 <- cmdstan_model("R/unit-07/ms0703.stan")
fs03 <- ms03$sample(data = d_stan, seed = 123)

fs03$draws() |>
    spread_rvars(sigma["param"]) |>
    mutate(
        sigma_rank = sigma |> map(~ apply(draws_of(.x, with_chains = TRUE), 1, order))
    ) |>
    select(
        param, sigma_rank
    )

# ggplot2 new stat vignette

StatChull <- ggproto("StatChull", Stat,
    compute_group = function(data, scales) {
        data[chull(data$x, data$y), , drop = FALSE]
    },
    required_aes = c("x", "y")
)

stat_chull <- function(
    mapping = NULL, data = NULL, geom = "polygon",
    position = "identity", na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE, ...) {
    ggplot2::layer(
        stat = StatChull, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

mpg |>
    ggplot(aes(displ, hwy)) +
    geom_point() +
    stat_chull(fill = NA, color = "red")

bayesplot::color_scheme_set("mix-blue-red")
bayesplot::mcmc_trace(fs03$draws(), pars = c("sigma[1]", "sigma[2]"))
bayesplot::mcmc_rank_hist(fs03$draws(), pars = c("sigma[1]", "sigma[2]", "a[1]"))
fs03$cmdstan_summary()

d_stan_classic <- list(
    N = nrow(d),
    K = 1,
    J = n_distinct(d$district),
    L = 1,
    jj = as.integer(d$district),
    u = matrix(rep(1, n_distinct(d$district)), ncol = 1),
    x = matrix(d$urban, ncol = 1),
    y = d$use_contraception
)

ms04 <- cmdstan_model("R/unit-07/ms0704.stan")

fs04 <- ms04$sample(data = d_stan_classic, seed = 123)
fs04$cmdstan_summary()
