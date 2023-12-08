library(cmdstanr)
library(tidyverse)
library(posterior)
library(tidybayes)

data(bangladesh, package = "rethinking")

d <- as_tibble(bangladesh) |>
    mutate(
        use_contraception = use.contraception,
        district = factor(district),
        urbanity = urban
    )

d_stan <- compose_data(d)

ms01 <- cmdstan_model("R/unit-07/ms0701.stan")
fs01 <- ms01$sample(data = d_stan, seed = 123)

x <- matrix(runif(200), ncol = 10)
apply(x, 1, which.max)

rx <- rvar(x)
apply(draws_of(rx), 1, which.max)

ggplot() +
    geom_raster(data = draws_of(rx))


ds01 <- fs01$draws() |>
    spread_rvars(a[district]) |>
    mutate(
        a_ndraw = a |> map_int(~ ndraws(.x)),
        a_trank = a |> map(~ apply(draws_of(.x, with_chains = TRUE), 1, which.max))
    ) |>
    select(district, a_trank) |>
    unnest(a_trank) |>
    group_by(district) |>
    mutate(draw = row_number()) |>
    ungroup()


ds01 |> ggplot(aes(y = factor(district), x = draw)) +
    geom_tile(aes(fill = factor(a_trank))) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))

a <- ds01$a |> split_chains()
a
rvar_apply(a, 1, function(x) x)
as_draws_array(a[[1]])

ds01$a_trank |> head()

draws_of(a[[1]], with_chains = TRUE)

ms02 <- cmdstan_model("R/unit-07/ms0702.stan")

fs02 <- ms02$sample(data = d_stan, seed = 123)

fs02$summary(variables = "tau")


obs <- d |>
    group_by(district, urbanity) |>
    summarize(
        n_use = sum(use_contraception),
        p_use = mean(use_contraception)
    )

fit <- fs02 |>
    recover_types(d) |>
    spread_rvars(a[district], b[district])

obs |>
    left_join(fit) |>
    mutate(
        q = a + b * urbanity,
        p = exp(q) / (1 + exp(q))
    ) |>
    ggplot(
        aes(xdist = p, y = district)
    ) +
    facet_wrap(~urbanity) +
    stat_pointinterval(color = "tomato") +
    geom_point(aes(x = p_use, size = n_use), color = "grey", alpha = .7) +
    geom_point(aes(x = p_use), color = "black", size = 3, shape = 21) +
    scale_size_continuous(range = c(3, 10))


loo::loo_compare(
    fs01$loo(),
    fs02$loo()
)

dsim <- d |>
    modelr::data_grid(district, urbanity) |>
    mutate(use_contraception = 0) |>
    compose_data()

str(fs02)

ms02$generate_quantities(data = dsim, fitted_params = fs02)
