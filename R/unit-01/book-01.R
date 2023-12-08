# Code Accompanying Reading Notes for Chapters 1-3

1+ 1
# Grid approximation
library(rethinking)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

globe_grid <- function(n_water, n_tosses, grid_size = NULL, prior = NULL) {

    # Generate default uniform prior
    if (is.null(prior)) {
        if (is.null(grid_size)) {
            grid_size <- 20
        }
        prior <- rep(1, grid_size)
    }

    p_grid <- seq(from = 0, to = 1, length.out = length(prior))

    data_likelihood <- dbinom(x = n_water, size = n_tosses, prob = p_grid)

    unstandardized_posterior <- data_likelihood * prior

    posterior_probability <- unstandardized_posterior / sum(unstandardized_posterior)

    return(posterior_probability)
}


test_01 <- globe_grid(6, 9)
plot(1:20, test_01, type = "b")


test_02 <- globe_grid(6, 9, grid_size = 5)
plot(1:5, test_02, type = "b")

test_update <- globe_grid(2, 9, prior = test_01)
plot(1:20, test_update, type = "b")


globe_quad_naive <- function(n_water, n_tosses) {
    n_land <- n_tosses - n_water


    quap(
        alist(
            W ~ dbinom(W + L, p),
            p ~ dunif(0, 1)
        ),
        data = list(W = n_water, L = n_land)
    )
}


# do not really get how to work with the return from quap

n_samples <- 1000
p <- c(0.5, rep(NA, n_samples - 1))
W <- 6
L <- 3

bound_p <- function(p) {
    dplyr::case_when(
        p < 0 ~ abs(p),
        p > 1 ~ 2 - p,
        TRUE ~ p
    )
}

for (i in 2:n_samples) {
    p_old <- p[i - 1]
    p_new <- bound_p(rnorm(1, p_old, 0.1))
    likelihood <- dbinom(W, W + L, c(p_old, p_new))
    p_shift <- likelihood[2] / likelihood[1]
    b_shift <- runif(1) < p_shift
    if (b_shift) {
        p[i] <- p_new
    } else {
        p[i] <- p_old
    }
}

# Homweork

# Q1: Different Data
post <- globe_grid(4, 15)
post

library(tibble)


# Q2: simulate the next 5 tosses
# - reconstruct the posterior tidywise
d <-
    tibble(
        p = seq(0, 1, length.out = 100),
        prior = 1
    ) |>
    mutate(
        likelihood = dbinom(4, 15, prob = p),
        posterior = likelihood * prior / sum(likelihood * prior)
    )


set.seed(5)

sim <- d |>
    sample_n(10000, weight = posterior, replace = TRUE) |>
    mutate(next_five = rbinom(n = n(), size = 5, p = p))  |>
    count(next_five)

# - plot the posterior and likelihood
d |>
    ggplot(aes(p, posterior)) +
    geom_col()

# - plot the predictive posterior
sim |>
    ggplot(aes(next_five, n)) +
    geom_col()

# Q3: probability of 3 or more waters
sim |>
    mutate(p = n / sum(n)) |>
    group_by(next_five >= 3) |>
    summarize(
        n = sum(n),
        p = sum(p)
    )

# Q4: probability distribution of number of tosses given p and W
pbinom(5, 10, 0.7)
pbinom(5, 8, 0.7)
pbinom(5, 12, 0.7)
pbinom(5, 5, 0.7)

rbinom(10, 20, 0.7)

d_toss <-
    tibble(n_tosses = 5:20) |>
    mutate(
        likelihood_sim = n_tosses |> map_int(~sum(rbinom(10000, .x, 0.7) == 5)),
        likelihood_dist = dbinom(5, n_tosses, 0.7),
        probability_sim = likelihood_sim / sum(likelihood_sim),
        probability_dist = likelihood_dist / sum(likelihood_dist)
    )

d_toss |>
    pivot_longer(
        cols = c(-n_tosses, -matches("likelihood")),
        names_pattern = ("probability_(.+)"),
        names_to = "method",
        values_to = "p"
    )  |>
    ggplot(aes(n_tosses, p)) +
    geom_col(aes(fill = method), position = "dodge")
