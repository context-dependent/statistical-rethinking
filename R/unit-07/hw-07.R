library(cmdstanr)
library(tidyverse)
library(tidybayes)
library(posterior)

data(bangladesh, package = "rethinking")
d <- bangladesh |>
    rename(
        safe = use.contraception,
        age_z = age.centered,
        kids = living.children
    ) |>
    mutate(
        district = factor(district),
        kids = factor(kids)
    ) |>
    as_tibble()

d |>
    count(kids)

d_stan <- compose_data(d)

# 1. Direct effect of urbanity on contraception use
# - Lecture code estimates the _total_ effect of urban by stratifying on district.
# - Here, we estimate the _direct_ effect by stratifying additionally on number of children.

mh0701rt <- rethinking::ulam(
    alist(
        safe ~ bernoulli_logit(theta),
        theta <- a[district] +
            b[district] * urban,
        transpars > vector[61]:a <<- a_bar + v[, 1],
        transpars > vector[61]:b <<- b_bar + v[, 2],
        transpars > matrix[61, 2]:v <- compose_noncentered(sigma, L_Rho, Z),
        matrix[2, 61]:Z ~ normal(0, 1),
        a_bar ~ normal(0, 1),
        b_bar ~ normal(0, 1),
        cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky(4),
        vector[2]:sigma ~ exponential(1),
        gq > matrix[2, 2]:Rho <<- Chol_to_Corr(L_Rho)
    ),
    data = d_stan, chains = 4, cores = 4, iter = 1000
)

rethinking::stancode(mh0701rt)

rethinking::traceplot(mh0701rt, pars = c("sigma[1]", "sigma[2]"))
rethinking::precis(mh0701rt, pars = c("a_bar", "b_bar"))
mh01 <- cmdstan_model("R/unit-07/mh0701.stan")
fh01 <- mh01$sample(data = d_stan, seed = 123, chains = 4, parallel_chains = 4)

fh01$draws() |>
    bayesplot::mcmc_trace(pars = c("sigma[1]", "sigma[2]", "b_bar"))

fh01 |>
    recover_types(d) |>
    spread_rvars(b_kids, s_b_kids[kids]) |>
    mutate(kid_effect = b_kids * s_b_kids) |>
    ggplot(aes(xdist = kid_effect, y = kids)) +
    stat_halfeye()

fh01 |>
    recover_types(d) |>
    spread_rvars(a_bar, b_bar) |>
    expand_grid(urban = c(1, 0)) |>
    mutate(
        theta = a_bar + b_bar * urban,
        p = exp(theta) / (1 + exp(theta))
    ) |>
    select(urban, p) |>
    pivot_wider(names_from = urban, values_from = p) |>
    mutate(
        diff = `1` - `0`
    ) |>
    pivot_longer(cols = everything(), names_to = "urban", values_to = "p") |>
    ggplot(aes(xdist = p, y = 1, fill = urban)) +
    stat_halfeye(alpha = .7) +
    scale_x_continuous(limits = c(0, 1))


# 2. Direct effect of kids on contraception use
# - can use same model as above, just look at kids coefs

obs <- d |>
    group_by(kids) |>
    summarize(p_safe = mean(safe))

fh01 |>
    recover_types(d) |>
    spread_rvars(a_bar, b_kids, s_b_kids[kids]) |>
    mutate(
        theta = a_bar + b_kids * s_b_kids,
        p = exp(theta) / (1 + exp(theta))
    ) |>
    ggplot(aes(
        x = kids
    )) +
    stat_halfeye(aes(
        ydist = p,
    )) +
    geom_point(aes(
        y = p_safe
    ), data = obs, color = "red", size = 3, shape = 21) +
    scale_y_continuous(limits = c(0, 1))

# Model says:
# A The effect of kids on contraception use is positive overall.
# B The biggest jump is from 1 to 2 kids.
# C The effect of kids increases monotonically with the number of kids

# Data Says:
# A and B are both intuitive and borne out in the data,
# but C isn't reflected.
# Instead, we see a slight drop from 3 to 4 kids in the observed likelihood of contraceptive use.
# I'd venture a couple of explanatory guesses:
# - The effect of additional kids isn't additive
# - '4' kids actually means '4+' kids, collecting all mothers with 4 or more kids
# - The effect is confounded by unobserved variables (e.g. income, education, etc.)
# - At some point, the effect of children on the mothers preferences is outweighed
#   by the effect of the mother's preferences (or family's, community's, etc.)
#   on the number of children she has.

# 3. Letting the effect of kids vary by district

mh03 <- cmdstan_model("R/unit-07/mh0703.stan")
fh03 <- mh03$sample(data = d_stan, seed = 123, chains = 4, parallel_chains = 4)

fh03 |>
    recover_types(d) |>
    spread_rvars(b_kids[district]) |>
    ggplot(aes(xdist = b_kids, y = district)) +
    stat_pointinterval()


fh03 |>
    recover_types(d) |>
    spread_rvars(b_kids[district]) |>
    median_hdci(b_kids) |>
    arrange(b_kids) |>
    mutate(district = fct_inorder(district)) |>
    ggplot(aes(x = district, y = b_kids)) +
    geom_point()

fh03 |>
    recover_types(d) |>
    spread_rvars(a[district], b_kids[district]) |>
    expand_grid(four_kids = c(0, 1)) |>
    mutate(
        theta = a + b_kids * four_kids,
        p = exp(theta) / (1 + exp(theta)),
        district = fct_reorder(district, median(theta))
    ) |>
    ggplot(aes(x = district, ydist = p, color = factor(four_kids))) +
    stat_pointinterval(point_interval = "median_hdci", position = position_dodge())

# Model Says:
# - The effect of kids varies quite a bit by district
# - The above plot implies a positive association between
#   baseline contraceptive use and the effect of kids on contraception use.
#   In places where contraception use is high, having more kids has a bigger effect.
# - This makes sense, as unobserved variation in cultural norms and
#   family size are likely driving both.
