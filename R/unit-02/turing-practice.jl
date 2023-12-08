using DataFrames, Turing, CSV, Random, StatsBase, LaTeXStrings, StatsPlots
import StatisticalRethinking as SR

d = DataFrame(CSV.File(SR.sr_datadir("milk.csv"), missingstring="NA"))

describe(d)

d.K = standardize(ZScoreTransform, d.kcal_per_g)
d.M = standardize(ZScoreTransform, log.(d.mass))
d.N = d.neocortex_perc
non_miss = findall(!ismissing, d.N)
d[non_miss, :N] = standardize(ZScoreTransform, disallowmissing(d.N[non_miss]))

dcc = d[completecases(d[!, [:K, :N, :M]]), :]

@model function g5_01(N, K)
    a ~ Normal(0, 1)
    bN ~ Normal(0, 1)
    sigma ~ Exponential(1)
    K ~ MvNormal(a .+ bN .* N, sigma)
end

m5_01 = g5_01(dcc.N, dcc.K)
m5_01_prior = sample(m5_01, Prior(), 1000)
m5_01_prior_df = DataFrame(m5_01_prior)

μ = SR.link(m5_01_prior_df, [:a, :bN], [-2, 2])
μ = hcat(μ...)

p1 = plot(; xlim = [-2, 2], ylim = [-2, 2],
    xlab = "ncp (std)", ylab = "kcal per g (std)",
    title = L"\alpha \sim \mathcal{N}(0, 1), \beta \sim \mathcal{N}(0, 1)"
)

for y ∈ first(eachrow(μ), 50)
    plot!(p1, [-2, 2], y; c=:black, alpha = 0.3, label=false)
end


@model function g5_02(N, K)
    a ~ Normal(0, .2)
    bN ~ Normal(0, .5)
    sigma ~ Exponential(1)
    K ~ MvNormal(a .+ bN .* N, sigma)
end

m5_02 = g5_02(dcc.N, dcc.K)
spr5_02 = sample(m5_02, Prior(), 1000) 
spr5_02_df = DataFrame(spr5_02)
μ_02 = SR.link(spr5_02_df, [:a, :bN], [-2, 2])
μ_02 = hcat(μ_02...)

p2 = plot(; xlim = [-2, 2], ylim = [-2, 2],
    xlab = "ncp (std)", ylab = "kcal per g (std)",
    title = L"\alpha \sim \mathcal{N}(0, 0.2), \beta \sim \mathcal{N}(0, 0.5)"
)

for y ∈ first(eachrow(μ_02), 50)
    plot!(p2, [-2, 2], y; c=:black, alpha = 0.3, label = false)
end

plot(p1, p2)

spo5_02 = sample(m5_02, NUTS(), 1000)
spo5_02_df = DataFrame(spo5_02)

extrema(dcc.N)
x = range(extrema(dcc.N)...; length = 30)
μ5_02 = SR.link(spo5_02_df, [:a, :bN], x)
μ5_02 = hcat(μ5_02...)

μ5_02_mean = mean.(eachcol(μ5_02))
μ5_02_PI = SR.PI.(eachcol(μ5_02))
μ5_02_PI = vcat(μ5_02_PI'...)

p3 = plot(; 
    title = "Counterfactual holding M=0", 
    xlab="ncp (std)", ylab = "kcal per g (std)"
)

plot!(p3, x, [μ5_02_mean, μ5_02_mean]; lw=2, fillrange=μ5_02_PI, fillalpha=.1, color=:black)

