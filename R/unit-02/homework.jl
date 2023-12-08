using Distributions
using StatsPlots
using StatsBase
using CSV
using LinearAlgebra
using Random
using DataFrames
using StatisticalRethinking
using StatisticalRethinking: link
using Turing
using Query

function generate_height_from_age(
    age::Vector{Int64},
    b_ah::Float64=5.0,
    b_aw::Float64=0.1,
    b_hw::Float64=0.5,
    sigma::Float64=2.0
)
    mu_height = age .+ (b_ah .* age)
    sim_height = 70 .+ rand.(Normal.(mu_height, sigma))
    mu_weight = 2 .+ (b_hw .* sim_height) .+ (b_aw .* age)
    sim_weight = rand.(Normal.(mu_weight, sigma))
    DataFrame(
        age=age,
        height=sim_height,
        weight=sim_weight
    )
end

s02 = generate_height_from_age(rand(0:12, 100))

s02

@model function m02_01(age, weight)
    sigma ~ Uniform(0, 50)
    b ~ Normal(0, 100)
    a ~ Normal(65, 120)
    mu = a .+ b .* age
    return weight ~ MvNormal(mu, sigma)
end

m02_01_sim_samples = sample(m02_01(s02.age, s02.weight), NUTS(), 1000)

plot(m02_01_sim_samples)

m02_01_sim_post = DataFrame(sample(m02_01_sim_samples, 100)) |>
                  @mutate(age = rand(0:12)) |>
                  @mutate(predicted_weight = rand.(Normal.(_.a .+ (_.b * _.age), _.sigma))) |>
                  DataFrame

m02_01_sim_post

p = @df s02 scatter(:age, :weight; alpha=0.3)

@df m02_01_sim_post scatter!(:age, :predicted_weight; c="red", alpha=0.3)

for row in eachrow(m02_01_sim_post)
    plot!(x -> row.a + row.b * x; c="black", alpha=0.01, label=false)
end

display(p)

d = DataFrame(CSV.File(sr_datadir("Howell1.csv")))

d_u13 = d |>
        @filter(_.age < 13) |>
        @mutate(sex = _.male + 1) |>
        DataFrame

m02_01_samples = @df d_u13 sample(m02_01(:age, :weight), NUTS(), 1000)

m02_01_post = DataFrame(sample(m02_01_samples, 250)) |>
              @mutate(age = rand(0:12)) |>
              @mutate(predicted_weight = rand.(Normal.(_.a .+ (_.b * _.age), _.sigma))) |>
              DataFrame

p = @df d_u13 scatter(:age, :weight; alpha=0.3, label="data")

@df m02_01_post scatter!(:age .+ 0.2, :predicted_weight; alpha=0.3, c="red", label="prediction")

for row in eachrow(m02_01_post)
    plot!(x -> row.a + row.b * x; c="black", alpha=0.01, label=false)
end

display(p)

m02_01_samples

# Question 3

function gen_as(
    n::Int64,
    a_h_s=[50.0, 51.0],
    a_w_s=[5.0, 5.5],
    b_ah_s=[2.0, 2.1],
    b_aw_s=[0.1, 0.15],
    b_hw_s=[0.4, 0.6],
    sigma::Float64=2.0
)
    s = rand([1, 2], n)
    age = rand(0:12, n)
    mu_height = a_h_s[s] .+ (b_ah_s[s] .* age)
    sim_height = rand.(Normal.(mu_height, sigma))
    mu_weight = a_w_s[s] .+ (b_hw_s[s] .* sim_height) .+ (b_aw_s[s] .* age)
    sim_weight = rand.(Normal.(mu_weight, sigma))
    DataFrame(
        sex=s,
        age=age,
        height=sim_height,
        weight=sim_weight
    )
end

g03 = gen_as(200)

@df g03 scatter(:age, :weight, color = :sex)

@model function f03(age, sex, weight)
    sigma ~ Uniform(0, 50)
    a ~ MvNormal([75, 75], 20)
    b ~ MvNormal([0, 0], 10)
    mu = @. a[sex] + b[sex] * age
    return weight ~ MvNormal(mu, sigma)
end

mod03 = @df g03 f03(:age, :sex, :weight)
chn03 = sample(mod03, NUTS(), 200)
sam03 = DataFrame(chn03)


pred03 = DataFrame(age = rand(0:12, 200)) 

pred03.mu_2 = @. sam03[:, "a[2]"] + sam03[:, "b[2]"] * pred03.age
pred03.mu_1 = @. sam03[:, "a[1]"] + sam03[:, "b[1]"] * pred03.age
pred03.sigma = sam03.sigma

pred03_02 = DataFrames.stack(pred03, [:mu_1, :mu_2])

pred03_02.sex = zeros(Int64, 400)

for (i, v) in enumerate(pred03_02.variable)
    pred03_02.sex[i] = parse(Int64, v[4])
end

pred03_03 = pred03_02 |>
    @rename(:value => :mu_weight) |>
    @mutate(predicted_weight = rand.(Normal.(_.mu_weight, _.sigma))) |>
    DataFrame

p = @df g03 scatter(:age, :weight, color=:sex, label=false)

for (a, b) in zip(sam03[:, "a[1]"], sam03[:, "b[1]"])
    plot!(x -> a + b * x, c="black", alpha = .01, label=false)
end

for (a, b) in zip(sam03[:, "a[2]"], sam03[:, "b[2]"])
    plot!(x -> a + b * x, c="black", alpha = .01, label=false)
end

@df pred03_03 scatter!(:age .+ .2, :predicted_weight, color=:sex, label = false, alpha=0.3)

display(p)

mod03_d = f03(d_u13.age, d_u13.sex, d_u13.weight)
chn03_d = sample(mod03_d, NUTS(), 1000)
sam03_d = DataFrame(chn03_d)


pred03_d = DataFrame(age = rand(0:12, 1000)) 

pred03_d.mu_2 = @. sam03_d[:, "a[2]"] + sam03_d[:, "b[2]"] * pred03_d.age
pred03_d.mu_1 = @. sam03_d[:, "a[1]"] + sam03_d[:, "b[1]"] * pred03_d.age
pred03_d.sigma = sam03_d.sigma

pred03_d_02 = DataFrames.stack(pred03_d, [:mu_1, :mu_2])

pred03_d_02.sex = zeros(Int64, 2000)

for (i, v) in enumerate(pred03_d_02.variable)
    pred03_d_02.sex[i] = parse(Int64, v[4])
end

pred03_d_03 = pred03_d_02 |>
    @rename(:value => :mu_weight) |>
    @mutate(predicted_weight = rand.(Normal.(_.mu_weight, _.sigma))) |>
    DataFrame

p = @df d_u13 scatter(:age, :weight, color=:sex, label=false)

for (a, b) in zip(sam03_d[:, "a[1]"], sam03_d[:, "b[1]"])
    plot!(x -> a + b * x, c="black", alpha = .01, label=false)
end

for (a, b) in zip(sam03_d[:, "a[2]"], sam03_d[:, "b[2]"])
    plot!(x -> a + b * x, c="black", alpha = .01, label=false)
end

@df pred03_d_03 scatter!(:age .+ .2 .* :sex, :predicted_weight, color=:sex, label = false, alpha=0.2)

display(p)

sam03_d.a_c = sam03_d[:, "a[2]"] .- sam03_d[:, "a[1]"]
sam03_d.b_c = sam03_d[:, "b[2]"] .- sam03_d[:, "b[1]"]


@df sam03_d scatter(age_d, :a_c .+ :b_c .* age_d)

age_d = rand(0:12, 1000)


for (a, b) in zip(sam03_d[:, "a_c"], sam03_d[:, "b_c"])
    plot!(x -> a + b * x, c="black", alpha = .1, label=false)
end

display(p)