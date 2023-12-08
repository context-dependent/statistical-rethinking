using DataFrames, Turing, CSV, Random, StatsBase, LaTeXStrings, StatsPlots
import StatisticalRethinking as SR

d = DataFrame(CSV.File(SR.sr_datadir("oxboys.csv"), delim=";"))
n = size(d, 1)

d.delta = zeros(Float64, n)
for i in 2:n
    if d.Occasion[i] > 1 
        d.delta[i] = d.height[i] - d.height[i-1]
    end
end

d2 = d[d.Occasion .> 1, :]

d2.delta_std = standardize(ZScoreTransform, d2.delta)

