using DataFrames, CSV, GLM, Statistics, Plots, StatsPlots, MLBase;

# Load data
d = DataFrame(CSV.File("R/unit-05/penguins.csv"))

m = lm(@formula(bill_length_z ~ 0 + bill_depth_z), d)


