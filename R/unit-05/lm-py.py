import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression

# Read the data
d = pd.read_csv('penguins.csv')
x = d.bill_length_z.to_numpy()
y = d.bill_depth_z.to_numpy()

# Fit the model
m = LinearRegression(fit_intercept=False)
m.fit(x.reshape(-1, 1), y)