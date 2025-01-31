import pandas as pd 
from functions.conjoint_assist import calculate_IRR

# %% read data

df_heat = pd.read_csv('data/heat-conjoint.csv')
df_pv = pd.read_csv('data/pv-conjoint.csv')

amce_heat = pd.read_csv('data/heat-amce.csv')
amce_pv = pd.read_csv('data/pv-amce.csv')

# %% run IRRs

amce_heat = calculate_IRR(df_heat, amce_heat)