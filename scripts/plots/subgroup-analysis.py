import pandas as pd
import seaborn as sns

# %% get data

pv_data = pd.read_csv('data/pv-conjoint.csv')
heat_data = pd.read_csv('data/heat-conjoint.csv')

# aggregate into push and pull attributes
coerciveness = {
    "push": ["ban", "tax", "pv"], 
    "pull": ["heatpump", "tradeoffs", "distribution"]
}

levels_none = {
    "tax": "0%", 
    "ban": "none", 
    "pv": "none", 
    "tradeoffs": "none",
    "distribution": "none"
}


# %%
