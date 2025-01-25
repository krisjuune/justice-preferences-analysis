import pandas as pd
import numpy as np
import seaborn as sns
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.cm import get_cmap


# %% get data

mm_pv = pd.read_csv('data/pv_justice_class_MMs.csv')
mm_heat = pd.read_csv('data/heat_justice_class_MMs.csv')

# %% define push vs pull 

# aggregate into push and pull attributes
coerciveness = {
    "push": ["ban", "tax", "pv"], 
    "pull": ["heatpump", "tradeoffs", "distribution"]
}

levels_none = {
    "tax": "0%", 
    "ban": "No ban", 
    "pv": "No obligation", 
    "tradeoffs": "No tradeoffs",
    "distribution": "No agreed cantonal production requirements"
}

levels_strong = {
    "tax": "75%",
    "tax": "100%",
    "ban": "all",
    "pv": "all-non-residential",
    "pv": "all"
}

def get_coerciveness(mm, coerciveness, levels_none, levels_strong):

    def get_policy_type(feature):
        if feature in coerciveness['push']:
            return 'push'
        elif feature in coerciveness['pull']:
            return 'pull'
        else:
            return np.nan
    
    mm['policy_type'] = mm['feature'].apply(get_policy_type)
    
    # determine if each row is a none level
    mm['none_level'] = mm.apply(
        lambda row: row['level'] == levels_none.get(row['feature'], ""), axis=1
    )

    mm['strong_level'] = mm.apply(
        lambda row: row['level'] == levels_strong.get(row['feature'], ""), axis=1
    )

    # Calculate variance from std.error
    mm['variance'] = mm['std.error'] ** 2
    
    # Calculate weighted mean and combined variance
    grouped = mm.groupby(['BY', 'policy_type', 'none_level', 'strong_level']).apply(
        lambda g: pd.Series({
            'average_estimate': np.average(g['estimate'], weights=1 / g['variance']),
            'combined_variance': 1 / np.sum(1 / g['variance'])
        })
    ).reset_index()
    
    # Calculate combined standard deviation from combined variance
    grouped['combined_std_dev'] = np.sqrt(grouped['combined_variance'])
    
    return mm

mm_heat = get_coerciveness(mm_heat, coerciveness, levels_none, levels_strong)
mm_pv = get_coerciveness(mm_pv, coerciveness, levels_none, levels_strong)

mm_heat['experiment'] = 'Heating Decarbonisation'
mm_pv['experiment'] = 'Renewable Energy Deployment'

# Combine both datasets
mm_combined = pd.concat([mm_heat, mm_pv], ignore_index=True)


# %% get average MMs

#TODO get av std error

# get average MM estimates
average_estimates = (
    mm_combined
    .groupby(['BY', 'experiment', 'policy_type', 'none_level', 'strong_level'])['estimate']
    .mean()
    .reset_index()
    .rename(columns={'estimate': 'average_estimate'})
)

# %% define plot features

# Define y-axis labels
mm_combined['y_label'] = mm_combined.apply(
    lambda row: (
        "Strong push policy in place" if row['policy_type'] == "push" and row['strong_level'] and not row['none_level'] else
        "Weak push policy in place" if row['policy_type'] == "push" and not row['strong_level'] and not row['none_level'] else
        "No push policy" if row['policy_type'] == "push" and row['none_level'] else
        "Pull policy in place" if row['policy_type'] == "pull" and not row['none_level'] else
        "No pull policy"
    ),
    axis=1
)

# Define custom order for y-axis
y_order = [
    'Pull policy in place', 
    'No pull policy',
    'Weak push policy in place',
    'Strong push policy in place', 
    'No push policy'
]

# Map colors for each 'BY' category
colors = get_cmap('viridis', 3)
color_mapping = {category: colors(i) for i, category in enumerate(average_estimates['BY'].unique())}


