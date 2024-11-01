import pandas as pd
import numpy as np
import seaborn as sns
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.cm import get_cmap

# %% get data

df_pv = pd.read_csv('data/pv_justice_class_MMs.csv')
df_heat = pd.read_csv('data/heat_justice_class_MMs.csv')

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

def combine_coerciveness_mm(df_pv, df_heat, coerciveness, levels_none):
    # combine the dataframes
    df_combined = pd.concat([df_pv, df_heat], ignore_index=True)
    
    # define policy type based on 'coerciveness' dictionary
    def get_policy_type(feature):
        if feature in coerciveness['push']:
            return 'push'
        elif feature in coerciveness['pull']:
            return 'pull'
        else:
            return np.nan
    
    df_combined['policy_type'] = df_combined['feature'].apply(get_policy_type)
    
    # determine if each row is a none level
    df_combined['none_level'] = df_combined.apply(
        lambda row: row['level'] == levels_none.get(row['feature'], ""), axis=1
    )

    df_combined['strong_level'] = df_combined.apply(
        lambda row: row['level'] == levels_strong.get(row['feature'], ""), axis=1
    )
    
    return df_combined

df_combined = combine_coerciveness_mm(df_pv, df_heat, coerciveness, levels_none)

# get average MM estimates
average_estimates = (
    df_combined
    .groupby(['BY', 'policy_type', 'none_level', 'strong_level'])['estimate']
    .mean()
    .reset_index()
    .rename(columns={'estimate': 'average_estimate'})
)

# %% plot average estimates per justice for push vs pull

# Define custom y-axis labels
average_estimates['y_label'] = average_estimates.apply(
    lambda row: (
        "Strong push policy in place" if row['policy_type'] == "push" and row['strong_level'] and not row['none_level'] else
        "Weak push policy in place" if row['policy_type'] == "push" and not row['strong_level'] and not row['none_level'] else
        "No push policy" if row['policy_type'] == "push" and row['none_level'] else
        "Pull policy in place" if row['policy_type'] == "pull" and not row['none_level'] else
        "No pull policy"
    ),
    axis=1
)

# Specify the custom order for the y-axis categories
y_order = [
    'Strong push policy in place', 
    'Weak push policy in place',
    'No push policy',
    'Pull policy in place', 
    'No pull policy'
]

colors = get_cmap('viridis', 3)
color_mapping = {category: colors(i) for i, category in enumerate(average_estimates['BY'].unique())}

# Set up the plot with Seaborn
sns.set(style="whitegrid")
plt.figure(figsize=(10, 6))  # Adjust figure size as needed

# Create the scatter plot
scatter_plot = sns.scatterplot(
    data = average_estimates,
    x = "average_estimate",
    y = "y_label",
    hue = "BY",
    palette = color_mapping,  
    size=[100] * len(average_estimates), 
    sizes=(100, 100),
    legend = "full"
)

# Adjust the y-ticks to the specified order
scatter_plot.set_yticks(y_order)
scatter_plot.set_yticklabels(y_order)

# Set labels and title
scatter_plot.set_xlabel("Average Marginal Means")
scatter_plot.set_ylabel("")
# plt.title("Average Estimate by Policy Type and None Level", fontsize=16)

# Position the legend in the top right corner
handles = [plt.Line2D([0], [0], marker='o', color='w', label=profile, markersize=10, markerfacecolor=color_mapping[profile]) 
           for profile in average_estimates['BY'].unique()]

plt.legend(handles=handles, title="Justice Profiles", loc='upper right', bbox_to_anchor=(1, 1), frameon=True)

# Show the plot
plt.show()

# %% get uncertainty too

def combine_coerciveness_mm(df_pv, df_heat, coerciveness, levels_none, levels_strong):
    # Combine the dataframes
    df_combined = pd.concat([df_pv, df_heat], ignore_index=True)
    
    # Define policy type based on 'coerciveness' dictionary
    def get_policy_type(feature):
        if feature in coerciveness['push']:
            return 'push'
        elif feature in coerciveness['pull']:
            return 'pull'
        else:
            return np.nan
    
    df_combined['policy_type'] = df_combined['feature'].apply(get_policy_type)
    
    # Determine if each row is a none level or strong level
    df_combined['none_level'] = df_combined.apply(
        lambda row: row['level'] == levels_none.get(row['feature'], ""), axis=1
    )

    df_combined['strong_level'] = df_combined.apply(
        lambda row: row['level'] == levels_strong.get(row['feature'], ""), axis=1
    )
    
    # Calculate variance from std.error
    df_combined['variance'] = df_combined['std.error'] ** 2
    
    # Calculate weighted mean and combined variance
    grouped = df_combined.groupby(['BY', 'policy_type', 'none_level', 'strong_level']).apply(
        lambda g: pd.Series({
            'average_estimate': np.average(g['estimate'], weights=1 / g['variance']),
            'combined_variance': 1 / np.sum(1 / g['variance'])
        })
    ).reset_index()
    
    # Calculate combined standard deviation from combined variance
    grouped['combined_std_dev'] = np.sqrt(grouped['combined_variance'])
    
    return grouped

df_combined = combine_coerciveness_mm(df_pv, df_heat, coerciveness, levels_none, levels_strong)

# %% with errorbars

# Define y-axis labels
df_combined['y_label'] = df_combined.apply(
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

# Set up the plot with Seaborn
sns.set(style="whitegrid")
plt.figure(figsize=(10, 6))

# Create the scatter plot
scatter_plot = sns.scatterplot(
    data = df_combined,
    x = "average_estimate",
    y = "y_label",
    hue = "BY",
    palette = color_mapping,
    legend = "full"
)

# Add error bars
for _, row in df_combined.iterrows():
    plt.errorbar(
        x = row["average_estimate"],
        y = row["y_label"],
        xerr = row["combined_std_dev"],
        fmt = 'o',
        color = color_mapping[row["BY"]],
        capsize = 4
    )

# Set y-ticks to match the custom order
scatter_plot.set_yticks(range(len(y_order)))
scatter_plot.set_yticklabels(y_order)

# Labels and legend
scatter_plot.set_xlabel("Average Marginal Means")
scatter_plot.set_ylabel("")

# Custom legend for 'BY' categories
handles = [
    plt.Line2D([0], [0], marker='o', color='w', label=profile, markersize=10, 
               markerfacecolor=color_mapping[profile]) 
    for profile in average_estimates['BY'].unique()
]
plt.legend(handles=handles, title="Justice Profiles", loc='upper right', bbox_to_anchor=(1, 1))

plt.show()
# %%
