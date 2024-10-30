import pandas as pd
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

# %%
