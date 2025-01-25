import pandas as pd
import numpy as np
import seaborn as sns
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.cm import get_cmap

# %% get data

df_pv = pd.read_csv('data/pv_justice_class_MMs.csv')
df_heat = pd.read_csv('data/heat_justice_class_MMs.csv')


########################### utilitarian vs non ########################

# %% define util packages

# define util packages
levels_util_heat = {
    "year": "2050",
    "tax": "100%", 
    "tax": "75%",
    "ban": "No ban", 
    "heatpump": "Subsidy",
    "exemptions": "none"
}

levels_util_pv = {
    "imports": "30%",
    "imports": "20%",
    "pv": "No obligation", 
    "tradeoffs": ["Lakes", "Rivers", "Forests", "Agriculture", "Alpine"],
    "distribution": "No agreed cantonal production requirements"
}

# Function to classify policy packages as utilitarian or non-utilitarian
def classify_policy(df, util_levels):
    # Function to classify each row as Utilitarian or Non-Utilitarian
    def is_utilitarian(row):
        feature = row['feature']
        level = row['level']
        
        if feature in util_levels:
            expected_value = util_levels[feature]
            if isinstance(expected_value, list):  # Handle multiple possible values
                return 'Utilitarian' if level in expected_value else 'Non-Utilitarian'
            else:
                return 'Utilitarian' if level == expected_value else 'Non-Utilitarian'
        return 'Non-Utilitarian'

    # Apply classification
    df['policy_package'] = df.apply(is_utilitarian, axis=1)
    return df

# Apply classification to heating decarbonisation data
df_heat_classified = classify_policy(df_heat, levels_util_heat)
df_heat_classified['experiment'] = 'Heating Decarbonisation'

# Apply classification to renewable energy deployment data
df_pv_classified = classify_policy(df_pv, levels_util_pv)
df_pv_classified['experiment'] = 'Renewable Energy Deployment'

# Combine both datasets
df_combined = pd.concat([df_heat, df_pv], ignore_index=True)

# %% calculate MMs and std error

# Calculate average marginal means and standard error
average_estimates = (
    df_combined
    .groupby(['BY', 'experiment', 'policy_package'])
    .agg(
        average_estimate=('estimate', 'mean'),
        std_error=('estimate', lambda x: np.std(x, ddof=1) / np.sqrt(len(x)))  # Standard error calculation
    )
    .reset_index()
)

# Calculate confidence intervals (95% CI using 1.96 multiplier)
average_estimates['ci_lower'] = average_estimates['average_estimate'] - 1.96 * average_estimates['std_error']
average_estimates['ci_upper'] = average_estimates['average_estimate'] + 1.96 * average_estimates['std_error']

# Calculate variance from standard error
average_estimates['variance'] = average_estimates['std_error'] ** 2

# Compute weighted average estimate
average_estimates = average_estimates.groupby(['BY', 'experiment', 'policy_package']).apply(
    lambda g: pd.Series({
        'average_estimate': np.average(g['average_estimate'], weights=1 / g['variance']),
        'combined_variance': 1 / np.sum(1 / g['variance'])
    })
).reset_index()

# Calculate standard deviation from combined variance
average_estimates['combined_std_dev'] = np.sqrt(average_estimates['combined_variance'])

print(average_estimates)


# %% define plot

colors = get_cmap('viridis', 3)
color_mapping = {category: colors(i) for i, category in enumerate(average_estimates['BY'].unique())}

# get specified order for y axis
y_order = [
    "Heating Decarbonisation - Utilitarian", 
    "Heating Decarbonisation - Non-Utilitarian",
    "Renewable Energy Deployment - Utilitarian", 
    "Renewable Energy Deployment - Non-Utilitarian"
]

average_estimates['y_label'] = average_estimates.apply(
    lambda row: f"{row['experiment']} - {row['policy_package']}", axis=1
)

average_estimates['y_label'] = pd.Categorical(
    average_estimates['y_label'],
    categories=y_order, 
    ordered=True
)

profile_offsets = {
    'egalitarian': -0.15,
    'universal': 0.0,
    'utilitarian': 0.15
}
# Convert categorical y-axis labels to numeric positions and add offset
average_estimates['y_offset'] = average_estimates['BY'].map(profile_offsets)
average_estimates['y_numeric'] = (
    average_estimates['y_label'].astype(str).map(lambda x: y_order.index(x))
    + average_estimates['y_offset']
)

# %% make plot 

# Set up the plot
sns.set(style="whitegrid")
plt.figure(figsize=(10, 6))

# Plot the scatterplot with adjusted y positions
scatter_plot = sns.scatterplot(
    data=average_estimates,
    x="average_estimate",
    y="y_numeric",  # Use numeric y-axis with offset applied
    hue="BY",
    palette=color_mapping, 
    size=[100] * len(average_estimates),
    sizes=(100, 100),
    legend="full"
)

# Adjust the y-axis labels back to the original categories
scatter_plot.set_yticks(range(len(y_order)))
scatter_plot.set_yticklabels(y_order)

# Set axis labels
scatter_plot.set_xlabel("Average Marginal Means")
scatter_plot.set_ylabel("Experiment and Policy Package")

# Apply the offset in the plot
for _, row in average_estimates.iterrows():
    plt.errorbar(
        x=row["average_estimate"],
        y=y_order.index(row["y_label"]) + row["y_offset"],  # Add offset
        xerr=row["combined_std_dev"],
        fmt='o',
        color=color_mapping[row["BY"]],
        capsize=4
    )

# legend
handles = [
    plt.Line2D([0], [0], marker='o', color='w', label=profile, markersize=10, 
               markerfacecolor=color_mapping[profile]) 
    for profile in average_estimates['BY'].unique()
]
plt.legend(handles=handles, title="Justice Profiles", loc='upper right', bbox_to_anchor=(1.2, 1))


plt.show()
# %%

#TODO format plot either as subplot or with side separators for the 
# two experiments

#TODO think about errorbars and why the std so large here compared 
# to push pull