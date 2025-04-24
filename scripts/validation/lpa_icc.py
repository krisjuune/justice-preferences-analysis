import pandas as pd
import pingouin as pg
import numpy as np

# %% import data

lpa_data = pd.read_csv("data/lpa_input_full.csv")

lpa_data = lpa_data[(lpa_data['speeder'] == False) & 
              (lpa_data['laggard'] == False) & 
              (lpa_data['inattentive'] == False)]

# %% check ICC for justice principles

df_long = lpa_data.melt(id_vars=['ID'], 
                        value_vars=['justice_general_1', 'justice_tax_1', 'justice_subsidy_1',
                                    'justice_general_2', 'justice_tax_2', 'justice_subsidy_2',
                                    'justice_general_3', 'justice_tax_3', 'justice_subsidy_3',
                                    'justice_general_4', 'justice_tax_4', 'justice_subsidy_4'],
                        var_name='variable', value_name='score')

# Extract principle number from variable name (1, 2, 3, 4)
df_long['principle'] = df_long['variable'].str.extract(r'(\d+)')

icc_results = pg.intraclass_corr(data=df_long, 
                                 targets='ID', 
                                 raters='principle', 
                                 ratings='score')

print(icc_results)

# %% cronbach's alpha

def cronbach_alpha(df):
    # df should be a dataframe where each column is a measurement/item for a principle
    item_scores = df.values
    item_variances = item_scores.var(axis=0, ddof=1)
    total_score_var = item_scores.sum(axis=1).var(ddof=1)
    n_items = df.shape[1]
    
    return (n_items / (n_items - 1)) * (1 - item_variances.sum() / total_score_var)

alpha_results = {}
for principle in df_long['principle'].unique():
    # Pivot to create one column per variable (item) for each principle
    df_pivot = df_long[df_long['principle'] == principle].pivot(index='ID', columns='variable', values='score')
    alpha_results[principle] = cronbach_alpha(df_pivot)

alpha_results

# %% save

icc_results.to_csv("output/icc_results.csv")

# %%
