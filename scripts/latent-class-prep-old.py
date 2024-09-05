import pandas as pd
import numpy as np 
import plotly.express as px
import matplotlib.pyplot as plt
import seaborn as sns
from functions.data_assist import apply_mapping

# %% clean data for LPA

#TODO remove responses that answered the same for all justice questions in data prep, 
# add it instead as column called inattentive with values 0 or 1 

columns = ['justice-general_1', 'justice-tax_1', 'justice-subsidy_1', 
           'justice-general_2', 'justice-tax_2', 'justice-subsidy_2', 
           'justice-general_3', 'justice-tax_3', 'justice-subsidy_3', 
           'justice-general_4', 'justice-tax_4', 'justice-subsidy_4'
]

# create a Boolean mask where all specified columns have the same value
mask = (df[columns].nunique(axis=1) == 1)

# count the number of rows where the mask is True
count_same_values = mask.sum()
print(f"Number of respondents who answered the same for all justice questions: {count_same_values}")

# filter the DataFrame to exclude rows where the mask is True
df = df[~mask]

# %% ################################### recode justice #############################################
numerical_values = [0, 1, 2, 3, 4, 5]
binary_values = [0, 0, 0, 1, 1, 1]

# these are used in the justice section
likert_values = ['Stimme überhaupt nicht zu', 
                 'Stimme nicht zu', 
                 'Stimme eher nicht zu', 
                 'Stimme eher zu', 
                 'Stimme zu', 
                 'Stimme voll und ganz zu']
likert_scale = np.array(list(zip(likert_values, binary_values)))

# create a dictionary to factorise values
justice_dict = {**dict(likert_scale)}

df_justice = apply_mapping(df, justice_dict, column_pattern=['justice', 'rating'])

# %% get scores for principles 

# justice columns dictionary
justice_columns = {
    'utilitarian': ['justice-general_1', 'justice-tax_1', 'justice-subsidy_1'], 
    'egalitarian': ['justice-general_2', 'justice-tax_2', 'justice-subsidy_2'], 
    'sufficientarian': ['justice-general_3', 'justice-tax_3', 'justice-subsidy_3'], 
    'limitarian': ['justice-general_4', 'justice-tax_4', 'justice-subsidy_4']
}

# convert columns to numeric types
for columns in justice_columns.values():
    for col in columns:
        df_justice[col] = pd.to_numeric(df[col], errors='coerce')

# # get mean for each key and append to dataframe
# for key, columns in justice_columns.items():
#     df[key] = df[columns].mean(axis=1)

# for binary scale values
for key, columns in justice_columns.items():
    df_justice[key] = df_justice[columns].sum(axis=1)

#%% explore

columns = ['utilitarian', 'egalitarian', 'sufficientarian', 'limitarian']

# histograms
for column in columns:
    fig = px.histogram(df_justice, x=column, nbins=6, title=f'Histogram of {column}', labels={column: column})
    fig.update_layout(xaxis_title=column, yaxis_title='Count')
    fig.show()

# scatter matric plot
fig = px.scatter_matrix(df_justice, dimensions=['utilitarian', 'egalitarian', 'sufficientarian', 'limitarian'],
                        title='Scatter Matrix of Categories')
fig.show()

# seaborn pairplot
sns.pairplot(df_justice[['utilitarian', 'egalitarian', 'sufficientarian', 'limitarian']])
plt.suptitle('Pair Plot of Categories', y=1.02)
plt.show()

# correlation heatmaps
correlation_matrix = df_justice[['utilitarian', 'egalitarian', 'sufficientarian', 'limitarian']].corr()
plt.figure(figsize=(8, 6))
sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', vmin=-1, vmax=1, center=0)
plt.title('Correlation Heatmap of Categories')
plt.show()

# linear regressions between pairs
for i in range(len(columns)):
    for j in range(i + 1, len(columns)):
        col_x = columns[i]
        col_y = columns[j]
        fig = px.scatter(df_justice, x=col_x, y=col_y, trendline='ols', 
                         title=f'Regression between {col_x} and {col_y}', 
                         labels={col_x: col_x, col_y: col_y})
        fig.update_layout(xaxis_title=col_x, yaxis_title=col_y)
        fig.show()

#%% save lca data for running in R

lpa_data = df_justice[['ID', 'utilitarian', 'egalitarian', 'sufficientarian', 'limitarian']]
lpa_data.to_csv('data/lpa_data.csv', index=False)
