import pandas as pd
import numpy as np 
import plotly.express as px
import matplotlib.pyplot as plt
from functions.data_assist import apply_mapping, rename_columns

#%% reset working directory
import os
print(os.getcwd())
wd = '/Users/kristiinajoon/Documents/Projects/Qualtrics'
os.chdir(wd)



#%% ############################# read data ##################################

df = pd.read_csv('data/conjoint.csv')

# check data 
pd.set_option('display.max_columns', None) # displays all columns when printing parts of the df
columns = df.columns.tolist()
df = df.drop([0, 1]) # remove first two rows



# %% ############################# clean data ################################

# fix typos
df.rename(columns={'languge': 'language'}, inplace=True)

# fix data types
df['Finished'] = df['Finished'].replace(
    {'true': True, 'True': True, 'false': False, 'False': False}
).astype(bool)

columns_to_num = ['Duration (in seconds)', 
                  'household-size', 
                  'trust_1', 
                  'trust_2',
                  'trust_3', 
                  'satisfaction_1', 
                  'literacy6_5']
for col in columns_to_num:
    df[col] = pd.to_numeric(df[col], errors='coerce')

# add column for IDs and duration in min
df['ID'] = range(1, len(df) + 1)
df['duration_min'] = df['Duration (in seconds)'] / 60
df['duration_min'].round(3) # do I need to store it in df['dur...'] as well?

# filter out incompletes
df = df[df['DistributionChannel'] != 'preview'] # filter out previews
df = df[df['Finished'] == True] # filter out recorded incompletes
df = df.dropna(subset=['canton']) # filter out quota fulls

# speeders and laggards
# calculate the 5% and 99% quantiles
lower_threshold = df['duration_min'].quantile(0.05)
upper_threshold = df['duration_min'].quantile(0.99)
print(f"Lower threshold (lowest 5% quartile): {lower_threshold} minutes")
print(f"Upper threshold (highest 1% quartile): {upper_threshold} minutes")
df['speeder'] = df['duration_min'] < lower_threshold
df['laggard'] = df['duration_min'] > upper_threshold 

# inattentives based on justice section (exact same answer for all questions)
just_columns = ['justice-general_1', 'justice-tax_1', 'justice-subsidy_1', 
           'justice-general_2', 'justice-tax_2', 'justice-subsidy_2', 
           'justice-general_3', 'justice-tax_3', 'justice-subsidy_3', 
           'justice-general_4', 'justice-tax_4', 'justice-subsidy_4'
]
attention_mask = (df[just_columns].nunique(axis=1) == 1)
df['inattentive'] = attention_mask

# count the number of rows where the attention filters are True
print(f"Number of speeders (5% fastest): {df['speeder'].sum()}")
print(f"Number of laggards (1% slowest): {df['laggard'].sum()}")
print(f"Number of inattentive respondents: {df['inattentive'].sum()}")

# filter out rows of speeders, laggards, or inattentives
df_filtered = df[~((df['speeder'] == True) | 
                   (df['laggard'] == True) |
                   (df['inattentive'] == True)
                  )]

# remove non-functional empty columns 
empty_columns = [col for col in df.columns if col.endswith('_Table')]
df = df.drop(columns=empty_columns)

# rename columns for pv experiment
df = rename_columns(df, 'TargetMix', 'mix')
df = rename_columns(df, 'Imports', 'imports')
df = rename_columns(df, 'RooftopSolarPV', 'pv')
df = rename_columns(df, 'Infrastructure', 'tradeoffs')
df = rename_columns(df, 'Distribution', 'distribution')



# %% ########################## recode demographics ##############################

# recode likert scales in conjoints
numerical_values = [0, 1, 2, 3, 4, 5]
binary_values = [0, 0, 0, 1, 1, 1]
rating_values = ['Stark dagegen',
                 'Dagegen',
                 'Eher dagegen',
                 'Eher dafür',
                 'Dafür',
                 'Stark dafür']
rating_scale = np.array(list(zip(rating_values, binary_values)))
likert_dict = {**dict(rating_scale)}
df = apply_mapping(df, likert_dict, column_pattern=['justice', 'rating'])

#TODO recode demographic values

# gender

# age

# language region

# 


# %% ################################# recode justice ##############################

# recode justice likert scales
likert_values = ['Stimme überhaupt nicht zu', 
                 'Stimme nicht zu', 
                 'Stimme eher nicht zu', 
                 'Stimme eher zu', 
                 'Stimme zu', 
                 'Stimme voll und ganz zu']
likert_scale = np.array(list(zip(likert_values, numerical_values)))
justice_dict = {**dict(likert_scale)}
df_justice = apply_mapping(df, justice_dict, column_pattern=['justice', 'rating'])

# justice columns dictionary
justice_columns = {
    'utilitarian': ['justice-general_1', 'justice-tax_1', 'justice-subsidy_1'], 
    'egalitarian': ['justice-general_2', 'justice-tax_2', 'justice-subsidy_2'], 
    'sufficientarian': ['justice-general_3', 'justice-tax_3', 'justice-subsidy_3'], 
    'limitarian': ['justice-general_4', 'justice-tax_4', 'justice-subsidy_4']
}

# convert columns to numeric types
for just_columns in justice_columns.values():
    for col in just_columns:
        df_justice[col] = pd.to_numeric(df[col], errors='coerce')

# get mean for each key and append to dataframe
for key, just_columns in justice_columns.items():
    df[key] = df[just_columns].mean(axis=1).round(3)

# # for binary scale values, append sum to df
# for key, cjust_olumns in justice_columns.items():
#     df_justice[key] = df_justice[just_columns].sum(axis=1)

lpa_data = df_justice[['ID', 
                       'utilitarian', 
                       'egalitarian', 
                       'sufficientarian', 
                       'limitarian', 
                       'speeder', 
                       'laggard', 
                       'inattentive']]
lpa_data.to_csv('data/lpa_input.csv', index=False)

# now run lpa analysis

# %% ######################################### check sample #################################################

# basics
print(df.info())
print(df.describe())

# gender counts
fig = px.histogram(df, x="gender", histnorm='percent')
fig.show()

# age counts
fig = px.histogram(df, x="age", histnorm='percent')
fig.show()

# language region
fig = px.histogram(df, x="region", histnorm='percent')
fig.show()

#TODO make list of descriptions
