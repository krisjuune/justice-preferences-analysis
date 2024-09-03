import pandas as pd
import numpy as np 
import plotly.express as px
import matplotlib.pyplot as plt
from functions.data-assist import apply_mapping, rename_columns

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

# filter out incompletes
df = df[df['DistributionChannel'] != 'preview'] # filter out previews
df = df[df['Finished'] == True] # filter out recorded incomplete responses
df = df.dropna(subset=['canton']) # filter out quota fulls

# filter out speeders and very slow responses
# calculate the 5% and 99% quantiles
lower_threshold = df['duration_min'].quantile(0.05)
upper_threshold = df['duration_min'].quantile(0.99)

# print the thresholds
print(f"Lower threshold (2.5% quartile): {lower_threshold} minutes")
print(f"Upper threshold (97.5% quartile): {upper_threshold} minutes")
below_lower_threshold = df[df['duration_min'] < lower_threshold].shape[0]
above_upper_threshold = df[df['duration_min'] > upper_threshold].shape[0]
print(f"Number of responses above the upper threshold: {above_upper_threshold}")
print(f"Number of responses below the lower threshold: {below_lower_threshold}")

# filter out speeders (fastest 5%) and very slow respondants (slowest 1%)
df = df[(df['duration_min'] >= lower_threshold) & (df['duration_min'] <= upper_threshold)]

#TODO add columns for speeders and laggards with values 0 or 1 instead of filtering them out

# fig = px.histogram(df, x='duration_min', nbins=60, title='Histogram of Duration (in minutes)')
# fig.update_xaxes(title_text='Duration (minutes)')
# fig.update_yaxes(title_text='Frequency')
# fig.show()

# fig = px.histogram(df_time, x='duration_min', nbins=60, title='Histogram of Duration (in minutes)')
# fig.update_xaxes(title_text='Duration (minutes)')
# fig.update_yaxes(title_text='Frequency')
# fig.show()

# remove empty columns 
empty_columns = [col for col in df.columns if col.endswith('_Table')]
df = df.drop(columns=empty_columns)

# rename columns for pv experiment
df = rename_columns(df, 'TargetMix', 'mix')
df = rename_columns(df, 'Imports', 'imports')
df = rename_columns(df, 'RooftopSolarPV', 'pv')
df = rename_columns(df, 'Infrastructure', 'tradeoffs')
df = rename_columns(df, 'Distribution', 'distribution')

# %% ############################### recode ###################################
# factorise categorical data 

# recheck the scales used
# likert_scale = df['justice-general_1'].unique() # check the list of unique values
# rating_scale = df['1_heat-rating_1'].unique()

numerical_values = [0, 1, 2, 3, 4, 5]
binary_values = [0, 0, 0, 1, 1, 1]

# # these are used in the justice section
# likert_values = ['Stimme überhaupt nicht zu', 
#                  'Stimme nicht zu', 
#                  'Stimme eher nicht zu', 
#                  'Stimme eher zu', 
#                  'Stimme zu', 
#                  'Stimme voll und ganz zu']
# likert_scale = np.array(list(zip(likert_values, numerical_values)))

# these are used to rate the conjoints
rating_values = ['Stark dagegen',
                 'Dagegen',
                 'Eher dagegen',
                 'Eher dafür',
                 'Dafür',
                 'Stark dafür']
rating_scale = np.array(list(zip(rating_values, numerical_values)))

# create a dictionary to factorise values
likert_dict = {**dict(rating_scale)}

df = apply_mapping(df, likert_dict, column_pattern=['justice', 'rating'])

#TODO recode demographic values





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
